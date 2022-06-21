const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const activeTag = std.meta.activeTag;

const lexer = @import("lexer.zig");
const utils = @import("utils.zig");

const Value = @import("common.zig").Value;
const ASTNode = @import("common.zig").ASTNode;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const ASTNodePtrList = @import("common.zig").ASTNodePtrList;
const Program = @import("common.zig").Program;
const Op = @import("common.zig").Op;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

pub const Parser = struct {
    program: Program,
    alloc: mem.Allocator,

    const ParserError = error{
        StrayToken,
        EmptyList,
        ExpectedKeyword,
        ExpectedEnumLit,
        ExpectedOptionalNumber,
        ExpectedItems,
        ExpectedNode,
        UnexpectedItems,
        ExpectedValue,
        ExpectedStatement,
        UnknownKeyword,
        UnexpectedLabelDefinition,
        InvalidAsmOp,
    } || mem.Allocator.Error;

    pub fn init(alloc: mem.Allocator) Parser {
        return .{ .program = Program{
            .ast = ASTNodeList.init(alloc),
            .defs = ASTNodePtrList.init(alloc),
        }, .alloc = alloc };
    }

    fn validateListLength(ast: []const lexer.Node, require: usize) ParserError!void {
        if (ast.len < require) return error.ExpectedItems;
        if (ast.len > require) return error.UnexpectedItems;
    }

    fn expectNode(comptime nodetype: meta.Tag(lexer.Node.NodeType), node: *const lexer.Node) b: {
        break :b ParserError!@TypeOf(@field(node.node, @tagName(nodetype)));
    } {
        if (node.node != nodetype) {
            return error.ExpectedNode;
        }
        return @field(node.node, @tagName(nodetype));
    }

    fn parseValue(self: *Parser, node: *const lexer.Node) ParserError!Value {
        _ = self;
        return switch (node.node) {
            .T => .T,
            .Nil => .Nil,
            .Number => |n| .{ .Number = n },
            .String => |s| .{ .String = s },
            .Codepoint => |c| .{ .Codepoint = c },
            .EnumLit => |e| .{ .EnumLit = e },
            else => error.ExpectedValue,
        };
    }

    fn parseStatement(self: *Parser, node: *const lexer.Node) ParserError!ASTNode {
        return switch (node.node) {
            .List => |l| try self.parseList(l.items),
            .Quote => |q| blk: {
                const body = try self.parseStatements(q.items);
                break :blk ASTNode{
                    .node = .{ .Quote = .{ .body = body } },
                    .srcloc = node.location,
                };
            },
            .Keyword => |i| ASTNode{ .node = .{ .Call = i }, .srcloc = node.location },
            .Child => @panic("TODO"),
            else => ASTNode{ .node = .{ .Value = try self.parseValue(node) }, .srcloc = node.location },
        };
    }

    fn parseStatements(self: *Parser, nodes: []const lexer.Node) ParserError!ASTNodeList {
        var ast = ASTNodeList.init(self.alloc);
        for (nodes) |node|
            try ast.append(try self.parseStatement(&node));
        return ast;
    }

    fn parseList(self: *Parser, ast: []const lexer.Node) ParserError!ASTNode {
        if (ast.len == 0)
            return error.EmptyList;

        return switch (ast[0].node) {
            .Keyword => |k| b: {
                if (mem.eql(u8, k, "word")) {
                    const name = try expectNode(.Keyword, &ast[1]);

                    const ast_body = try expectNode(.Quote, &ast[2]);
                    const body = try self.parseStatements(ast_body.items);

                    break :b ASTNode{
                        .node = .{ .Decl = .{ .name = name, .body = body } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "until")) {
                    try validateListLength(ast, 3);

                    const ast_cond = try expectNode(.Quote, &ast[1]);
                    const cond = try self.parseStatements(ast_cond.items);

                    const ast_body = try expectNode(.Quote, &ast[2]);
                    const body = try self.parseStatements(ast_body.items);

                    break :b ASTNode{
                        .node = .{ .Loop = .{
                            .loop = .{ .Until = .{ .cond = cond } },
                            .body = body,
                        } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "asm")) {
                    try validateListLength(ast, 3);
                    const asm_stack = try self.parseValue(&ast[1]);
                    if (asm_stack != .Nil and asm_stack != .Number)
                        return error.ExpectedOptionalNumber;
                    const asm_op_kwd = try self.parseValue(&ast[2]);
                    if (asm_op_kwd != .EnumLit)
                        return error.ExpectedEnumLit;
                    const asm_op_e = meta.stringToEnum(Op.Tag, asm_op_kwd.EnumLit) orelse
                        return error.InvalidAsmOp;
                    const asm_op = Op.fromTag(asm_op_e) catch return error.InvalidAsmOp;
                    break :b ASTNode{
                        .node = .{ .Asm = .{ .stack = WK_STACK, .op = asm_op } },
                        .srcloc = ast[0].location,
                    };
                } else {
                    //std.log.info("Unknown keyword: {s}", .{k});
                    break :b error.UnknownKeyword;
                }
            },
            .List => |l| try self.parseList(l.items),
            else => try self.parseStatement(&ast[0]),
        };
    }

    // Extract definitions
    pub fn extractDefs(self: *Parser) ParserError!void {
        for (self.program.ast.items) |*node| if (node.node == .Decl) {
            try self.program.defs.append(node);
        };
    }

    pub fn parse(self: *Parser, lexed: *const lexer.NodeList) ParserError!Program {
        // Setup the entry function
        // TODO: this should be in codegen
        //
        // Add a dummy node that we'll replace later with the actual function.
        // Do this since we can't just use a pointer to the node, as appending
        // to program.ast will eventually invalidate the pointer.
        //
        try self.program.ast.append(ASTNode{ .node = .{
            .Value = .{ .Nil = {} },
        }, .srcloc = 0 });

        var main_func = ASTNodeList.init(self.alloc);

        for (lexed.items) |*node| switch (node.node) {
            .List => |l| try self.program.ast.append(try self.parseList(l.items)),
            else => try main_func.append(try self.parseStatement(node)),
        };

        // Exit the program
        // TODO: this should be in codegen
        try main_func.append(ASTNode{ .node = .{
            .Asm = .{ .stack = 0, .op = .Ohalt },
        }, .srcloc = 0 });
        // Replace the dummy node
        self.program.ast.items[0].node = .{ .Decl = .{ .name = "_start", .body = main_func } };

        try self.extractDefs();

        return self.program;
    }
};

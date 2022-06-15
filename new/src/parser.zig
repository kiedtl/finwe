const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const activeTag = std.meta.activeTag;

const lexer = @import("lexer.zig");

const Value = @import("common.zig").Value;
const ASTNode = @import("common.zig").ASTNode;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const ASTNodePtrList = @import("common.zig").ASTNodePtrList;
const Program = @import("common.zig").Program;

pub const Parser = struct {
    program: Program,
    alloc: mem.Allocator,

    const ParserError = error{
        StrayToken,
        EmptyList,
        ExpectedKeyword,
        ExpectedItems,
        ExpectedNode,
        UnexpectedItems,
        ExpectedValue,
        ExpectedStatement,
        UnknownKeyword,
        UnexpectedLabelDefinition,
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
        if (activeTag(node.node) != nodetype) {
            return error.ExpectedNode;
        }
        return @field(node.node, @tagName(nodetype));
    }

    fn parseValue(self: *Parser, node: *const lexer.Node) ParserError!Value {
        _ = self;
        return switch (node.node) {
            .Number => |n| .{ .Number = n },
            .String => |s| .{ .String = s },
            .Codepoint => |c| .{ .Codepoint = c },
            else => error.ExpectedValue,
        };
    }

    fn parseStatement(self: *Parser, node: *const lexer.Node) ParserError!ASTNode {
        return switch (node.node) {
            .List => |l| try self.parseList(l.items),
            .Keyword => |i| ASTNode{ .node = .{ .Call = i }, .srcloc = node.location },
            else => ASTNode{ .node = .{ .Value = try self.parseValue(node) }, .srcloc = node.location },
        };
    }

    fn parseList(self: *Parser, ast: []const lexer.Node) ParserError!ASTNode {
        if (ast.len == 0)
            return error.EmptyList;

        return switch (ast[0].node) {
            .Keyword => |k| b: {
                if (mem.eql(u8, k, "word")) {
                    const name = try expectNode(.Keyword, &ast[1]);

                    var body = ASTNodeList.init(self.alloc);
                    for (ast[1..]) |node|
                        try body.append(try self.parseStatement(&node));

                    break :b ASTNode{
                        .node = .{ .Decl = .{ .name = name, .body = body } },
                        .srcloc = ast[0].location,
                    };
                } else {
                    break :b error.UnknownKeyword;
                }
            },
            .List => |l| try self.parseList(l.items),
            else => try self.parseStatement(&ast[0]),
        };
    }

    pub fn extractDefs(self: *Parser) ParserError!void {
        for (self.program.ast.items) |*node| if (node.node == .Decl) {
            try self.program.defs.append(node);
        };
    }

    pub fn parse(self: *Parser, lexed: *const lexer.NodeList) ParserError!Program {
        for (lexed.items) |*node| switch (node.node) {
            .List => |l| try self.program.ast.append(try self.parseList(l.items)),
            else => try self.program.ast.append(try self.parseStatement(node)),
        };

        try self.extractDefs();

        return self.program;
    }
};

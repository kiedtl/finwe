const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const activeTag = std.meta.activeTag;
const assert = std.debug.assert;

const common = @import("common.zig");
const lexer = @import("lexer.zig");
const utils = @import("utils.zig");

const BlockAnalysis = @import("analyser.zig").BlockAnalysis;
const ASTNode = @import("common.zig").ASTNode;
const TypeInfo = common.TypeInfo;
const Value = common.Value;
const ASTNodeList = @import("common.zig").ASTNodeList;
const ASTNodePtrList = @import("common.zig").ASTNodePtrList;
const Program = @import("common.zig").Program;
const Op = @import("common.zig").Op;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

pub const Parser = struct {
    program: *Program,
    alloc: mem.Allocator,

    pub const ParserError = error{
        StrayToken,
        EmptyList,
        ExpectedKeyword,
        ExpectedEnumLit,
        ExpectedOptionalNumber,
        ExpectedString,
        ExpectedItems,
        ExpectedNode,
        UnexpectedItems,
        ExpectedValue,
        ExpectedStatement,
        UnknownKeyword,
        UnexpectedLabelDefinition,
        InvalidAsmOp,
        InvalidAsmFlag,
        InvalidType,
        MissingEnumType,
        NotAnEnum,
        InvalidEnumField,
        NoSuchType,
        UnknownIdent,
    } || mem.Allocator.Error;

    pub fn init(program: *Program, alloc: mem.Allocator) Parser {
        return .{
            .program = program,
            .alloc = alloc,
        };
    }

    pub fn initTypes(self: *Parser) void {
        self.program.addNativeType(common.Op.Tag, "Op");
    }

    fn validateListLength(ast: []const lexer.Node, require: usize) ParserError!void {
        if (ast.len < require) return error.ExpectedItems;
        if (ast.len > require) return error.UnexpectedItems;
    }

    fn expectNode(self: *Parser, comptime nodetype: meta.Tag(lexer.Node.NodeType), node: *const lexer.Node) b: {
        break :b ParserError!@TypeOf(@field(node.node, @tagName(nodetype)));
    } {
        if (node.node != nodetype) {
            return self.err(error.ExpectedNode, node.location);
        }
        return @field(node.node, @tagName(nodetype));
    }

    fn parseValue(self: *Parser, node: *const lexer.Node) ParserError!Value {
        return switch (node.node) {
            .T => .{ .typ = .Bool, .val = .{ .u8 = 1 } },
            .Nil => .{ .typ = .Bool, .val = .{ .u8 = 0 } },
            .U8 => |n| .{ .typ = .U8, .val = .{ .u8 = n } },
            .U16 => |n| .{ .typ = .U16, .val = .{ .u16 = n } },
            .Char8 => |c| .{ .typ = .Char8, .val = .{ .u8 = c } },
            .Char16 => |c| .{ .typ = .Char16, .val = .{ .u16 = c } },
            .String => |s| b: {
                self.program.statics.append(.{
                    .type = .U8,
                    .count = s.items.len + 1,
                    .default = .{ .String = s },
                }) catch unreachable;
                break :b .{
                    .typ = .{ .StaticPtr = self.program.statics.items.len - 1 },
                    .val = .None,
                };
            },
            .EnumLit => |e| .{ .typ = .AmbigEnumLit, .val = .{ .AmbigEnumLit = e } },
            else => error.ExpectedValue,
        };
    }

    fn parseArity(self: *Parser, node: *const lexer.Node) ParserError!BlockAnalysis {
        var arity = BlockAnalysis{};
        var norm_stack = true;
        var before = true;

        const ast_arity = try self.expectNode(.List, node);
        for (ast_arity.items) |*arity_item| {
            var dst: *TypeInfo.List32 = undefined;
            if (before) {
                dst = if (norm_stack) &arity.args else &arity.rargs;
            } else {
                dst = if (norm_stack) &arity.stack else &arity.rstack;
            }

            if (arity_item.node == .Keyword and mem.eql(u8, arity_item.node.Keyword, "--")) {
                assert(before);
                before = false;
            } else if (arity_item.node == .Keyword and mem.eql(u8, arity_item.node.Keyword, "|")) {
                norm_stack = false;
            } else {
                dst.append(try self.parseType(arity_item)) catch unreachable;
            }
        }

        return arity;
    }

    // TODO: (AnySz) (PtrSz) (Ptr8) (Ptr16) etc
    fn parseType(self: *Parser, node: *const lexer.Node) ParserError!TypeInfo {
        return switch (node.node) {
            .VarNum => |n| .{ .TypeRef = n },
            .Keyword => |item| b: {
                if (meta.stringToEnum(TypeInfo.Tag, item)) |p| {
                    var r: ?TypeInfo = null;
                    inline for (meta.fields(TypeInfo)) |field|
                        if (field.type == void and
                            mem.eql(u8, field.name, @tagName(p)))
                        {
                            r = @unionInit(TypeInfo, field.name, {});
                        };
                    if (r) |ret| {
                        break :b ret;
                    } else {
                        return self.err(error.InvalidType, node.location);
                    }
                } else {
                    std.log.err("Invalid type in arity def: {s}", .{item});
                    return self.err(error.InvalidType, node.location);
                }
            },
            else => return self.err(error.ExpectedNode, node.location),
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
            .Keyword => |i| b: {
                if (mem.eql(u8, i, "return")) {
                    break :b ASTNode{ .node = .Return, .srcloc = node.location };
                }
                break :b ASTNode{ .node = .{ .Call = .{ .name = i } }, .srcloc = node.location };
            },
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
                    const name = try self.expectNode(.Keyword, &ast[1]);

                    var arity: ?BlockAnalysis = null;
                    if (ast.len == 4)
                        arity = try self.parseArity(&ast[2]);

                    const body_ind: usize = if (ast.len == 4) @as(usize, 3) else 2;
                    const ast_body = try self.expectNode(.Quote, &ast[body_ind]);
                    const body = try self.parseStatements(ast_body.items);

                    break :b ASTNode{ .node = .{ .Decl = .{
                        .name = name,
                        .arity = arity,
                        .body = body,
                    } }, .srcloc = ast[0].location };
                } else if (mem.eql(u8, k, "mac")) {
                    const name = try self.expectNode(.Keyword, &ast[1]);

                    const ast_body = try self.expectNode(.Quote, &ast[2]);
                    const body = try self.parseStatements(ast_body.items);

                    break :b ASTNode{
                        .node = .{ .Mac = .{ .name = name, .body = body } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "wild")) {
                    try validateListLength(ast, 3);

                    const arity = try self.parseArity(&ast[1]);
                    const ast_body = try self.expectNode(.Quote, &ast[2]);
                    const block = try self.parseStatements(ast_body.items);

                    break :b ASTNode{
                        .node = .{ .Wild = .{ .arity = arity, .body = block } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "as")) {
                    try validateListLength(ast, 2);

                    switch (ast[1].node) {
                        .VarNum => |n| break :b ASTNode{
                            .node = .{ .Cast = .{ .to = .Any, .ref = n } },
                            .srcloc = ast[0].location,
                        },
                        else => break :b ASTNode{
                            .node = .{ .Cast = .{ .to = try self.parseType(&ast[1]) } },
                            .srcloc = ast[0].location,
                        },
                    }
                } else if (mem.eql(u8, k, "return")) {
                    try validateListLength(ast, 1);
                    break :b ASTNode{ .node = .Return, .srcloc = ast[0].location };
                } else if (mem.eql(u8, k, "until")) {
                    try validateListLength(ast, 3);

                    const ast_cond = try self.expectNode(.Quote, &ast[1]);
                    const cond = try self.parseStatements(ast_cond.items);

                    const ast_body = try self.expectNode(.Quote, &ast[2]);
                    const body = try self.parseStatements(ast_body.items);

                    break :b ASTNode{
                        .node = .{ .Loop = .{
                            .loop = .{ .Until = .{ .cond = cond, .cond_prep = .Unchecked } },
                            .body = body,
                        } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "when")) {
                    const yup = try self.parseStatements((try self.expectNode(.Quote, &ast[1])).items);
                    const nah = if (ast.len > 2) try self.parseStatements((try self.expectNode(.Quote, &ast[2])).items) else null;
                    break :b ASTNode{
                        .node = .{ .When = .{ .yup = yup, .nah = nah } },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "cond")) {
                    var cond_node = ASTNode.Cond{
                        .branches = ASTNode.Cond.Branch.List.init(self.alloc),
                        .else_branch = null,
                    };

                    var all_branches = std.ArrayList(ASTNodeList).init(self.alloc);
                    defer all_branches.deinit();

                    for (ast[1..]) |*node| {
                        const q = try self.expectNode(.Quote, node);
                        try all_branches.append(try self.parseStatements(q.items));
                    }

                    // FIXME: make it an error for a cond statement with only
                    // one argument

                    var i: usize = 0;
                    while (i < all_branches.items.len) : (i += 2) {
                        if (i == all_branches.items.len - 1) {
                            cond_node.else_branch = all_branches.items[i];
                        } else {
                            const cond = all_branches.items[i + 0];
                            const body = all_branches.items[i + 1];
                            try cond_node.branches.append(ASTNode.Cond.Branch{
                                .cond = cond,
                                .body = body,
                            });
                        }
                    }

                    break :b ASTNode{
                        .node = .{ .Cond = cond_node },
                        .srcloc = ast[0].location,
                    };
                } else if (mem.eql(u8, k, "asm")) {
                    try validateListLength(ast, 3);

                    const asm_flags = try self.parseValue(&ast[1]);
                    if (asm_flags.typ != .StaticPtr and
                        self.program.statics.items[asm_flags.typ.StaticPtr].default != .String)
                        return error.ExpectedString;

                    var asm_stack: usize = WK_STACK;
                    var asm_keep = false;
                    var asm_short = false;
                    var asm_generic = false;
                    const str = self.program.statics.items[asm_flags.typ.StaticPtr].default.String;
                    for (str.items) |char| switch (char) {
                        'k' => asm_keep = true,
                        'r' => asm_stack = RT_STACK,
                        's' => asm_short = true,
                        'g' => asm_generic = true,
                        else => return error.InvalidAsmFlag,
                    };

                    const asm_op_kwd = try self.parseValue(&ast[2]);
                    if (asm_op_kwd.typ != .AmbigEnumLit)
                        return error.ExpectedEnumLit;
                    const asm_op_lowered = try self.lowerEnumValue(asm_op_kwd.val.AmbigEnumLit);
                    const asm_op_e = meta.stringToEnum(
                        Op.Tag,
                        self.program.types.items[asm_op_lowered.type].def.Enum.fields.items[asm_op_lowered.field].name,
                    ) orelse return error.InvalidAsmOp;
                    const asm_op = Op.fromTag(asm_op_e) catch return error.InvalidAsmOp;
                    break :b ASTNode{
                        .node = .{ .Asm = .{
                            .stack = asm_stack,
                            .short = asm_short,
                            .generic = asm_generic,
                            .keep = asm_keep,
                            .op = asm_op,
                        } },
                        .srcloc = ast[0].location,
                    };
                } else {
                    std.log.info("Unknown keyword: {s}", .{k});
                    break :b error.UnknownKeyword;
                }
            },
            .List => |l| try self.parseList(l.items),
            else => try self.parseStatement(&ast[0]),
        };
    }

    // Extract definitions
    pub fn extractDefs(self: *Parser) ParserError!void {
        var iter = self.program.ast.iterator();
        while (iter.next()) |node|
            if (node.node == .Decl) {
                try self.program.defs.append(node);
            } else if (node.node == .Mac) {
                try self.program.macs.append(node);
            };
    }

    pub fn lowerEnumValue(self: *Parser, lit: lexer.Node.EnumLit) ParserError!TypeInfo.EnumLit {
        if (lit.of == null)
            return error.MissingEnumType;
        for (self.program.types.items, 0..) |t, i| {
            if (mem.eql(u8, t.name, lit.of.?)) {
                if (t.def != .Enum)
                    return error.NotAnEnum;
                for (t.def.Enum.fields.items, 0..) |field, field_i| {
                    if (mem.eql(u8, field.name, lit.v)) {
                        return TypeInfo.EnumLit{ .type = i, .field = field_i };
                    }
                }
                return error.InvalidEnumField;
            }
        }
        return error.NoSuchType;
    }

    // Earlier we couldn't know what type an Enum literal belonged to. At this
    // stage we find and set that information.
    //
    // Also check calls to determine what type they are.
    pub fn postProcess(self: *Parser) ParserError!void {
        const _S = struct {
            pub fn walkNodes(parser: *Parser, nodes: ASTNodeList) ParserError!void {
                var iter = nodes.iterator();
                while (iter.next()) |node|
                    try walkNode(parser, node);
            }

            pub fn walkNode(parser: *Parser, node: *ASTNode) ParserError!void {
                switch (node.node) {
                    .Value => |v| switch (v.typ) {
                        .AmbigEnumLit => {
                            const lowered = try parser.lowerEnumValue(v.val.AmbigEnumLit);
                            node.node = .{ .Value = .{ .typ = .{ .EnumLit = lowered.type }, .val = .{ .EnumLit = lowered } } };
                        },
                        else => {},
                    },
                    .Decl => |b| try walkNodes(parser, b.body),
                    .Mac => |b| try walkNodes(parser, b.body),
                    .Wild => |b| try walkNodes(parser, b.body),
                    .Quote => |b| try walkNodes(parser, b.body),
                    .Loop => |d| {
                        switch (d.loop) {
                            .Until => |u| try walkNodes(parser, u.cond),
                        }
                        try walkNodes(parser, d.body);
                    },
                    .When => |when| {
                        try walkNodes(parser, when.yup);
                        if (when.nah) |n| try walkNodes(parser, n);
                    },
                    .Cond => |cond| {
                        for (cond.branches.items) |branch| {
                            try walkNodes(parser, branch.cond);
                            try walkNodes(parser, branch.body);
                        }
                        if (cond.else_branch) |branch|
                            try walkNodes(parser, branch);
                    },
                    .Call => |c| if (c.ctyp == .Unchecked) {
                        if (for (parser.program.macs.items) |mac| {
                            if (mem.eql(u8, mac.node.Mac.name, c.name))
                                break true;
                        } else false) {
                            node.node.Call.ctyp = .Mac;
                        } else if (for (parser.program.defs.items) |decl| {
                            if (mem.eql(u8, decl.node.Decl.name, c.name))
                                break true;
                        } else false) {
                            node.node.Call.ctyp = .{ .Decl = 0 };
                        } else {
                            std.log.info("Unknown ident {s}", .{c.name});
                            return error.UnknownIdent;
                        }
                    },
                    else => {},
                }
            }
        };
        try _S.walkNodes(self, self.program.ast);
    }

    // Setup the entry function
    // TODO: this should be in codegen
    //
    pub fn setupMainFunc(self: *Parser) ParserError!void {
        var body = ASTNodeList.init(self.alloc);
        var iter2 = self.program.ast.iterator();
        while (iter2.next()) |ast_item| {
            if (ast_item.node != .Decl and ast_item.node != .Mac) {
                try body.append(ast_item.*);
                ast_item.node = .None;
                ast_item.srcloc = .{};
            }
        }
        try body.append(ASTNode{ .node = .{
            .Asm = .{ .stack = WK_STACK, .op = .Ohalt },
        }, .srcloc = .{} });
        try self.program.ast.insertAtInd(0, ASTNode{ .node = .{ .Call = .{
            .name = "_Start",
            .goto = true,
        } }, .srcloc = .{} });
        try self.program.ast.append(ASTNode{
            .node = .{ .Decl = .{ .name = "_Start", .body = body } },
            .srcloc = .{},
        });
    }

    pub fn parse(self: *Parser, lexed: *const lexer.NodeList) ParserError!void {
        for (lexed.items) |*node| switch (node.node) {
            .List => |l| try self.program.ast.append(try self.parseList(l.items)),
            else => try self.program.ast.append(try self.parseStatement(node)),
        };

        try self.setupMainFunc();
        try self.extractDefs();
        try self.postProcess();
    }

    pub fn err(self: *Parser, e: ParserError!void, srcloc: common.Srcloc) ParserError {
        if (e) {
            unreachable;
        } else |er| {
            self.program.errors.append(.{ .e = er, .l = srcloc }) catch unreachable;
            return er;
        }
    }
};

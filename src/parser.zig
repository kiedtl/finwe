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
const ErrorSet = @import("common.zig").Error.Set;
const UserType = @import("common.zig").UserType;

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
        UnknownLocal,
        UnexpectedLabelDefinition,
        InvalidAsmOp,
        InvalidAsmFlag,
        InvalidType,
        InvalidDeviceFieldType,
        MissingEnumType,
        NotAnEnumOrDevice,
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
            return self.program.perr(error.ExpectedNode, node.location);
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
                        return self.program.perr(error.InvalidType, node.location);
                    }
                } else {
                    return self.program.perr(error.InvalidType, node.location);
                }
            },
            .List => |lst| {
                if (lst.items.len == 0)
                    return error.EmptyList;

                return switch (lst.items[0].node) {
                    .Keyword => |k| if (meta.stringToEnum(TypeInfo.Expr.Tag, k)) |p| b: {
                        var r: ?TypeInfo = null;
                        inline for (meta.fields(TypeInfo.Expr)) |field|
                            if (mem.eql(u8, field.name, @tagName(p))) {
                                const arg = self.program.btype(try self.parseType(&lst.items[1]));
                                r = .{ .Expr = @unionInit(TypeInfo.Expr, field.name, arg) };
                            };
                        if (r) |ret| {
                            break :b ret;
                        } else {
                            return self.program.perr(error.InvalidType, node.location);
                        }
                    } else {
                        return self.program.perr(error.InvalidType, node.location);
                    },
                    else => return self.program.perr(error.ExpectedKeyword, node.location),
                };
            },
            else => return self.program.perr(error.ExpectedNode, node.location),
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
            .Var => |s| ASTNode{
                .node = .{ .VDeref = .{ .name = s } },
                .srcloc = node.location,
            },
            .VarPtr => |s| ASTNode{
                .node = .{ .VRef = .{ .name = s } },
                .srcloc = node.location,
            },
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
                } else if (mem.eql(u8, k, "local")) {
                    try validateListLength(ast, 4);
                    const name = try self.expectNode(.Keyword, &ast[1]);
                    const ltyp = try self.parseType(&ast[2]);
                    const llen = try self.expectNode(.U8, &ast[3]);

                    break :b ASTNode{
                        .node = .{ .VDecl = .{
                            .name = name,
                            .utyp = ltyp,
                            .llen = llen,
                            .lind = 0,
                        } },
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
                    const asm_val = try self.lowerEnumValue(asm_op_kwd.val.AmbigEnumLit);
                    const asm_typ = self.program.types.items[asm_val.Value.typ.EnumLit];
                    const asm_name = asm_typ.def.Enum.fields.items[asm_val.Value.val.EnumLit].name;
                    var asm_op_e: ?Op.Tag = null;
                    // meta.stringToEnum causes stack corruption in certain case
                    // I really should file an issue about this...
                    inline for (@typeInfo(Op.Tag).Enum.fields) |enumField| {
                        if (mem.eql(u8, asm_name, enumField.name)) {
                            asm_op_e = @field(Op.Tag, enumField.name);
                        }
                    }
                    const asm_op = Op.fromTag(
                        asm_op_e orelse return error.InvalidAsmOp,
                    ) catch return error.InvalidAsmOp;
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
                } else if (mem.eql(u8, k, "device")) {
                    const name = try self.expectNode(.Keyword, &ast[1]);
                    const addr = try self.expectNode(.U8, &ast[2]);
                    var fields = ASTNode.TypeDef.Field.AList.init(self.alloc);
                    for (ast[3..]) |node| {
                        const fielddef = try self.expectNode(.Quote, &node);
                        try validateListLength(fielddef.items, 2);
                        const fieldname = try self.expectNode(.Keyword, &fielddef.items[0]);
                        const fieldtype = try self.parseType(&fielddef.items[1]);
                        if (fieldtype.bits(self.program) == null)
                            return self.program.perr(error.InvalidDeviceFieldType, node.location);
                        fields.append(.{ .name = fieldname, .type = fieldtype }) catch unreachable;
                    }
                    break :b ASTNode{
                        .node = .{ .TypeDef = .{
                            .name = name,
                            .def = .{ .Device = .{ .start = addr, .fields = fields } },
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

    pub fn lowerEnumValue(self: *Parser, lit: lexer.Node.EnumLit) ParserError!ASTNode.Type {
        if (lit.of == null)
            return error.MissingEnumType;
        for (self.program.types.items, 0..) |t, i| {
            if (mem.eql(u8, t.name, lit.of.?)) switch (t.def) {
                .Enum => |edef| {
                    for (edef.fields.items, 0..) |field, field_i| {
                        if (mem.eql(u8, field.name, lit.v)) {
                            return .{ .Value = .{
                                .typ = .{ .EnumLit = i },
                                .val = .{ .EnumLit = field_i },
                            } };
                        }
                    }
                    return error.InvalidEnumField;
                },
                .Device => |ddef| {
                    for (ddef.fields.items, 0..) |field, field_i| {
                        if (mem.eql(u8, field.name, lit.v)) {
                            const bit = field.type.bits(self.program).?;
                            const typ: TypeInfo = if (bit == 16) .Dev16 else .Dev8;
                            return .{ .Value = .{ .typ = typ, .val = .{ .Device = .{
                                .dev_i = i,
                                .field = field_i,
                            } } } };
                        }
                    }
                    return error.InvalidEnumField;
                },
                //else => return error.NotAnEnumOrDevice,
            };
        }
        return error.NoSuchType;
    }

    pub fn postProcess(parser_: *Parser) ErrorSet!void {
        // Add typedefs
        try parser_.program.walkNodes(null, parser_.program.ast, parser_, struct {
            pub fn f(node: *ASTNode, _: ?*ASTNode, self: *Program, parser: *Parser) ErrorSet!void {
                // FIXME: check for name collisions
                if (node.node == .TypeDef) switch (node.node.TypeDef.def) {
                    .Device => |devdef| {
                        var fields = UserType.DeviceField.AList.init(parser.alloc);
                        for (devdef.fields.items) |field| {
                            fields.append(.{ .name = field.name, .type = field.type }) catch unreachable;
                        }
                        self.types.append(UserType{
                            .node = node,
                            .name = node.node.TypeDef.name,
                            .def = .{ .Device = .{ .start = devdef.start, .fields = fields } },
                        }) catch unreachable;
                    },
                };
            }
        }.f);

        // Earlier we couldn't know what type an Enum literal belonged to. At this
        // stage we find and set that information.
        //
        // Also check calls to determine what type they are.
        try parser_.program.walkNodes(null, parser_.program.ast, parser_, struct {
            pub fn f(node: *ASTNode, parent: ?*ASTNode, self: *Program, parser: *Parser) ErrorSet!void {
                switch (node.node) {
                    .Value => |v| switch (v.typ) {
                        .AmbigEnumLit => node.node = try parser.lowerEnumValue(v.val.AmbigEnumLit),
                        else => {},
                    },
                    .Call => |c| if (c.ctyp == .Unchecked) {
                        if (for (self.macs.items) |mac| {
                            if (mem.eql(u8, mac.node.Mac.name, c.name))
                                break true;
                        } else false) {
                            node.node.Call.ctyp = .Mac;
                        } else if (for (self.defs.items) |decl| {
                            if (mem.eql(u8, decl.node.Decl.name, c.name))
                                break true;
                        } else false) {
                            node.node.Call.ctyp = .{ .Decl = 0 };
                        } else {
                            std.log.info("Unknown ident {s}", .{c.name});
                            return self.perr(error.UnknownIdent, node.srcloc);
                        }
                    },
                    .VDecl => |*vd| {
                        parent.?.node.Decl.locals.append(.{
                            .name = vd.name,
                            .rtyp = vd.utyp,
                            .llen = vd.llen,
                            .ind = 0,
                        }) catch unreachable;
                        vd.lind = parent.?.node.Decl.locals.len - 1;
                    },
                    .VRef => |*v| {
                        v.lind = for (parent.?.node.Decl.locals.constSlice(), 0..) |local, i| {
                            if (mem.eql(u8, v.name, local.name))
                                break i;
                        } else return self.perr(error.UnknownLocal, node.srcloc);
                    },
                    .VDeref => |*v| {
                        v.lind = for (parent.?.node.Decl.locals.constSlice(), 0..) |local, i| {
                            if (mem.eql(u8, v.name, local.name))
                                break i;
                        } else return self.perr(error.UnknownLocal, node.srcloc);
                    },
                    else => {},
                }
            }
        }.f);
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

    pub fn parse(self: *Parser, lexed: *const lexer.NodeList) ErrorSet!void {
        for (lexed.items) |*node| switch (node.node) {
            .List => |l| try self.program.ast.append(try self.parseList(l.items)),
            else => try self.program.ast.append(try self.parseStatement(node)),
        };

        try self.setupMainFunc();
        try self.extractDefs();
        try self.postProcess();
    }
};

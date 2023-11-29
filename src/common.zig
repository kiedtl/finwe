const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const analyser = @import("analyser.zig");
const common = @This();

const LinkedList = @import("list.zig").LinkedList;
const StackBuffer = @import("buffer.zig").StackBuffer;

// ----------------------------------------------------------------------------

pub const WK_STACK = 0;
pub const RT_STACK = 1;
pub const STACK_SZ = 255;

pub var gpa = std.heap.GeneralPurposeAllocator(.{
    // Probably should enable this later on to track memory usage, if
    // allocations become too much
    .enable_memory_limit = false,

    .safety = true,

    // Probably would enable this later?
    .thread_safe = false,

    .never_unmap = false,
}){};

pub const String = std.ArrayList(u8);

pub const ASTNodeList = LinkedList(ASTNode);
//pub const ASTNodeList = std.ArrayList(ASTNode);
pub const ASTNodePtrList = std.ArrayList(*ASTNode);
//pub const ASTNodePtrList = LinkedList(*ASTNode);
//
pub const TypeInfo = union(enum) {
    Bool,
    U8,
    U16,
    Char8,
    Char16,
    EnumLit: usize,
    AnyPtr,
    AnyPtr16,
    Ptr8: Ptr,
    Ptr16: Ptr,
    Any,
    Any8,
    Any16,

    StaticPtr: usize,
    Quote,

    Expr: Expr,

    AmbigEnumLit,
    TypeRef: usize,

    pub const Tag = meta.Tag(@This());
    pub const List32 = @import("buffer.zig").StackBuffer(@This(), 32);

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        switch (self) {
            .EnumLit, .TypeRef => |n| try writer.print("{s}<{}>", .{ @tagName(self), n }),
            .Ptr8, .Ptr16 => |n| try writer.print("{s}<{}>", .{ @tagName(self), n }),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
        try writer.print("", .{});
    }

    pub const Ptr = struct {
        typ: usize,
        ind: usize, // Always >=1. 1 == *Typ, 2 == **Typ, etc

        pub fn eq(a: Ptr, b: Ptr) bool {
            return a.typ == b.typ and a.ind == b.ind;
        }
    };

    pub const EnumLit = struct {
        type: usize, // index into Program.types
        field: usize,
    };

    pub const Expr = union(enum) {
        AnySz: usize,
        Child: usize,
        // Ptr8: usize,
        // Ptr16: usize,

        pub const Tag = meta.Tag(@This());

        pub fn eq(a: @This(), b: @This()) bool {
            if (@as(Expr.Tag, a) != @as(Expr.Tag, b)) return false;
            return switch (a) {
                .AnySz => |ar| ar == b.AnySz,
                .Child => |ar| ar == b.Child,
            };
        }

        pub fn resolveTypeRef(
            self: @This(),
            arity: *const analyser.BlockAnalysis,
            program: *Program,
        ) TypeInfo {
            return switch (self) {
                .AnySz => |s| b: {
                    const arg_t = program.builtin_types.items[s].resolveTypeRef(arity, program);
                    if (arg_t.bits(program)) |b| {
                        break :b if (b == 16) .Any16 else .Any8;
                    } else {
                        //break :b .{ .Expr = .{ .AnySz = program.btype(arg_t) } };
                        break :b .Any;
                    }
                },
                .Child => |s| b: {
                    const arg_t = program.builtin_types.items[s].resolveTypeRef(arity, program);
                    break :b switch (arg_t) {
                        .AnyPtr, .AnyPtr16 => .Any,
                        .Ptr8, .Ptr16 => |p| program.builtin_types.items[p.typ],
                        else => unreachable,
                    };
                },
            };
        }
    };

    pub fn resolveTypeRef(a: *@This(), arity: *const analyser.BlockAnalysis, program: *Program) TypeInfo {
        return switch (a.*) {
            .TypeRef => |r| arity.args.constSlice()[arity.args.len - r - 1],
            .Expr => |e| e.resolveTypeRef(arity, program),
            else => a.*,
        };
    }

    pub fn eq(a: @This(), b: @This()) bool {
        if (@as(Tag, a) != @as(Tag, b)) return false;
        inline for (meta.fields(@This())) |field|
            if (mem.eql(u8, field.name, @tagName(a)))
                if (field.type == usize or field.type == void)
                    return @field(a, field.name) != @field(b, field.name)
                else
                    return @field(a, field.name).eq(@field(b, field.name));
        unreachable;
    }

    pub fn ptrize16(a: @This(), program: *Program) @This() {
        return switch (a) {
            .Ptr16 => |p| .{ .Ptr16 = .{ .typ = p.typ, .ind = p.ind + 1 } },
            else => TypeInfo.ptr16(program, a, 1),
        };
    }

    pub fn ptr16(program: *Program, to: TypeInfo, indirection: usize) TypeInfo {
        assert(indirection >= 1);
        const ind: ?usize = for (program.builtin_types.items, 0..) |typ, i| {
            if (to.eq(typ)) break i;
        } else null;
        if (ind) |_ind| {
            return .{ .Ptr16 = .{ .typ = _ind, .ind = indirection } };
        } else {
            program.builtin_types.append(to) catch unreachable;
            return .{ .Ptr16 = .{
                .typ = program.builtin_types.items.len - 1,
                .ind = indirection,
            } };
        }
    }

    pub fn isGeneric(self: @This()) bool {
        return switch (self) {
            .AnyPtr16, .Any, .Any8, .Any16, .AnyPtr, .TypeRef => true,
            else => false,
        };
    }

    pub fn doesInclude(self: @This(), other: @This(), program: *const Program) bool {
        return (self != .EnumLit and @as(Tag, self) == @as(Tag, other)) or switch (self) {
            .Any => true,
            .Any8 => other.bits(program) != null and other.bits(program).? == 8,
            .Any16 => other.bits(program) != null and other.bits(program).? == 16,
            .AnyPtr => other == .Ptr8 or other == .Ptr16,
            .AnyPtr16 => other == .Ptr16 or other == .StaticPtr,
            .EnumLit => |e| other == .EnumLit and e == other.EnumLit,
            else => false, // self == other case already handled earlier
        };
    }

    pub fn bits(self: @This(), program: *const Program) ?u5 {
        return switch (self) {
            .U8, .Char8, .Ptr8, .Bool, .Any8 => 8,
            .AnyPtr16, .StaticPtr, .U16, .Char16, .Ptr16, .Any16 => 16,
            .AnyPtr, .Any => null,
            .EnumLit => |e| if (program.types.items[e].def.Enum.is_short) 16 else 8,
            .Expr, .Quote, .AmbigEnumLit, .TypeRef => unreachable,
        };
    }
};

pub const Value = struct {
    typ: TypeInfo,
    val: V,

    pub const V = union(enum) {
        u8: u8,
        u16: u16,
        EnumLit: TypeInfo.EnumLit,
        AmbigEnumLit: lexer.Node.EnumLit,
        None,
    };

    pub fn toU16(self: Value, program: *Program) u16 {
        return switch (self.typ) {
            .U16, .Char16, .Ptr16 => self.val.u16,
            .EnumLit => |e| b: {
                const v = program.types.items[e].def.Enum.fields.items[self.val.EnumLit.field];
                break :b v.value_b.? << 4 | v.value_a;
            },
            .AmbigEnumLit => unreachable,
            .Any, .Any8, .Any16, .AnyPtr => unreachable,
            .StaticPtr => @panic("Codegen bug: string is static data, need UAL"),
            else => unreachable,
        };
    }

    pub fn toU8(self: Value, program: *Program) u8 {
        return switch (self.typ) {
            .Bool, .U8, .Char8, .Ptr8 => self.val.u8,
            .EnumLit => |e| program.types.items[e].def.Enum.fields.items[self.val.EnumLit.field].value_a, // TODO: value_b
            .AmbigEnumLit => unreachable,
            .Any, .Any8, .Any16, .AnyPtr => unreachable,
            .StaticPtr => @panic("Codegen bug: string is static data, need UAL"),
            else => unreachable,
        };
    }
};

pub const ASTNode = struct {
    __prev: ?*ASTNode = null,
    __next: ?*ASTNode = null,

    node: ASTNode.Type,
    srcloc: Srcloc,
    romloc: usize = 0,

    pub const Tag = std.meta.Tag(ASTNode.Type);

    pub const Type = union(enum) {
        None, // Placeholder for removed ast values
        Decl: Decl, // word declaration
        Mac: Mac, // macro declaration
        Call: Call,
        Wild: Wild,
        Loop: Loop,
        When: When,
        Cond: Cond,
        Asm: Ins,
        Value: Value,
        Quote: Quote,
        Cast: Cast,
        VDecl: VDecl,
        VRef: VRef,
        VDeref: VDeref,
        Return,

        pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (comptime !mem.eql(u8, f, "")) @compileError("Unknown format: '" ++ f ++ "'");

            const s = @tagName(self);
            switch (self) {
                .Call => |c| switch (c.ctyp) {
                    .Decl => |d| try writer.print("Call<{s}, {}>", .{ c.name, d }),
                    .Mac => try writer.print("CallMac<{s}>", .{c.name}),
                    .Unchecked => try writer.print("Call<{s}?>", .{c.name}),
                },
                .Wild => |w| try writer.print("Wild<{}>", .{w.arity}),
                .Loop => |l| switch (l.loop) {
                    .Until => |u| try writer.print("Until<{s}>", .{@tagName(u.cond_prep)}),
                },
                .Asm => |a| try writer.print("Asm<{}>", .{a}),
                .Value => |v| try writer.print("Val<{}, {}>", .{ v.typ, v.val }),
                .Cast => |c| try writer.print("Cast<{}, {} ${?}>", .{ c.of, c.to, c.ref }),
                else => try writer.print("{s}", .{s}),
            }
        }
    };

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) @compileError("Unknown format string: '" ++ f ++ "'");
        try writer.print("{}", .{self.node});
    }

    pub const VDecl = struct {
        name: []const u8,
        lind: usize,
        utyp: TypeInfo,
        llen: usize,
    };

    pub const VRef = struct {
        name: []const u8,
        lind: ?usize = null, // Into decl.locals
        sind: ?usize = null, // Into program.statics, set in analysis postProcess
    };

    pub const VDeref = struct {
        name: []const u8,
        lind: ?usize = null, // Into decl.locals
        sind: ?usize = null, // Into program.statics, set in analysis postProcess
    };

    pub const Wild = struct {
        arity: analyser.BlockAnalysis,
        body: ASTNodeList,
    };

    pub const Cast = struct {
        of: TypeInfo = .Any,
        to: TypeInfo,
        ref: ?usize = null,
    };

    pub const Call = struct {
        name: []const u8,
        ctyp: union(enum) {
            Mac,
            Decl: usize, // variant, 0 if not generic
            Unchecked,
        } = .Unchecked,
        goto: bool = false,
    };

    pub const When = struct {
        yup: ASTNodeList,
        nah: ?ASTNodeList,
    };

    pub const Cond = struct {
        branches: Branch.List,
        else_branch: ?ASTNodeList,

        pub const Branch = struct {
            cond: ASTNodeList,
            body: ASTNodeList,

            pub const List = std.ArrayList(Branch);
        };
    };

    pub const Loop = struct {
        loop: Loop.Type,
        body: ASTNodeList,

        pub const Type = union(enum) {
            Until: struct {
                cond: ASTNodeList,
                cond_prep: enum { Unchecked, Dup, DupShort }, // TODO: Dup2, DupShort2, etc
            },
        };
    };

    pub const Decl = struct {
        name: []const u8,
        arity: ?analyser.BlockAnalysis = null,
        analysis: analyser.BlockAnalysis = analyser.BlockAnalysis{},
        variations: StackBuffer(analyser.BlockAnalysis, 8) = StackBuffer(analyser.BlockAnalysis, 8).init(null),
        body: ASTNodeList,
        is_analysed: bool = false,
        variant: usize = 0,
        calls: usize = 0,
        locals: StackBuffer(Local, 8) = StackBuffer(Local, 8).init(null),

        pub const Local = struct {
            name: []const u8,
            ind: usize,
            rtyp: TypeInfo,
            llen: usize,
        };
    };

    pub const Mac = struct {
        name: []const u8,
        analysis: analyser.BlockAnalysis = analyser.BlockAnalysis{},
        body: ASTNodeList,
        is_analysed: bool = false,
    };

    pub const Quote = struct {
        body: ASTNodeList,
    };

    fn _deepcloneASTList(lst: ASTNodeList, parent: ?*Decl, program: *Program) ASTNodeList {
        var new = ASTNodeList.init(gpa.allocator());
        var itr = lst.iterator();
        while (itr.next()) |item| {
            new.append(item.deepclone(parent, program)) catch unreachable;
        }
        return new;
    }

    pub fn deepclone(self: @This(), parent: ?*Decl, program: *Program) @This() {
        var new = self.node;
        switch (new) {
            .When => {
                new.When.yup = _deepcloneASTList(new.When.yup, parent, program);
                if (new.When.nah) |n|
                    new.When.nah = _deepcloneASTList(n, parent, program);
            },
            .Cond => |*c| {
                if (c.else_branch != null)
                    c.else_branch = _deepcloneASTList(c.else_branch.?, parent, program);
                for (c.branches.items, 0..) |br, i| {
                    c.branches.items[i].body = _deepcloneASTList(br.body, parent, program);
                    c.branches.items[i].cond = _deepcloneASTList(br.cond, parent, program);
                }
            },
            .Loop => {
                new.Loop.body = _deepcloneASTList(new.Loop.body, parent, program);
                switch (new.Loop.loop) {
                    .Until => |u| new.Loop.loop.Until.cond = _deepcloneASTList(u.cond, parent, program),
                }
            },
            .Decl => new.Decl.body = _deepcloneASTList(new.Decl.body, &new.Decl, program),
            .Mac => new.Mac.body = _deepcloneASTList(new.Mac.body, parent, program),
            .Quote => new.Quote.body = _deepcloneASTList(new.Quote.body, parent, program),
            .Wild => new.Wild.body = _deepcloneASTList(new.Wild.body, parent, program),
            .VDecl => {},
            .VDeref, .VRef => {},
            .None, .Call, .Asm, .Value, .Cast, .Return => {},
        }

        return .{
            .node = new,
            .srcloc = self.srcloc,
            .romloc = self.romloc,
        };
    }
};

pub const Srcloc = struct {
    line: usize = 0,
    column: usize = 0,
    // file: []const u8,
};

pub const Error = struct {
    e: Set,
    l: Srcloc,

    pub const Set = parser.Parser.ParserError || analyser.Error;
    pub const AList = std.ArrayList(@This());
};

pub const Static = struct {
    type: TypeInfo,
    count: usize,
    default: union(enum) {
        String: String,
        None,
    },
    romloc: usize = 0,

    pub const AList = std.ArrayList(@This());
};

pub const Program = struct {
    ast: ASTNodeList,
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
    statics: Static.AList,
    types: Type.AList,
    builtin_types: std.ArrayList(TypeInfo),

    errors: Error.AList,

    // scopes: Scope.AList,
    // pub const Scope = struct {
    //     defs: ASTNodePtrList,
    //     macs: ASTNodePtrList,
    //     pub const AList = std.ArrayList(Scope);
    // };

    pub fn init(alloc: mem.Allocator) @This() {
        return Program{
            .ast = ASTNodeList.init(alloc),
            .defs = ASTNodePtrList.init(alloc),
            .macs = ASTNodePtrList.init(alloc),
            .statics = Static.AList.init(alloc),
            .types = common.Type.AList.init(alloc),
            .builtin_types = std.ArrayList(TypeInfo).init(alloc),
            .errors = common.Error.AList.init(alloc),
            //.defs = ASTNodeList.init(alloc),
        };
    }

    pub fn addNativeType(self: *Program, comptime T: type, name: []const u8) void {
        switch (@typeInfo(T)) {
            .Enum => |info| {
                if (info.tag_type != u16 and info.tag_type != u8)
                    @compileError("Enum type must be either u8 or u16");
                var t = Type{ .node = null, .name = name, .def = .{ .Enum = .{
                    .is_short = info.tag_type == u16,
                    .fields = std.ArrayList(Type.EnumField).init(gpa.allocator()),
                } } };
                inline for (info.fields) |field| {
                    const va: u8 = if (info.tag_type == u16) @as(u16, @intCast(field.value)) & 0xFF else @intCast(field.value);
                    const vb: ?u8 = if (info.tag_type == u16) (@as(u16, @intCast(field.value)) >> 4) & 0xFF else null;
                    t.def.Enum.fields.append(.{ .name = field.name, .value_a = va, .value_b = vb }) catch unreachable;
                }
                self.types.append(t) catch unreachable;
            },
            else => @compileError("TODO"),
        }
    }

    // Record type in builtin_types if not there otherwise find existing record
    pub fn btype(self: *Program, typ: TypeInfo) usize {
        return for (self.builtin_types.items, 0..) |t, i| {
            if (t.eq(typ)) break i;
        } else b: {
            self.builtin_types.append(typ) catch unreachable;
            break :b self.builtin_types.items.len - 1;
        };
    }

    pub fn perr(self: *Program, e: parser.Parser.ParserError, srcloc: common.Srcloc) parser.Parser.ParserError {
        self.errors.append(.{ .e = e, .l = srcloc }) catch unreachable;
        return e;
    }

    pub fn aerr(self: *Program, e: analyser.Error, srcloc: common.Srcloc) analyser.Error {
        self.errors.append(.{ .e = e, .l = srcloc }) catch unreachable;
        return e;
    }
};

pub const Type = struct {
    node: ?usize, // null means native (i.e. compiler internal)
    name: []const u8,
    def: Def,

    pub const AList = std.ArrayList(Type);

    pub const Def = union(enum) {
        Enum: Enum,
    };

    pub const Enum = struct {
        is_short: bool = false,
        fields: std.ArrayList(EnumField),
    };

    pub const EnumField = struct {
        name: []const u8,
        value_a: u8,
        value_b: ?u8,
    };
};

pub const OpTag = enum(u16) {
    Oraw,
    Olit,
    Osr,
    Ojmp,
    Ojcn,
    Ojsr,
    Ozj,
    Ohalt,
    Onac,
    Onip,
    Oswp,
    Orot,
    Oovr,
    Odup,
    Oroll,
    Oinc,
    Odrop,
    Oeq,
    Oneq,
    Olt,
    Ogt,
    Oeor,
    Odmod,
    Omul,
    Odiv,
    Oadd,
    Osub,
    Ostash,
    Olda,
    Osta,
    Odei,
    Odeo,
};

pub const Op = union(OpTag) {
    Oraw: u8,
    Olit,
    Osr: ?u8,
    Ojmp,
    Ojcn,
    Ojsr,
    Ozj: ?u8,
    Ohalt,
    Onac: []const u8,
    Onip,
    Oswp,
    Orot,
    Oovr,
    Odup,
    Oroll: ?u8,
    Oinc,
    Odrop,
    Oeq,
    Oneq,
    Olt,
    Ogt,
    Oeor,
    Odmod,
    Omul,
    Odiv,
    Oadd,
    Osub,
    Ostash,
    Olda,
    Osta,
    Odei,
    Odeo,

    pub const Tag = meta.Tag(Op);

    pub fn fromTag(tag: Tag) !Op {
        return switch (tag) {
            .Oraw, .Onac => error.NeedsArg,
            .Olit => .Olit,
            .Osr => .{ .Osr = null },
            .Ojmp => .Ojmp,
            .Ojsr => .Ojsr,
            .Ojcn => .Ojcn,
            .Ozj => .{ .Ozj = null },
            .Ohalt => .Ohalt,
            .Onip => .Onip,
            .Oswp => .Oswp,
            .Orot => .Orot,
            .Oovr => .Oovr,
            .Odup => .Odup,
            .Oroll => .{ .Oroll = null },
            .Oinc => .Oinc,
            .Odrop => .Odrop,
            .Oeq => .Oeq,
            .Oneq => .Oneq,
            .Olt => .Olt,
            .Ogt => .Ogt,
            .Oeor => .Oeor,
            .Odmod => .Odmod,
            .Omul => .Omul,
            .Odiv => .Odiv,
            .Oadd => .Oadd,
            .Osub => .Osub,
            .Ostash => .Ostash,
            .Olda => .Olda,
            .Osta => .Osta,
            .Odei => .Odei,
            .Odeo => .Odeo,
        };
    }

    pub fn format(
        value: @This(),
        comptime f: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        switch (value) {
            .Oraw => |l| try fmt.format(writer, "{}", .{l}),
            .Ozj => |j| try fmt.format(writer, "{?}", .{j}),
            .Osr => |j| try fmt.format(writer, "{?}", .{j}),
            .Onac => |n| try fmt.format(writer, "'{s}'", .{n}),
            .Oroll => |i| try fmt.format(writer, "{?}", .{i}),
            .Omul => |a| try fmt.format(writer, "{?}", .{a}),
            .Oadd => |a| try fmt.format(writer, "{?}", .{a}),
            .Osub => |a| try fmt.format(writer, "{?}", .{a}),
            else => try fmt.format(writer, "@", .{}),
        }
    }
};

pub const Ins = struct {
    stack: usize,
    keep: bool = false,
    short: bool = false,
    op: Op,

    // Generic, may be either short or not
    // Will be lowered once analyser is done on calling function
    generic: bool = false,

    pub const List = std.ArrayList(Ins);

    pub fn format(
        value: @This(),
        comptime f: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        const stk: []const u8 = if (value.stack == RT_STACK) "r" else " ";
        const osz: []const u8 = if (value.short) "2" else " ";
        try fmt.format(writer, "[{s}{s}] {s: <5} {}", .{
            stk, osz, @tagName(value.op), value.op,
        });
    }
};

// ----------------------------------------------------------------------------

test "Op.fromTag" {
    inline for (@typeInfo(Op.Tag).Enum.fields) |variant| {
        const e = @field(Op.Tag, variant.name);
        if (Op.fromTag(e)) |v| {
            try std.testing.expectEqual(e, v);
        } else |_| {}
    }
}

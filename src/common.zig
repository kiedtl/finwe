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
    Ptr8: Ptr,
    Ptr16: Ptr,
    Any,
    Any8,
    Any16,

    String,
    Quote,

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
        indirection: usize, // Always >=1. 1 == *Typ, 2 == **Typ, etc
    };

    pub const EnumLit = struct {
        type: usize, // index into Program.types
        field: usize,
    };

    pub fn eq(a: @This(), b: @This()) bool {
        return switch (a) {
            .EnumLit => |e| b == .EnumLit and e == b.EnumLit,
            .Ptr8 => |p| b == .Ptr8 and p.typ == b.Ptr8.typ and p.indirection == b.Ptr8.indirection,
            .Ptr16 => |p| b == .Ptr16 and p.typ == b.Ptr16.typ and p.indirection == b.Ptr16.indirection,
            .AmbigEnumLit => unreachable,
            else => @as(Tag, a) == @as(Tag, b),
        };
    }

    pub fn ptr16(program: *Program, to: TypeInfo, indirection: usize) TypeInfo {
        assert(indirection >= 1);
        const ind: ?usize = for (program.builtin_types.items, 0..) |typ, i| {
            if (to.eq(typ)) break i;
        } else null;
        if (ind) |_ind| {
            return .{ .Ptr16 = .{ .typ = _ind, .indirection = indirection } };
        } else {
            program.builtin_types.append(to) catch unreachable;
            return .{ .Ptr16 = .{
                .typ = program.builtin_types.items.len - 1,
                .indirection = indirection,
            } };
        }
    }

    pub fn isGeneric(self: @This()) bool {
        return switch (self) {
            .Any, .Any8, .Any16, .AnyPtr, .TypeRef => true,
            else => false,
        };
    }

    pub fn doesInclude(self: @This(), other: @This(), program: *const Program) bool {
        return (self != .EnumLit and @as(Tag, self) == @as(Tag, other)) or switch (self) {
            .Any => true,
            .Any8 => other.bits(program) != null and other.bits(program).? == 8,
            .Any16 => other.bits(program) != null and other.bits(program).? == 16,
            .AnyPtr => other == .Ptr8 or other == .Ptr16,
            .EnumLit => |e| other == .EnumLit and e == other.EnumLit,
            else => false, // self == other case already handled earlier
        };
    }

    pub fn bits(self: @This(), program: *const Program) ?u5 {
        return switch (self) {
            .U8, .Char8, .Ptr8, .Bool, .Any8 => 8,
            .U16, .Char16, .Ptr16, .Any16 => 16,
            .AnyPtr, .Any => null,
            .EnumLit => |e| if (program.types.items[e].def.Enum.is_short) 16 else 8,
            .String => unreachable, // TODO: remove string
            .Quote, .AmbigEnumLit, .TypeRef => unreachable,
        };
    }
};

pub const Value = struct {
    typ: TypeInfo,
    val: V,

    pub const V = union(enum) {
        u8: u8,
        u16: u16,
        String: String,
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
            .String => @panic("Free-standing strings are unimplemented"),
            else => unreachable,
        };
    }

    pub fn toU8(self: Value, program: *Program) u8 {
        return switch (self.typ) {
            .Bool, .U8, .Char8, .Ptr8 => self.val.u8,
            .EnumLit => |e| program.types.items[e].def.Enum.fields.items[self.val.EnumLit.field].value_a, // TODO: value_b
            .AmbigEnumLit => unreachable,
            .Any, .Any8, .Any16, .AnyPtr => unreachable,
            .String => @panic("Free-standing strings are unimplemented"),
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

    fn _deepcloneASTList(lst: ASTNodeList) ASTNodeList {
        var new = ASTNodeList.init(gpa.allocator());
        var itr = lst.iterator();
        while (itr.next()) |item| {
            new.append(item.deepclone()) catch unreachable;
        }
        return new;
    }

    pub fn deepclone(self: @This()) @This() {
        var new = self.node;
        switch (new) {
            .When => {
                new.When.yup = _deepcloneASTList(new.When.yup);
                if (new.When.nah) |n|
                    new.When.nah = _deepcloneASTList(n);
            },
            .Cond => |*c| {
                if (c.else_branch != null)
                    c.else_branch = _deepcloneASTList(c.else_branch.?);
                for (c.branches.items, 0..) |br, i| {
                    c.branches.items[i].body = _deepcloneASTList(br.body);
                    c.branches.items[i].cond = _deepcloneASTList(br.cond);
                }
            },
            .Loop => {
                new.Loop.body = _deepcloneASTList(new.Loop.body);
                switch (new.Loop.loop) {
                    .Until => |u| new.Loop.loop.Until.cond = _deepcloneASTList(u.cond),
                }
            },
            .Decl => new.Decl.body = _deepcloneASTList(new.Decl.body),
            .Mac => new.Mac.body = _deepcloneASTList(new.Mac.body),
            .Quote => new.Quote.body = _deepcloneASTList(new.Quote.body),
            .Wild => new.Wild.body = _deepcloneASTList(new.Wild.body),
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

pub const Program = struct {
    ast: ASTNodeList,
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
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

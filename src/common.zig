const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;

const lexer = @import("lexer.zig");
const analysis = @import("analyser.zig");
const common = @This();

const LinkedList = @import("list.zig").LinkedList;

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

//pub const ASTNodeList = LinkedList(ASTNode);
pub const ASTNodeList = std.ArrayList(ASTNode);
pub const ASTNodePtrList = std.ArrayList(*ASTNode);

// pub const Value = union(enum) {
//     U8: u8,
//     EnumLit: []const u8,

//     pub const Tag = std.meta.Tag(Value);

//     pub fn asBool(self: Value) bool {
//         return switch (self) {
//             .U8 => |n| n != 0,
//             .EnumLit => true,
//         };
//     }
// };

pub const ASTNode = struct {
    __prev: ?*ASTNode = null,
    __next: ?*ASTNode = null,

    node: ASTNode.Type,
    srcloc: usize,
    romloc: usize = 0,

    pub const Tag = std.meta.Tag(ASTNode.Type);

    pub const Type = union(enum) {
        None, // Placeholder for removed ast values
        Decl: Decl, // word declaraction
        Mac: Mac, // macro declaraction
        Call: Call,
        Loop: Loop,
        Cond: Cond,
        Asm: Ins,
        Value: ASTValue,
        Quote: Quote,
    };

    pub const ASTValue = union(enum) {
        T,
        Nil,
        U8: u8,
        Codepoint: u8,
        String: String,
        AmbigEnumLit: lexer.Node.EnumLit,
        EnumLit: EnumLit,

        pub const Tag = std.meta.Tag(ASTValue);

        pub fn toU8(self: ASTValue, program: *Program) u8 {
            return switch (self) {
                .T => 1,
                .Nil => 0,
                .U8 => |v| v,
                .Codepoint => |v| v,
                .String => @panic("Free-standing strings are unimplemented"),
                .EnumLit => |e| program.types.items[e.type].def.Enum.fields.items[e.field].value_a, // TODO: value_b
                .AmbigEnumLit => unreachable,
            };
        }
    };

    pub const Call = struct {
        name: []const u8,
        ctyp: enum { Mac, Decl, Unchecked } = .Unchecked,
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
            Until: struct { cond: ASTNodeList },
        };
    };

    pub const Decl = struct {
        name: []const u8,
        analysis: ?analysis.BlockAnalysis = null,
        body: ASTNodeList,
    };

    pub const Mac = struct {
        name: []const u8,
        analysis: ?analysis.BlockAnalysis = null,
        body: ASTNodeList,
    };

    pub const Quote = struct {
        body: ASTNodeList,
    };

    pub const EnumLit = struct {
        type: usize, // index into Program.types
        field: usize,
    };
};

pub const Program = struct {
    ast: ASTNodeList,
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
    types: Type.AList,

    // scopes: Scope.AList,
    // pub const Scope = struct {
    //     defs: ASTNodePtrList,
    //     macs: ASTNodePtrList,
    //     pub const AList = std.ArrayList(Scope);
    // };

    pub fn addNativeType(self: *Program, comptime T: type, name: []const u8) void {
        switch (@typeInfo(T)) {
            .Enum => |info| {
                if (info.tag_type != u16 and info.tag_type != u8)
                    @compileError("Enum type must be either u8 or u16");
                var t = Type{ .node = null, .name = name, .def = .{ .Enum = .{ .fields = std.ArrayList(Type.EnumField).init(gpa.allocator()) } } };
                inline for (info.fields) |field| {
                    const va: u8 = if (info.tag_type == u16) @intCast(u16, field.value) & 0xFF else @intCast(u8, field.value);
                    const vb: ?u8 = if (info.tag_type == u16) (@intCast(u16, field.value) >> 4) & 0xFF else null;
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
    Odup,
    Oroll: ?u8,
    Odrop: ?u8,
    Oeq,
    Oneq,
    Olt,
    Ogt,
    Oeor,
    Odmod,
    Omul,
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
            .Odup => .Odup,
            .Oroll => .{ .Oroll = null },
            .Odrop => .{ .Odrop = null },
            .Oeq => .Oeq,
            .Oneq => .Oneq,
            .Olt => .Olt,
            .Ogt => .Ogt,
            .Oeor => .Oeor,
            .Odmod => .Odmod,
            .Omul => .Omul,
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
            .Ozj => |j| try fmt.format(writer, "{}", .{j}),
            .Osr => |j| try fmt.format(writer, "{}", .{j}),
            .Onac => |n| try fmt.format(writer, "'{s}'", .{n}),
            .Oroll => |i| try fmt.format(writer, "{}", .{i}),
            .Omul => |a| try fmt.format(writer, "{}", .{a}),
            .Oadd => |a| try fmt.format(writer, "{}", .{a}),
            .Osub => |a| try fmt.format(writer, "{}", .{a}),
            else => try fmt.format(writer, "@", .{}),
        }
    }
};

pub const Ins = struct {
    stack: usize,
    keep: bool = false,
    short: bool = false,
    op: Op,

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

        var str: []const u8 = undefined;
        inline for (@typeInfo(Op.Tag).Enum.fields) |enum_field| {
            if (@intToEnum(Op.Tag, enum_field.value) == value.op) {
                str = enum_field.name;
                //break; // FIXME: Wait for that bug to be fixed, then uncomment
            }
        }

        try fmt.format(writer, "<[{}] {s} {}>", .{ value.stack, str, value.op });
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

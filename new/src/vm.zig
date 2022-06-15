const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const meta = std.meta;

const Value = @import("common.zig").Value;
const ASTNode = @import("common.zig").ASTNode;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;

pub const Op = union(enum) {
    O, // nop
    Olit: Value,
    Osave,
    Oj: ?usize,

    pub const Tag = meta.Tag(Op);
};

pub const Ins = struct {
    stack: usize,
    op: Op,

    pub const List = std.ArrayList(Ins);

    pub fn format(value: @This(), comptime f: []const u8, opts: fmt.FormatOptions, writer: anytype) !void {
        _ = opts;

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

        try fmt.format(writer, "<[{}] {s}>", .{ value.stack, str });
    }
};

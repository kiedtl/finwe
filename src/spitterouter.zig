const std = @import("std");

const Ins = @import("common.zig").Ins;
const RT_STACK = @import("common.zig").RT_STACK;

pub fn spitout(program: []const Ins) !void {
    var stdout = std.io.getStdOut().writer();
    for (program) |ins| {
        var byte = switch (ins.op) {
            .Oraw => |v| v,
            .Oeq => 0x08,
            .Oneq => 0x09,
            .Odeo => 0x17,
            .Olit => 0x80,
            .Odup => 0x06,
            .Ojcn => 0x0d,
            .Ohalt => 0x0,
            else => @panic("unimplemented"),
        };
        if (ins.short)
            byte |= 0x20;
        if (ins.stack == RT_STACK)
            byte |= 0x40;
        if (ins.keep)
            byte |= 0x80;
        try stdout.writeByte(byte);
    }
}

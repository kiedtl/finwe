const std = @import("std");

const Ins = @import("common.zig").Ins;
const RT_STACK = @import("common.zig").RT_STACK;

pub fn spitout(program: []const Ins) !void {
    var stdout = std.io.getStdOut().writer();
    for (program) |ins| {
        var byte = switch (ins.op) {
            .Oraw => |v| v,
            .Ohalt => 0x0,
            // .Oinc => 0x01,
            .Odrop => 0x02,
            .Onip => 0x03,
            .Oswp => 0x04,
            .Orot => 0x05,
            .Odup => 0x06,
            .Oovr => 0x07,
            .Oeq => 0x08,
            .Oneq => 0x09,
            .Ojmp => 0x0c,
            .Ojcn => 0x0d,
            .Ojsr => 0x0e,
            .Odeo => 0x17,
            .Olit => 0x80,
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

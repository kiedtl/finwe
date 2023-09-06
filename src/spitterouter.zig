const std = @import("std");

const Ins = @import("common.zig").Ins;

pub fn spitout(program: []const Ins) !void {
    for (program) |ins| {
        var byte = switch (ins.op) {
            .Oraw => |v| v,
            .Oeq => 0x08,
            .Odeo => 0x17,
            .Olit => 0x80,
            .Odup => 0x06,
            .Ojcn => 0x0d,
            .Ohalt => 0x0,
            else => @panic("unimplemented"),
        };
        try std.io.getStdOut().writer().writeByte(byte);
    }
}

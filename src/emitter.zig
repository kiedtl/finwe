const std = @import("std");

const Ins = @import("common.zig").Ins;
const RT_STACK = @import("common.zig").RT_STACK;

pub fn spitout(program: []const Ins) !void {
    var stdout = std.io.getStdOut().writer();
    for (program) |ins| {
        var byte = switch (ins.op) {
            .Oraw => |v| v,
            .Ohalt => 0x0,
            .Oinc => 0x01,
            .Odrop => 0x02,
            .Onip => 0x03,
            .Oswp => 0x04,
            .Orot => 0x05,
            .Odup => 0x06,
            .Oovr => 0x07,
            .Oeq => 0x08,
            .Oneq => 0x09,
            .Ogt => 0x0a,
            .Olt => 0x0b,
            .Ojmp => 0x0c,
            .Ojcn => 0x0d,
            .Ojsr => 0x0e,
            .Osth => 0x0f,
            // ldz/stz/ldr/str
            .Olda => 0x14,
            .Osta => 0x15,
            .Odei => 0x16,
            .Odeo => 0x17,
            .Oadd => 0x18,
            .Osub => 0x19,
            .Omul => 0x1a,
            .Odiv => 0x1b,
            .Oand => 0x1c,
            .Oora => 0x1d,
            .Oeor => 0x1e,
            .Osft => 0x1f,
            .Olit => 0x80,
            else => {
                std.log.err("{} not implemented", .{ins});
                @panic("TODO");
            },
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

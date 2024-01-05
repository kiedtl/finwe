const std = @import("std");

const Ins = @import("common.zig").Ins;
const OpTag = @import("common.zig").OpTag;
const RT_STACK = @import("common.zig").RT_STACK;

pub fn spitout(writer: anytype, program: []const Ins) !void {
    for (program) |ins| {
        if (ins.op == .Oraw) {
            try writer.writeByte(ins.op.Oraw);
        } else {
            const byte = @intFromEnum(@as(OpTag, ins.op));
            const ms: u8 = if (ins.short) 0x20 else 0;
            const mr: u8 = if (ins.stack == RT_STACK) 0x40 else 0;
            const mk: u8 = if (ins.keep) 0x80 else 0;
            try writer.writeByte(byte | ms | mr | mk);
        }
    }
}

const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");
const Program = common.Program;
const Error = common.Error;
const Srcloc = common.Srcloc;
const gpa = &common.gpa;

pub fn printError(e: Error, lines: []const []const u8) void {
    var stderr = std.io.getStdErr().writer();

    for (e.l.line -| 3..e.l.line) |line|
        stderr.print("\x1b[38;5;15m{: >4} |\x1b[m {s}\n", .{ line, lines[line] }) catch unreachable;
    for (0..e.l.column + 4 + 2) |_|
        stderr.print(" ", .{}) catch unreachable;
    stderr.print("\x1b[91;1m^\x1b[m\n", .{}) catch unreachable;
    stderr.print("\x1b[91m{}:\x1b[37;1m {}\x1b[m\n", .{ e.e, e.e }) catch unreachable;
    stderr.print("      \x1b[36mat \x1b[33m{}\x1b[m:\x1b[34m{}\x1b[m\n", .{
        e.l.line, e.l.column,
    }) catch unreachable;
}

pub fn printErrors(program: *Program, src: []const u8) void {
    var lines = std.ArrayList([]const u8).init(gpa.allocator());
    defer lines.deinit();

    var iter = mem.splitScalar(u8, src, '\n');
    while (iter.next()) |line| lines.append(line) catch unreachable;

    for (program.errors.items) |err|
        printError(err, lines.items);
}

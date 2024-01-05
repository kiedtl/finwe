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
        stderr.print("\x1b[38;5;15m{: >4} |\x1b[m {s}\n", .{ line + 1, lines[line] }) catch unreachable;
    for (0..e.l.column + 4 + 2) |_|
        stderr.print(" ", .{}) catch unreachable;
    stderr.print("\x1b[91;1m^\x1b[m\n", .{}) catch unreachable;
    stderr.print("\x1b[91m{}:\x1b[37;1m {}\x1b[m\n", .{ e.e, e.e }) catch unreachable;
    stderr.print("      \x1b[36mat \x1b[m{s}:\x1b[33m{}\x1b[m:\x1b[34m{}\x1b[m\n", .{
        e.l.file, e.l.line, e.l.column,
    }) catch unreachable;
}

pub fn printErrors(program: *Program, filename: []const u8) void {
    const Buf = struct {
        path: []const u8,
        buf: []u8,
        lines: std.ArrayList([]const u8),

        const AList = std.ArrayList(@This());

        pub fn addOrGet(self: *AList, path: []const u8) []const []const u8 {
            for (self.items) |buf| {
                if (mem.eql(u8, buf.path, path))
                    return buf.lines.items;
            }

            const file = std.fs.cwd().openFile(path, .{}) catch unreachable;
            const size = file.getEndPos() catch unreachable;
            defer file.close();

            var new: @This() = undefined;
            new.path = path;
            new.buf = gpa.allocator().alloc(u8, size) catch unreachable;
            _ = file.readAll(new.buf) catch unreachable;

            new.lines = std.ArrayList([]const u8).init(gpa.allocator());
            var iter = mem.splitScalar(u8, new.buf, '\n');
            while (iter.next()) |line|
                new.lines.append(line) catch unreachable;
            self.append(new) catch unreachable();

            return self.items[self.items.len - 1].lines.items;
        }
    };

    var bufs = Buf.AList.init(gpa.allocator());
    defer bufs.deinit();
    defer for (bufs.items) |buf| {
        buf.lines.deinit();
        gpa.allocator().free(buf.buf);
    };

    _ = Buf.addOrGet(&bufs, filename);

    for (program.errors.items) |err|
        printError(err, Buf.addOrGet(&bufs, err.l.file));
}

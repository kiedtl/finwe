const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");
const Program = common.Program;
const Error = common.Error;
const Srcloc = common.Srcloc;
const TypeFmt = common.TypeFmt;
const gpa = &common.gpa;
const AnalysisFmt = @import("analyser.zig").AnalysisFmt;

pub fn printError(program: *Program, e: Error, lines: []const []const u8) void {
    var stderr = std.io.getStdErr().writer();

    for (e.l.line -| 3..e.l.line) |line|
        stderr.print("\x1b[38;5;15m{: >4} |\x1b[m {s}\n", .{ line + 1, lines[line] }) catch unreachable;
    for (0..e.l.column + 4 + 2) |_|
        stderr.print(" ", .{}) catch unreachable;
    stderr.print("\x1b[91;1m^\x1b[m\n", .{}) catch unreachable;
    stderr.print("\x1b[91m{s}:\x1b[37;1m ", .{@errorName(e.e)}) catch unreachable;
    switch (e.e) {
        error.ExpectedItems => stderr.print("Not enough arguments (min {}, got {})", .{
            e.ctx.usize1.?, e.ctx.usize2.?,
        }) catch unreachable,
        error.UnexpectedItems => stderr.print("Too many arguments (max {}, got {})", .{
            e.ctx.usize1.?, e.ctx.usize2.?,
        }) catch unreachable,
        error.ExpectedNode => stderr.print("Expected {}, got {}", .{
            e.ctx.lexnodetype1.?, e.ctx.lexnodetype2.?,
        }) catch unreachable,
        error.ExpectedValue => stderr.print("Expected value, got {}", .{
            e.ctx.lexnodetype1.?,
        }) catch unreachable,
        error.ExpectedNum => stderr.print("Expected number value, got {}", .{
            e.ctx.lexnodetype1.?,
        }) catch unreachable,
        error.InvalidType => if (e.ctx.string1) |str| {
            stderr.print("\"{s}\" is not a valid type", .{str}) catch unreachable;
        } else {
            stderr.print("Expression cannot be parsed into type", .{}) catch unreachable;
        },
        error.InvalidMetadata => if (e.ctx.string1) |str| {
            stderr.print("\"{s}\" is not a recognized metadata", .{str}) catch unreachable;
        } else {
            stderr.print("Expression is not valid metadata", .{}) catch unreachable;
        },
        error.InvalidKeyword => stderr.print("Invalid keyword \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.InvalidEnumField => stderr.print("No such field \"{s}\" in {s}", .{
            e.ctx.string1.?, e.ctx.string2.?,
        }) catch unreachable,
        error.NoSuchType => stderr.print("No such type \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.InvalidFieldType => stderr.print("Type {} cannot be in container field", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.UnknownLocal => stderr.print("No such variable \"{s}\" in scope", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.UnknownIdent => stderr.print("No such function \"{s}\" in scope", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.GenericNotMatching => stderr.print("Arg {} not included by parameter {}", .{
            TypeFmt.from(e.ctx.burtype1.?, program), TypeFmt.from(e.ctx.burtype2.?, program),
        }) catch unreachable,
        error.TypeNotMatching => stderr.print("Arg {} doesn't match parameter {}", .{
            TypeFmt.from(e.ctx.burtype1.?, program), TypeFmt.from(e.ctx.burtype2.?, program),
        }) catch unreachable,
        error.CannotCallMethod => stderr.print("Cannot call method on type {}", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.StructNotForStack => stderr.print("Struct {} does not fit on stack", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.CannotGetIndex => stderr.print("{} cannot be indexed", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.InvalidIndexType => stderr.print("{} cannot be used as index, only u8/u16", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.IndexWouldOverflow => stderr.print("Index of {} would overflow (type size is {})", .{
            e.ctx.ushort1.?, e.ctx.ushort2.?,
        }) catch unreachable,
        error.IndexTooLarge => stderr.print("Index {} larger than container length {}", .{
            e.ctx.ushort1.?, e.ctx.ushort2.?,
        }) catch unreachable,
        error.CannotGetFieldMultiPtr => stderr.print("Cannot get field of {} (too many indirections)", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.CannotGetField => stderr.print("Cannot get field of {}", .{
            TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.NoSuchField => stderr.print("No such field \"{s}\" in {}", .{
            e.ctx.string1.?, TypeFmt.from(e.ctx.burtype1.?, program),
        }) catch unreachable,
        error.CannotSplitIntoShort => stderr.print("Split() targets must be byte-sized", .{}) catch unreachable,
        error.CannotSplitByte => stderr.print("Split() argument must be short-sized", .{}) catch unreachable,
        error.StackMismatch => stderr.print("Stack doesn't match arity: ({s} vs {s})", .{
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
        }) catch unreachable,
        error.StackNotEmpty => stderr.print("Main and tests must end with empty stack", .{}) catch unreachable,
        error.StackBranching1 => stderr.print("Stack different across when branches (if: {s}; else: {s})", .{
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
        }) catch unreachable,
        // HINT: add else branch
        error.StackBranching2 => stderr.print("Stack at end of when clause must not change (when: {s}; previous: {s})", .{
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
        }) catch unreachable,
        // error.Template => stderr.print("ohno {} {}", .{
        //     e.ctx.usize1.?, e.ctx.usize2.?,
        // }) catch unreachable,
        else => stderr.print("TODO: error message", .{}) catch unreachable,
    }
    stderr.print("\x1b[m\n", .{}) catch unreachable;
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
        printError(program, err, Buf.addOrGet(&bufs, err.l.file));
}

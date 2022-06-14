const std = @import("std");
const mem = std.mem;

const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");

pub var gpa = std.heap.GeneralPurposeAllocator(.{
    // Probably should enable this later on to track memory usage, if
    // allocations become too much
    .enable_memory_limit = false,

    .safety = true,

    // Probably would enable this later?
    .thread_safe = false,

    .never_unmap = false,
}){};

pub fn main() anyerror!void {
    const file = try std.fs.cwd().openFile("code.zf", .{});
    defer file.close();

    const size = try file.getEndPos();
    const buf = try gpa.allocator().alloc(u8, size);
    defer gpa.allocator().free(buf);
    _ = try file.readAll(buf);

    var lexer = lexerm.Lexer.init(buf, gpa.allocator());
    const lexed = try lexer.lex();
    _ = lexed;

    var parser = parserm.Parser.init(gpa.allocator());
    var program = try parser.parse(&lexed);

    std.log.info("program:\n{any}", .{program.items});

    //try codegen.generate(&program, &emitted, gpa.allocator());

    //const outf = try std.fs.cwd().createFile("code.ch8", .{ .truncate = true });
    //try outf.writeAll(emitted.constSlice());
}

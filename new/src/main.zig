const std = @import("std");
const mem = std.mem;

const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");
const vm = @import("vm.zig");
const codegen = @import("codegen.zig");

const gpa = &@import("common.zig").gpa;

pub fn main() anyerror!void {
    const file = try std.fs.cwd().openFile("code.zf", .{});
    defer file.close();

    const size = try file.getEndPos();
    const buf = try gpa.allocator().alloc(u8, size);
    defer gpa.allocator().free(buf);
    _ = try file.readAll(buf);

    var lexer = lexerm.Lexer.init(buf, gpa.allocator());
    const lexed = try lexer.lex();

    var parser = parserm.Parser.init(gpa.allocator());
    var parsed = try parser.parse(&lexed);

    var assembled = try codegen.generate(&parsed);

    std.log.info("program:\n{any}", .{assembled.items});

    //try codegen.generate(&program, &emitted, gpa.allocator());

    //const outf = try std.fs.cwd().createFile("code.ch8", .{ .truncate = true });
    //try outf.writeAll(emitted.constSlice());
}

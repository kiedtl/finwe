const std = @import("std");
const mem = std.mem;

const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");
const vmm = @import("vm.zig");
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
    const lexed = try lexer.lex(.Root);
    defer lexer.deinit();

    var parser = parserm.Parser.init(gpa.allocator());
    parser.initTypes();
    var parsed = try parser.parse(&lexed);

    var assembled = try codegen.generate(&parsed);
    for (assembled.items) |asmstmt, i| {
        _ = asmstmt;
        _ = i;
        //std.log.info("{} -\t{}", .{ i, asmstmt });
    }
    //std.log.info("--------------------------------------------------", .{});

    var vm = vmm.VM.init(assembled.items);
    try vm.execute();
}

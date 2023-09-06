const std = @import("std");
const clap = @import("clap");
const mem = std.mem;

const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");
const vmm = @import("vm.zig");
const codegen = @import("codegen.zig");
const spitterouter = @import("spitterouter.zig");

const gpa = &@import("common.zig").gpa;

pub fn main() anyerror!void {
    const params = comptime [_]clap.Param(clap.Help){
        clap.parseParam("-x, --spitout       Directly output UXN binary.") catch unreachable,
        clap.parseParam("<FILE>...") catch unreachable,
    };

    // Initalize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var args = clap.parse(clap.Help, &params, .{ .diagnostic = &diag }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return;
    };
    defer args.deinit();

    for (args.positionals()) |filename| {
        const file = try std.fs.cwd().openFile(filename, .{});
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
            std.log.info("{} -\t{}", .{ i, asmstmt });
        }
        std.log.info("--------------------------------------------------", .{});

        if (args.flag("--spitout")) {
            try spitterouter.spitout(assembled.items);
        } else {
            var vm = vmm.VM.init(assembled.items);
            try vm.execute();
        }
    }
}

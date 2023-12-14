const std = @import("std");
const clap = @import("clap");
const mem = std.mem;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const analyser = @import("analyser.zig");
const codegen = @import("codegen.zig");
const common = @import("common.zig");
const emitter = @import("emitter.zig");
const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");
const vmm = @import("vm.zig");

const gpa = &@import("common.zig").gpa;

pub fn main() anyerror!void {
    const params = comptime [_]clap.Param(clap.Help){
        clap.parseParam("-x, --spitout       Directly output UXN binary.") catch unreachable,
        clap.parseParam("-1, --debug-asm     Output ASM to stderr.") catch unreachable,
        clap.parseParam("-2, --debug-inf     Output word analysis to stderr.") catch unreachable,
        clap.parseParam("<str>...") catch unreachable,
    };

    // Initalize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var args = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
    }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return;
    };
    defer args.deinit();

    for (args.positionals) |filename| {
        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        const size = try file.getEndPos();
        const buf = try gpa.allocator().alloc(u8, size);
        defer gpa.allocator().free(buf);
        _ = try file.readAll(buf);

        var lexer = lexerm.Lexer.init(buf, gpa.allocator());
        const lexed = try lexer.lex(.Root);
        defer lexer.deinit();

        var program = common.Program.init(gpa.allocator());

        var parser = parserm.Parser.init(&program, gpa.allocator());
        parser.initTypes();
        parser.parse(&lexed) catch |e| {
            if (program.errors.items.len > 0) {
                errors.printErrors(&program, buf);
                std.os.exit(1);
            } else {
                @panic(@errorName(e));
            }
        };

        analyser.analyse(&program) catch |e| {
            if (program.errors.items.len > 0) {
                errors.printErrors(&program, buf);
                std.os.exit(1);
            } else {
                @panic(@errorName(e));
            }
        };

        if (args.args.@"debug-inf" != 0)
            for (program.defs.items) |def| {
                const d = def.node.Decl;
                std.log.info("Word {s}: {}", .{ d.name, d.analysis });
            };

        const assembled = try codegen.generate(&program);
        if (args.args.@"debug-asm" != 0)
            for (assembled.items, 0..) |asmstmt, i| {
                std.log.info("{} -\t{}", .{ i, asmstmt });
            };
        std.log.info("--------------------------------------------------", .{});

        if (args.args.spitout != 0) {
            const stdout = std.io.getStdOut().writer();
            try emitter.spitout(stdout, assembled.items);
        } else {
            var vm = vmm.VM.init(assembled.items);
            vm.execute();
        }
    }

    // FIXME: all memory is leaked lol
}

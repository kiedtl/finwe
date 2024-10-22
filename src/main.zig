const std = @import("std");
const clap = @import("clap");
const mem = std.mem;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const analyser = @import("analyser.zig");
const codegen = @import("codegen.zig");
const common = @import("common.zig");
const lexerm = @import("lexer.zig");
const parserm = @import("parser.zig");
const optimizer = @import("optimizer.zig");
const vmm = @import("vm.zig");

const gpa = &@import("common.zig").gpa;

pub fn main() anyerror!void {
    const alloc = gpa.allocator();
    // defer {
    //     _ = gpa.deinit();
    // }

    const params = comptime clap.parseParamsComptime(
        \\-h, --help               Print this help message.
        \\-t, --test               Run test harness.
        \\-1, --debug-asm          Output ASM to stderr.
        \\-2, --debug-inf          Output word analysis to stderr.
        \\-x, --emit <str>         Output UXN rom to file.
        \\-a, --dump-asm <str>...  Print UXN bytecode for function.
        \\-d, --emit-debug         Output debug info to syms file. Requires -x.
        \\-g, --graphical          Enable graphical mode.
        \\<str>...
    );

    // Initalize our diagnostics, which can be used for reporting useful errors.
    // This is optional. You can also pass `.{}` to `clap.parse` if you don't
    // care about the extra information `Diagnostics` provides.
    var diag = clap.Diagnostic{};
    var args = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .allocator = alloc,
        .diagnostic = &diag,
    }) catch |err| {
        // Report useful error and exit
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return;
    };
    defer args.deinit();

    if (args.args.help != 0) {
        const stderr = std.io.getStdErr().writer();
        stderr.print("Usage: finwe [options] [args]\n", .{}) catch {};
        clap.help(stderr, clap.Help, &params, .{
            .description_on_new_line = false,
            .description_indent = 0,
            .indent = 3,
            .spacing_between_parameters = 0,
        }) catch {};
        return;
    }

    for (args.positionals) |filename| {
        const file_path = std.fs.cwd().realpathAlloc(alloc, filename) catch |e| {
            std.log.err("Couldn't get absolute file path: {}", .{e});
            std.process.exit(1);
        };
        defer alloc.free(file_path);

        const file_dir = std.fs.path.dirname(file_path) orelse "/";

        var program = common.Program.init(alloc, file_dir) catch |e| {
            switch (e) {
                error.CannotOpenFileDir => {
                    std.log.err("Can't open file directory.", .{});
                },
                error.CannotOpenSelfDir => {
                    std.log.err("Can't open compiler directory.", .{});
                },
            }
            std.process.exit(1);
        };
        defer program.deinit();

        const file = try std.fs.cwd().openFile(filename, .{});
        defer file.close();

        const size = try file.getEndPos();
        const buf = try alloc.alloc(u8, size);
        defer alloc.free(buf);
        _ = try file.readAll(buf);

        var lexer = lexerm.Lexer.init(&program, buf, filename, alloc);
        const lexed = lexer.lexList(.Root) catch {
            assert(program.errors.items.len > 0);
            errors.printErrors(&program, filename);
            std.process.exit(1);
        };
        defer lexer.deinit();
        defer lexerm.Node.deinitMain(lexed, alloc);

        program.flag_dampe = args.args.emit == null;
        program.flag_graphical = args.args.graphical != 0;

        var parser = parserm.Parser.init(&program, args.args.@"test" > 0, alloc);
        parser.parse(&lexed) catch |e| {
            if (program.errors.items.len > 0) {
                errors.printErrors(&program, filename);
                std.process.exit(1);
            } else {
                @panic(@errorName(e));
            }
        };

        analyser.analyse(&program, args.args.@"test" > 0) catch |e| {
            if (program.errors.items.len > 0) {
                errors.printErrors(&program, filename);
                std.process.exit(1);
            } else {
                @panic(@errorName(e));
            }
        };

        if (args.args.@"debug-inf" != 0)
            for (program.defs.items) |def| {
                const d = def.node.Decl;
                std.log.info("Word {s}: {}", .{ d.name, d.analysis });
            };

        var intermediate = common.Ins.List.init(gpa.allocator());
        try codegen.generate(&program, &intermediate);
        try optimizer.optimize(&program, &intermediate, false);
        for (args.args.@"dump-asm") |funcname|
            try codegen.printAsmFor(&program, &intermediate, funcname);
        var assembled = common.Ins.List.init(gpa.allocator());
        try codegen.resolveUAs(&program, &intermediate, &assembled);
        try optimizer.optimize(&program, &assembled, true);

        if (args.args.@"debug-asm" != 0)
            for (assembled.items, 0..) |asmstmt, i| {
                std.log.info("{} -\t{}", .{ i, asmstmt });
            };
        std.log.info("--------------------------------------------", .{});

        if (args.args.emit) |fname| {
            if (mem.eql(u8, fname, "-")) {
                const stdout = std.io.getStdOut().writer();
                try codegen.emitBytecode(stdout, assembled.items);
            } else {
                const out = try std.fs.cwd().createFile(fname, .{});
                defer out.close();
                try codegen.emitBytecode(out.writer(), assembled.items);
            }
            if (args.args.@"emit-debug" != 0) {
                const sfname = std.fmt.allocPrint(alloc, "{s}.syms", .{fname}) catch unreachable;
                defer alloc.free(sfname);
                const outsyms = try std.fs.cwd().createFile(sfname, .{});
                defer outsyms.close();
                try codegen.emitDebug(outsyms.writer(), &program);
            }
        } else if (args.args.@"test" != 0) {
            var vm = vmm.VM.init(&program, assembled.items);
            defer vm.deinit();
            vm.executeTests();
        } else {
            var vm = vmm.VM.init(&program, assembled.items);
            defer vm.deinit();
            vm.execute();
        }
    }
}

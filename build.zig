const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const clap_module = b.addModule("clap", .{
        .source_file = .{ .path = "third_party/zig-clap/clap.zig" },
    });

    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "bur",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("clap", clap_module);

    exe.linkLibC();
    exe.addIncludePath(.{ .path = "third_party/uxn/src/" });
    exe.addCSourceFiles(.{ .files = &[_][]const u8{
        "third_party/uxn/src/uxn.c",
        "third_party/uxn/src/uxnemu.c",
        "third_party/uxn/src/devices/system.c",
        "third_party/uxn/src/devices/console.c",
        "third_party/uxn/src/devices/screen.c",
        "third_party/uxn/src/devices/audio.c",
        "third_party/uxn/src/devices/file.c",
        "third_party/uxn/src/devices/controller.c",
        "third_party/uxn/src/devices/mouse.c",
        "third_party/uxn/src/devices/datetime.c",
    } });
    exe.addIncludePath(.{ .path = "/usr/include/SDL2/" });
    exe.linkSystemLibrary("SDL2");

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}

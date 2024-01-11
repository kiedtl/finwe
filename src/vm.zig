const std = @import("std");
const mem = std.mem;
const math = std.math;
const fmt = std.fmt;
const meta = std.meta;
const assert = std.debug.assert;
const linux = std.os.linux;

const codegen = @import("codegen.zig");
const ASTNode = @import("common.zig").ASTNode;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Value = @import("common.zig").Value;
const Srcloc = @import("common.zig").Srcloc;
const Ins = @import("common.zig").Ins;
const Program = @import("common.zig").Program;
const StackBuffer = @import("buffer.zig").StackBuffer;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;
const STACK_SZ = @import("common.zig").STACK_SZ;

const gpa = &@import("common.zig").gpa;

const c = @cImport({
    @cInclude("uxn.h");
    @cInclude("devices/system.h");
    @cInclude("devices/console.h");
    @cInclude("devices/screen.h");
    @cInclude("devices/audio.h");
    @cInclude("devices/file.h");
    @cInclude("devices/controller.h");
    @cInclude("devices/mouse.h");
    @cInclude("devices/datetime.h");
});

extern "c" fn set_zoom(z: u8, win: c_int) void;
extern "c" fn emu_init() c_int;
extern "c" fn emu_restart(u: [*c]c.Uxn, rom: [*c]u8, soft: c_int) void;
extern "c" fn emu_redraw(u: [*c]c.Uxn) c_int;
extern "c" fn emu_resize(width: c_int, height: c_int) c_int;
extern "c" fn emu_end(uxn: [*c]c.Uxn) c_int;
extern "c" fn base_emu_deo(uxn: [*c]c.Uxn, addr: c_char) void;

const Item = struct { romloc: usize, node: *ASTNode };

const Range = struct {
    a: usize,
    b: usize,
    rst: [0x100]u8,
    rst_ptr: usize,
};

pub const VM = struct {
    uxn: c.Uxn = undefined,
    ram: []u8,
    here: usize = 0,
    assembled: []const Ins,
    program: *Program,
    captured_stdout: std.ArrayList(u8),
    captured_stderr: std.ArrayList(u8),
    protected: std.ArrayList(Range),

    is_testing: bool = false,
    is_test_done: bool = false,
    is_breakpoint: bool = false,
    is_privileged: bool = false,

    pub fn init(program: *Program, assembled: []const Ins) VM {
        const ram = gpa.allocator().alloc(u8, 0x10000 * c.RAM_PAGES) catch
            @panic("please uninstall Chrome before proceeding (OOM)");
        @memset(ram, 0);

        var self = VM{
            .ram = ram,
            .here = assembled.len,
            .program = program,
            .assembled = assembled,
            .captured_stdout = std.ArrayList(u8).init(gpa.allocator()),
            .captured_stderr = std.ArrayList(u8).init(gpa.allocator()),
            .protected = std.ArrayList(Range).init(gpa.allocator()),
        };
        self.load();
        self.uxn.ram = ram.ptr;

        c.system_connect(0x0, c.SYSTEM_VERSION, c.SYSTEM_DEIMASK, c.SYSTEM_DEOMASK);
        c.system_connect(0x1, c.CONSOLE_VERSION, c.CONSOLE_DEIMASK, c.CONSOLE_DEOMASK);
        c.system_connect(0x2, c.SCREEN_VERSION, c.SCREEN_DEIMASK, c.SCREEN_DEOMASK);
        c.system_connect(0x3, c.AUDIO_VERSION, c.AUDIO_DEIMASK, c.AUDIO_DEOMASK);
        c.system_connect(0x4, c.AUDIO_VERSION, c.AUDIO_DEIMASK, c.AUDIO_DEOMASK);
        c.system_connect(0x5, c.AUDIO_VERSION, c.AUDIO_DEIMASK, c.AUDIO_DEOMASK);
        c.system_connect(0x6, c.AUDIO_VERSION, c.AUDIO_DEIMASK, c.AUDIO_DEOMASK);
        c.system_connect(0x8, c.CONTROL_VERSION, c.CONTROL_DEIMASK, c.CONTROL_DEOMASK);
        c.system_connect(0x9, c.MOUSE_VERSION, c.MOUSE_DEIMASK, c.MOUSE_DEOMASK);
        c.system_connect(0xa, c.FILE_VERSION, c.FILE_DEIMASK, c.FILE_DEOMASK);
        c.system_connect(0xb, c.FILE_VERSION, c.FILE_DEIMASK, c.FILE_DEOMASK);
        c.system_connect(0xc, c.DATETIME_VERSION, c.DATETIME_DEIMASK, c.DATETIME_DEOMASK);

        set_zoom(2, 0);
        if (emu_init() == 0)
            @panic("Emulator failed to init.");

        return self;
    }

    pub fn deinit(self: *VM) void {
        gpa.allocator().free(self.ram);
        self.captured_stdout.deinit();
        self.captured_stderr.deinit();
        self.protected.deinit();
    }

    pub fn load(self: *VM) void {
        var fbstream = std.io.fixedBufferStream(self.ram);
        var writer = fbstream.writer();
        writer.writeByteNTimes(0, 0x0100) catch unreachable;
        codegen.emitBytecode(writer, self.assembled) catch unreachable;
    }

    fn _getFuncNameForAddr(self: *VM, items: []const Item, addr: u16) ?*ASTNode {
        if (addr > self.here)
            return null;

        var prev: ?Item = null;
        for (items) |item| {
            if (item.romloc > addr) {
                if (prev != null and prev.?.romloc <= addr) {
                    return prev.?.node;
                } else {
                    return null;
                }
            }
            prev = item;
        }
        return null;
    }

    pub fn printBacktrace(self: *VM, rst: []const u8, rst_ptr: usize) void {
        const stderr = std.io.getStdErr().writer();

        var items = std.ArrayList(Item).init(gpa.allocator());
        defer items.deinit();

        for (self.program.defs.items) |def|
            items.append(.{ .romloc = def.romloc + 0x100, .node = def }) catch unreachable;

        std.sort.insertion(Item, items.items, {}, struct {
            pub fn lessThan(_: void, lhs: Item, rhs: Item) bool {
                return lhs.romloc < rhs.romloc;
            }
        }.lessThan);

        var ptr = rst_ptr;
        while (ptr > 0) : (ptr -= 2) {
            const addr = @as(u16, @intCast(rst[ptr -| 2])) << 8 | rst[ptr -| 1];
            stderr.print("  \x1b[36mat \x1b[m{x:0>4}", .{addr}) catch unreachable;
            if (self._getFuncNameForAddr(items.items, addr)) |node| {
                const s = if (node.node.Decl.is_test) "<test>" else node.node.Decl.name;
                stderr.print(" \x1b[37;1m{s: <15}\x1b[m", .{s}) catch unreachable;
                stderr.print(" ({s}:\x1b[33m{}\x1b[m:\x1b[34m{}\x1b[m)\n", .{
                    node.srcloc.file, node.srcloc.line, node.srcloc.column,
                }) catch unreachable;
            } else {
                stderr.print(" \x1b[37;1m???\x1b[m\n", .{}) catch unreachable;
            }
        }
    }

    pub fn execute(self: *VM) void {
        self.is_testing = false;

        const stderr = std.io.getStdErr().writer();

        var ctr: usize = 0;
        var pc: c_ushort = c.PAGE_PROGRAM;
        while (true) : (ctr += 1) {
            self._checkInstruction(pc, stderr);
            pc = c.uxn_eval_once(&self.uxn, pc);
            if (pc <= 1) break;
            assert(!self.is_breakpoint);
        }

        _ = emu_end(&self.uxn);
    }

    fn _failTest(stderr: anytype, comptime format: []const u8, args: anytype, romloc: usize, srcloc: Srcloc) !void {
        const failstr = "\x1b[2D\x1b[31m!!\x1b[m\n";
        stderr.print("{s}", .{failstr}) catch unreachable;
        stderr.print("\x1b[32m> \x1b[m" ++ format ++ "\n", args) catch unreachable;
        stderr.print("  \x1b[36mat \x1b[m{s}:\x1b[33m{}\x1b[m:\x1b[34m{}\x1b[m", .{
            srcloc.file, srcloc.line, srcloc.column,
        }) catch unreachable;
        stderr.print(" (\x1b[36mpc \x1b[m{x:0>4})\n", .{
            romloc + 0x100,
        }) catch unreachable;
        stderr.print("\n", .{}) catch unreachable;
        return error.Fail;
    }

    fn _handleBreak(self: *VM, pc: c_ushort, stderr: anytype, program: *Program) !void {
        // TODO: underflow checks
        const wst: [*c]u8 = @ptrCast(&self.uxn.wst.dat[self.uxn.wst.ptr -| 1]);

        const tosb = wst.*;
        const toss = @as(u16, @intCast((wst - @as(u8, 1)).*)) << 8 | tosb;
        const sosb = (wst - @as(u8, 2)).*;
        const soss = @as(u16, @intCast((wst - @as(u8, 3)).*)) << 8 | sosb;

        const breakpoint = for (program.breakpoints.items) |*brk| {
            if (brk.romloc + 0x100 == pc) break brk;
        } else return; // TODO: warn about unknown breakpoints
        const btyp = breakpoint.type;

        breakpoint.is_executed = true;

        switch (btyp) {
            .RCheckpoint => self.is_test_done = true,
            .TosShouldEq, .TosShouldEqSos => {
                const v: Value = switch (btyp) {
                    .TosShouldEqSos => |t| if (t.bits(program) == 16)
                        .{ .typ = .U16, .val = .{ .u16 = soss } }
                    else
                        .{ .typ = .U8, .val = .{ .u8 = sosb } },
                    .TosShouldEq => |v| v,
                    else => unreachable,
                };

                if (v.typ.bits(program).? == 16) {
                    if (v.toU16(program) != toss) {
                        try _failTest(stderr, "Expected 0x{x:0>4}, got 0x{x:0>4}", .{
                            v.toU16(program), toss,
                        }, breakpoint.romloc, breakpoint.srcloc);
                    }
                    self.uxn.wst.ptr -= 2;
                } else if (v.typ.bits(program).? == 8) {
                    if (v.toU8(program) != tosb) {
                        try _failTest(stderr, "Expected 0x{x:0>2}, got 0x{x:0>2}", .{
                            v.toU8(program), tosb,
                        }, breakpoint.romloc, breakpoint.srcloc);
                    }
                    self.uxn.wst.ptr -= 1;
                } else unreachable;
            },
            .TosShouldNeq, .TosShouldNeqSos => {
                const v: Value = switch (btyp) {
                    .TosShouldNeqSos => |t| if (t.bits(program) == 16)
                        .{ .typ = .U16, .val = .{ .u16 = soss } }
                    else
                        .{ .typ = .U8, .val = .{ .u8 = sosb } },
                    .TosShouldNeq => |v| v,
                    else => unreachable,
                };

                if (v.typ.bits(program).? == 16) {
                    if (v.toU16(program) == toss) {
                        try _failTest(stderr, "Unexpected 0x{x:0>4} == 0x{x:0>4}", .{
                            toss, soss,
                        }, breakpoint.romloc, breakpoint.srcloc);
                    }
                    self.uxn.wst.ptr -= 2;
                } else if (v.typ.bits(program).? == 8) {
                    if (v.toU8(program) == tosb) {
                        try _failTest(stderr, "Unexpected 0x{x:0>2} == 0x{x:0>2}", .{
                            tosb, sosb,
                        }, breakpoint.romloc, breakpoint.srcloc);
                    }
                    self.uxn.wst.ptr -= 1;
                } else unreachable;
            },
            .StdoutShouldEq => |v| {
                defer self.captured_stdout.shrinkAndFree(0);
                if (self.captured_stdout.items.len != v.items.len)
                    try _failTest(stderr, "Unexpected stdout output (len: {} vs {})", .{
                        v.items.len, self.captured_stdout.items.len,
                    }, breakpoint.romloc, breakpoint.srcloc);
                if (!mem.eql(u8, self.captured_stdout.items, v.items))
                    try _failTest(stderr, "Unexpected stdout output", .{}, breakpoint.romloc, breakpoint.srcloc);
            },
        }
    }

    fn _finalCheck(self: *VM, cur_test: *const ASTNode, stderr: anytype, program: *Program) !void {
        if (!self.is_test_done)
            try _failTest(stderr, "Test never returned!", .{}, cur_test.romloc, cur_test.srcloc);

        for (program.breakpoints.items) |brk| {
            if (brk.parent_test.? == &cur_test.node.Decl)
                if (brk.must_execute and !brk.is_executed)
                    if (brk.type == .RCheckpoint)
                        try _failTest(stderr, "Test never returned (test harness bug, should've caught earlier)!", .{}, brk.romloc, brk.srcloc)
                    else
                        try _failTest(stderr, "Assertion never checked (early return?)", .{}, brk.romloc, brk.srcloc);
        }

        self.is_test_done = false;
    }

    pub fn executeTests(self: *VM) void {
        const stderr = std.io.getStdErr().writer();

        self.is_testing = true;

        test_loop: for (self.program.defs.items) |decl_node| {
            const decl = decl_node.node.Decl;
            if (!decl.is_test) continue;
            assert(decl.is_analysed);

            stderr.print("{s}", .{decl.name}) catch unreachable;
            stderr.writeByteNTimes(' ', 50 - decl.name.len) catch unreachable;
            stderr.print("\x1b[34..\x1b[m", .{}) catch unreachable;

            // TODO: assert that stacks are empty after each test
            self.protected.shrinkAndFree(0);
            self.uxn.wst.ptr = 0;
            self.uxn.rst.ptr = 0;
            @memset(self.ram[0..], 0);
            self.load();

            var pc: c_ushort = @as(c_ushort, @intCast(decl_node.romloc)) + 0x100;
            assert(pc != 0xFFFF);

            while (true) {
                self._checkInstruction(pc, stderr);
                pc = c.uxn_eval_once(&self.uxn, pc);
                if (pc == 0) @panic("test halted");
                if (pc == 1) break;

                if (self.is_breakpoint) {
                    self.is_breakpoint = false;
                    _handleBreak(self, pc, stderr, self.program) catch continue :test_loop;
                }

                if (self.is_test_done) {
                    break;
                }
            }

            _finalCheck(self, decl_node, stderr, self.program) catch continue :test_loop;

            stderr.print("\x1b[2D\x1b[36mOK\x1b[m\n", .{}) catch unreachable;

            if (self.captured_stdout.items.len > 0) {
                stderr.print("\x1b[32m= \x1b[mTest stdout =\n", .{}) catch unreachable;
                stderr.print("{s}", .{self.captured_stdout.items}) catch unreachable;
                _printHappyPercent(stderr);
                stderr.print("\x1b[32m= \x1b[mEnd stdout =\n", .{}) catch unreachable;
                stderr.print("\n", .{}) catch unreachable;
                self.captured_stdout.shrinkAndFree(0);
            }

            if (self.captured_stderr.items.len > 0) {
                stderr.print("\x1b[32m= \x1b[mTest stderr =\n", .{}) catch unreachable;
                stderr.print("{s}", .{self.captured_stderr.items}) catch unreachable;
                _printHappyPercent(stderr);
                stderr.print("\x1b[32m= \x1b[mEnd stderr =\n", .{}) catch unreachable;
                stderr.print("\n", .{}) catch unreachable;
                self.captured_stderr.shrinkAndFree(0);
            }
        }

        _ = emu_end(&self.uxn);
    }

    fn _findProtectedRangeStrict(self: *VM, addr: u16) ?usize {
        return for (self.protected.items, 0..) |prange, i| {
            if (prange.a == addr) break i;
        } else null;
    }

    fn _findProtectedRangeContaining(self: *VM, addr: u16) ?usize {
        return for (self.protected.items, 0..) |prange, i| {
            if (addr >= prange.a and addr < prange.b) break i;
        } else null;
    }

    fn _checkInstruction(self: *VM, pc: c_ushort, stderr: anytype) void {
        const ins = self.uxn.ram[pc];
        switch (ins & 0x3f) {
            0x15, // STA
            0x35, // STA2
            => if (self.protected.items.len > 0 and !self.is_privileged) {
                const stk = if (ins & 0x40 != 0) &self.uxn.rst else &self.uxn.wst;
                const wst: [*c]u8 = @ptrCast(&stk.dat[stk.ptr -| 1]);
                const addr = @as(u16, @intCast((wst - @as(u8, 1)).*)) << 8 | wst.*;

                for (self.protected.items) |protected_range| {
                    if (addr >= protected_range.a and addr < protected_range.b) {
                        _printHappyPercent(stderr);
                        stderr.print("Protection exception\n", .{}) catch unreachable;
                        stderr.print("Write to {x:0>4} (in {x:0>4}..{x:0>4})\n", .{
                            addr, protected_range.a, protected_range.b,
                        }) catch unreachable;
                        self.printBacktrace(&self.uxn.rst.dat, self.uxn.rst.ptr);
                        stderr.print("Initial protection:\n", .{}) catch unreachable;
                        self.printBacktrace(&protected_range.rst, protected_range.rst_ptr);
                        stderr.print("Aborting.\n", .{}) catch unreachable;
                        std.os.exit(1);
                    }
                }
            },
            else => {},
        }
    }
};

pub export fn emu_deo(u: [*c]c.Uxn, addr: c_char) callconv(.C) void {
    const self = @fieldParentPtr(VM, "uxn", u);
    switch (addr) {
        0x0e => switch (u.*.dev[0x0e]) {
            0x0b => if (self.is_testing) {
                self.is_breakpoint = true;
            },
            0x40 => {
                self.printBacktrace(&self.uxn.rst.dat, self.uxn.rst.ptr);
            },
            0x41 => {
                //
                // TODO: rst
                //
                const wst: [*c]u8 = @ptrCast(&self.uxn.wst.dat[self.uxn.wst.ptr -| 1]);
                const count = @as(u16, @intCast((wst - @as(u8, 1)).*)) << 8 | wst.*;
                const begin = @as(u16, @intCast((wst - @as(u8, 3)).*)) << 8 | (wst - @as(u8, 2)).*;
                self.uxn.wst.ptr -= 4;
                if (self._findProtectedRangeContaining(begin)) |i| {
                    const stderr = std.io.getStdErr().writer();
                    const existing = self.protected.items[i];
                    _printHappyPercent(stderr);
                    stderr.print("Protection exception\n", .{}) catch unreachable;
                    stderr.print("Range {x:0>4} already protected (in {x:0>4}..{x:0>4})\n", .{
                        begin, existing.a, existing.b,
                    }) catch unreachable;
                    self.printBacktrace(&self.uxn.rst.dat, self.uxn.rst.ptr);
                    stderr.print("Initial protection:\n", .{}) catch unreachable;
                    self.printBacktrace(&existing.rst, existing.rst_ptr);
                    stderr.print("Aborting.\n", .{}) catch unreachable;
                    std.os.exit(1);
                } else {
                    self.protected.append(.{
                        .a = begin,
                        .b = begin + count,
                        .rst = self.uxn.rst.dat,
                        .rst_ptr = self.uxn.rst.ptr,
                    }) catch unreachable;
                }
            },
            0x42 => {
                //
                // TODO: rst
                //
                const wst: [*c]u8 = @ptrCast(&self.uxn.wst.dat[self.uxn.wst.ptr -| 1]);
                const paddr = @as(u16, @intCast((wst - @as(u8, 1)).*)) << 8 | wst.*;
                self.uxn.wst.ptr -= 2;
                if (self._findProtectedRangeStrict(paddr)) |i| {
                    _ = self.protected.swapRemove(i);
                } else {
                    const stderr = std.io.getStdErr().writer();
                    _printHappyPercent(stderr);
                    stderr.print("Protection exception\n", .{}) catch unreachable;
                    stderr.print("Bad range to unprotect {x:0>4}\n", .{paddr}) catch unreachable;
                    self.printBacktrace(&self.uxn.rst.dat, self.uxn.rst.ptr);
                    stderr.print("Aborting.\n", .{}) catch unreachable;
                    std.os.exit(1);
                }
            },
            0x43 => {
                self.is_privileged = true;
            },
            0x44 => {
                self.is_privileged = false;
            },
            0x45 => {
                //
                // TODO: rst
                //
                const wst: [*c]u8 = @ptrCast(&self.uxn.wst.dat[self.uxn.wst.ptr -| 1]);
                const paddr = @as(u16, @intCast((wst - @as(u8, 1)).*)) << 8 | wst.*;
                self.uxn.wst.ptr -= 2;
                if (self._findProtectedRangeContaining(paddr) == null) {
                    const stderr = std.io.getStdErr().writer();
                    _printHappyPercent(stderr);
                    stderr.print("Protection exception\n", .{}) catch unreachable;
                    stderr.print("Address {x:0>4} not protected\n", .{paddr}) catch unreachable;
                    self.printBacktrace(&self.uxn.rst.dat, self.uxn.rst.ptr);
                    stderr.print("Aborting.\n", .{}) catch unreachable;
                    std.os.exit(1);
                }
            },
            else => base_emu_deo(u, addr),
        },
        0x18 => if (self.is_testing)
            self.captured_stdout.append(u.*.dev[0x18]) catch unreachable
        else
            base_emu_deo(u, addr),
        0x19 => if (self.is_testing)
            self.captured_stderr.append(u.*.dev[0x19]) catch unreachable
        else
            base_emu_deo(u, addr),
        else => base_emu_deo(u, addr),
    }
}

// Print '%' just in case program didn't print a newline, then print a bunch of
// spaces if a newline was output; the spaces will not stay on a line and we
// can output a carriage return to get back to the start of the line.
// Otherwise, the spaces will wrap to the next line, where we can safely
// carriage return to the start of the line.
//
// Stolen from a blogpost describing this same feature in the fish/zsh shells
//
fn _printHappyPercent(stderr: anytype) void {
    var wsz: std.os.linux.winsize = undefined;
    const fd = @as(usize, @bitCast(@as(isize, 1)));
    const rc = linux.syscall3(.ioctl, fd, linux.T.IOCGWINSZ, @intFromPtr(&wsz));
    const columns = switch (linux.getErrno(rc)) {
        .SUCCESS => @as(usize, @intCast(wsz.ws_col)),
        .INTR => return, // F you linux
        else => return, // Not a tty, stick head in sand immediately
    };
    stderr.print("\x1b[7m%\x1b[m", .{}) catch unreachable;
    stderr.writeByteNTimes(' ', columns - 1) catch unreachable;
    stderr.print("\r", .{}) catch unreachable;
}

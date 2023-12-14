const std = @import("std");
const mem = std.mem;
const math = std.math;
const fmt = std.fmt;
const meta = std.meta;
const assert = std.debug.assert;

const emitter = @import("emitter.zig");
const ASTNode = @import("common.zig").ASTNode;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;
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

pub const VM = struct {
    uxn: c.Uxn = undefined,

    pub fn init(assembled: []const Ins) VM {
        const ram = gpa.allocator().alloc(u8, 0x10000 * c.RAM_PAGES) catch
            @panic("please uninstall Chrome before proceeding (OOM)");
        @memset(ram, 0);

        var fbstream = std.io.fixedBufferStream(ram);
        var writer = fbstream.writer();
        writer.writeByteNTimes(0, 0x0100) catch unreachable;
        emitter.spitout(writer, assembled) catch unreachable;

        var self = mem.zeroes(VM);
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

    pub fn execute(self: *VM) void {
        // TODO: argument handling for roms that need it
        //self.uxn.dev[0x17] = argc - i;

        var pc: c_ushort = c.PAGE_PROGRAM;
        while (true) {
            pc = c.uxn_eval_once(&self.uxn, pc);
            if (pc <= 1) break;
        }

        _ = emu_end(&self.uxn);
    }
};

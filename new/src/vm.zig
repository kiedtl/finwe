const std = @import("std");
const mem = std.mem;
const math = std.math;
const fmt = std.fmt;
const meta = std.meta;
const assert = std.debug.assert;

const Value = @import("common.zig").Value;
const ASTNode = @import("common.zig").ASTNode;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

const VMError = error{
    StackUnderflow,
    InvalidType,
    OutOfMemory,
};

pub const VM = struct {
    stacks: std.ArrayList(ValueList),
    program: []const Ins,
    pc: usize,
    stopped: bool = false,

    pub fn init(program: []const Ins) VM {
        return .{
            .stacks = std.ArrayList(ValueList).init(gpa.allocator()),
            .program = program,
            .pc = 0,
        };
    }

    pub fn execute(self: *VM) VMError!void {
        assert(!self.stopped);
        while (!self.stopped and self.pc < self.program.len) {
            try self.executeIns(self.program[self.pc]);
            self.pc += 1;
        }
    }

    pub fn executeIns(self: *VM, ins: Ins) VMError!void {
        //std.log.info("pc: {}\tins: {}", .{ self.pc, ins });
        switch (ins.op) {
            .O => {},
            .Olit => |v| try self.push(ins.stack, v),
            .Osr => |f| {
                try self.pushInt(ins.stack, self.pc + 1);
                self.pc = f orelse try self.popUsize(ins.stack);
                self.pc -= 1; // Compensate for the pc+1 later on
            },
            .Oj => |j| {
                self.pc = j orelse try self.popUsize(ins.stack);
                self.pc -= 1; // Compensate for the pc+1 later on
            },
            .Ozj => |j| {
                const addr = j orelse try self.popUsize(ins.stack);
                if ((try self.popAny(ins.stack)).asBool()) {
                    self.pc = addr;
                    self.pc -= 1; // Compensate for the pc+1 later on
                }
            },
            .Ohalt => self.stopped = true,
            .Onac => |f| try (findBuiltin(f).?.func)(self, ins.stack),
            .Opick => |i| {
                const ind = i orelse (try self.popUsize(ins.stack));
                const len = (try self.stack(ins.stack)).items.len;
                if (ind >= (try self.stack(ins.stack)).items.len) {
                    return error.StackUnderflow;
                }
                const new = try (try self.stack(ins.stack)).items[len - ind - 1].clone();
                try self.push(ins.stack, new);
            },
            .Oroll => |i| {
                const ind = i orelse (try self.popUsize(ins.stack));
                const len = (try self.stack(ins.stack)).items.len;
                if (ind >= (try self.stack(ins.stack)).items.len) {
                    return error.StackUnderflow;
                }
                const item = (try self.stack(ins.stack)).orderedRemove(len - ind - 1);
                try self.push(ins.stack, item);
            },
            .Odrop => |i| {
                const count = i orelse (try self.popUsize(ins.stack));
                const len = (try self.stack(ins.stack)).items.len;
                (try self.stack(ins.stack)).shrinkAndFree(len - count);
            },
            .Ocmp => {
                const b = (try self.pop(ins.stack, .Number)).Number;
                const a = (try self.pop(ins.stack, .Number)).Number;
                const r = if (a == b) @as(f64, 0.0) else if (a > b) @as(f64, 1.0) else @as(f64, -1.0);
                try self.pushNum(ins.stack, r);
            },
            .Onot => try self.pushBool(ins.stack, !(try self.popAny(ins.stack)).asBool()),
            .Odmod => |d| {
                const dvs = d orelse (try self.pop(ins.stack, .Number)).Number;
                const dvd = (try self.pop(ins.stack, .Number)).Number;
                try self.pushNum(ins.stack, @mod(dvd, dvs));
                try self.pushNum(ins.stack, dvd / dvs);
            },
            .Omul => |ma| {
                const a = ma orelse (try self.pop(ins.stack, .Number)).Number;
                const b = (try self.pop(ins.stack, .Number)).Number;
                try self.pushNum(ins.stack, a * b);
            },
        }
    }

    pub fn stack(self: *VM, s: usize) VMError!*ValueList {
        while (self.stacks.items.len <= s) {
            try self.stacks.append(ValueList.init(gpa.allocator()));
        }
        return &self.stacks.items[s];
    }

    pub fn popUsize(self: *VM, stk: usize) VMError!usize {
        return @floatToInt(
            usize,
            (try self.pop(stk, .Number)).Number,
        );
    }

    pub fn pop(self: *VM, stk: usize, expect: Value.Tag) VMError!Value {
        const v = try self.popAny(stk);
        if (expect != v) {
            return error.InvalidType;
        }
        return v;
    }

    pub fn popAny(self: *VM, stk: usize) VMError!Value {
        if (self.stacks.items.len < stk or self.stacks.items[stk].items.len == 0) {
            return error.StackUnderflow;
        }
        return self.stacks.items[stk].pop();
    }

    pub fn push(self: *VM, stk: usize, value: Value) VMError!void {
        while (self.stacks.items.len <= stk) {
            try self.stacks.append(ValueList.init(gpa.allocator()));
        }
        try self.stacks.items[stk].append(value);
    }

    pub fn pushBool(self: *VM, stk: usize, value: bool) VMError!void {
        try self.push(stk, if (value) .{ .T = {} } else .{ .Nil = {} });
    }

    pub fn pushInt(self: *VM, stk: usize, value: anytype) VMError!void {
        try self.push(stk, .{ .Number = @intToFloat(f64, value) });
    }

    pub fn pushNum(self: *VM, stk: usize, value: f64) VMError!void {
        try self.push(stk, .{ .Number = value });
    }
};

// Builtins
//
// (This is temporary)
//
// {{{

pub const Builtin = struct {
    name: []const u8,
    func: fn (vm: *VM, stack: usize) VMError!void,
};

pub const BUILTINS = [_]Builtin{
    Builtin{
        .name = "print-stack",
        .func = struct {
            pub fn f(vm: *VM, stk: usize) VMError!void {
                for ((try vm.stack(stk)).items) |item, i| {
                    std.log.info("{}\t{}", .{ i, item });
                }
            }
        }.f,
    },
    Builtin{
        .name = "sqrt",
        .func = struct {
            pub fn f(vm: *VM, stk: usize) VMError!void {
                const n = (try vm.pop(stk, .Number)).Number;
                try vm.pushNum(stk, math.sqrt(n));
            }
        }.f,
    },
    Builtin{
        .name = "do",
        .func = struct {
            pub fn f(vm: *VM, stk: usize) VMError!void {
                const addr = @floatToInt(usize, (try vm.pop(stk, .Number)).Number);
                try vm.executeIns(Ins{ .stack = RT_STACK, .op = .{ .Osr = addr } });
            }
        }.f,
    },
};

pub fn findBuiltin(name: []const u8) ?Builtin {
    return for (&BUILTINS) |builtin| {
        if (mem.eql(u8, builtin.name, name))
            break builtin;
    } else null;
}

// }}}

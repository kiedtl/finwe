const std = @import("std");
const mem = std.mem;
const fmt = std.fmt;
const meta = std.meta;
const assert = std.debug.assert;

const Value = @import("common.zig").Value;
const ASTNode = @import("common.zig").ASTNode;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;

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
        switch (ins.op) {
            .O => {},
            .Olit => |v| try self.push(ins.stack, v),
            .Oj => |j| self.pc = j orelse @floatToInt(
                usize,
                (try self.pop(ins.stack, .Number)).Number,
            ),
            .Osave => try self.pushInt(ins.stack, self.pc + 1),
            .Ohalt => self.stopped = true,
        }
    }

    pub fn pop(self: *VM, stack: usize, expect: Value.Tag) VMError!Value {
        const v = try self.popAny(stack);
        if (expect != v) {
            return error.InvalidType;
        }
        return v;
    }

    pub fn popAny(self: *VM, stack: usize) VMError!Value {
        if (self.stacks.items.len < stack or self.stacks.items[stack].items.len == 0) {
            return error.StackUnderflow;
        }
        return self.stacks.items[stack].pop();
    }

    pub fn push(self: *VM, stack: usize, value: Value) VMError!void {
        while (self.stacks.items.len <= stack) {
            try self.stacks.append(ValueList.init(gpa.allocator()));
        }
        try self.stacks.items[stack].append(value);
    }

    pub fn pushInt(self: *VM, stack: usize, value: anytype) VMError!void {
        try self.push(stack, .{ .Number = @intToFloat(f64, value) });
    }

    pub fn pushNum(self: *VM, stack: usize, value: f64) VMError!void {
        try self.push(stack, .{ .Number = value });
    }
};

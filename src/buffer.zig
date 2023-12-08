// Originally ripped out of:
// https://github.com/fengb/zigbot9001, main.zig

const std = @import("std");
const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

pub const StackBufferError = error{
    IndexOutOfRange,
    NoSpaceLeft,
};

pub fn StackBuffer(comptime T: type, comptime capacity: usize) type {
    return struct {
        data: [capacity]T = undefined,
        len: usize = 0,
        capacity: usize = capacity,

        const Self = @This();

        pub fn init(data: ?[]const T) Self {
            if (data) |d| {
                var b: Self = .{ .len = d.len };
                @memcpy(&b.data, d);
                return b;
            } else {
                return .{};
            }
        }

        pub fn reinit(self: *Self, data: ?[]const T) void {
            self.clear();
            self.* = Self.init(data);
        }

        pub fn slice(self: *Self) []T {
            return self.data[0..self.len];
        }

        pub fn constSlice(self: *const Self) []const T {
            return self.data[0..self.len];
        }

        pub fn orderedRemove(self: *Self, index: usize) StackBufferError!T {
            if (self.len == 0 or index >= self.len) {
                return error.IndexOutOfRange;
            }

            const newlen = self.len - 1;
            if (newlen == index) {
                return try self.pop();
            }

            const item = self.data[index];
            for (self.data[index..newlen], 0..) |*data, j|
                data.* = self.data[index + 1 + j];
            self.len = newlen;
            return item;
        }

        pub fn pop(self: *Self) !T {
            if (self.len == 0) return error.IndexOutOfRange;
            self.len -= 1;
            return self.data[self.len];
        }

        pub fn resizeTo(self: *Self, size: usize) void {
            assert(size < self.capacity);
            self.len = size;
        }

        pub fn clear(self: *Self) void {
            self.resizeTo(0);
        }

        pub fn insert(self: *Self, ind: usize, item: T) StackBufferError!void {
            if (self.len >= capacity) {
                return error.NoSpaceLeft;
            }

            //@setRuntimeSafety(false);

            var i = self.len - ind;
            while (i > 0) {
                i -= 1;
                self.data[ind + i + 1] = self.data[ind..self.len][i];
            }

            self.data[ind] = item;
            self.len += 1;
        }

        // FIXME: this is stupidly inefficient... too lazy to correct it...
        pub fn insertSlice(self: *Self, ind: usize, items: []const T) StackBufferError!void {
            if (self.len + items.len >= capacity) {
                return error.NoSpaceLeft;
            }

            for (items, 0..) |item, i|
                try self.insert(ind + i, item);
        }

        pub fn append(self: *Self, item: T) StackBufferError!void {
            if (self.len >= capacity) {
                return error.NoSpaceLeft;
            }

            self.data[self.len] = item;
            self.len += 1;
        }

        pub fn appendSlice(self: *Self, items: []const T) StackBufferError!void {
            for (items) |item| try self.append(item);
        }

        pub inline fn isFull(self: *Self) bool {
            return self.len == self.capacity;
        }

        pub inline fn last(self: *Self) ?T {
            return if (self.len > 0) self.data[self.len - 1] else null;
        }
    };
}

test "insert" {
    {
        var b = StackBuffer(u8, 128).init("fefifofum");
        try b.insert(0, 'a');
        try testing.expectEqualSlices(u8, "afefifofum", b.constSlice());
    }
    {
        var b = StackBuffer(u8, 128).init("fefifofum");
        try b.insert(3, 'a');
        try testing.expectEqualSlices(u8, "fefaifofum", b.constSlice());
    }
}

test "insertSlice" {
    {
        var b = StackBuffer(u8, 128).init("fefifofum");
        try b.insertSlice(0, "GLORY_TO_ZIG");
        try testing.expectEqualSlices(u8, "GLORY_TO_ZIGfefifofum", b.constSlice());
    }
    {
        var b = StackBuffer(u8, 128).init("fefifofum");
        try b.insertSlice(3, "ABCD");
        try testing.expectEqualSlices(u8, "fefABCDifofum", b.constSlice());
    }
}

// Convenience type that will "eventually" be filled with data, and will panic
// if data is accessed before then. Will also panic if data is mutated more than
// once.

const std = @import("std");

pub fn Eventually(comptime T: type) type {
    return struct {
        inner: ?T = null,

        pub fn assertNotYet(self: *const @This()) void {
            std.debug.assert(self.inner == null);
        }

        pub fn mut(self: *@This()) *?T {
            self.assertNotYet();
            return &self.inner;
        }

        pub fn get(self: *const @This()) T {
            return self.inner.?;
        }

        pub fn ref(self: *const @This()) *const T {
            return &self.inner;
        }
    };
}

const std = @import("std");
const meta = std.meta;

pub fn unwrapAs(comptime U: type, instance: U, expect: meta.Tag(U)) ?U {
    return if (instance == expect) instance else null;
}

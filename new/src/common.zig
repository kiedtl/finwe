const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;

pub const String = std.ArrayList(u8);

const LinkedList = @import("list.zig").LinkedList;
const StackBuffer = @import("buffer.zig").StackBuffer;
const StackBufferError = @import("buffer.zig").StackBufferError;

// ----------------------------------------------------------------------------

pub const WK_STACK = 0;
pub const RT_STACK = 1;

pub var gpa = std.heap.GeneralPurposeAllocator(.{
    // Probably should enable this later on to track memory usage, if
    // allocations become too much
    .enable_memory_limit = false,

    .safety = true,

    // Probably would enable this later?
    .thread_safe = false,

    .never_unmap = false,
}){};

pub const ValueList = std.ArrayList(Value);

//pub const ASTNodeList = LinkedList(ASTNode);
pub const ASTNodeList = std.ArrayList(ASTNode);
pub const ASTNodePtrList = std.ArrayList(*ASTNode);

pub const Value = union(enum) {
    T,
    Nil,
    Number: f64,
    Codepoint: u21,
    EnumLit: []const u8,
    Stack: []const u8,
    // TODO: remove strings in favor of a struct{ vec } or something
    String: String,
    // TODO: refs, stacks, vec lits, table lits, struct lits

    pub const Tag = std.meta.Tag(Value);

    pub fn asBool(self: Value) bool {
        return switch (self) {
            .Nil => false,
            .Number => |n| n != 0,
            .Codepoint => |c| c != 0,
            .T, .Stack, .EnumLit, .String => true,
        };
    }

    pub fn clone(self: Value) !Value {
        return switch (self) {
            .T, .Nil, .Number, .Codepoint, .EnumLit, .Stack => return self,
            .String => |s| b: {
                var new = String.init(gpa.allocator());
                try new.appendSlice(s.items);
                break :b Value{ .String = new };
            },
        };
    }
};

pub const ASTNode = struct {
    __prev: ?*ASTNode = null,
    __next: ?*ASTNode = null,

    node: Type,
    srcloc: usize,
    romloc: usize = 0,

    pub const Tag = std.meta.Tag(ASTNode.Type);

    pub const Type = union(enum) {
        Decl: Decl, // word declaraction
        Call: []const u8,
        Loop: Loop,
        Cond: Cond,
        Asm: Ins,
        Value: Value,
        Quote: Quote,
        StackOp: StackOp,
    };

    pub const Cond = struct {
        branches: Branch.List,
        else_branch: ?ASTNodeList,

        pub const Branch = struct {
            cond: ASTNodeList,
            body: ASTNodeList,

            pub const List = std.ArrayList(Branch);
        };
    };

    pub const Loop = struct {
        loop: Loop.Type,
        body: ASTNodeList,

        pub const Type = union(enum) {
            Until: struct { cond: ASTNodeList },
        };
    };

    pub const Decl = struct {
        name: []const u8,
        body: ASTNodeList,
    };

    pub const Quote = struct {
        body: ASTNodeList,
    };

    pub const StackOp = struct {
        stack: []const u8,
        op: StackOp.Type,

        pub const Type = enum { Push, Pop, PushK, PopK };
    };
};

pub const Program = struct {
    stacks: StackInfo.List,
    ast: ASTNodeList,
    defs: ASTNodePtrList,

    pub const StackInfo = struct {
        stack: []const u8,
        pub const List = std.ArrayList(StackInfo);
    };

    pub fn stackId(self: *Program, stack: []const u8) usize {
        if (self.stacks.items.len == 0) {
            self.stacks.append(.{ .stack = "_" }) catch unreachable;
            self.stacks.append(.{ .stack = "_Return" }) catch unreachable;
        }
        return for (self.stacks.items) |item, i| {
            if (mem.eql(u8, item.stack, stack)) {
                break i;
            }
        } else b: {
            self.stacks.append(.{ .stack = stack }) catch unreachable;
            break :b self.stacks.items.len - 1;
        };
    }
};

pub const Op = union(enum) {
    O, // nop
    Olit: Value,
    Osr: ?usize,
    Oj: ?usize,
    Ozj: ?usize,
    Ohalt,
    Onac: []const u8,
    Opick: ?usize,
    Oroll: ?usize,
    Odrop: ?usize,
    Ocmp,
    Onot,
    Odmod: ?f64,
    Omul: ?f64,
    Oadd: ?f64,
    Omov: usize,

    pub const Tag = meta.Tag(Op);

    pub fn fromTag(tag: Tag) !Op {
        return switch (tag) {
            .O => .O,
            .Omov, .Onac, .Olit => error.NeedsArg,
            .Osr => .{ .Osr = null },
            .Oj => .{ .Oj = null },
            .Ozj => .{ .Ozj = null },
            .Ohalt => .Ohalt,
            .Opick => .{ .Opick = null },
            .Oroll => .{ .Oroll = null },
            .Odrop => .{ .Odrop = null },
            .Ocmp => .Ocmp,
            .Onot => .Onot,
            .Odmod => .{ .Odmod = null },
            .Omul => .{ .Omul = null },
            .Oadd => .{ .Oadd = null },
        };
    }

    pub fn format(
        value: @This(),
        comptime f: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        switch (value) {
            .Olit => |l| try fmt.format(writer, "{}", .{l}),
            .Oj => |j| try fmt.format(writer, "{}", .{j}),
            .Ozj => |j| try fmt.format(writer, "{}", .{j}),
            .Osr => |j| try fmt.format(writer, "{}", .{j}),
            .Onac => |n| try fmt.format(writer, "'{s}'", .{n}),
            .Opick => |i| try fmt.format(writer, "{}", .{i}),
            .Oroll => |i| try fmt.format(writer, "{}", .{i}),
            .Odmod => |d| try fmt.format(writer, "{}", .{d}),
            .Omul => |a| try fmt.format(writer, "{}", .{a}),
            .Oadd => |a| try fmt.format(writer, "{}", .{a}),
            .Omov => |s| try fmt.format(writer, "{}", .{s}),
            else => try fmt.format(writer, "@", .{}),
        }
    }
};

pub const Ins = struct {
    stack: usize,
    op: Op,

    pub const List = std.ArrayList(Ins);

    pub fn format(
        value: @This(),
        comptime f: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        var str: []const u8 = undefined;
        inline for (@typeInfo(Op.Tag).Enum.fields) |enum_field| {
            if (@intToEnum(Op.Tag, enum_field.value) == value.op) {
                str = enum_field.name;
                //break; // FIXME: Wait for that bug to be fixed, then uncomment
            }
        }

        try fmt.format(writer, "<[{}] {s} {}>", .{ value.stack, str, value.op });
    }
};

// ----------------------------------------------------------------------------

test "Op.fromTag" {
    inline for (@typeInfo(Op.Tag).Enum.fields) |variant| {
        const e = @field(Op.Tag, variant.name);
        if (Op.fromTag(e)) |v| {
            try std.testing.expectEqual(e, v);
        } else |_| {}
    }
}

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
    Number: f64,
    Codepoint: u21,
    // TODO: remove strings in favor of a struct{ vec } or something
    String: String,
    // TODO: refs, stacks, vec lits, table lits, struct lits

    pub const Tag = std.meta.Tag(Value);
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
        Asm: Ins,
        Value: Value,
        Child: Child,
    };

    pub const Child = union(enum) {
        Number: f64,
        String: String,
    };

    pub const Decl = struct {
        name: []const u8,
        body: ASTNodeList,
    };
};

pub const Program = struct {
    ast: ASTNodeList,
    defs: ASTNodePtrList,
};

pub const Op = union(enum) {
    O, // nop
    Olit: Value,
    Osave,
    Oj: ?usize,
    Ohalt,

    pub const Tag = meta.Tag(Op);

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

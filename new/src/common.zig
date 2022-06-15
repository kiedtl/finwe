const std = @import("std");
const mem = std.mem;

pub const String = std.ArrayList(u8);

const LinkedList = @import("list.zig").LinkedList;
const StackBuffer = @import("buffer.zig").StackBuffer;
const StackBufferError = @import("buffer.zig").StackBufferError;

// ----------------------------------------------------------------------------

pub const WK_STACK = 1;
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
    String: String,
    // TODO: refs, stacks, vec lits, table lits, struct lits
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

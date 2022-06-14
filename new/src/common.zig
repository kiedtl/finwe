const std = @import("std");
const mem = std.mem;

pub const String = std.ArrayList(u8);

const LinkedList = @import("list.zig").LinkedList;
const StackBuffer = @import("buffer.zig").StackBuffer;
const StackBufferError = @import("buffer.zig").StackBufferError;

pub const ValueList = std.ArrayList(Value);

//pub const ASTNodeList = LinkedList(ASTNode);
pub const ASTNodeList = std.ArrayList(ASTNode);
pub const ASTNodePtrArrayList = std.ArrayList(*ASTNode);

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

const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const assert = std.debug.assert;

//const ASTNode = @import("common.zig").ASTNode;
//const ASTNodeList = @import("common.zig").ASTNodeList;
//const Program = @import("common.zig").Program;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;
const OpTag = @import("common.zig").OpTag;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

pub const Pattern = struct {
    pat: Type,

    pub const Type = union(enum) {
        O: OpTag, // Specific opcode
        R, // Any raw byte
        B: u8, // Specific raw byte
    };
};

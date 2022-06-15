const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;

const vm = @import("vm.zig");

const StackBufferError = @import("buffer.zig").StackBufferError;

const Ins = @import("vm.zig").Ins;
const Op = @import("vm.zig").Op;

const ASTNode = @import("common.zig").ASTNode;
const Value = @import("common.zig").Node.Value;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Program = @import("common.zig").Program;
const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

const CodegenError = error{
    DuplicateWord,
    UnknownIdent,
} || mem.Allocator.Error || StackBufferError;

// UA: Unresolved Address
//
// As we walk through the AST generating bytecode, we store references to
// identifiers in a UAList and insert a dummy null value into the ROM; later on,
// we iterate through the UAList, replacing the dummy values with the real
// references.
//
// The reason for this is that although we know what each identifiers are
// (they were extracted earlier), we don't know where they'll be in the ROM
// until *after* the codegen process.
//
// The UAType determines how the ual-resolver should insert the address:
//      - Data:      insert as a raw byte.
//      - Call:      insert and mask with 0x2000.
//      - Jump:      insert and mask with 0x1000.
//      - IndexLoad: insert and mask with 0xA000.
//
const UA = struct {
    loc: usize,
    ident: []const u8,
    node: *ASTNode,

    pub const List = std.ArrayList(UA);
};

//
//
// -----------------------------------------------------------------------------
//
//

fn emit(buf: *Ins.List, node: ?*ASTNode, stack: usize, op: Op) CodegenError!void {
    _ = node;
    try buf.append(Ins{ .stack = stack, .op = op });
}

fn emitUA(buf: *Ins.List, ual: *UA.List, ident: []const u8, node: *ASTNode) CodegenError!void {
    try ual.append(.{
        .loc = buf.items.len,
        .ident = ident,
        .node = node,
    });
    switch (node.node) {
        .Call => {
            try emit(buf, null, 0, .O);
            try emit(buf, null, 0, .O);
        },
        else => unreachable,
    }
}

fn genNode(buf: *Ins.List, node: *ASTNode, ual: *UA.List) CodegenError!void {
    switch (node.node) {
        .Value => |v| try emit(buf, node, WK_STACK, .{ .Olit = v }),
        .Decl => |d| {
            // Create a new scope
            node.romloc = buf.items.len;
            for (d.body.items) |*bodynode|
                try genNode(buf, bodynode, ual);
        },
        .Call => |i| try emitUA(buf, ual, i, node),
        .Child => @panic("unimplemented"),
    }
}

pub fn generate(program: *Program) CodegenError!Ins.List {
    var buf = Ins.List.init(gpa.allocator());
    var ual = UA.List.init(gpa.allocator());

    for (program.ast.items) |*node| {
        try genNode(&buf, node, &ual);
    }

    ual_search: for (ual.items) |ua| {
        for (program.defs.items) |def| {
            if (mem.eql(u8, def.node.Decl.name, ua.ident)) {
                buf.items[ua.loc + 0] = .{ .stack = RT_STACK, .op = .Osave };
                buf.items[ua.loc + 1] = .{ .stack = RT_STACK, .op = .{ .Oj = def.romloc } };

                continue :ual_search;
            }
        }

        // If we haven't matched a UA with a label by now, it's an invalid
        // identifier
        return error.UnknownIdent;
    }

    return buf;
}

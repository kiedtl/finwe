const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;

const vm = @import("vm.zig");

const StackBufferError = @import("buffer.zig").StackBufferError;

const ASTNode = @import("common.zig").ASTNode;
const Value = @import("common.zig").Node.Value;
const ValueList = @import("common.zig").ValueList;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Program = @import("common.zig").Program;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;

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

// "emit returning index"
fn emitRI(buf: *Ins.List, node: ?*ASTNode, stack: usize, op: Op) CodegenError!usize {
    _ = node;
    try buf.append(Ins{ .stack = stack, .op = op });
    return buf.items.len - 1;
}

fn emit(buf: *Ins.List, node: ?*ASTNode, stack: usize, op: Op) CodegenError!void {
    _ = try emitRI(buf, node, stack, op);
}

fn emitDUP(buf: *Ins.List, stack: usize) CodegenError!void {
    try emit(buf, null, stack, .{ .Opick = 0 });
}

fn emitUA(buf: *Ins.List, ual: *UA.List, ident: []const u8, node: *ASTNode) CodegenError!void {
    try ual.append(.{
        .loc = buf.items.len,
        .ident = ident,
        .node = node,
    });
    switch (node.node) {
        .Call => try emit(buf, null, 0, .{ .Oj = null }),
        else => unreachable,
    }
}

fn genNode(program: *Program, buf: *Ins.List, node: *ASTNode, ual: *UA.List) CodegenError!void {
    switch (node.node) {
        .None => {},
        .Value => |v| try emit(buf, node, WK_STACK, .{ .Olit = v }),
        .Mac => {},
        .Decl => |d| {
            node.romloc = buf.items.len;
            for (d.body.items) |*bodynode|
                try genNode(program, buf, bodynode, ual);
            try emit(buf, node, RT_STACK, .{ .Oj = null });
        },
        .Quote => |q| {
            const quote_jump_addr = buf.items.len;
            try emit(buf, node, WK_STACK, .{ .Oj = 0 }); // Dummy value, replaced later
            const quote_begin_addr = buf.items.len;
            for (q.body.items) |*bodynode|
                try genNode(program, buf, bodynode, ual);
            try emit(buf, node, RT_STACK, .{ .Oj = null });
            const quote_end_addr = buf.items.len;
            try emit(buf, node, WK_STACK, .{ .Olit = .{ .Number = @intToFloat(f64, quote_begin_addr) } });
            buf.items[quote_jump_addr].op.Oj = quote_end_addr; // Replace dummy value
        },
        .Loop => |l| {
            const loop_begin = buf.items.len;
            for (l.body.items) |*bodynode|
                try genNode(program, buf, bodynode, ual);
            switch (l.loop) {
                .Until => |u| {
                    try emit(buf, node, WK_STACK, .{ .Opick = 0 }); // DUP
                    for (u.cond.items) |*bodynode|
                        try genNode(program, buf, bodynode, ual);
                    try emit(buf, node, WK_STACK, .Onot);
                    try emit(buf, node, WK_STACK, .{ .Ozj = loop_begin });
                },
            }
        },
        .Asm => |a| {
            try emit(buf, node, a.stack, a.op);
        },
        .Call => |f| {
            if (vm.findBuiltin(f)) |_| {
                try emit(buf, node, WK_STACK, .{ .Onac = f });
            } else if (for (program.macs.items) |mac| {
                if (mem.eql(u8, mac.node.Mac.name, f))
                    break mac;
            } else null) |mac| {
                for (mac.node.Mac.body.items) |*item|
                    try genNode(program, buf, item, ual);
            } else {
                try emitUA(buf, ual, f, node);
            }
        },
        .Cond => |cond| {
            var end_jumps = std.ArrayList(usize).init(gpa.allocator()); // Dummy jumps to fix
            defer end_jumps.deinit();

            for (cond.branches.items) |branch| {
                //try emitDUP(buf, WK_STACK);
                try genNodeList(program, buf, branch.cond.items, ual);
                try emit(buf, node, WK_STACK, .Onot);
                const body_jmp = try emitRI(buf, node, WK_STACK, .{ .Ozj = 0 });
                try genNodeList(program, buf, branch.body.items, ual);
                const end_jmp = try emitRI(buf, node, WK_STACK, .{ .Oj = 0 });
                try end_jumps.append(end_jmp);
                buf.items[body_jmp].op.Ozj = buf.items.len;
            }

            if (cond.else_branch) |branch| {
                try genNodeList(program, buf, branch.items, ual);
            }

            // TODO: remove the very last end jump if there's no else_branch,
            // since it's completely unnecessary

            const cond_end_addr = buf.items.len;
            for (end_jumps.items) |end_jump| {
                buf.items[end_jump].op.Oj = cond_end_addr;
            }
        },
    }
}

pub fn genNodeList(program: *Program, buf: *Ins.List, nodes: []ASTNode, ual: *UA.List) CodegenError!void {
    for (nodes) |*node|
        try genNode(program, buf, node, ual);
}

pub fn generate(program: *Program) CodegenError!Ins.List {
    var buf = Ins.List.init(gpa.allocator());
    var ual = UA.List.init(gpa.allocator());

    for (program.ast.items) |*node| {
        try genNode(program, &buf, node, &ual);
    }

    ual_search: for (ual.items) |ua| {
        for (program.defs.items) |def| {
            if (mem.eql(u8, def.node.Decl.name, ua.ident)) {
                buf.items[ua.loc] = .{ .stack = RT_STACK, .op = .{ .Osr = def.romloc } };

                continue :ual_search;
            }
        }

        // If we haven't matched a UA with a label by now, it's an invalid
        // identifier
        std.log.info("Unknown ident {s}", .{ua.ident});
        return error.UnknownIdent;
    }

    return buf;
}

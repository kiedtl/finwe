const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;

// const vm = @import("vm.zig");

const StackBufferError = @import("buffer.zig").StackBufferError;

const TypeFmt = @import("common.zig").TypeFmt;
const ASTNode = @import("common.zig").ASTNode;
const ASTNodeList = @import("common.zig").ASTNodeList;
const Program = @import("common.zig").Program;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;
const OpTag = @import("common.zig").OpTag;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

const CodegenError = mem.Allocator.Error || StackBufferError;

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
fn emitRI(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, s: bool, op: Op) CodegenError!usize {
    _ = node;
    try buf.append(Ins{ .stack = stack, .op = op, .keep = k, .short = s });
    return buf.items.len - 1;
}

fn emit(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, s: bool, op: Op) CodegenError!void {
    _ = try emitRI(buf, node, stack, k, s, op);
}

fn reemitAddr16(buf: *Ins.List, ind: usize, value: usize) void {
    const addr = @as(u16, @intCast(value)) + 0x100;
    buf.items[ind + 0].op.Oraw = @as(u8, @intCast(addr >> 8));
    buf.items[ind + 1].op.Oraw = @as(u8, @intCast(addr & 0xFF));
}

fn emitUA(buf: *Ins.List, ual: *UA.List, ident: []const u8, node: *ASTNode) CodegenError!void {
    try emitIMM16(buf, null, 0, false, .Olit, 0);
    try ual.append(.{
        .loc = buf.items.len - 2,
        .ident = ident,
        .node = node,
    });
    switch (node.node) {
        .Call => |c| try emit(buf, null, 0, false, true, if (c.goto) .Ojmp else .Ojsr),
        .Value => |v| switch (v.typ) {
            .StaticPtr => {},
            else => unreachable,
        },
        .Here, .VRef, .VDeref => {},
        else => unreachable,
    }
}

fn emitIMM(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, op: OpTag, imm: u8) CodegenError!void {
    try emit(buf, node, stack, k, false, Op.fromTag(op) catch unreachable);
    try emit(buf, node, stack, k, false, .{ .Oraw = imm });
}

fn emitIMM16(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, op: OpTag, imm: u16) CodegenError!void {
    try emit(buf, node, stack, k, true, Op.fromTag(op) catch unreachable);
    try emit(buf, node, stack, k, false, .{ .Oraw = @intCast(imm >> 8) });
    try emit(buf, node, stack, k, false, .{ .Oraw = @intCast(imm & 0xFF) });
}

fn emitARG(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, op: OpTag, imm: u8) CodegenError!void {
    try emitIMM(buf, node, stack, false, .Olit, imm);
    try emit(buf, node, stack, k, true, Op.fromTag(op) catch unreachable);
}

fn emitARG16(buf: *Ins.List, node: ?*ASTNode, stack: usize, k: bool, op: OpTag, imm: u16) CodegenError!void {
    try emitIMM16(buf, node, stack, false, .Olit, imm);
    try emit(buf, node, stack, k, true, Op.fromTag(op) catch unreachable);
}

fn genNode(program: *Program, buf: *Ins.List, node: *ASTNode, ual: *UA.List) CodegenError!void {
    switch (node.node) {
        .TypeDef => {},
        .Return => {
            try emit(buf, node, RT_STACK, false, true, .Ojmp);
        },
        .Debug => {},
        .Here => try emitUA(buf, ual, "", node),
        .Builtin => |builtin| switch (builtin.type) {
            .Make => {},
            .SizeOf => |sizeof| {
                const s = sizeof.resolved.size(program).?;
                try emitIMM(buf, node, WK_STACK, false, .Olit, s);
            },
        },
        .Breakpoint => |brk| {
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x00);
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0e);
            try emit(buf, node, WK_STACK, false, false, .Odeo);
            program.breakpoints.append(brk) catch unreachable;
            program.breakpoints.items[program.breakpoints.items.len - 1].romloc = buf.items.len;
        },
        .Cast => |c| {
            // std.log.info("codegen: casting {} -> {}", .{ c.of, c.to.builtin });
            const from = c.of.bits(program).?;
            const to = c.to.bits(program).?;
            if (from == 16 and to == 8) {
                try emit(buf, node, WK_STACK, false, false, .Onip);
            } else if (from == 8 and to == 16) {
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oswp);
            } else {
                // nothing
            }
        },
        .None => {},
        .GetChild => |gch| switch (gch.kind) {
            .unresolved => unreachable,
            .stk_one_s => {},
            .stk_two_b => |gch2b| if (gch2b.ind == 0) {
                try emit(buf, null, 0, false, false, .Onip);
            } else if (gch2b.ind == 1) {
                try emit(buf, null, 0, false, false, .Odrop);
            },
            .stk_one_b => {},
            .mem => |gchmem| {
                if (gchmem.is_short) {
                    try emitIMM16(buf, node, WK_STACK, false, .Olit, gchmem.offset);
                } else {
                    try emitIMM(buf, node, WK_STACK, false, .Olit, gchmem.offset);
                }
                try emit(buf, null, 0, false, gchmem.is_short, .Oadd);
            },
        },
        .VDecl => {},
        .VRef => |v| try emitUA(buf, ual, v.name, node),
        .VDeref => |v| {
            try emitUA(buf, ual, v.name, node);
            // LDA instruction
            // Could do this in emitUA, but then we'd have to pass program
            const t = program.statics.items[v.sind.?].type;
            const is_short = t.bits(program).? == 16;
            try emit(buf, null, 0, false, is_short, .Olda);
        },
        .Value => |v| {
            if (v.typ == .StaticPtr) {
                try emitUA(buf, ual, "", node);
            } else if (v.typ.bits(program).? == 16) {
                try emitIMM16(buf, node, WK_STACK, false, .Olit, v.toU16(program));
            } else {
                try emitIMM(buf, node, WK_STACK, false, .Olit, v.toU8(program));
            }
        },
        .Mac => {},
        .Decl => {},
        .Wild => |w| try genNodeList(program, buf, w.body, ual),
        .Quote => {
            @panic("unimplemented");
            // (TODO: shouldn't be gen'ing quotes, they need to be made decls
            // by parser code)
            //
            // const quote_jump_addr = buf.items.len;
            // try emit(buf, node, WK_STACK, false, false, .{ .Oj = 0 }); // Dummy value, replaced later
            // const quote_begin_addr = @intCast(buf.items.len);
            // for (q.body.items) |*bodynode|
            //     try genNode(program, buf, bodynode, ual);
            // try emit(buf, node, RT_STACK, false, false, .{ .Oj = null });
            // const quote_end_addr = @intCast(buf.items.len);
            // try emitARG16(buf, node, WK_STACK, false, .Olit, quote_begin_addr);
            // buf.items[quote_jump_addr].op.Oj = quote_end_addr; // Replace dummy value
        },
        .Loop => |l| {
            switch (l.loop) {
                .While => |u| {
                    const begin: u16 = @intCast(buf.items.len);

                    assert(u.cond_prep != .Unchecked);
                    try emit(buf, node, WK_STACK, false, u.cond_prep == .DupShort, .Odup);
                    try genNodeList(program, buf, u.cond, ual);

                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oeq);

                    try emit(buf, node, WK_STACK, false, true, .Olit);
                    const addr_slot = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                    try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                    try emit(buf, node, WK_STACK, false, true, .Ojcn);

                    try genNodeList(program, buf, l.body, ual);
                    try emitARG16(buf, node, WK_STACK, false, .Ojmp, 0x0100 + begin);
                    reemitAddr16(buf, addr_slot, buf.items.len);
                },
                .Until => |u| {
                    const loop_begin: u16 = @intCast(buf.items.len);
                    try genNodeList(program, buf, l.body, ual);

                    assert(u.cond_prep != .Unchecked);
                    try emit(buf, node, WK_STACK, false, u.cond_prep == .DupShort, .Odup);
                    try genNodeList(program, buf, u.cond, ual);
                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oeq);
                    try emitARG16(buf, node, WK_STACK, false, .Ojcn, 0x0100 + loop_begin);
                },
            }
        },
        .Asm => |a| {
            try emit(buf, node, a.stack, a.keep, a.short, a.op);
        },
        .Call => |f| {
            if (for (program.macs.items) |mac| {
                if (mem.eql(u8, mac.node.Mac.name, f.name))
                    break mac;
            } else null) |mac| {
                try genNodeList(program, buf, mac.node.Mac.body, ual);
            } else {
                try emitUA(buf, ual, f.name, node);
            }
        },
        .When => |when| {
            // Structure
            // - When else branch
            //   1. Jump-if-true to IF
            //   2. ELSE
            //   3. Jump-to END
            //   4. IF
            //   5. <end>
            // - When no else branch
            //   1. Jump-if-true to IF
            //   2.
            //   3. Jump-to END
            //   4. IF
            //   5. <end>

            // 1.
            try emit(buf, node, WK_STACK, false, true, .Olit);
            const blk = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
            try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
            try emit(buf, node, WK_STACK, false, true, .Ojcn);

            // 2.
            if (when.nah) |nah|
                try genNodeList(program, buf, nah, ual);

            // 3.
            try emit(buf, node, WK_STACK, false, true, .Olit);
            const end = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
            try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
            try emit(buf, node, WK_STACK, false, true, .Ojmp);

            reemitAddr16(buf, blk, buf.items.len); // - blk_jump - 2);

            // 4.
            try genNodeList(program, buf, when.yup, ual);

            reemitAddr16(buf, end, buf.items.len); // - end_jump - 2);
        },
        .Cond => |cond| {
            var end_jumps = std.ArrayList(usize).init(gpa.allocator()); // Dummy jumps to fix
            defer end_jumps.deinit();

            for (cond.branches.items) |branch| {
                //try emitDUP(buf, WK_STACK);
                try genNodeList(program, buf, branch.cond, ual);
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oeq);

                try emit(buf, node, WK_STACK, false, false, .Olit);
                const addr_slot = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, false, .Ojcn);

                try genNodeList(program, buf, branch.body, ual);
                try emit(buf, node, WK_STACK, false, false, .Olit);
                const end_jmp = try emitRI(buf, node, WK_STACK, true, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, false, .Ojmp);
                try end_jumps.append(end_jmp);

                // TODO: overflow checks if body > 256 bytes
                const body_addr: u8 = @intCast(buf.items.len - addr_slot);
                buf.items[addr_slot].op.Oraw = body_addr;
            }

            if (cond.else_branch) |branch| {
                try genNodeList(program, buf, branch, ual);
            }

            // TODO: remove the very last end jump if there's no else_branch,
            // since it's completely unnecessary

            const cond_end_addr = buf.items.len;
            for (end_jumps.items) |end_jump| {
                buf.items[end_jump].op.Oraw = @intCast(cond_end_addr - end_jump);
            }
        },
    }
}

pub fn genNodeList(program: *Program, buf: *Ins.List, nodes: ASTNodeList, ual: *UA.List) CodegenError!void {
    var iter = nodes.iterator();
    while (iter.next()) |node|
        try genNode(program, buf, node, ual);
}

pub fn generate(program: *Program) CodegenError!Ins.List {
    var buf = Ins.List.init(gpa.allocator());
    var ual = UA.List.init(gpa.allocator());

    for (program.defs.items) |def| {
        const d = def.node.Decl;
        try genNodeList(program, &buf, program.ast, &ual);
        if (d.calls == 0) {
            //std.log.info("skipping {s}", .{d.name});
            continue;
        }
        def.romloc = buf.items.len;
        // const a = d.arity orelse @import("analyser.zig").BlockAnalysis{};
        // std.log.info("codegen: {s: >12}_{}\t{x}\t{s}", .{
        //     d.name,              d.variant,
        //     node.romloc + 0x100, a,
        // });
        try genNodeList(program, &buf, d.body, &ual);
        if (d.is_test) {
            try emit(&buf, def, WK_STACK, false, false, .Ohalt);
        } else {
            try emit(&buf, def, RT_STACK, false, true, .Ojmp);
        }
    }

    program.romloc_code_end = buf.items.len;

    for (program.statics.items) |*data| {
        data.romloc = buf.items.len;
        switch (data.default) {
            .String => |s| {
                assert(s.items.len + 1 == data.count); // TODO: pad when this isn't the case
                for (s.items) |b| try emit(&buf, null, 0, false, false, .{ .Oraw = b });
                try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
            },
            .None => {
                const typsz = data.type.size(program).?;
                assert(data.count > 0);
                const totalsz = typsz * data.count;
                for (0..totalsz) |_| try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
            },
        }
    }

    const here = buf.items.len;

    // std.log.info("here: {x}", .{buf.items.len + 0x100});
    // for (program.statics.items) |*data| {
    //     std.log.info("Static: {x}...{x}", .{ data.romloc + 0x100, data.romloc + data.count + 0x100 });
    // }

    for (ual.items) |ua| switch (ua.node.node) {
        .Here => reemitAddr16(&buf, ua.loc, here),
        .VRef => |v| reemitAddr16(&buf, ua.loc, program.statics.items[v.sind.?].romloc),
        .VDeref => |v| reemitAddr16(&buf, ua.loc, program.statics.items[v.sind.?].romloc),
        .Value => |v| {
            assert(v.typ == .StaticPtr);
            const static = program.statics.items[v.typ.StaticPtr];
            reemitAddr16(&buf, ua.loc, static.romloc);
        },
        .Call => |c| {
            const node = c.ctyp.Decl.node.?;
            assert(c.ctyp.Decl.variant == node.node.Decl.variant);
            assert(mem.eql(u8, ua.ident, node.node.Decl.name));

            if (node.romloc == 0xFFFF) {
                std.log.err("[Bug] codegen: word {s} (var {}) was never generated", .{
                    node.node.Decl.name, node.node.Decl.variant,
                });
                unreachable;
            }

            reemitAddr16(&buf, ua.loc, node.romloc);
        },
        else => unreachable,
    };

    return buf;
}

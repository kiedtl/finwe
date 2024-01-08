const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;

// const vm = @import("vm.zig");
const analyser = @import("analyser.zig");

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
        .Value => |v| switch (v.val.typ) {
            .StaticPtr => {},
            else => unreachable,
        },
        .Quote, .Here, .VRef, .VDeref => {},
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
            .SplitCast => {},
            .Make => {},
            .SizeOf => |sizeof| {
                //std.log.info("sizeof: {}", .{sizeof.resolved});
                const s = sizeof.resolved.size(program).?;
                if (s > 255) {
                    try emitIMM16(buf, node, WK_STACK, false, .Olit, s);
                } else {
                    try emitIMM(buf, node, WK_STACK, false, .Olit, @as(u8, @intCast(s)));
                }
            },
        },
        .Breakpoint => |brk| {
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0b);
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0e);
            try emit(buf, node, WK_STACK, false, false, .Odeo);
            program.breakpoints.append(brk) catch unreachable;
            program.breakpoints.items[program.breakpoints.items.len - 1].romloc = buf.items.len;
            program.breakpoints.items[program.breakpoints.items.len - 1].srcloc = node.srcloc;
        },
        .Cast => |c| {
            //std.log.info("codegen: {},{}: casting {} -> {}", .{ node.srcloc.line, node.srcloc.column, c.of, c.to });
            for (c.resolved.constSlice(), 0..) |to, i| {
                const from = c.from.constSlice()[i];
                const from_bits = from.bits(program).?;
                const to_bits = to.bits(program).?;

                if (from_bits == 16 and to_bits == 8) {
                    assert(i == 0);
                    try emit(buf, node, WK_STACK, false, false, .Onip);
                } else if (from_bits == 8 and to_bits == 16) {
                    assert(i == 0);
                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oswp);
                } else {
                    // nothing
                }
            }
        },
        .None => {},
        .GetChild => |gch| switch (gch.kind) {
            .unresolved => unreachable,
            .stk_one_s => {},
            .stk_two_b => |gch2b| if (gch2b.ind == 0) {
                try emit(buf, null, 0, false, false, .Onip);
            } else if (gch2b.ind == 1) {
                try emit(buf, null, 0, false, false, .Opop);
            },
            .stk_one_b => {},
            .mem => |gchmem| {
                assert(gchmem.offset != 0xFFFF);
                if (gchmem.is_short) {
                    try emitIMM16(buf, node, WK_STACK, false, .Olit, gchmem.offset);
                } else {
                    assert(gchmem.offset <= 255);
                    try emitIMM(buf, node, WK_STACK, false, .Olit, @intCast(gchmem.offset));
                }
                try emit(buf, null, 0, false, gchmem.is_short, .Oadd);
            },
        },
        .GetIndex => |gind| {
            assert(gind.multiplier != 0xFFFF);
            if (gind.ind == .known) {
                const v = gind.ind.known * gind.multiplier;
                try emitIMM16(buf, node, WK_STACK, false, .Olit, v);
            }
            if (gind.ind == .stk_b) {
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oswp);
            }
            if (gind.ind != .known) {
                try emitIMM16(buf, node, WK_STACK, false, .Olit, gind.multiplier);
                try emit(buf, null, 0, false, true, .Omul);
            }
            try emit(buf, null, 0, false, true, .Oadd);
        },
        .VDecl => {},
        .VRef => |v| try emitUA(buf, ual, v.name, node),
        .VDeref => |v| {
            try emitUA(buf, ual, v.name, node);
            // LDA instruction
            // Could do this in emitUA, but then we'd have to pass program
            const t = program.statics.items[v.localptr.?.ind.?].type;
            const is_short = t.bits(program).? == 16;
            try emit(buf, null, 0, false, is_short, .Olda);
        },
        .Value => |v| {
            const stk: usize = if (v.ret) RT_STACK else WK_STACK;
            if (v.val.typ == .StaticPtr) {
                try emitUA(buf, ual, "", node);
            } else if (v.val.typ.bits(program).? == 16) {
                try emitIMM16(buf, node, stk, false, .Olit, v.val.toU16(program));
            } else {
                try emitIMM(buf, node, stk, false, .Olit, v.val.toU8(program));
            }
        },
        .Decl => {},
        .Import => {},
        .Wild => |w| try genNodeList(program, buf, w.body, ual),
        .RBlock => |r| try genNodeList(program, buf, r.body, ual),
        .Quote => |q| {
            try emitUA(buf, ual, q.def.node.Decl.name, node);

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
                    try emit(buf, node, WK_STACK, false, false, .Oequ);

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
                    try emit(buf, node, WK_STACK, false, false, .Oequ);
                    try emitARG16(buf, node, WK_STACK, false, .Ojcn, 0x0100 + loop_begin);
                },
            }
        },
        .Asm => |a| try emit(buf, node, a.stack, a.keep, a.short, a.op),
        .Call => |f| if (f.is_inline) {
            try genNodeList(program, buf, f.node.?.node.Decl.body, ual);
        } else {
            try emitUA(buf, ual, f.name, node);
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
                try emit(buf, node, WK_STACK, false, false, .Oequ);

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
            // std.log.info("skipping {s}_{}", .{ d.name, d.variant });
            continue;
        } else {
            // std.log.info("*** genn {s}_{}", .{ d.name, d.variant });
        }
        assert(d.is_analysed);
        def.romloc = buf.items.len;
        // const a = d.arity orelse analyser.BlockAnalysis{};
        // std.log.info("codegen: {s: >12}_{}\t{x}\t{s}", .{
        //     d.name,             d.variant,
        //     def.romloc + 0x100, analyser.AnalysisFmt.from(&a, program),
        // });
        try genNodeList(program, &buf, d.body, &ual);
        if (d.is_test) {
            try emit(&buf, def, WK_STACK, false, false, .Obrk);
        } else {
            try emit(&buf, def, RT_STACK, false, true, .Ojmp);
        }
    }

    //program.romloc_code_end = buf.items.len;

    for (program.statics.items) |*data| {
        data.romloc = buf.items.len;
        switch (data.default) {
            .Mixed, .String => |s| {
                if (data.default == .String)
                    assert(s.items.len + 1 == data.count); // TODO: pad when this isn't the case
                for (s.items) |b| try emit(&buf, null, 0, false, false, .{ .Oraw = b });
                try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
                program.romloc_code_end = buf.items.len;
            },
            .None => {
                const typsz = data.type.size(program).?;
                const totalsz = typsz * data.count;
                assert(totalsz > 0);
                for (0..totalsz) |_|
                    try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
            },
        }
    }

    const here = buf.items.len;

    // // std.log.info("here: {x}", .{buf.items.len + 0x100});
    // for (program.statics.items) |*data| {
    //     std.log.info("Static: {s} {x}...{x}", .{
    //         @tagName(data.default), data.romloc + 0x100, data.romloc + (data.type.size(program).? * data.count) + 0x100,
    //     });
    // }

    for (ual.items) |ua| switch (ua.node.node) {
        .Here => reemitAddr16(&buf, ua.loc, here),
        .VRef => |v| {
            const loc = program.statics.items[v.localptr.?.ind.?].romloc;
            // std.log.info("Static @ {s}: {x}", .{ v.name, loc + 0x100 });
            reemitAddr16(&buf, ua.loc, loc);
        },
        .VDeref => |v| {
            const loc = program.statics.items[v.localptr.?.ind.?].romloc;
            // std.log.info("Static $ {s}: {x}", .{ v.name, loc + 0x100 });
            reemitAddr16(&buf, ua.loc, loc);
        },
        .Value => |v| {
            assert(v.val.typ == .StaticPtr);
            const static = program.statics.items[v.val.typ.StaticPtr];
            reemitAddr16(&buf, ua.loc, static.romloc);
        },
        .Quote => |q| {
            if (q.def.romloc == 0xFFFF) {
                std.log.err("[Bug] codegen: lambda {s}_{} not generated", .{
                    q.def.node.Decl.name, q.def.node.Decl.variant,
                });
                unreachable;
            }
            reemitAddr16(&buf, ua.loc, q.def.romloc);
        },
        .Call => |c| {
            const node = c.node.?;
            assert(c.variant == node.node.Decl.variant);

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

pub fn emitBytecode(writer: anytype, program: []const Ins) !void {
    for (program) |ins| {
        if (ins.op == .Oraw) {
            try writer.writeByte(ins.op.Oraw);
        } else {
            const byte = @intFromEnum(@as(OpTag, ins.op));
            const ms: u8 = if (ins.short) 0x20 else 0;
            const mr: u8 = if (ins.stack == RT_STACK) 0x40 else 0;
            const mk: u8 = if (ins.keep) 0x80 else 0;
            try writer.writeByte(byte | ms | mr | mk);
        }
    }
}

const std = @import("std");
const meta = std.meta;
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
const ErrorSet = @import("common.zig").Error.Set;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

const CodegenError = mem.Allocator.Error || StackBufferError;

pub const UA = struct {
    label_type: LabelType,
    label_src: union(enum) {
        Node: *ASTNode,
        StaticInd: usize,

        pub fn eq(a: @This(), b: @This()) bool {
            return @as(meta.Tag(@This()), a) == @as(meta.Tag(@This()), b) and
                switch (a) {
                .Node => |n| n == b.Node,
                .StaticInd => |n| n == b.StaticInd,
            };
        }
    },

    pub const List = std.ArrayList(UA);
};

pub const LabelType = enum {
    None,
    LoopBegin,
    LoopEnd,
    DeclBegin,
    Here,
    StaticsHere,
    Static,
    WhenMainBodyBegin,
    WhenEnd,
    BreakpointEnd,
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

fn emitDataLabel(buf: *Ins.List, ind: usize) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xlbl = .{ .label_type = .Static, .label_src = .{ .StaticInd = ind } },
    });
}

fn emitLabel(buf: *Ins.List, ident: LabelType, node: *ASTNode) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xlbl = .{ .label_type = ident, .label_src = .{ .Node = node } },
    });
}

fn emitDataUA(buf: *Ins.List, i: usize) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xtua = .{ .label_src = .{ .StaticInd = i }, .label_type = .Static },
    });
    try emit(buf, null, WK_STACK, false, false, .{ .Oraw = 0 });
}

fn emitUA(buf: *Ins.List, ident: LabelType, node: *ASTNode) CodegenError!void {
    try emit(buf, node, WK_STACK, false, false, .{
        .Xtua = .{ .label_src = .{ .Node = node }, .label_type = ident },
    });
    try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
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

fn genCondPrep(_: *Program, buf: *Ins.List, node: *ASTNode, cp: ASTNode.CondPrep) CodegenError!void {
    switch (cp) {
        .unchecked => unreachable,
        .none => {},
        .dup_1_b => try emit(buf, node, WK_STACK, false, false, .Odup),
        .dup_1_s => try emit(buf, node, WK_STACK, false, true, .Odup),
        .dup_2_bb => try emit(buf, node, WK_STACK, false, true, .Odup),
        .dup_2_sb => {
            //
            // ( short byte -- short byte short byte )
            // i.e. DUP OVR2
            //
            try emit(buf, node, WK_STACK, false, false, .Odup);
            try emit(buf, node, WK_STACK, false, true, .Oovr);
        },
        .dup_2_bs => {
            //
            // ( byte short -- byte short byte short )
            // i.e. ROTk ROT ROT
            //
            try emit(buf, node, WK_STACK, true, false, .Orot);
            try emit(buf, node, WK_STACK, false, false, .Orot);
            try emit(buf, node, WK_STACK, false, false, .Orot);
        },
        .dup_2_ss => {
            //
            // ( short short -- short short short short )
            // i.e. OVR2 OVR2
            //
            try emit(buf, node, WK_STACK, false, true, .Oovr);
            try emit(buf, node, WK_STACK, false, true, .Oovr);
        },
    }
}

fn genNode(program: *Program, buf: *Ins.List, node: *ASTNode) CodegenError!void {
    switch (node.node) {
        .TypeDef => {},
        .Return => {
            try emit(buf, node, RT_STACK, false, true, .Ojmp);
        },
        .Debug => {},
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
            .Here => {
                try emit(buf, node, WK_STACK, false, true, .Olit);
                try emitUA(buf, .Here, node);
            },
            .StaticsHere => {
                try emit(buf, node, WK_STACK, false, true, .Olit);
                try emitUA(buf, .StaticsHere, node);
            },
        },
        .Breakpoint => |*brk| {
            brk.breakpoint_ind = program.breakpoints.items.len;
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0b);
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0e);
            try emit(buf, node, WK_STACK, false, false, .Odeo);
            program.breakpoints.append(brk.*) catch unreachable;
            try emitLabel(buf, .BreakpointEnd, node);
            //program.breakpoints.items[program.breakpoints.items.len - 1].romloc = buf.items.len;
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
            assert(gind.order != .unchecked);
            assert(gind.multiplier != 0xFFFF);
            assert(gind.ind != .stk_unresolved);
            if (gind.ind == .known) {
                const v = gind.ind.known * gind.multiplier;
                try emitIMM16(buf, node, WK_STACK, false, .Olit, v);
                assert(gind.order == .moot);
            }
            if (gind.ind == .stk_b) {
                if (gind.order == .ti) try emit(buf, null, 0, false, false, .Orot);
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oswp);
            }
            if (gind.ind != .known and gind.multiplier > 1) {
                // Swap if target is first and index isn't byte
                // (If index is byte we've already swapped)
                //
                if (gind.order == .ti and gind.ind != .stk_b)
                    try emit(buf, null, 0, false, true, .Oswp);
                try emitIMM16(buf, node, WK_STACK, false, .Olit, gind.multiplier);
                try emit(buf, null, 0, false, true, .Omul);
            }
            try emit(buf, null, 0, false, true, .Oadd);
        },
        .VDecl => {},
        .VRef => |v| {
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitDataUA(buf, v.localptr.?.ind.?);
        },
        .VDeref => |v| {
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitDataUA(buf, v.localptr.?.ind.?);
            // LDA instruction
            // Could do this in emitUA, but then we'd have to pass program
            const t = program.statics.items[v.localptr.?.ind.?].type;
            const is_short = t.bits(program).? == 16;
            try emit(buf, null, 0, false, is_short, .Olda);
        },
        .Value => |v| {
            const stk: usize = if (v.ret) RT_STACK else WK_STACK;
            if (v.val.typ == .StaticPtr) {
                try emit(buf, node, WK_STACK, false, true, .Olit);
                try emitDataUA(buf, v.val.typ.StaticPtr);
            } else if (v.val.typ.bits(program).? == 16) {
                try emitIMM16(buf, node, stk, false, .Olit, v.val.toU16(program));
            } else {
                try emitIMM(buf, node, stk, false, .Olit, v.val.toU8(program));
            }
        },
        .Decl => {},
        .Import => {},
        .Wild => |w| try genNodeList(program, buf, w.body),
        .RBlock => |r| try genNodeList(program, buf, r.body),
        .Quote => |q| {
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitUA(buf, .DeclBegin, q.def);
        },
        .Loop => |l| {
            switch (l.loop) {
                .While => |u| {
                    try emitLabel(buf, .LoopBegin, node);
                    try genCondPrep(program, buf, node, u.cond_prep);
                    try genNodeList(program, buf, u.cond);

                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oequ);

                    try emit(buf, node, WK_STACK, false, true, .Olit);
                    try emitUA(buf, .LoopEnd, node);
                    try emit(buf, node, WK_STACK, false, true, .Ojcn);

                    try genNodeList(program, buf, l.body);
                    try emit(buf, node, WK_STACK, false, true, .Olit);
                    try emitUA(buf, .LoopBegin, node);
                    try emit(buf, node, WK_STACK, false, true, .Ojmp);
                    try emitLabel(buf, .LoopEnd, node);
                },
                .Until => |u| {
                    try emitLabel(buf, .LoopBegin, node);
                    try genNodeList(program, buf, l.body);

                    try genCondPrep(program, buf, node, u.cond_prep);
                    try genNodeList(program, buf, u.cond);
                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oequ);
                    try emit(buf, node, WK_STACK, false, true, .Olit);
                    try emitUA(buf, .LoopBegin, node);
                    try emit(buf, node, WK_STACK, false, true, .Ojcn);
                },
            }
        },
        .Asm => |a| try emit(buf, node, a.stack, a.keep, a.short, a.op),
        .Call => |f| if (f.is_inline) {
            try genNodeList(program, buf, f.node.?.node.Decl.body);
        } else {
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitUA(buf, .DeclBegin, f.node.?);
            try emit(buf, node, WK_STACK, false, true, .Ojsr);
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
            try emitUA(buf, .WhenMainBodyBegin, node);
            try emit(buf, node, WK_STACK, false, true, .Ojcn);

            // 2.
            if (when.nah) |nah|
                try genNodeList(program, buf, nah);

            // 3.
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitUA(buf, .WhenEnd, node);
            try emit(buf, node, WK_STACK, false, true, .Ojmp);

            // 4.
            try emitLabel(buf, .WhenMainBodyBegin, node);
            try genNodeList(program, buf, when.yup);

            try emitLabel(buf, .WhenEnd, node);
        },
        .Cond => |cond| {
            if (true) unreachable;
            var end_jumps = std.ArrayList(usize).init(gpa.allocator()); // Dummy jumps to fix
            defer end_jumps.deinit();

            for (cond.branches.items) |branch| {
                try genCondPrep(program, buf, node, branch.cond_prep);
                try genNodeList(program, buf, branch.cond);
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oequ);

                try emit(buf, node, WK_STACK, false, true, .Olit);
                const addr_slot = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, true, .Ojcn);

                try genNodeList(program, buf, branch.body);
                try emit(buf, node, WK_STACK, false, true, .Olit);
                const end_jmp = try emitRI(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, false, .{ .Oraw = 0 });
                try emit(buf, node, WK_STACK, false, true, .Ojmp);
                try end_jumps.append(end_jmp);

                // TODO: overflow checks if body > 256 bytes
                //const body_addr: u8 = @intCast(buf.items.len - addr_slot);
                //buf.items[addr_slot].op.Oraw = body_addr;
                reemitAddr16(buf, addr_slot, buf.items.len);
            }

            if (cond.else_branch) |branch| {
                try genNodeList(program, buf, branch);
            }

            // TODO: remove the very last end jump if there's no else_branch,
            // since it's completely unnecessary

            const cond_end_addr = buf.items.len;
            for (end_jumps.items) |end_jump|
                reemitAddr16(buf, end_jump, cond_end_addr);
        },
    }
}

pub fn genNodeList(program: *Program, buf: *Ins.List, nodes: ASTNodeList) CodegenError!void {
    var iter = nodes.iterator();
    while (iter.next()) |node|
        try genNode(program, buf, node);
}

pub fn generate(program: *Program) CodegenError!Ins.List {
    var buf = Ins.List.init(gpa.allocator());

    for (program.defs.items) |def| {
        const d = def.node.Decl;
        try genNodeList(program, &buf, program.ast);
        if (d.calls == 0) {
            // std.log.info("skipping {s}_{}", .{ d.name, d.variant });
            continue;
        }

        if (!d.is_analysed) {
            std.log.err("[Bug] codegen: word {s} (var {}) was never analysed", .{
                d.name, d.variant,
            });
            unreachable;
        }

        // const a = d.arity orelse analyser.BlockAnalysis{};
        // std.log.info("codegen: {s: >12}_{}\t{x}\t{s}", .{
        //     d.name,             d.variant,
        //     def.romloc + 0x100, analyser.AnalysisFmt.from(&a, program),
        // });
        try emitLabel(&buf, .DeclBegin, def);
        try genNodeList(program, &buf, d.body);
        if (d.is_test) {
            try emit(&buf, def, WK_STACK, false, false, .Obrk);
        } else {
            try emit(&buf, def, RT_STACK, false, true, .Ojmp);
        }
    }

    program.romloc_code_end = buf.items.len;

    for (program.statics.items, 0..) |*data, data_i| {
        try emitDataLabel(&buf, data_i);
        const typsz = data.type.size(program).?;
        const totalsz = typsz * data.count;
        assert(totalsz > 0);
        var done: usize = 0;

        switch (data.default) {
            .Mixed, .String => |s| {
                const emitnullbyte = data.default == .String;
                assert((s.items.len + if (emitnullbyte) @as(usize, 1) else 0) <= totalsz);

                for (s.items) |b| try emit(&buf, null, 0, false, false, .{ .Oraw = b });
                if (emitnullbyte) try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
                done += s.items.len + if (emitnullbyte) @as(usize, 1) else 0;
            },
            .None => {},
        }

        for (0..(totalsz - done)) |_|
            try emit(&buf, null, 0, false, false, .{ .Oraw = 0 });
    }

    {
        var i: usize = 0;
        while (i < buf.items.len) {
            if (buf.items[i].op == .Xlbl) {
                const next_norm = for (buf.items[i + 1 ..], 0..) |ins, j| {
                    if (ins.op != .Xlbl) break i + 1 + j;
                } else break;
                buf.items[next_norm].labels.append(.{ .for_ua = buf.items[i].op.Xlbl }) catch
                    unreachable;
                _ = buf.orderedRemove(i);
            } else {
                i += 1;
            }
        }
    }

    for (buf.items, 0..) |*ins, addr| for (ins.labels.constSlice()) |label| {
        switch (label.for_ua.label_type) {
            .DeclBegin => label.for_ua.label_src.Node.romloc = addr,
            .BreakpointEnd => program.breakpoints.items[
                label.for_ua.label_src.Node.node.Breakpoint.breakpoint_ind.?
            ].romloc = addr,
            .Static => program.statics.items[label.for_ua.label_src.StaticInd].romloc = addr,
            else => {},
        }
    };

    for (buf.items, 0..) |*ins, addr| if (ins.op == .Xtua) {
        if (ins.op.Xtua.label_type == .Here or
            ins.op.Xtua.label_type == .StaticsHere)
        {
            continue;
        }
        const resolved = b: for (buf.items, 0..) |otherins, i| {
            for (otherins.labels.constSlice()) |label|
                if (label.for_ua.label_src.eq(ins.op.Xtua.label_src) and
                    label.for_ua.label_type == ins.op.Xtua.label_type)
                {
                    break :b i;
                };
        } else {
            std.log.err("codegen: BUG: UA cannot be resolved: {}", .{ins.op.Xtua});
            unreachable;
        };
        ins.op = .{ .Oraw = 0 };
        reemitAddr16(&buf, addr, resolved);
    };

    const here = buf.items.len;

    // std.log.info("here: {x}", .{buf.items.len + 0x100});
    // for (program.statics.items) |*data| {
    //     std.log.info("Static: {s} {x}...{x}", .{
    //         @tagName(data.default), data.romloc + 0x100, data.romloc + (data.type.size(program).? * data.count) + 0x100,
    //     });
    // }

    for (buf.items, 0..) |*ins, addr| if (ins.op == .Xtua) {
        const resolved = switch (ins.op.Xtua.label_type) {
            .Here => here,
            // FIXME: extra +0x100? reemit already does that? bug?
            .StaticsHere => program.romloc_code_end + 0x100,
            else => unreachable,
        };
        ins.op = .{ .Oraw = 0 };
        reemitAddr16(&buf, addr, resolved);
    };

    return buf;
}

pub fn emitBytecode(writer: anytype, program: []const Ins) !void {
    for (program) |ins| {
        assert(ins.op != .Xtua);
        assert(ins.op != .Xlbl);
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

pub fn emitDebug(writer: anytype, program: *Program) !void {
    const Item = struct {
        romloc: usize,
        node: *ASTNode,
        parent: ?*ASTNode,
        pub const AList = std.ArrayList(@This());
    };

    var items = Item.AList.init(gpa.allocator());
    defer items.deinit();

    try program.walkNodes(null, program.ast, &items, struct {
        pub fn f(node: *ASTNode, parent: ?*ASTNode, _: *Program, buf: *Item.AList) ErrorSet!void {
            if (node.romloc != 0xFFFF)
                buf.append(.{
                    .romloc = node.romloc + 0x100,
                    .node = node,
                    .parent = parent,
                }) catch unreachable;
        }
    }.f);

    std.sort.insertion(Item, items.items, {}, struct {
        pub fn lessThan(_: void, lhs: Item, rhs: Item) bool {
            return lhs.romloc < rhs.romloc;
        }
    }.lessThan);

    for (items.items) |item| {
        if (item.parent) |par| {
            const s = switch (par.node) {
                .Import => |i| i.name,
                .Decl => |d| d.name,
                else => unreachable,
            };
            try writer.print("{x:0>4}\t{s}\t{}\n", .{ item.romloc, s, item.node.srcloc });
        } else {
            try writer.print("{x:0>4}\t<???>\t{}\n", .{ item.romloc, item.node.srcloc });
        }
    }
}

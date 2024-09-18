const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const assert = std.debug.assert;

// const vm = @import("vm.zig");
const analyser = @import("analyser.zig");

const StackBufferError = @import("buffer.zig").StackBufferError;

const TypeFmt = @import("common.zig").TypeFmt;
const ASTNode = @import("common.zig").ASTNode;
const Scope = @import("common.zig").Scope;
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
        Node: struct { ptr: *ASTNode, gen_id: usize },
        Static: usize,

        pub fn eq(a: @This(), b: @This()) bool {
            return @as(meta.Tag(@This()), a) == @as(meta.Tag(@This()), b) and
                switch (a) {
                .Node => |n| if (n.ptr == b.Node.ptr and n.gen_id == b.Node.gen_id)
                    true
                else if (n.ptr.node == .Decl and b.Node.ptr.node == .Decl)
                    if (b.Node.ptr.node.Decl.folded_into) |folded_into|
                        folded_into == n.ptr
                    else
                        false
                else
                    false,
                .Static => |n| n == b.Static,
            };
        }
    },
    relative: RelativeCtl,

    pub const List = std.ArrayList(UA);
    pub const RelativeCtl = enum { X, Always, Auto, Never }; // X: n/a
};

pub const LabelType = union(enum) {
    LoopBegin,
    LoopContinue,
    LoopEnd,
    DeclBegin,
    DeclEnd,
    InlineDeclBegin,
    InlineDeclEnd,
    Here,
    StaticsHere,
    Static,
    WhenMainBodyBegin,
    WhenElseBodyBegin,
    WhenEnd,
    BreakpointEnd,
    CondEnd,
    CondBranchEnd: usize,

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        return switch (self) {
            .LoopBegin => try writer.print("loop", .{}),
            .LoopContinue => try writer.print("loop_test_continue", .{}),
            .LoopEnd => try writer.print("done", .{}),
            .DeclBegin => try writer.print("", .{}),
            .DeclEnd => try writer.print("end_decl", .{}),
            .InlineDeclBegin => try writer.print("inline", .{}),
            .InlineDeclEnd => try writer.print("end_inline", .{}),
            .Here => try writer.print("here", .{}),
            .StaticsHere => try writer.print("here_statics", .{}),
            .Static => try writer.print("", .{}),
            .WhenMainBodyBegin => try writer.print("if_true_body", .{}),
            .WhenElseBodyBegin => try writer.print("if_false_body", .{}),
            .WhenEnd => try writer.print("fi", .{}),
            .BreakpointEnd => try writer.print("end_brk", .{}),
            .CondEnd => try writer.print("case", .{}),
            .CondBranchEnd => |i| try writer.print("esac_{}", .{i}),
        };
    }

    pub fn eq(a: @This(), b: @This()) bool {
        return @as(meta.Tag(@This()), a) == @as(meta.Tag(@This()), b) and
            switch (a) {
            .CondBranchEnd => |n| n == b.CondBranchEnd,
            else => true,
        };
    }
};

//
//
// -----------------------------------------------------------------------------
//
//

fn emit(buf: *Ins.List, _: ?*ASTNode, stack: usize, k: bool, s: bool, op: Op) CodegenError!void {
    try buf.append(Ins{ .stack = stack, .op = op, .keep = k, .short = s });
}

fn reemitRaw16(buf: *Ins.List, ind: usize, value: usize) void {
    const addr = @as(u16, @intCast(value));
    buf.items[ind + 0].op.Oraw = @as(u8, @intCast(addr >> 8));
    buf.items[ind + 1].op.Oraw = @as(u8, @intCast(addr & 0xFF));
}

fn reemitAddr16(buf: *Ins.List, ind: usize, value: usize) void {
    const addr = @as(u16, @intCast(value)) + 0x100;
    buf.items[ind + 0].op.Oraw = @as(u8, @intCast(addr >> 8));
    buf.items[ind + 1].op.Oraw = @as(u8, @intCast(addr & 0xFF));
}

fn emitDataLabel(buf: *Ins.List, ind: usize) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xlbl = .{ .label_type = .Static, .label_src = .{ .Static = ind }, .relative = .X },
    });
}

fn emitLabel(buf: *Ins.List, ident: LabelType, node: *ASTNode) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xlbl = .{ .label_type = ident, .label_src = .{ .Node = .{ .ptr = node, .gen_id = node.gen_id } }, .relative = .X },
    });
}

fn emitDataUA(buf: *Ins.List, i: usize) CodegenError!void {
    try emit(buf, null, WK_STACK, false, false, .{
        .Xtua = .{ .label_src = .{ .Static = i }, .label_type = .Static, .relative = .Never },
    });
    try emit(buf, null, WK_STACK, false, false, .{ .Oraw = 0 });
}

fn emitUA(buf: *Ins.List, ident: LabelType, r: UA.RelativeCtl, node: *ASTNode) CodegenError!void {
    try emit(buf, node, WK_STACK, false, false, .{
        .Xtua = .{ .label_src = .{ .Node = .{ .ptr = node, .gen_id = node.gen_id } }, .label_type = ident, .relative = r },
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

const Ctx = struct {
    parent_decl: *ASTNode,
    is_parent_inline: ?*ASTNode = null,
};

fn genNode(program: *Program, buf: *Ins.List, node: *ASTNode, ctx: Ctx) CodegenError!void {
    switch (node.node) {
        .Debug, .TypeDef, .None, .Decl, .VDecl, .Import => return,
        else => {},
    }

    node.gen_id += 1;

    switch (node.node) {
        .Return => if (ctx.is_parent_inline) |call_node| {
            try emit(buf, node, WK_STACK, false, false, .Ojmi);
            try emitUA(buf, .InlineDeclEnd, .Always, call_node);
        } else {
            try emit(buf, node, RT_STACK, false, true, .Ojmp);
        },
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
                try emitUA(buf, .Here, .Never, node);
            },
            .StaticsHere => {
                try emit(buf, node, WK_STACK, false, true, .Olit);
                try emitUA(buf, .StaticsHere, .Never, node);
            },
        },
        .Breakpoint => {
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0b);
            try emitIMM(buf, node, WK_STACK, false, .Olit, 0x0e);
            try emit(buf, node, WK_STACK, false, false, .Odeo);
            try emitLabel(buf, .BreakpointEnd, node);
            program.breakpoints.items[program.breakpoints.items.len - 1].srcloc = node.srcloc;
        },
        .Cast => |c| {
            const stk: usize = if (c.ret) RT_STACK else WK_STACK;
            for (c.resolved.constSlice(), 0..) |to, i| {
                const from = c.from.constSlice()[i];
                // std.log.info("codegen: {}: casting {} -> {}", .{ node.srcloc, from, to });
                const from_bits = from.bits(program).?;
                const to_bits = to.bits(program).?;

                if (from_bits == 16 and to_bits == 8) {
                    assert(i == 0);
                    try emit(buf, node, stk, false, false, .Onip);
                } else if (from_bits == 8 and to_bits == 16) {
                    assert(i == 0);
                    try emitIMM(buf, node, stk, false, .Olit, 0);
                    try emit(buf, node, stk, false, false, .Oswp);
                } else {
                    // nothing
                }
            }
        },
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
        .Wild => |w| try genNodeList(program, buf, w.body, ctx),
        .RBlock => |r| try genNodeList(program, buf, r.body, ctx),
        .Quote => |q| {
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitUA(buf, .DeclBegin, .Never, q.def);
        },
        .Loop => |l| {
            switch (l.loop) {
                .While => |u| {
                    try emitLabel(buf, .LoopBegin, node);
                    try emitLabel(buf, .LoopContinue, node);
                    try genCondPrep(program, buf, node, u.cond_prep);
                    try genNodeList(program, buf, u.cond, ctx);

                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oequ);

                    try emit(buf, node, WK_STACK, false, false, .Ojci);
                    try emitUA(buf, .LoopEnd, .Always, node);

                    try genNodeList(program, buf, l.body, ctx);
                    try emit(buf, node, WK_STACK, false, false, .Ojmi);
                    try emitUA(buf, .LoopBegin, .Always, node);
                    try emitLabel(buf, .LoopEnd, node);
                },
                .Until => |u| {
                    try emitLabel(buf, .LoopBegin, node);
                    try genNodeList(program, buf, l.body, ctx);

                    try emitLabel(buf, .LoopContinue, node);
                    try genCondPrep(program, buf, node, u.cond_prep);
                    try genNodeList(program, buf, u.cond, ctx);
                    try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                    try emit(buf, node, WK_STACK, false, false, .Oequ);
                    try emit(buf, node, WK_STACK, false, true, .Ojci);
                    try emitUA(buf, .LoopBegin, .Always, node);
                    try emitLabel(buf, .LoopEnd, node);
                },
            }
        },
        .Break => |b| {
            try emit(buf, node, WK_STACK, false, false, .Ojmi);
            try emitUA(buf, .LoopEnd, .Always, b.loop.?);
        },
        .Continue => |c| {
            try emit(buf, node, WK_STACK, false, false, .Ojmi);
            try emitUA(buf, .LoopContinue, .Always, c.loop.?);
        },
        .Asm => |a| try emit(buf, node, a.stack, a.keep, a.short, a.op),
        .Call => |f| if (f.is_inline_override or
            f.node.?.node.Decl.is_inline == .AutoYes or
            f.node.?.node.Decl.is_inline == .Always)
        {
            try emitLabel(buf, .InlineDeclBegin, node);
            try genNodeList(program, buf, f.node.?.node.Decl.body, .{
                .parent_decl = f.node.?,
                .is_parent_inline = node,
            });
            try emitLabel(buf, .InlineDeclEnd, node);
        } else {
            try emit(buf, node, WK_STACK, false, false, if (f.goto) .Ojmi else .Ojsi);
            try emitUA(buf, .DeclBegin, .Always, f.node.?);
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
            try emit(buf, node, WK_STACK, false, false, .Ojci);
            try emitUA(buf, .WhenMainBodyBegin, .Always, node);

            // 2.
            try emitLabel(buf, .WhenElseBodyBegin, node);
            if (when.nah) |nah|
                try genNodeList(program, buf, nah, ctx);

            // 3.
            try emit(buf, node, WK_STACK, false, true, .Olit);
            try emitUA(buf, .WhenEnd, .Never, node);
            try emit(buf, node, WK_STACK, false, true, .Ojmp);

            // 4.
            try emitLabel(buf, .WhenMainBodyBegin, node);
            try genNodeList(program, buf, when.yup, ctx);

            try emitLabel(buf, .WhenEnd, node);
        },
        .Cond => |cond| {
            for (cond.branches.items, 0..) |branch, i| {
                try genCondPrep(program, buf, node, branch.cond_prep);
                try genNodeList(program, buf, branch.cond, ctx);
                try emitIMM(buf, node, WK_STACK, false, .Olit, 0);
                try emit(buf, node, WK_STACK, false, false, .Oequ);

                try emit(buf, node, WK_STACK, false, false, .Ojci);
                try emitUA(buf, .{ .CondBranchEnd = i }, .Always, node);

                try genNodeList(program, buf, branch.body, ctx);
                try emit(buf, node, WK_STACK, false, false, .Ojmi);
                try emitUA(buf, .CondEnd, .Always, node);

                try emitLabel(buf, .{ .CondBranchEnd = i }, node);
            }

            if (cond.else_branch) |branch| {
                try genNodeList(program, buf, branch, ctx);
            }

            // TODO: remove the very last end jump if there's no else_branch,
            // since it's completely unnecessary

            try emitLabel(buf, .CondEnd, node);
        },
        else => unreachable,
    }
}

pub fn genNodeList(program: *Program, buf: *Ins.List, nodes: ASTNodeList, ctx: Ctx) CodegenError!void {
    var iter = nodes.iterator();
    while (iter.next()) |node|
        try genNode(program, buf, node, ctx);
}

fn _staticTotalSize(program: *Program, ind: usize) usize {
    const data = program.statics.items[ind];
    const typsz = data.type.size(program).?;
    assert(data.count > 0);
    return typsz * data.count;
}

pub fn generate(program: *Program, buf: *Ins.List) CodegenError!void {
    // Main call if it's there
    // Nothing else will get generated (decls are ignored unless passed directly)
    try genNodeList(program, buf, program.ast, undefined);

    for (program.defs.items, 0..) |def, i| {
        const d = &def.node.Decl;
        if (d.skipGen()) {
            continue;
        }

        if (!d.is_analysed) {
            std.log.err("[Bug] codegen: word {s} (var {}) was never analysed", .{
                d.name, d.variant,
            });
            unreachable;
        }

        def.romloc = buf.items.len;
        try emitLabel(buf, .DeclBegin, def);
        try genNodeList(program, buf, d.body, .{ .parent_decl = def });
        assert(def.gen_id == 0);
        if (d.is_test) {
            try emit(buf, def, WK_STACK, false, false, .Obrk);
        } else {
            try emit(buf, def, RT_STACK, false, true, .Ojmp);
        }
        try emitLabel(buf, .DeclEnd, def);

        const fold_into = for (program.defs.items[0..i]) |prevdef| {
            if (prevdef.node.Decl.skipGen() or prevdef.node.Decl.folded_into != null)
                continue;
            if (prevdef.node.Decl.is_test)
                continue;
            if (prevdef.node.Decl.variant_of != d.variant_of)
                continue;
            const begin = prevdef.romloc;
            const end = for (buf.items[prevdef.romloc..], 0..) |ins, j| {
                if (ins.op == .Xlbl and ins.op.Xlbl.label_type == .DeclEnd and
                    ins.op.Xlbl.label_src.Node.ptr == prevdef)
                {
                    break prevdef.romloc + j;
                }
            } else unreachable;
            if (end - begin != buf.items.len - 1 - def.romloc)
                continue;
            const same = for (buf.items[begin..end], 0..) |previns, previ| {
                const curins = buf.items[def.romloc + previ];
                if (@as(OpTag, previns.op) != curins.op)
                    break false;
                switch (previns.op) {
                    .Oraw => |r| if (r != curins.op.Oraw) break false,
                    .Xlbl, .Xtua => |l| {
                        const c = if (previns.op == .Xtua) curins.op.Xtua else curins.op.Xlbl;
                        if (!l.label_type.eq(c.label_type))
                            break false;
                        if (l.label_type == .Static and c.label_type == .Static)
                            if (_staticTotalSize(program, l.label_src.Static) !=
                                _staticTotalSize(program, c.label_src.Static))
                                break false;
                    },
                    else => {},
                }
                if (previns.short != curins.short or
                    previns.keep != curins.keep or
                    previns.stack != curins.stack)
                {
                    break false;
                }
            } else true;

            if (same) break prevdef;
        } else null;

        if (fold_into) |prevdef| {
            assert(!d.is_test);
            buf.shrinkRetainingCapacity(def.romloc);
            d.folded_into = prevdef;
            def.romloc = 0xFFFF;
            // } else {
            //     const a = d.arity orelse analyser.BlockAnalysis{};
            //     const stdout = std.io.getStdOut().writer();
            //     stdout.print("{}\t{s}_{}\t\t{s}\n", .{
            //         d.bytecode_size, d.name,
            //         d.bytecode_size, analyser.AnalysisFmt.from(&a, program),
            //     }) catch unreachable;
        }
    }

    for (program.defs.items) |def|
        def.romloc = 0xFFFF;

    program.romloc_code_end = buf.items.len;

    // Sort static data, so that empty ones are last
    var statics = std.ArrayList(usize).init(gpa.allocator());
    defer statics.deinit();
    for (0..program.statics.items.len) |i| statics.append(i) catch unreachable;
    std.sort.insertion(usize, statics.items, program, struct {
        pub fn lessThan(p: *Program, lhs: usize, rhs: usize) bool {
            const is_a_nil = p.statics.items[lhs].default.items.len == 0;
            const is_b_nil = p.statics.items[rhs].default.items.len == 0;
            return !is_a_nil and is_b_nil;
        }
    }.lessThan);

    for (statics.items) |data_i| {
        const data = &program.statics.items[data_i];
        if (!data.used)
            continue;
        try emitDataLabel(buf, data_i);
        const totalsz = _staticTotalSize(program, data_i);
        var done: usize = 0;

        for (data.default.items) |default| switch (default) {
            .Byte => |b| {
                try emit(buf, null, 0, false, false, .{ .Oraw = b });
            },
            .Short => |s| {
                try emit(buf, null, 0, false, false, .{ .Oraw = @intCast(s >> 8) });
                try emit(buf, null, 0, false, false, .{ .Oraw = @intCast(s & 0xFF) });
            },
            .String => |s| {
                for (s.items) |b| try emit(buf, null, 0, false, false, .{ .Oraw = b });
                try emit(buf, null, 0, false, false, .{ .Oraw = 0 });
                done += s.items.len + 1;
            },
            .Embed => |e| {
                for (e.data) |b| try emit(buf, null, 0, false, false, .{ .Oraw = b });
                done += e.data.len;
            },
        };

        if (done > totalsz) {
            std.log.err("[Bug] {} has de facto size larger than defined ({} > {})", .{
                data.srcloc, done, totalsz,
            });
            unreachable;
        }

        for (0..(totalsz - done)) |_|
            try emit(buf, null, 0, false, false, .{ .Oraw = 0 });
    }
}

pub fn resolveUAs(program: *Program, buf: *Ins.List) CodegenError!void {
    // Emit a null byte just in case there are labels without an ins after it
    // (It'll be removed later on when emitting anyway)
    try emit(buf, null, 0, false, false, .{ .Oraw = 0 });

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
            .BreakpointEnd, .DeclBegin => label.for_ua.label_src.Node.ptr.romloc = addr,
            .Static => program.statics.items[label.for_ua.label_src.Static].romloc = addr,
            else => {},
        }
    };

    for (program.defs.items) |def| {
        if (def.node.Decl.folded_into) |folded_into|
            def.romloc = folded_into.romloc;
    }

    for (buf.items, 0..) |*ins, addr| if (ins.op == .Xtua) {
        if (ins.op.Xtua.label_type == .Here or
            ins.op.Xtua.label_type == .StaticsHere)
        {
            continue;
        }
        const resolved = b: for (buf.items, 0..) |otherins, i| {
            for (otherins.labels.constSlice()) |label|
                if (label.for_ua.label_src.eq(ins.op.Xtua.label_src) and
                    label.for_ua.label_type.eq(ins.op.Xtua.label_type))
                {
                    break :b i;
                };
        } else {
            std.log.err("codegen: BUG: UA cannot be resolved: {}", .{ins.op.Xtua});
            unreachable;
        };
        if (ins.op.Xtua.relative != .Never) {
            const resolved_rel: u16 = @bitCast(@as(
                i16,
                @intCast(@as(isize, @intCast(resolved)) -
                    @as(isize, @intCast(addr + 2))),
            ));
            ins.op = .{ .Oraw = 0 };
            reemitRaw16(buf, addr, resolved_rel);
        } else {
            ins.op = .{ .Oraw = 0 };
            reemitAddr16(buf, addr, resolved);
        }
    };

    const here = buf.items.len;

    for (buf.items, 0..) |*ins, addr| if (ins.op == .Xtua) {
        const resolved = switch (ins.op.Xtua.label_type) {
            .Here => here,
            // FIXME: extra +0x100? reemit already does that? bug?
            .StaticsHere => program.romloc_code_end + 0x100,
            else => unreachable,
        };
        ins.op = .{ .Oraw = 0 };
        reemitAddr16(buf, addr, resolved);
    };
}

pub fn printAsmFor(program: *Program, buf: *const Ins.List, func: []const u8) CodegenError!void {
    const stdout = std.io.getStdOut().writer();

    for (program.defs.items) |def| {
        if (!mem.eql(u8, def.node.Decl.name, func))
            continue;

        if (def.node.Decl.skipGen()) {
            if (def.node.Decl.variant > 0)
                std.log.warn("Function \x1b[1m{s}\x1b[m wasn't generated (optimized out).", .{func});
            continue;
        }

        if (def.node.Decl.folded_into != null) {
            std.log.warn("Instance of \x1b[1m{s}\x1b[m not generated (folded).", .{func});
            continue;
        }

        if (def.node.Decl.calls > 0)
            try _printAsmForDeclNode(program, stdout, buf, func, def);

        for (def.node.Decl.variations.items) |vari|
            if (vari.node.Decl.calls > 0) {
                try _printAsmForDeclNode(program, stdout, buf, func, vari);
            };
    }
}

fn _printAsmForDeclNode(_: *Program, stdout: anytype, buf: *const Ins.List, func: []const u8, declnode: *ASTNode) CodegenError!void {
    stdout.print("\x1b[1m{s}\x1b[m:\n", .{func}) catch unreachable;
    const begin = search: for (buf.items, 0..) |ins, ind| {
        if (ins.op == .Xlbl)
            if (ins.op.Xlbl.label_type == .DeclBegin and
                ins.op.Xlbl.label_src.Node.ptr == declnode)
            {
                break :search ind + 1;
            };
    } else unreachable;
    const end = search: for (buf.items[begin..], begin..) |ins, ind| {
        if (ins.op == .Xlbl)
            if (ins.op.Xlbl.label_type == .DeclEnd and
                ins.op.Xlbl.label_src.Node.ptr == declnode)
            {
                break :search ind;
            };
    } else unreachable;

    var i: usize = begin;
    while (i < end) {
        if (buf.items[i].op == .Olit and buf.items[i + 1].op == .Oraw) {
            if (buf.items[i].short) {
                stdout.print("  #{}{}\n", .{ buf.items[i + 1], buf.items[i + 2] }) catch
                    unreachable;
                i += 3;
            } else {
                stdout.print("  #{}\n", .{buf.items[i + 1]}) catch unreachable;
                i += 2;
            }
        } else if (buf.items[i].op == .Xlbl and buf.items[i].op.Xlbl.label_src == .Node) {
            const lbl = buf.items[i].op.Xlbl;
            const u: u21 = if (lbl.label_type == .DeclBegin) '@' else '&';
            stdout.print("{u}{}", .{ u, lbl.label_type }) catch unreachable;
            switch (lbl.label_type) {
                .DeclBegin => stdout.print("{s}_{}", .{
                    lbl.label_src.Node.ptr.node.Decl.name,
                    lbl.label_src.Node.ptr.node.Decl.variant,
                }) catch unreachable,
                .InlineDeclBegin, .InlineDeclEnd => stdout.print("_{s}", .{
                    lbl.label_src.Node.ptr.node.Call.node.?.node.Decl.name,
                }) catch unreachable,
                else => {},
            }
            stdout.print("\n", .{}) catch unreachable;
            i += 1;
        } else if (buf.items[i].op == .Ojsi and buf.items[i + 1].op == .Xtua and
            buf.items[i + 1].op.Xtua.label_type == .DeclBegin)
        {
            const uadecl = buf.items[i + 1].op.Xtua.label_src.Node.ptr.node.Decl;
            stdout.print("  {s}_{}\n", .{ uadecl.name, uadecl.variant }) catch unreachable;
            i += 3;
        } else {
            stdout.print("  {}\n", .{buf.items[i]}) catch unreachable;
            i += 1;
        }
    }
    stdout.print("Total: {} bytes\n\n", .{end - begin}) catch unreachable;
}

pub fn emitBytecode(writer: anytype, program: []const Ins) !void {
    var last_non_null: usize = 0;
    for (program, 0..) |ins, i|
        if (ins.op != .Oraw or ins.op.Oraw != 0) {
            last_non_null = i;
        };

    for (program[0 .. last_non_null + 1]) |ins| {
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

    try program.walkNodes(&items, struct {
        pub fn f(node: *ASTNode, parent: ?*ASTNode, _: *Scope, _: *Program, buf: *Item.AList) ErrorSet!void {
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
                .Import => |i| program.imports.get(i).?.name,
                .Decl => |d| d.name,
                else => unreachable,
            };
            try writer.print("{x:0>4}\t{s}\t{}\n", .{ item.romloc, s, item.node.srcloc });
        } else {
            try writer.print("{x:0>4}\t<???>\t{}\n", .{ item.romloc, item.node.srcloc });
        }
    }
}

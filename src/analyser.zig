const std = @import("std");
const mem = std.mem;
const assert = std.debug.assert;

const common = @import("common.zig");

const Program = common.Program;
const ASTNode = common.ASTNode;
const ASTNodeList = common.ASTNodeList;

pub const BlockAnalysis = struct {
    args: usize = 0,
    stack: usize = 0,
    rargs: usize = 0,
    rstack: usize = 0,

    pub fn mergeInto(self: @This(), b: *@This()) void {
        if (b.stack < self.args)
            b.args += self.args - b.stack;
        b.stack -|= self.args;
        b.stack += self.stack;

        if (b.rstack < self.rargs)
            b.rargs += self.rargs - b.rstack;
        b.rstack -|= self.rargs;
        b.rstack += self.rstack;
    }
};

fn analyseAsm(i: common.Ins) BlockAnalysis {
    var a = BlockAnalysis{};

    switch (i.op) {
        .Oraw => {}, // TODO: panic and refuse to analyse block
        .Olit => a.stack += 1,
        .Ojmp => a.args += 1,
        .Ojcn => a.args += 2,
        .Ojsr => a.rstack += 1,
        .Ohalt => {},
        .Odup => {
            a.args += 1;
            a.stack += 1;
        },
        .Odrop => a.args += 1,
        .Oeq, .Oneq, .Olt, .Ogt, .Oeor, .Omul, .Oadd, .Osub => {
            a.args += 2;
            a.stack += 1;
        },
        .Ostash => {
            a.rstack += 1;
            a.args += 1;
        },
        .Odeo => a.args += 1,
        .Osr, .Ozj, .Onac, .Oroll, .Odmod => unreachable,
    }

    if (i.keep) a.args = 0;
    if (i.keep) a.rargs = 0;

    if (i.stack == common.RT_STACK) {
        a.args ^= a.rargs;
        a.rargs ^= a.args;
        a.args ^= a.rargs;

        a.stack ^= a.rstack;
        a.rstack ^= a.stack;
        a.stack ^= a.rstack;
    }

    return a;
}

fn analyseBlock(program: *Program, block: ASTNodeList) BlockAnalysis {
    var a = BlockAnalysis{};

    for (block.items) |node| switch (node.node) {
        .None => {},
        .Mac, .Decl => unreachable,
        .Call => |c| switch (c.ctyp) {
            .Decl => {
                const d = for (program.defs.items) |decl| {
                    if (mem.eql(u8, decl.node.Decl.name, c.name))
                        break decl;
                } else unreachable;
                const analysis = d.node.Decl.analysis orelse analyseBlock(program, d.node.Decl.body);
                d.node.Decl.analysis = analysis;
                d.node.Decl.analysis.?.mergeInto(&a);
            },
            .Mac => {
                const m = for (program.defs.items) |mac| {
                    if (mem.eql(u8, mac.node.Mac.name, c.name))
                        break mac;
                } else unreachable;
                const analysis = m.node.Mac.analysis orelse analyseBlock(program, m.node.Mac.body);
                m.node.Mac.analysis = analysis;
                m.node.Mac.analysis.?.mergeInto(&a);
            },
            .Unchecked => unreachable, // parser.postProcess missed something
        },
        .Loop => |l| analyseBlock(program, l.body).mergeInto(&a),
        .Cond => {
            // TODO: implement
            // Outline:
            // - Check first branch, don't merge analysis
            // - Check every other branch block, assert they're all the same
            //   - Analyse else branch also
            // - Check condition blocks, assert they're all identical
            // - Finally, merge one condition block, and one main block
        },
        .Asm => |i| analyseAsm(i).mergeInto(&a),
        .Value => a.stack += 1,
        .Quote => a.stack += 1,
    };

    return a;
}

pub fn analyse(program: *Program) void {
    for (program.defs.items) |decl| {
        assert(decl.node.Decl.analysis == null);
        decl.node.Decl.analysis = analyseBlock(program, decl.node.Decl.body);
    }
}

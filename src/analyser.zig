const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");

const Program = common.Program;
const ASTNode = common.ASTNode;
const ASTNodeList = common.ASTNodeList;
const ASTValue = common.ASTNode.ASTValue;
const VTList32 = ASTValue.TList32;

pub const BlockAnalysis = struct {
    args: VTList32 = VTList32.init(null),
    stack: VTList32 = VTList32.init(null),
    rargs: VTList32 = VTList32.init(null),
    rstack: VTList32 = VTList32.init(null),

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        try writer.print("\n    args:   ", .{});
        for (self.args.constSlice()) |i| try writer.print("{s}, ", .{@tagName(i)});
        try writer.print("\n    stack:  ", .{});
        for (self.stack.constSlice()) |i| try writer.print("{s}, ", .{@tagName(i)});
        try writer.print("\n    rargs:  ", .{});
        for (self.rargs.constSlice()) |i| try writer.print("{s}, ", .{@tagName(i)});
        try writer.print("\n    rstack: ", .{});
        for (self.rstack.constSlice()) |i| try writer.print("{s}, ", .{@tagName(i)});
    }

    pub fn mergeInto(self: @This(), b: *@This()) void {
        if (b.stack.len < self.args.len)
            b.args.appendSlice(
                self.args.constSlice()[0 .. self.args.len - b.stack.len],
            ) catch unreachable;
        b.stack.resizeTo(b.stack.len -| self.args.len);
        b.stack.appendSlice(self.stack.constSlice()) catch unreachable;

        if (b.rstack.len < self.rargs.len)
            b.rargs.appendSlice(
                self.rargs.constSlice()[0 .. self.rargs.len - b.rstack.len],
            ) catch unreachable;
        b.rstack.resizeTo(b.rstack.len -| self.rargs.len);
        b.rstack.appendSlice(self.rstack.constSlice()) catch unreachable;
    }
};

fn analyseAsm(i: common.Ins) BlockAnalysis {
    var a = BlockAnalysis{};

    switch (i.op) {
        .Odeo => {
            a.args.append(if (i.short) .U16 else .U8) catch unreachable;
            a.stack.append(.U8) catch unreachable; // TODO: device
        },
        .Odup => {
            a.args.append(.Any) catch unreachable;
            a.stack.append(.Any) catch unreachable;
        },
        .Odrop => a.args.append(.Any) catch unreachable,
        .Oeor, .Omul, .Oadd, .Osub => {
            a.args.append(if (i.short) .Any16 else .Any8) catch unreachable;
            a.args.append(if (i.short) .Any16 else .Any8) catch unreachable;
            a.stack.append(if (i.short) .Any16 else .Any8) catch unreachable;
        },
        .Oeq, .Oneq, .Olt, .Ogt => {
            a.args.append(if (i.short) .Any16 else .Any8) catch unreachable;
            a.args.append(if (i.short) .Any16 else .Any8) catch unreachable;
            a.stack.append(.Bool) catch unreachable;
        },
        .Ohalt => {},
        else => {
            std.log.info("{} not implmented", .{i});
            @panic("todo");
        },
        // .Oraw => {}, // TODO: panic and refuse to analyse block
        // .Olit => @panic("todo"), // TODO: panic and refuse to analyse block
        // .Ojmp => a.args.append(.AnyAddr) catch unreachable,
        // .Ojcn => {
        //     a.args.append(.Bool) catch unreachable;
        //     a.args.append(.AnyAddr) catch unreachable;
        // },
        // .Ojsr => a.rstack.append(.AbsAddr), // FIXME: short mode?
        // .Ostash => {
        //     a.rstack += 1;
        //     a.args += 1;
        // },
        // .Osr, .Ozj, .Onac, .Oroll, .Odmod => unreachable,
    }

    if (i.keep) a.args.clear();
    if (i.keep) a.rargs.clear();

    if (i.stack == common.RT_STACK) {
        const tmp = a.args;
        a.args = a.rargs;
        a.rargs = tmp;

        const stmp = a.stack;
        a.stack = a.rstack;
        a.rstack = stmp;
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
        .Value => |v| a.stack.append(v) catch unreachable,
        .Quote => a.stack.append(.Addr16) catch unreachable,
        .Cast => |c| {
            _ = a.stack.pop() catch unreachable;
            a.stack.append(c.builtin) catch unreachable;
        },
    };

    return a;
}

pub fn analyse(program: *Program) void {
    for (program.defs.items) |decl| {
        assert(decl.node.Decl.analysis == null);
        decl.node.Decl.analysis = analyseBlock(program, decl.node.Decl.body);
    }
}

const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");

const Program = common.Program;
const ASTNode = common.ASTNode;
const ASTNodeList = common.ASTNodeList;
const TypeInfo = common.TypeInfo;
const Value = common.Value;
const VTList32 = TypeInfo.List32;

pub const BlockAnalysis = struct {
    args: VTList32 = VTList32.init(null),
    stack: VTList32 = VTList32.init(null),
    rargs: VTList32 = VTList32.init(null),
    rstack: VTList32 = VTList32.init(null),

    pub fn conformGenericTo(generic: @This(), caller: *const @This(), p: *const Program) @This() {
        // TODO: do rstack
        if (generic.rstack.len > 0 or generic.rargs.len > 0) @panic("TODO");

        std.log.info("CONFORM {}", .{generic});
        std.log.info("TO ARGS {}", .{caller});

        var r = generic;

        for (r.args.slice()) |*arg|
            if (arg.* == .TypeRef) {
                arg.* = r.args.constSlice()[r.args.len - arg.*.TypeRef - 1];
            };

        for (r.stack.slice()) |*stack|
            if (stack.* == .TypeRef) {
                stack.* = r.args.constSlice()[r.args.len - stack.*.TypeRef - 1];
            };

        var i = r.args.len;
        var j: usize = 0;
        while (i > 0) : (j += 1) {
            i -= 1;
            const arg = &r.args.slice()[i];
            const calleritem = if (j < caller.stack.len)
                caller.stack.constSlice()[caller.stack.len - j - 1]
            else if ((j - caller.stack.len) < caller.args.len)
                caller.args.constSlice()[caller.args.len - (j - caller.stack.len) - 1]
            else
                arg.*;
            if (arg.isGeneric()) {
                if (!arg.doesInclude(calleritem, p)) {
                    std.log.err("Generic {} @ {} does not encompass {}", .{ arg, i, calleritem });
                    @panic("whoopsies");
                }
                arg.* = calleritem;
            }
        }

        std.log.info("RESULTS {}\n\n", .{r});

        return r;
    }

    pub fn eqExact(a: @This(), b: @This()) bool {
        const S = struct {
            pub fn f(_a: VTList32, _b: VTList32) bool {
                if (_a.len != _b.len) return false;
                return for (_a.constSlice(), 0..) |item, i| {
                    if (!item.eq(_b.constSlice()[i])) break false;
                } else true;
            }
        };
        return S.f(a.args, b.args) or S.f(a.rargs, b.rargs) or
            S.f(a.stack, b.stack) or S.f(a.rstack, b.rstack);
    }

    pub fn isGeneric(self: @This()) bool {
        const S = struct {
            pub fn f(list: VTList32) bool {
                return for (list.constSlice()) |item| {
                    if (item.isGeneric()) break true;
                } else false;
            }
        };
        return S.f(self.args) or S.f(self.rargs) or
            S.f(self.stack) or S.f(self.rstack);
    }

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
        // std.log.info("MERGE {}", .{self});
        // std.log.info("INTO  {}", .{b});

        if (b.stack.len < self.args.len)
            b.args.insertSlice(
                0,
                self.args.constSlice()[0 .. self.args.len - b.stack.len],
            ) catch unreachable;
        b.stack.resizeTo(b.stack.len -| self.args.len);
        b.stack.appendSlice(self.stack.constSlice()) catch unreachable;

        if (b.rstack.len < self.rargs.len)
            b.rargs.insertSlice(
                0,
                self.rargs.constSlice()[0 .. self.rargs.len - b.rstack.len],
            ) catch unreachable;
        b.rstack.resizeTo(b.rstack.len -| self.rargs.len);
        b.rstack.appendSlice(self.rstack.constSlice()) catch unreachable;

        // std.log.info("RESULT {}\n\n--------------\n", .{b});
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
        // .Ojmp => a.args.append(.Ptr8) catch unreachable,
        // .Ojcn => {
        //     a.args.append(.Bool) catch unreachable;
        //     a.args.append(.Ptr8) catch unreachable;
        // },
        // .Ojsr => a.rstack.append(.AbsPtr), // FIXME: short mode?
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

fn analyseBlock(program: *Program, parent: *ASTNode.Decl, block: ASTNodeList, a: *BlockAnalysis) void {
    // std.log.info("*** analysing {s}", .{parent.name});
    var iter = block.iterator();
    while (iter.next()) |node| {
        // std.log.info("node: {}", .{node.node});
        switch (node.node) {
            .None => {},
            .Mac, .Decl => unreachable,
            .Call => |*c| switch (c.ctyp) {
                .Decl => {
                    const d = for (program.defs.items) |decl| {
                        if (mem.eql(u8, decl.node.Decl.name, c.name))
                            break decl;
                    } else unreachable;
                    // TODO: don't analyse if arity exists
                    if (!d.node.Decl.is_analysed) {
                        analyseBlock(program, parent, d.node.Decl.body, &d.node.Decl.analysis);
                        d.node.Decl.is_analysed = true;
                    }
                    const analysis = d.node.Decl.arity orelse d.node.Decl.analysis;
                    if (analysis.isGeneric()) {
                        const ungenericified = analysis.conformGenericTo(a, program);
                        const variant_ind: ?usize = for (d.node.Decl.variations.slice(), 0..) |an, i| {
                            if (ungenericified.eqExact(an)) break i;
                        } else null;
                        if (variant_ind == null)
                            d.node.Decl.variations.append(ungenericified) catch unreachable;
                        c.ctyp.Decl = variant_ind orelse d.node.Decl.variations.len - 1;
                        ungenericified.mergeInto(a);
                    } else {
                        d.node.Decl.analysis.mergeInto(a);
                    }
                },
                .Mac => {
                    const m = for (program.defs.items) |mac| {
                        if (mem.eql(u8, mac.node.Mac.name, c.name))
                            break mac;
                    } else unreachable;
                    if (!m.node.Mac.is_analysed) {
                        analyseBlock(program, parent, m.node.Mac.body, &m.node.Mac.analysis);
                        m.node.Mac.is_analysed = true;
                    }
                    m.node.Mac.analysis.mergeInto(a);
                },
                .Unchecked => unreachable, // parser.postProcess missed something
            },
            .Loop => |l| analyseBlock(program, parent, l.body, a),
            .Cond => {
                // TODO: implement
                // Outline:
                // - Check first branch, don't merge analysis
                // - Check every other branch block, assert they're all the same
                //   - Analyse else branch also
                // - Check condition blocks, assert they're all identical
                // - Finally, merge one condition block, and one main block
            },
            .Asm => |i| {
                // std.log.info("merging asm into main", .{});
                analyseAsm(i).mergeInto(a);
            },
            .Value => |v| a.stack.append(v.typ) catch unreachable,
            .Quote => a.stack.append(TypeInfo.ptr16(program, .Quote, 1)) catch unreachable,
            .Cast => |c| {
                const typ = switch (c) {
                    .builtin => |b| b,
                    .ref => |r| parent.arity.?.args.constSlice()[r],
                };
                _ = a.stack.pop() catch unreachable;
                a.stack.append(typ) catch unreachable;
            },
        }
    }
}

pub fn analyse(program: *Program) void {
    for (program.defs.items) |decl_node| {
        const decl = &decl_node.node.Decl;
        assert(!decl.is_analysed);
        if (!decl.is_analysed) {
            analyseBlock(program, decl, decl.body, &decl_node.node.Decl.analysis);
            decl.is_analysed = true;
        }
    }
}

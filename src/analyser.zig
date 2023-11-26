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

    pub fn resolveTypeRefs(self: @This(), arity: *const @This()) @This() {
        // std.log.info("resolving {}\n with {}", .{ self, arity });
        var r = self;
        for (r.stack.slice()) |*stack|
            if (stack.* == .TypeRef) {
                // std.log.info("[stack] setting ${}", .{stack.*.TypeRef});
                stack.* = arity.args.constSlice()[arity.args.len - stack.*.TypeRef - 1];
            };
        for (r.args.slice()) |*arg|
            if (arg.* == .TypeRef) {
                // std.log.info("[args] setting ${}", .{arg.*.TypeRef});
                arg.* = arity.args.constSlice()[arity.args.len - arg.*.TypeRef - 1];
            };
        return r;
    }

    pub fn conformGenericTo(generic: @This(), caller: *const @This(), p: *const Program) @This() {
        // TODO: do rstack
        if (generic.rstack.len > 0 or generic.rargs.len > 0) @panic("TODO");

        std.log.info("CONFORM {}", .{generic});
        std.log.info("TO ARGS {}", .{caller});

        var r = generic;

        var i = r.args.len;
        var j: usize = 0;
        while (i > 0) : (j += 1) {
            i -= 1;
            const arg = &r.args.slice()[i];
            if (arg.* == .TypeRef)
                arg.* = r.args.constSlice()[r.args.len - arg.*.TypeRef - 1];

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

        for (r.stack.slice()) |*stack|
            if (stack.* == .TypeRef) {
                stack.* = r.args.constSlice()[r.args.len - stack.*.TypeRef - 1];
            };

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
        return S.f(a.args, b.args) and S.f(a.rargs, b.rargs) and
            S.f(a.stack, b.stack) and S.f(a.rstack, b.rstack);
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

fn analyseAsm(i: *common.Ins, caller_an: *const BlockAnalysis, prog: *Program) BlockAnalysis {
    var a = BlockAnalysis{};

    const args_needed: usize = switch (i.op) {
        .Ohalt => 0,
        .Odeo, .Odup, .Odrop => 1,
        .Orot => 3,
        else => 2,
    };

    const stk = if (i.stack == common.WK_STACK) &caller_an.stack else &caller_an.rstack;
    const a1 = if (stk.len >= 1) stk.constSlice()[stk.len - 1] else null;
    const a2 = if (stk.len >= 2) stk.constSlice()[stk.len - 2] else null;
    const a3 = if (stk.len >= 3) stk.constSlice()[stk.len - 3] else null;
    const a1b: ?u5 = if (stk.len >= 1) a1.?.bits(prog) else null;
    const a2b: ?u5 = if (stk.len >= 2) a2.?.bits(prog) else null;
    const a3b: ?u5 = if (stk.len >= 3) a3.?.bits(prog) else null;

    std.log.info("ASM: {}, stk.len: {}, args_needed: {}, a1b: {?}, a2b: {?}", .{ i, stk.len, args_needed, a1b, a2b });
    std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });

    if (i.generic and stk.len >= args_needed and
        ((args_needed == 2 and a1b != null and a2b != null) or
        (args_needed == 3 and a1b != null and a2b != null) or
        (args_needed == 1 and a1b != null)))
    {
        i.short = switch (i.op) {
            .Odup, .Odrop, .Odeo => a1b.? == 16,
            .Orot => a1b.? == 16 and a2b.? == 16 and a3b.? == 16,
            else => a1b.? == 16 and a2b.? == 16,
        };
    }

    std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });

    const any: TypeInfo = if (i.generic) .Any else if (i.short) .Any16 else .Any8;

    switch (i.op) {
        .Orot => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a3 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
            a.stack.append(a3 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oovr => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oswp => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
        },
        .Odeo => {
            a.args.append(if (i.short) .U16 else .U8) catch unreachable;
            a.stack.append(.U8) catch unreachable; // TODO: device
        },
        .Odup => {
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Odrop => a.args.append(a1 orelse any) catch unreachable,
        .Oeor, .Omul, .Oadd, .Osub, .Odiv => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oeq, .Oneq, .Olt, .Ogt => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
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

fn analyseBlock(program: *Program, parent: *ASTNode.Decl, block: ASTNodeList, a: *BlockAnalysis) !void {
    var iter = block.iterator();
    while (iter.next()) |node| {
        std.log.info("{s}: node: {}", .{ parent.name, node.node });
        std.log.info("analysis: {}\n", .{a});
        switch (node.node) {
            .None => {},
            .Mac, .Decl => unreachable,
            .Wild => |w| {
                var wa = a.*;
                analyseBlock(program, parent, w.body, &wa) catch {};
                w.arity.resolveTypeRefs(&(parent.arity orelse BlockAnalysis{})).mergeInto(a);
            },
            .Call => |*c| switch (c.ctyp) {
                .Decl => {
                    const d = for (program.defs.items) |decl| {
                        if (mem.eql(u8, decl.node.Decl.name, c.name))
                            break decl;
                    } else unreachable;
                    const cdecl = &d.node.Decl;

                    if (cdecl.arity) |d_arity| {
                        if (!cdecl.is_analysed) {
                            const ungenericified = d_arity.conformGenericTo(a, program);
                            assert(a.isGeneric() or !ungenericified.isGeneric());
                            for (ungenericified.args.constSlice()) |arg|
                                cdecl.analysis.stack.append(arg) catch unreachable;
                            analyseBlock(program, cdecl, cdecl.body, &cdecl.analysis) catch {};
                            cdecl.is_analysed = true;
                        }

                        if (a.isGeneric() or !d_arity.isGeneric()) {
                            d_arity.resolveTypeRefs(&d_arity).mergeInto(a);
                            cdecl.calls += 1;
                        } else if (d_arity.isGeneric()) {
                            const ungenericified = d_arity.conformGenericTo(a, program);
                            assert(!ungenericified.isGeneric());
                            const var_ind: ?usize = for (cdecl.variations.slice(), 0..) |an, i| {
                                if (ungenericified.eqExact(an)) break i;
                            } else null;
                            if (var_ind == null)
                                cdecl.variations.append(ungenericified) catch unreachable;
                            c.ctyp.Decl = (var_ind orelse cdecl.variations.len - 1) + 1;

                            ungenericified.mergeInto(a);

                            if (var_ind == null) {
                                const newdef_ = d.deepclone();
                                const newdef = program.ast.appendAndReturn(newdef_) catch unreachable;
                                program.defs.append(newdef) catch unreachable;

                                newdef.node.Decl.variant = cdecl.variations.len;
                                newdef.node.Decl.arity = ungenericified;

                                var ab = BlockAnalysis{};
                                for (ungenericified.args.constSlice()) |arg|
                                    ab.stack.append(arg) catch unreachable;
                                newdef.node.Decl.arity = ungenericified;
                                newdef.node.Decl.calls += 1;
                                analyseBlock(program, &newdef.node.Decl, newdef.node.Decl.body, &ab) catch {};
                                newdef.node.Decl.is_analysed = true;
                            }
                        } else unreachable;
                    } else {
                        if (!cdecl.is_analysed) {
                            analyseBlock(program, cdecl, cdecl.body, &cdecl.analysis) catch {};
                            cdecl.is_analysed = true;
                        }
                        if (cdecl.analysis.isGeneric()) {
                            std.log.err("{s} is generic and has no declared arity", .{cdecl.name});
                            unreachable;
                        }
                        cdecl.analysis.mergeInto(a);
                        cdecl.calls += 1;
                    }
                },
                .Mac => {
                    const m = for (program.macs.items) |mac| {
                        if (mem.eql(u8, mac.node.Mac.name, c.name))
                            break mac;
                    } else unreachable;
                    if (!m.node.Mac.is_analysed) {
                        analyseBlock(program, parent, m.node.Mac.body, &m.node.Mac.analysis) catch {};
                        m.node.Mac.is_analysed = true;
                    }
                    m.node.Mac.analysis.mergeInto(a);
                },
                .Unchecked => unreachable, // parser.postProcess missed something
            },
            .Loop => |*l| {
                switch (l.loop) {
                    .Until => |*u| {
                        analyseBlock(program, parent, l.body, a) catch {};
                        const t = a.stack.last().?;
                        analyseBlock(program, parent, u.cond, a) catch {};
                        if (t.bits(program)) |b| {
                            u.cond_prep = if (b == 16) .DupShort else .Dup;
                        }
                    },
                }
            },
            .When => |w| {
                var whena = BlockAnalysis{};
                whena.args.append(.Bool) catch unreachable;
                whena.mergeInto(a);

                var ya_r = false;
                var ya = a.*;
                analyseBlock(program, parent, w.yup, &ya) catch {
                    ya_r = true;
                };

                var na_r = false;
                var na = a.*;
                if (w.nah) |n| analyseBlock(program, parent, n, &na) catch {
                    na_r = true;
                };

                if (ya_r and na_r) {
                    a.* = ya;
                } else if (ya_r) {
                    a.* = na;
                } else if (na_r) {
                    a.* = ya;
                }
            },
            .Return => break,
            .Cond => {
                @panic("TODO");
                // Outline:
                // - Check first branch, don't merge analysis
                // - Check every other branch block, assert they're all the same
                //   - Analyse else branch also
                // - Check condition blocks, assert they're all identical
                // - Finally, merge one condition block, and one main block
            },
            .Asm => |*i| {
                // std.log.info("merging asm into main", .{});
                analyseAsm(i, a, program).mergeInto(a);
            },
            .Value => |v| a.stack.append(v.typ) catch unreachable,
            .Quote => a.stack.append(TypeInfo.ptr16(program, .Quote, 1)) catch unreachable,
            .Cast => |*c| {
                const ctyp = switch (c.to) {
                    .builtin => |b| b,
                    .ref => |r| parent.arity.?.args.constSlice()[r],
                };

                const into = a.stack.last() orelse .Any;

                var casta = BlockAnalysis{};
                casta.args.append(into) catch unreachable;
                casta.stack.append(ctyp) catch unreachable;
                casta.mergeInto(a);

                if (!ctyp.isGeneric()) c.to = .{ .builtin = ctyp };
                if (!into.isGeneric()) c.of = into;

                // std.log.info("{s}_{}: casting {} -> {}", .{ parent.name, parent.calls, c.of, c.to });
            },
        }
    }
}

pub fn analyse(program: *Program) void {
    // for (program.defs.items) |decl_node| {
    //     const decl = &decl_node.node.Decl;
    //     if (!decl.is_analysed) {
    //         analyseBlock(program, decl, decl.body, &decl_node.node.Decl.analysis);
    //         decl.is_analysed = true;
    //     }
    // }

    const entrypoint_node = for (program.defs.items) |decl_node| {
        if (mem.eql(u8, decl_node.node.Decl.name, "_Start")) break decl_node;
    } else unreachable;
    const entrypoint = &entrypoint_node.node.Decl;
    entrypoint.calls += 1;

    assert(!entrypoint.is_analysed);
    entrypoint.is_analysed = true;
    analyseBlock(program, entrypoint, entrypoint.body, &entrypoint.analysis) catch {};
}

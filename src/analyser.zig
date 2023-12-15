const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");

const Program = common.Program;
const ASTNode = common.ASTNode;
const ASTNodeList = common.ASTNodeList;
const TypeInfo = common.TypeInfo;
const TypeFmt = common.TypeFmt;
const Value = common.Value;
const VTList16 = TypeInfo.List16;

pub const Error = error{
    NoSuchType,
    GenericNotMatching,
    TypeNotMatching,
    CannotGetFieldMultiPtr,
    CannotGetField,
    CannotGetChild,
    StructNotForStack,
};

pub const BlockAnalysis = struct {
    args: VTList16 = VTList16.init(null),
    stack: VTList16 = VTList16.init(null),
    rargs: VTList16 = VTList16.init(null),
    rstack: VTList16 = VTList16.init(null),

    pub fn resolveTypeRefs(self: @This(), arity: ?@This(), program: *Program) !@This() {
        // std.log.info("resolving {}\n with {}", .{ self, arity });
        var r = self;
        for (r.stack.slice()) |*stack|
            if (stack.isResolvable(program)) {
                stack.* = try stack.resolveTypeRef(arity, program);
                assert(!stack.isResolvable(program));
            };
        // std.log.info("[stack] setting ${}", .{stack.*.TypeRef});
        for (r.args.slice()) |*arg|
            if (arg.isResolvable(program)) {
                arg.* = try arg.resolveTypeRef(arity, program);
                assert(!arg.isResolvable(program));
            };
        // std.log.info("[args] setting ${}", .{arg.*.TypeRef});
        return r;
    }

    pub fn conformGenericTo(generic: @This(), caller: *const @This(), call_node: *ASTNode, p: *Program) Error!@This() {
        // TODO: do rstack
        if (generic.rstack.len > 0 or generic.rargs.len > 0) @panic("TODO");

        // std.log.info("CONFORM {}", .{generic});
        // std.log.info("TO ARGS {}", .{caller});

        var r = generic;

        var i = r.args.len;
        var j: usize = 0;
        while (i > 0) : (j += 1) {
            i -= 1;

            const calleritem = if (j < caller.stack.len)
                caller.stack.constSlice()[caller.stack.len - j - 1]
            else if ((j - caller.stack.len) < caller.args.len)
                caller.args.constSlice()[caller.args.len - (j - caller.stack.len) - 1]
            else {
                std.log.info("{}:{}", .{ call_node.srcloc.line, call_node.srcloc.column });
                @panic("can't call generic func from non-arity func"); // arg.*
            };

            const arg = &r.args.slice()[i];

            // std.log.info("\n", .{});
            // std.log.info("*** {}:{}: doing {} <- {}", .{ call_node.srcloc.line, call_node.srcloc.column, TypeFmt.from(arg.*, p), TypeFmt.from(calleritem, p) });

            while (!arg.eq(calleritem) and (arg.isResolvable(p) or arg.isGeneric(p))) {
                if (arg.isGeneric(p)) {
                    if (!arg.doesInclude(calleritem, p)) {
                        if (!arg.isResolvable(p)) {
                            std.log.err("Generic {} @ {} does not encompass {}", .{ arg, i, calleritem });
                            return p.aerr(error.GenericNotMatching, call_node.srcloc);
                        }
                    } else {
                        arg.* = arg.resolveGeneric(calleritem, p);
                    }
                }
                //std.log.info("- after generic: {}", .{TypeFmt.from(arg.*, p)});

                if (arg.isResolvable(p)) {
                    arg.* = try arg.resolveTypeRef(r, p);
                }
                //std.log.info("- after typeref: {}", .{TypeFmt.from(arg.*, p)});

                //std.log.info("eq?: {}, generic?: {}, ref?: {}", .{ arg.eq(calleritem), arg.isGeneric(p), arg.isResolvable(p) });
            }

            if (!arg.doesInclude(calleritem, p)) {
                std.log.err("Type {} (arg ${}) does not encompass {}", .{ arg, i, calleritem });
                return p.aerr(error.TypeNotMatching, call_node.srcloc);
            }
        }

        for (r.stack.slice()) |*stack|
            if (stack.* == .TypeRef or stack.* == .Expr) {
                stack.* = try stack.resolveTypeRef(r, p);
            };

        // std.log.info("RESULTS {}\n\n", .{r});

        return r;
    }

    pub fn eqExact(a: @This(), b: @This()) bool {
        const S = struct {
            pub fn f(_a: VTList16, _b: VTList16) bool {
                if (_a.len != _b.len) return false;
                return for (_a.constSlice(), 0..) |item, i| {
                    if (!item.eq(_b.constSlice()[i])) break false;
                } else true;
            }
        };
        return S.f(a.args, b.args) and S.f(a.rargs, b.rargs) and
            S.f(a.stack, b.stack) and S.f(a.rstack, b.rstack);
    }

    pub fn isGeneric(self: @This(), program: *Program) bool {
        const S = struct {
            pub fn f(list: VTList16, p: *Program) bool {
                return for (list.constSlice()) |item| {
                    if (item.isGeneric(p)) break true;
                } else false;
            }
        };
        return S.f(self.args, program) or S.f(self.rargs, program) or
            S.f(self.stack, program) or S.f(self.rstack, program);
    }

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        try writer.print("\n    args:   ", .{});
        for (self.args.constSlice()) |i| try writer.print("{}, ", .{i});
        try writer.print("\n    stack:  ", .{});
        for (self.stack.constSlice()) |i| try writer.print("{}, ", .{i});
        try writer.print("\n    rargs:  ", .{});
        for (self.rargs.constSlice()) |i| try writer.print("{}, ", .{i});
        try writer.print("\n    rstack: ", .{});
        for (self.rstack.constSlice()) |i| try writer.print("{}, ", .{i});
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
        .Olda, .Odeo, .Odup, .Odrop => 1,
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

    // std.log.info("ASM: {}, stk.len: {}, args_needed: {}, a1b: {?}, a2b: {?}", .{ i, stk.len, args_needed, a1b, a2b });
    // std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });

    if (i.generic and stk.len >= args_needed and
        ((args_needed == 2 and a1b != null and a2b != null) or
        (args_needed == 3 and a1b != null and a2b != null) or
        (args_needed == 1 and a1b != null)))
    {
        i.short = switch (i.op) {
            .Osta => if (a1.?.deptrize(prog).bits(prog)) |b| b == 16 else false,
            .Olda => if (a1.?.deptrize(prog).bits(prog)) |b| b == 16 else false,
            .Odup, .Odrop, .Odeo => a1b.? == 16,
            .Orot => a1b.? == 16 and a2b.? == 16 and a3b.? == 16,
            else => a1b.? == 16 and a2b.? == 16,
        };
    }

    //std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });

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
            a.args.append(.AnyDev) catch unreachable; // TODO: device
        },
        .Oinc, .Odup => {
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Onip => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Odrop => a.args.append(a1 orelse any) catch unreachable,
        .Osft, .Oand, .Oora, .Oeor, .Omul, .Oadd, .Osub, .Odiv => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oeq, .Oneq, .Olt, .Ogt => {
            a.args.append(a1 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.stack.append(.Bool) catch unreachable;
        },
        .Osth => {
            a.args.append(a1 orelse any) catch unreachable;
            a.rstack.append(a1 orelse any) catch unreachable;
        },
        .Olda => {
            a.args.append(a1 orelse .AnyPtr16) catch unreachable;
            a.stack.append(
                if (a1) |t| prog.builtin_types.items[t.Ptr16.typ] else .Any,
            ) catch unreachable;
        },
        .Osta => {
            a.args.append(a1 orelse .AnyPtr16) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
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

pub const AnalyserInfo = struct {
    early_return: bool = false,
};

fn analyseBlock(program: *Program, parent: *ASTNode.Decl, block: ASTNodeList, a: *BlockAnalysis) Error!AnalyserInfo {
    //std.log.info("analysing {s}", .{parent.name});

    var info = AnalyserInfo{};
    var iter = block.iterator();
    while (iter.next()) |node| {
        // if (mem.eql(u8, parent.name, "_Start")) {
        //     std.log.info("{s}_{}: node: {}", .{ parent.name, parent.variant, node.node });
        //     std.log.info("analysis: {}\n", .{a});
        //     if (a.stack.len > 0) {
        //         const tos = a.stack.last().?;
        //         if (tos == .Ptr16) {
        //             const t = program.builtin_types.items[tos.Ptr16.typ];
        //             std.log.info("*** TOS: (Ptr16) {}\n", .{t});
        //             if (t == .Struct)
        //                 std.log.info("      1: {}\n", .{program.types.items[t.Struct].def.Struct.fields.items[0].type});
        //         } else std.log.info("*** TOS: {}\n", .{tos});
        //     } else std.log.info("*** TOS:\n", .{});
        // }
        switch (node.node) {
            .None => {},
            .TypeDef => {},
            .Debug => std.log.debug("analysis: {}", .{a}),
            .Here => a.stack.append(TypeInfo.ptrize16(.U8, program)) catch unreachable,
            .Mac, .Decl => unreachable,
            .Wild => |w| {
                var wa = a.*;
                _ = try analyseBlock(program, parent, w.body, &wa);
                (try w.arity.resolveTypeRefs(parent.arity, program)).mergeInto(a);
            },
            .Call => |*c| switch (c.ctyp) {
                .Decl => {
                    const d = for (program.defs.items) |decl| {
                        if (!decl.node.Decl.is_test and
                            mem.eql(u8, decl.node.Decl.name, c.name))
                            break decl;
                    } else unreachable;
                    const cdecl = &d.node.Decl;

                    if (cdecl.arity) |d_arity| {
                        if (!cdecl.is_analysed) {
                            const ungenericified = try d_arity.conformGenericTo(a, node, program);
                            assert(a.isGeneric(program) or !ungenericified.isGeneric(program));
                            for (ungenericified.args.constSlice()) |arg|
                                cdecl.analysis.stack.append(arg) catch unreachable;
                            _ = try analyseBlock(program, cdecl, cdecl.body, &cdecl.analysis);
                            cdecl.is_analysed = true;
                        }

                        if (a.isGeneric(program) or !d_arity.isGeneric(program)) {
                            (try d_arity.resolveTypeRefs(d_arity, program)).mergeInto(a);
                            if (!d_arity.isGeneric(program))
                                cdecl.calls += 1;
                        } else if (d_arity.isGeneric(program)) {
                            const ungenericified = try d_arity.conformGenericTo(a, node, program);
                            assert(!ungenericified.isGeneric(program));
                            const var_ind: ?usize = for (cdecl.variations.slice(), 0..) |an, i| {
                                if (ungenericified.eqExact(an)) break i;
                            } else null;
                            if (var_ind == null)
                                cdecl.variations.append(ungenericified) catch unreachable;
                            c.ctyp.Decl = (var_ind orelse cdecl.variations.len - 1) + 1;

                            // if (mem.eql(u8, parent.name, "_Start"))
                            //     std.log.info("{s} returned {?}", .{ cdecl.name, ungenericified.stack.last() });
                            ungenericified.mergeInto(a);

                            if (var_ind == null) {
                                const newdef_ = d.deepclone(null, program);
                                const newdef = program.ast.appendAndReturn(newdef_) catch unreachable;
                                program.defs.append(newdef) catch unreachable;

                                newdef.node.Decl.variant = cdecl.variations.len;
                                newdef.node.Decl.arity = ungenericified;

                                var ab = BlockAnalysis{};
                                for (ungenericified.args.constSlice()) |arg|
                                    ab.stack.append(arg) catch unreachable;
                                newdef.node.Decl.arity = ungenericified;
                                newdef.node.Decl.calls += 1;
                                _ = try analyseBlock(program, &newdef.node.Decl, newdef.node.Decl.body, &ab);
                                newdef.node.Decl.is_analysed = true;
                            }
                        } else unreachable;
                    } else {
                        if (!cdecl.is_analysed) {
                            _ = try analyseBlock(program, cdecl, cdecl.body, &cdecl.analysis);
                            cdecl.is_analysed = true;
                        }
                        if (cdecl.analysis.isGeneric(program)) {
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
                        _ = try analyseBlock(program, parent, m.node.Mac.body, &m.node.Mac.analysis);
                        m.node.Mac.is_analysed = true;
                    }
                    m.node.Mac.analysis.mergeInto(a);
                },
                .Unchecked => unreachable, // program.postProcess missed something
            },
            .Loop => |*l| {
                switch (l.loop) {
                    .Until => |*u| {
                        // std.log.info("{s}_{}: until: a: {}", .{ parent.name, parent.variant, a });
                        _ = try analyseBlock(program, parent, l.body, a);
                        // std.log.info("{s}_{}: until: b: {}", .{ parent.name, parent.variant, a });
                        const t = a.stack.last().?;
                        a.stack.append(t) catch unreachable;
                        _ = try analyseBlock(program, parent, u.cond, a);
                        assert(a.stack.last().? == .Bool);
                        _ = a.stack.pop() catch unreachable;
                        if (t.bits(program)) |b| {
                            u.cond_prep = if (b == 16) .DupShort else .Dup;
                        }
                    },
                    .While => |*u| {
                        const oldlen = a.stack.len;
                        const t = a.stack.last().?;
                        a.stack.append(t) catch unreachable;
                        _ = try analyseBlock(program, parent, u.cond, a);
                        assert(a.stack.last().? == .Bool);
                        assert(a.stack.len == oldlen + 1);
                        _ = a.stack.pop() catch unreachable;
                        if (t.bits(program)) |b| {
                            u.cond_prep = if (b == 16) .DupShort else .Dup;
                        }

                        _ = try analyseBlock(program, parent, l.body, a);
                    },
                }
            },
            .When => |w| {
                var whena = BlockAnalysis{};
                whena.args.append(.Bool) catch unreachable;
                whena.mergeInto(a);

                var ya = a.*;
                const ya_r = (try analyseBlock(program, parent, w.yup, &ya)).early_return;

                var na_r = false;
                var na = a.*;
                if (w.nah) |n|
                    if ((try analyseBlock(program, parent, n, &na)).early_return) {
                        na_r = true;
                    };

                if (ya_r and na_r) {
                    a.* = ya;
                } else if (ya_r) {
                    a.* = na;
                } else if (na_r) {
                    a.* = ya;
                } else {
                    a.* = ya;
                }
            },
            .Return => {
                info.early_return = true;
                break;
            },
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
            .Value => |v| switch (v.typ) {
                .StaticPtr => |ind| a.stack.append(
                    program.statics.items[ind].type.ptrize16(program),
                ) catch unreachable,
                else => a.stack.append(v.typ) catch unreachable,
            },
            .VDecl => |*vd| {
                parent.locals.slice()[vd.lind].rtyp = try vd.utyp.resolveTypeRef(parent.arity, program);
            },
            .VRef => |v| {
                const t = parent.locals.slice()[v.lind.?].rtyp;
                a.stack.append(t.ptrize16(program)) catch unreachable;
            },
            .VDeref => |v| {
                const t = parent.locals.slice()[v.lind.?].rtyp;
                if (t.size(program)) |sz| if (sz > 2)
                    return program.aerr(error.StructNotForStack, node.srcloc);
                a.stack.append(t) catch unreachable;
            },
            .Quote => a.stack.append(TypeInfo.ptr16(program, .Quote, 1)) catch unreachable,
            .GetChild => |*gch| {
                // error message: (replacing nuh uh)
                // Error: Need to know type at this point
                //  Hint: Try explicit cast.
                const b = a.stack.last() orelse @panic("nuh uh");
                const tstruct = switch (b) {
                    // TODO: Ptr8 (it's easy, just change ptrize16 to be more generic)
                    .Ptr16 => |ptr| b: {
                        switch (program.ztype(ptr.typ)) {
                            .Struct => |s| {
                                if (ptr.ind > 1) {
                                    return program.aerr(error.CannotGetFieldMultiPtr, node.srcloc);
                                }
                                break :b &program.types.items[s].def.Struct;
                            },
                            else => |typ| {
                                std.log.info("cannot get child from {}", .{
                                    TypeFmt.from(typ, program),
                                });
                                return program.aerr(error.CannotGetField, node.srcloc);
                            },
                        }
                    },
                    .Struct => |s| b: {
                        const tstruct = &program.types.items[s].def.Struct;
                        if (b.size(program)) |sz| if (sz > 2)
                            @panic("/dev/sda is on fire"); // How did this happen
                        //std.log.info("need protection (ptr)", .{});
                        //return program.aerr(error.CannotGetField, node.srcloc);
                        break :b tstruct;
                    },
                    else => {
                        std.log.info("field from {}? wat", .{TypeFmt.from(b, program)});
                        return program.aerr(error.CannotGetField, node.srcloc);
                    },
                };
                const i = for (tstruct.fields.items, 0..) |f, i| {
                    if (mem.eql(u8, f.name, gch.name)) break i;
                } else @panic("no such field");
                _ = a.stack.pop() catch unreachable;
                if (b == .Struct) {
                    if (b.isGeneric(program)) {
                        gch.kind = .unresolved;
                    } else {
                        // TODO: zero-bit fields
                        if (tstruct.fields.items.len == 2) {
                            assert(tstruct.fields.items[0].type.bits(program).? == 8);
                            assert(tstruct.fields.items[1].type.bits(program).? == 8);
                            gch.kind = .{ .stk_two_b = .{ .ind = @intCast(i) } };
                        } else if (tstruct.fields.items.len == 1) {
                            const bits = tstruct.fields.items[i].type.bits(program).?;
                            gch.kind = if (bits == 16) .stk_one_s else .stk_one_b;
                        } else unreachable;
                    }
                    a.stack.append(tstruct.fields.items[i].type) catch unreachable;
                } else {
                    const generic = program.ztype(b.Ptr16.typ).isGeneric(program);
                    gch.kind = if (generic) .unresolved else .{ .mem = .{
                        .offset = tstruct.fields.items[i].offset,
                        .is_short = b == .Ptr16,
                    } };
                    a.stack.append(tstruct.fields.items[i].type.ptrize16(program)) catch unreachable;
                }
            },
            .Breakpoint => |brk| {
                switch (brk.type) {
                    .TosShouldEq => |v| {
                        const tos = a.stack.pop() catch {
                            @panic("Must have known stack contents before breakpoint");
                        };
                        if (!tos.eq(v.typ)) {
                            std.log.err("Type error: {} (breakpoint) != {} (stack)", .{
                                v.typ, tos,
                            });
                            return program.aerr(error.TypeNotMatching, node.srcloc);
                        }
                    },
                }
            },
            .Builtin => |*builtin| switch (builtin.type) {
                .Make => |*make| {
                    make.resolved = try make.original.resolveTypeRef(parent.arity, program);
                    var b = BlockAnalysis{};
                    if (make.resolved.size(program)) |sz| {
                        if (sz > 2) {
                            return program.aerr(error.StructNotForStack, node.srcloc);
                        }
                    }
                    if (make.resolved != .Struct) {
                        @panic("Type must have resolved to struct at this point");
                    }
                    const s = program.types.items[make.resolved.Struct];
                    for (s.def.Struct.fields.items) |field|
                        b.args.append(field.type) catch unreachable;
                    b.stack.append(make.resolved) catch unreachable;
                    try b.mergeInto(a, program, node.srcloc);
                },
                .SizeOf => |*sizeof| {
                    a.stack.append(.U8) catch unreachable;
                    sizeof.resolved = try sizeof.original.resolveTypeRef(parent.arity, program);
                },
            },
            .Cast => |*c| {
                if (c.ref) |r| {
                    c.to = parent.arity.?.args.constSlice()[r];
                }
                c.to = try c.to.resolveTypeRef(parent.arity, program);

                const into = a.stack.last() orelse .Any;

                var casta = BlockAnalysis{};
                casta.args.append(into) catch unreachable;
                casta.stack.append(c.to) catch unreachable;
                casta.mergeInto(a);

                if (!into.isGeneric(program)) c.of = into;

                // std.log.info("{s}_{}: casting {} -> {}", .{ parent.name, parent.variant, c.of, c.to });
            },
        }
    }

    return info;
}

pub fn postProcess(self: *Program) Error!void {
    const _S = struct {
        pub fn walkNodes(program: *Program, parent: ?*ASTNode, nodes: ASTNodeList) Error!void {
            var iter = nodes.iterator();
            while (iter.next()) |node|
                try walkNode(program, parent, node);
        }

        pub fn walkNode(program: *Program, parent: ?*ASTNode, node: *ASTNode) Error!void {
            switch (node.node) {
                .Decl => unreachable,
                .Mac => |b| try walkNodes(program, parent, b.body),
                .Wild => |b| try walkNodes(program, parent, b.body),
                .Quote => |b| try walkNodes(program, parent, b.body),
                .Loop => |d| {
                    switch (d.loop) {
                        .Until => |u| try walkNodes(program, parent, u.cond),
                        .While => |u| try walkNodes(program, parent, u.cond),
                    }
                    try walkNodes(program, parent, d.body);
                },
                .When => |when| {
                    try walkNodes(program, parent, when.yup);
                    if (when.nah) |n| try walkNodes(program, parent, n);
                },
                .Cond => |cond| {
                    for (cond.branches.items) |branch| {
                        try walkNodes(program, parent, branch.cond);
                        try walkNodes(program, parent, branch.body);
                    }
                    if (cond.else_branch) |branch|
                        try walkNodes(program, parent, branch);
                },
                .VDecl => |vd| {
                    // FIXME: we should do all this in one shot, after before
                    // walking over decls

                    // std.log.info("{}: added static of type {} from parent {s}", .{
                    //     program.statics.items.len,
                    //     parent.?.node.Decl.locals.slice()[vd.lind].rtyp,
                    //     parent.?.node.Decl.name,
                    // });

                    program.statics.append(.{
                        .type = parent.?.node.Decl.locals.slice()[vd.lind].rtyp,
                        .count = vd.llen,
                        .default = .None,
                    }) catch unreachable;
                    parent.?.node.Decl.locals.slice()[vd.lind].ind = program.statics.items.len - 1;
                },
                .VRef => |*v| {
                    v.sind = for (parent.?.node.Decl.locals.constSlice()) |local| {
                        if (mem.eql(u8, v.name, local.name))
                            break local.ind;
                    } else unreachable;
                    // std.log.info("{s}: @{s}: linked to {?}", .{
                    //     parent.?.node.Decl.name, v.name, v.sind,
                    // });
                },
                .VDeref => |*v| {
                    v.sind = for (parent.?.node.Decl.locals.constSlice()) |local| {
                        if (mem.eql(u8, v.name, local.name))
                            break local.ind;
                    } else unreachable;
                    // std.log.info("{s}: ${s}: linked to {?}", .{
                    //     parent.?.node.Decl.name, v.name, v.sind,
                    // });
                },
                else => {},
            }
        }
    };
    for (self.defs.items) |def|
        if (def.node.Decl.calls > 0)
            try _S.walkNodes(self, def, def.node.Decl.body);
}

pub fn analyse(program: *Program, tests: bool) Error!void {
    // for (program.defs.items) |decl_node| {
    //     const decl = &decl_node.node.Decl;
    //     if (!decl.is_analysed) {
    //         analyseBlock(program, decl, decl.body, &decl_node.node.Decl.analysis);
    //         decl.is_analysed = true;
    //     }
    // }

    if (tests) {
        for (program.defs.items) |d| if (d.node.Decl.is_test) {
            const utest = &d.node.Decl;
            utest.calls += 1;

            assert(!utest.is_analysed);
            utest.is_analysed = true;
            _ = try analyseBlock(program, utest, utest.body, &utest.analysis);
        };
    } else {
        const entrypoint_node = for (program.defs.items) |decl_node| {
            if (mem.eql(u8, decl_node.node.Decl.name, "_Start")) break decl_node;
        } else unreachable;
        const entrypoint = &entrypoint_node.node.Decl;
        entrypoint.calls += 1;

        assert(!entrypoint.is_analysed);
        entrypoint.is_analysed = true;
        _ = try analyseBlock(program, entrypoint, entrypoint.body, &entrypoint.analysis);
    }

    try postProcess(program);
}

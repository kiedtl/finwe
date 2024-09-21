const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");
const codegen = @import("codegen.zig");

const Program = common.Program;
const ASTNode = common.ASTNode;
const Scope = common.Scope;
const Srcloc = common.Srcloc;
const ErrorSet = common.Error.Set;
const ASTNodeList = common.ASTNodeList;
const TypeInfo = common.TypeInfo;
const TypeFmt = common.TypeFmt;
const Value = common.Value;
const VTList16 = TypeInfo.List16;

const StackBuffer = @import("buffer.zig").StackBuffer;

pub const Error = error{
    UnknownLocal,
    NoSuchType,
    NoSuchField,
    GenericNotMatching,
    TypeNotMatching,
    CannotGetFieldMultiPtr,
    CannotGetField,
    CannotGetChild,
    CannotGetIndex,
    CannotCallMethod,
    InvalidIndexType,
    IndexTooLarge,
    IndexWouldOverflow,
    StructNotForStack,
    CannotSplitIntoShort,
    CannotSplitByte,
    StackUnderflow,
    StackMismatch,
    StackNotEmpty,
    StackBranching1,
    StackBranching2,
    StackImbalanceLoop,
    ExpectedStruct,
    NakedBreak,
    NakedContinue,
    NoreturnCannotReturn,
    UnsizedArityItem,
};

pub const AnalysisFmt = struct {
    an: *const BlockAnalysis,
    prog: *const Program,

    pub fn from(an: *const BlockAnalysis, p: *const Program) @This() {
        return .{ .an = an, .prog = p };
    }

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        var short = false;

        if (comptime mem.eql(u8, f, "s")) {
            short = true;
        } else if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        if (!short) {
            try writer.print("\n    args:   ", .{});
            for (self.an.args.constSlice()) |i|
                try writer.print("{}, ", .{TypeFmt.from(i, self.prog)});
            try writer.print("\n    stack:  ", .{});
            for (self.an.stack.constSlice()) |i|
                try writer.print("{}, ", .{TypeFmt.from(i, self.prog)});
            try writer.print("\n    rargs:  ", .{});
            for (self.an.rargs.constSlice()) |i|
                try writer.print("{}, ", .{TypeFmt.from(i, self.prog)});
            try writer.print("\n    rstack: ", .{});
            for (self.an.rstack.constSlice()) |i|
                try writer.print("{}, ", .{TypeFmt.from(i, self.prog)});
        } else {
            try writer.print("(", .{});
            for (self.an.args.constSlice()) |i|
                try writer.print("{} ", .{TypeFmt.from(i, self.prog)});
            try writer.print("--", .{});
            for (self.an.stack.constSlice()) |i|
                try writer.print(" {}", .{TypeFmt.from(i, self.prog)});

            if (self.an.rargs.len > 0 or self.an.rstack.len > 0) {
                try writer.print(" | ", .{});
                for (self.an.rargs.constSlice()) |i|
                    try writer.print("{} ", .{TypeFmt.from(i, self.prog)});
                try writer.print("--", .{});
                for (self.an.rstack.constSlice()) |i|
                    try writer.print(" {}", .{TypeFmt.from(i, self.prog)});
            }

            try writer.print(")", .{});
        }
    }
};

pub const BlockAnalysis = struct {
    args: VTList16 = VTList16.init(null),
    stack: VTList16 = VTList16.init(null),
    rargs: VTList16 = VTList16.init(null),
    rstack: VTList16 = VTList16.init(null),

    pub fn getTypeRefs(self: @This(), buf: anytype, program: *Program) void {
        for (self.args.constSlice()) |typ| typ.getTypeRefs(buf, program);
        for (self.rargs.constSlice()) |typ| typ.getTypeRefs(buf, program);
        for (self.stack.constSlice()) |typ| typ.getTypeRefs(buf, program);
        for (self.rstack.constSlice()) |typ| typ.getTypeRefs(buf, program);
    }

    pub fn resolveTypeRef(self: *const @This(), scope: ?*Scope, arity: ?BlockAnalysis, program: *Program) Error!@This() {
        var new = self.*;
        for (new.args.slice()) |*item| item.* = try item.resolveTypeRef(scope, arity, program);
        for (new.rargs.slice()) |*item| item.* = try item.resolveTypeRef(scope, arity, program);
        for (new.stack.slice()) |*item| item.* = try item.resolveTypeRef(scope, arity, program);
        for (new.rstack.slice()) |*item| item.* = try item.resolveTypeRef(scope, arity, program);
        return new;
    }

    pub fn reverse(self: *@This()) void {
        const tmp = self.args;
        self.args = self.rargs;
        self.rargs = tmp;

        const stmp = self.stack;
        self.stack = self.rstack;
        self.rstack = stmp;

        const S = struct {
            pub fn revTypeRefs(lst: *VTList16) void {
                for (lst.slice()) |*v| if (v.* == .TypeRef) {
                    v.*.TypeRef.r = !v.*.TypeRef.r;
                };
            }
        };

        S.revTypeRefs(&self.args);
        S.revTypeRefs(&self.rargs);
        S.revTypeRefs(&self.stack);
        S.revTypeRefs(&self.rstack);
    }

    pub fn resolveTypeRefs(self: @This(), scope: ?*Scope, arity: ?@This(), program: *Program) !@This() {
        var r = self;
        for (r.stack.slice()) |*stack|
            if (stack.isResolvable(program)) {
                stack.* = try stack.resolveTypeRef(scope, arity, program);
                assert(!stack.isResolvable(program));
            };
        for (r.args.slice()) |*arg|
            if (arg.isResolvable(program)) {
                arg.* = try arg.resolveTypeRef(scope, arity, program);
                assert(!arg.isResolvable(program));
            };
        for (r.rstack.slice()) |*stack|
            if (stack.isResolvable(program)) {
                stack.* = try stack.resolveTypeRef(scope, arity, program);
                assert(!stack.isResolvable(program));
            };
        for (r.rargs.slice()) |*arg|
            if (arg.isResolvable(program)) {
                arg.* = try arg.resolveTypeRef(scope, arity, program);
                assert(!arg.isResolvable(program));
            };
        return r;
    }

    fn _resolveFully(paramspec: TypeInfo, argtyp: TypeInfo, scope: ?*Scope, arity: BlockAnalysis, caller: ?*ASTNode, p: *Program, dry_run: bool) Error!TypeInfo {
        var r = paramspec;
        while (!r.eq(argtyp) and (r.isResolvable(p) or r.isGeneric(p))) {
            if (r.isGeneric(p)) {
                if (!r.doesInclude(argtyp, p)) {
                    if (!r.isResolvable(p)) {
                        if (dry_run) {
                            return error.GenericNotMatching;
                        } else {
                            return p.aerr(error.GenericNotMatching, caller.?.srcloc, .{ r, argtyp });
                        }
                    }
                } else {
                    r = r.resolveGeneric(argtyp, p);
                }
            }
            // std.log.info("- after generic: {}", .{TypeFmt.from(r, p)});

            if (r.isResolvable(p)) {
                r = try r.resolveTypeRef(scope, arity, p);
            }
            // std.log.info("- after typeref: {}", .{TypeFmt.from(r, p)});

            // std.log.info("eq?: {}, generic?: {}, ref?: {}", .{ r.eq(argtyp), r.isGeneric(p), r.isResolvable(p) });
        }
        return r;
    }

    fn _getArgForInd(arglist: *const BlockAnalysis, paramlen: usize, i: usize, caller: ?*ASTNode, p: *Program) Error!TypeInfo {
        const j = paramlen - (i + 1);
        return if (j < arglist.stack.len)
            arglist.stack.constSlice()[arglist.stack.len - j - 1]
        else if ((j - arglist.stack.len) < arglist.args.len)
            arglist.args.constSlice()[arglist.args.len - (j - arglist.stack.len) - 1]
        else {
            return p.aerr(error.StackUnderflow, caller.?.srcloc, .{i + 1});
        };
    }

    pub fn _resolveFullyRecurse(r: *@This(), caller: *const @This(), scope: ?*Scope, call_node: ?*ASTNode, p: *Program, i: usize, dry_run: bool) Error!void {
        // std.log.info("\n", .{});
        // std.log.info("- beginning  {}", .{i});

        var refs = StackBuffer(TypeInfo.TypeRef, 12).init(null);
        r.args.slice()[i].getTypeRefs(&refs, p);
        for (refs.slice()) |*ref| {
            ref.*.n = r.args.len - ref.*.n - 1;
            // std.log.info("- refs {}", .{ref.*});
        }

        // ignore return-stack refs for now, conform() will do another run for that
        for (refs.constSlice()) |ref| if (ref.n < i and !ref.r)
            try _resolveFullyRecurse(r, caller, scope, call_node, p, ref.n, dry_run);

        const calleritem = try _getArgForInd(caller, r.args.len, i, call_node, p);
        const resolved = try _resolveFully(r.args.slice()[i], calleritem, scope, r.*, call_node, p, dry_run);

        if (!resolved.doesInclude(calleritem, p)) {
            if (dry_run) {
                return error.TypeNotMatching;
            } else {
                return p.aerr(error.TypeNotMatching, call_node.?.srcloc, .{ calleritem, resolved });
            }
        }

        r.args.slice()[i] = resolved;
    }

    pub fn conformGenericTo(
        generic: @This(),
        extra_type_args: []const TypeInfo,
        scope: ?*Scope,
        p_caller: *const @This(),
        call_node: ?*ASTNode,
        p: *Program,
        dry_run: bool,
        reversed: bool,
    ) Error!@This() {
        // const s: []const u8 = if (reversed) " - " else "";
        // std.log.info("{s}CONFORM {s}", .{ s, AnalysisFmt.from(&generic, p) });
        // std.log.info("{s}TO ARGS {s}", .{ s, AnalysisFmt.from(p_caller, p) });

        //std.log.info("\n", .{});
        var caller = p_caller.*;
        var r = generic;

        for (extra_type_args, 0..) |_, i|
            caller.stack.append(extra_type_args[extra_type_args.len - i - 1]) catch unreachable;

        // if (extra_type_args.len > 0)
        //     std.log.info("ARGS v2 {s}", .{AnalysisFmt.from(&caller, p)});

        // std.log.info("Calling {s}: {s}", .{
        //     call_node.?.node.Call.name, AnalysisFmt.from(&caller, p),
        // });

        var i = r.args.len;
        while (i > 0) {
            i -= 1;
            try _resolveFullyRecurse(&r, &caller, scope, call_node, p, i, dry_run);
        }

        if (!reversed and (r.rstack.len > 0 or r.rargs.len > 0)) {
            var tmpcaller = p_caller.*;
            tmpcaller.reverse();
            var tmp = r;
            tmp.reverse();
            // std.log.info("R PAR {s}", .{AnalysisFmt.from(&tmp, p)});
            tmp = try conformGenericTo(tmp, extra_type_args, scope, &tmpcaller, call_node, p, dry_run, true);
            // std.log.info("R ARG {s}", .{AnalysisFmt.from(&tmpcaller, p)});
            // std.log.info("R RES {s}", .{AnalysisFmt.from(&tmp, p)});
            tmp.reverse();
            r.rstack = tmp.rstack;
            r.rargs = tmp.rargs;
        }

        for (r.stack.slice()) |*stack|
            if (stack.isResolvable(p)) {
                stack.* = try stack.resolveTypeRef(scope, r, p);
            };

        // std.log.info("{s}RESULTS {s}", .{ s, AnalysisFmt.from(&r, p) });
        // if (!reversed) std.log.info("\n", .{});

        return r;
    }

    fn _chkLst(_a: VTList16, _b: VTList16) bool {
        if (_a.len != _b.len) return false;
        return for (_a.constSlice(), 0..) |item, i| {
            if (!item.eq(_b.constSlice()[i])) break false;
        } else true;
    }

    pub fn eqExactStacks(a: *const @This(), b: *const @This()) bool {
        return _chkLst(a.stack, b.stack) and _chkLst(a.rstack, b.rstack);
    }

    pub fn eqExact(a: *const @This(), b: *const @This()) bool {
        return _chkLst(a.args, b.args) and _chkLst(a.rargs, b.rargs) and eqExactStacks(a, b);
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

    pub fn mergeInto(self: @This(), b: *@This(), p: *Program, srcloc: common.Srcloc) !void {
        //std.log.info("MERGE {}", .{self});
        //std.log.info("INTO ({},{}) {}", .{ srcloc.line, srcloc.column, b });

        if (b.stack.len < self.args.len)
            b.args.insertSlice(
                0,
                self.args.constSlice()[0 .. self.args.len - b.stack.len],
            ) catch unreachable;
        if (b.stack.len >= self.args.len)
            for (b.stack.slice()[b.stack.len - self.args.len ..], 0..) |stkitem, i| {
                const arg = self.args.constSlice()[i];
                if (!arg.doesInclude(stkitem, p)) {
                    return p.aerr(error.TypeNotMatching, srcloc, .{ stkitem, arg });
                }
            };
        b.stack.resizeTo(b.stack.len -| self.args.len);
        b.stack.appendSlice(self.stack.constSlice()) catch unreachable;

        if (b.rstack.len < self.rargs.len)
            b.rargs.insertSlice(
                0,
                self.rargs.constSlice()[0 .. self.rargs.len - b.rstack.len],
            ) catch unreachable;
        if (b.rstack.len >= self.rargs.len)
            for (b.rstack.slice()[b.rstack.len - self.rargs.len ..], 0..) |stkitem, i| {
                const arg = self.rargs.constSlice()[i];
                if (!arg.doesInclude(stkitem, p)) {
                    return p.aerr(error.TypeNotMatching, srcloc, .{ stkitem, arg });
                }
            };
        b.rstack.resizeTo(b.rstack.len -| self.rargs.len);
        b.rstack.appendSlice(self.rstack.constSlice()) catch unreachable;

        // std.log.info("RESULT {}\n\n--------------\n", .{b});
    }
};

fn analyseAsm(i: *common.Ins, _: Srcloc, caller_an: *const BlockAnalysis, ctx: Ctx, prog: *Program) BlockAnalysis {
    var a = BlockAnalysis{};

    const args_needed: usize = switch (i.op) {
        .Obrk => 0,
        .Ojsr, .Osth, .Oinc, .Olda, .Odei, .Odup, .Opop => 1,
        .Orot => 3,
        else => 2,
    };

    if (ctx.r_blk) {
        i.stack = common.RT_STACK;
    }

    const stk = if (i.stack == common.WK_STACK) &caller_an.stack else &caller_an.rstack;
    const a1 = if (stk.len >= 1) stk.constSlice()[stk.len - 1] else null;
    const a2 = if (stk.len >= 2) stk.constSlice()[stk.len - 2] else null;
    const a3 = if (stk.len >= 3) stk.constSlice()[stk.len - 3] else null;
    const a1b: ?u5 = if (stk.len >= 1) a1.?.bits(prog) else null;
    const a2b: ?u5 = if (stk.len >= 2) a2.?.bits(prog) else null;
    const a3b: ?u5 = if (stk.len >= 3) a3.?.bits(prog) else null;

    // if (i.op == .Osth and i.generic) {
    //     std.log.info("ASM: {}, stk.len: {}, args_needed: {}, a1b: {?}, a2b: {?}", .{ i, stk.len, args_needed, a1b, a2b });
    //     std.log.info("ARG: {s}", .{AnalysisFmt.from(caller_an, prog)});
    //     std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });
    // }

    if (i.generic and stk.len >= args_needed and
        ((args_needed == 2 and a1b != null and a2b != null) or
        (args_needed == 3 and a1b != null and a2b != null) or
        (args_needed == 1 and a1b != null)))
    {
        i.short = switch (i.op) {
            .Osta => if (a1.?.deptrize(prog).bits(prog)) |b| b == 16 else false,
            .Olda => if (a1.?.deptrize(prog).bits(prog)) |b| b == 16 else false,
            .Odei, .Odeo => a1.? == .Dev16,
            .Osth, .Oinc, .Odup, .Opop => a1b.? == 16,
            .Orot => a1b.? == 16 and a2b.? == 16 and a3b.? == 16,
            .Osft => a2b.? == 16,
            else => a1b.? == 16 and a2b.? == 16,
        };
    }

    // if (i.op == .Osth and i.generic)
    // std.log.info("FLG: s={}, g={}", .{ i.short, i.generic });

    const any: TypeInfo = if (i.generic) .Any else if (i.short) .Any16 else .Any8;
    const any_or_unsigned: TypeInfo = if (i.generic) .Any else if (i.short) .U16 else .U8;

    switch (i.op) {
        .Orot => {
            a.args.append(a3 orelse any) catch unreachable;
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a3 orelse any) catch unreachable;
        },
        .Oovr => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
        },
        .Oswp => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a2 orelse any) catch unreachable;
        },
        .Odeo => {
            a.args.append(if (i.short) .Any16 else .Any8) catch unreachable;
            a.args.append(if (i.short) .Dev16 else .Dev8) catch unreachable;
        },
        .Odei => {
            a.args.append(if (i.short) .Dev16 else .Dev8) catch unreachable;
            a.stack.append(if (i.short) .U16 else .U8) catch unreachable;
        },
        .Odup => {
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oinc => {
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Onip => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Opop => a.args.append(a1 orelse any) catch unreachable,
        .Osft => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(.U8) catch unreachable;
            a.stack.append(if (i.short) .U16 else .U8) catch unreachable;
        },
        .Oand, .Oora, .Oeor, .Omul, .Oadd, .Osub, .Odiv => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(a1 orelse any) catch unreachable;
        },
        .Oequ, .Oneq, .Olth, .Ogth => {
            a.args.append(a2 orelse any) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
            a.stack.append(.Bool) catch unreachable;
        },
        .Osth => {
            a.args.append(a1 orelse any_or_unsigned) catch unreachable;
            a.rstack.append(a1 orelse any_or_unsigned) catch unreachable;
        },
        .Olda => {
            a.args.append(a1 orelse .AnyPtr16) catch unreachable;
            a.stack.append(if (a1) |t| t.deptrize(prog) else .Any) catch unreachable;
        },
        .Osta => {
            a.args.append(a2 orelse .AnyPtr16) catch unreachable;
            a.args.append(a1 orelse any) catch unreachable;
        },
        .Ojsr => {
            if (!i.short) @panic("TODO: analyser: Ojsr non-short mode");
            const fnarity = prog.ztype(a1.?.Ptr16.typ).Fn.arity;
            assert(fnarity.rargs.len == 0);
            assert(fnarity.rstack.len == 0);
            a.args.appendSlice(fnarity.args.constSlice()) catch unreachable;
            a.stack.appendSlice(fnarity.stack.constSlice()) catch unreachable;
            a.args.append(a1.?) catch unreachable;

            // Actually, the function returns, so doesn't make sense to do this
            //a.rstack.append(.Ptr16) catch unreachable;
        },
        .Obrk => {},
        .Oraw, .Olit => @panic("TODO: analyser: LIT/<raw>"),
        else => {
            std.log.info("{} not implmented", .{i});
            @panic("todo");
        },
    }

    if (i.keep) a.args.clear();
    if (i.keep) a.rargs.clear();

    if (i.stack == common.RT_STACK) {
        a.reverse();
    }

    return a;
}

const AnalyserInfo = struct {
    continues: bool = false,
    early_return: bool = false,
    no_return: bool = false,

    pub fn early_or_no_return(self: @This()) bool {
        return self.early_return or self.no_return;
    }
};

const Ctx = struct {
    r_blk: bool = false,
    w_blk: bool = false,

    conditional: bool = false,
    loop: ?struct { node: *ASTNode, expected_arity: *const BlockAnalysis } = null,
};

fn analyseBlock(program: *Program, parent: *ASTNode.Decl, block: ASTNodeList, a: *BlockAnalysis, ctx: Ctx) ErrorSet!AnalyserInfo {
    //std.log.info("analysing {s}", .{parent.name});

    var info = AnalyserInfo{};
    var iter = block.iterator();
    while (iter.next()) |node| {
        // std.log.debug("{s},{},{}: Current: {s}", .{
        //     node.srcloc.file,             node.srcloc.line, node.srcloc.column,
        //     AnalysisFmt.from(a, program),
        // });
        assert(!a.isGeneric(program));
        // if (mem.eql(u8, parent.name, "main")) {
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
            .Import => {},
            .TypeDef => {},
            .Debug => std.log.debug("[debug] Current analysis: {s}", .{
                AnalysisFmt.from(a, program),
            }),
            .Decl => {}, // Only analyse if/when called
            .RBlock => |r| {
                var nctx = ctx;
                nctx.r_blk = !nctx.r_blk;
                _ = try analyseBlock(program, parent, r.body, a, nctx);
            },
            .Wild => |w| {
                var nctx = ctx;
                nctx.w_blk = true;
                var wa = a.*;
                _ = try analyseBlock(program, parent, w.body, &wa, nctx);
                var arity = w.arity;
                if (ctx.r_blk) arity.reverse();
                try (try arity.resolveTypeRefs(parent.scope, parent.arity, program))
                    .mergeInto(a, program, node.srcloc);
            },
            .Call => |*c| {
                if (c.node == null) {
                    const scope = if (!c.is_method) parent.scope else switch (a.stack.last().?) {
                        .EnumLit => |n| program.types.items[n].scope,
                        .Struct => |n| program.types.items[n].scope,
                        .Ptr16, .Ptr8 => |p| switch (program.ztype(p.typ)) {
                            .Struct => |n| program.types.items[n].scope,
                            .EnumLit => |n| program.types.items[n].scope,
                            else => return program.aerr(error.CannotCallMethod, node.srcloc, .{program.ztype(p.typ)}),
                        },
                        else => |t| return program.aerr(error.CannotCallMethod, node.srcloc, .{t}),
                    };

                    if (try scope.findDeclForCaller(program, c.name, node, parent.arity, parent.scope, a, ctx.r_blk)) |found| {
                        assert(found.node == .Decl);
                        c.variant = 0;
                        c.node = found;
                    } else {
                        return program.perr(error.UnknownIdent, node.srcloc, .{c.name});
                    }
                }

                const d = c.node.?;
                const cdecl = &d.node.Decl;

                c.node = d;
                c.is_inline_override = cdecl.is_inline == .Always or ctx.r_blk;

                if (cdecl.arity) |d_arity| {
                    var type_args = @TypeOf(c.args).init(null);
                    for (c.args.constSlice()) |arg|
                        type_args.append(try arg.resolveTypeRef(parent.scope, parent.arity, program)) catch unreachable;
                    var ungenericified = d_arity;
                    if (ctx.r_blk) ungenericified.reverse();
                    ungenericified = try ungenericified.conformGenericTo(type_args.constSlice(), cdecl.scope.parent, a, node, program, false, false);
                    const var_ind: ?usize = for (cdecl.variations.items, 0..) |an, i| {
                        if (ungenericified.eqExact(&an.node.Decl.arity.?))
                            break i;
                    } else null;

                    c.variant = (var_ind orelse cdecl.variations.items.len) + 1;

                    if (var_ind) |ind| {
                        c.node = cdecl.variations.items[ind];
                    } else {
                        const newdef = d.deepcloneDecl(&program.ast, false, program);
                        newdef.node.Decl.arity = ungenericified;
                        c.node = newdef;

                        try checkFuncAritySanity(newdef, program);

                        var ab = BlockAnalysis{};
                        const sr = if (ctx.r_blk) c.args.len else 0;
                        const sn = if (ctx.r_blk) 0 else c.args.len;

                        const end1 = ungenericified.rargs.len - sr;
                        for (ungenericified.rargs.constSlice()[0..end1]) |arg|
                            ab.rstack.append(arg) catch unreachable;
                        const end2 = ungenericified.args.len - sn;
                        for (ungenericified.args.constSlice()[0..end2]) |arg|
                            ab.stack.append(arg) catch unreachable;

                        const r = try analyseBlock(program, &newdef.node.Decl, newdef.node.Decl.body, &ab, .{ .r_blk = ctx.r_blk });
                        newdef.node.Decl.is_analysed = true;
                        newdef.node.Decl.is_noreturn = r.no_return;

                        try checkEndState(&ab, &ungenericified, .FuncEnd, newdef.srcloc, false, program);
                    }

                    c.node.?.node.Decl.calls += 1;

                    // Need to remove special arg types from arity before merging
                    for (c.args.constSlice()) |_| {
                        _ = ungenericified.args.pop() catch unreachable;

                        // Would need to resolve to assert this, can't be bothered
                        //assert(type_arg.eq(caller_type_arg));
                    }
                    try ungenericified.mergeInto(a, program, node.srcloc);
                } else {
                    assert(!ctx.r_blk);

                    // if (!cdecl.is_analysed) {
                    //     _ = try analyseBlock(program, cdecl, cdecl.body, &cdecl.analysis, .{});
                    //     cdecl.is_analysed = true;
                    // }

                    const r = try analyseBlock(program, cdecl, cdecl.body, a, .{});
                    cdecl.is_analysed = true;
                    cdecl.is_noreturn = r.no_return;

                    if (a.isGeneric(program)) {
                        std.log.err("{s} is generic and has no declared arity", .{cdecl.name});
                        unreachable;
                    }

                    //try cdecl.analysis.mergeInto(a, program, node.srcloc);
                    cdecl.calls += 1;
                }

                if (cdecl.is_noreturn) {
                    info.no_return = true;
                    c.goto = true;
                    break;
                }
            },
            .Loop => |*l| {
                var nctx = ctx;

                switch (l.loop) {
                    .While, .Until => |*u| if (u.cond_arity) |*arity| {
                        arity.* = try arity.resolveTypeRefs(parent.scope, parent.arity, program);
                        u.cond_prep = try ASTNode.CondPrep.fromArity(arity, program);
                    },
                }

                switch (l.loop) {
                    .Until => |*u| {
                        const olda = a.*;
                        nctx.loop = .{ .node = node, .expected_arity = &olda };
                        _ = try analyseBlock(program, parent, l.body, a, nctx);

                        if (!ctx.w_blk) {
                            try checkEndState(a, &olda, .LoopBody, node.srcloc, false, program);
                        }

                        const t = a.stack.last();
                        try u.cond_prep.execute(a);

                        _ = try analyseBlock(program, parent, u.cond, a, ctx);
                        assert(a.stack.last().? == .Bool);
                        _ = a.stack.pop() catch unreachable;

                        if (u.cond_arity == null)
                            if (t.?.bits(program)) |b| {
                                assert(u.cond_prep == .unchecked);
                                u.cond_prep = if (b == 16) .dup_1_s else .dup_1_b;
                            };
                    },
                    .While => |*u| {
                        const oldlen = a.stack.len;
                        const t = a.stack.last().?;
                        try u.cond_prep.execute(a);

                        _ = try analyseBlock(program, parent, u.cond, a, ctx);
                        assert(a.stack.last().? == .Bool);
                        assert(a.stack.len == oldlen + 1);

                        _ = a.stack.pop() catch unreachable;

                        if (u.cond_arity == null)
                            if (t.bits(program)) |b| {
                                assert(u.cond_prep == .unchecked);
                                u.cond_prep = if (b == 16) .dup_1_s else .dup_1_b;
                            };

                        const olda = a.*;
                        nctx.loop = .{ .node = node, .expected_arity = &olda };
                        _ = try analyseBlock(program, parent, l.body, a, nctx);

                        if (!ctx.w_blk) {
                            try checkEndState(a, &olda, .LoopBody, node.srcloc, false, program);
                        }
                    },
                }
            },
            .Break => |*b| {
                b.loop.mut().* = if (ctx.loop) |loop_info|
                    loop_info.node
                else
                    return program.aerr(error.NakedBreak, node.srcloc, .{});
                break;
            },
            .Continue => |*b| {
                b.loop.mut().* = if (ctx.loop) |loop_info|
                    loop_info.node
                else
                    return program.aerr(error.NakedBreak, node.srcloc, .{});
                info.continues = true;
                break;
            },
            .When => |w| {
                var nctx = ctx;
                nctx.conditional = true;

                var whena = BlockAnalysis{};
                whena.args.append(.Bool) catch unreachable;
                try whena.mergeInto(a, program, node.srcloc);

                var ya = a.*;
                const ya_r = (try analyseBlock(program, parent, w.yup, &ya, nctx)).early_or_no_return();

                var na_r = false;
                var na = a.*;
                if (w.nah) |n|
                    if ((try analyseBlock(program, parent, n, &na, nctx)).early_or_no_return()) {
                        na_r = true;
                    };

                if (w.nah != null and !na_r and !ya_r) {
                    // (when [ foo ] [ bar ])
                    try checkEndState(&na, &ya, .WhenElseBranch, node.srcloc, false, program);
                } else if (w.nah == null and !ya_r) {
                    // (when [ foo ])
                    try checkEndState(&ya, a, .WhenBranch, node.srcloc, false, program);
                } else if (w.nah == null and ya_r) {
                    // (when [ return ])
                    // no checks, single branch returns
                } else if (w.nah != null and ya_r and na_r) {
                    // (when [ return ] [ return ])
                    // no checks, both branches returns
                } else if (w.nah != null and ya_r and !na_r) {
                    try checkEndState(&na, a, .WhenBranch, node.srcloc, false, program);
                }

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
                if (parent.arity) |*ar|
                    try checkEndState(a, ar, .FuncEnd, node.srcloc, false, program);
                info.early_return = true;
                break;
            },
            .Cond => |*cond| {
                assert(!ctx.r_blk);
                //const tos = a.stack.last();

                for (cond.branches.items) |*branch| {
                    if (branch.cond_arity) |*arity| {
                        arity.* = try arity.resolveTypeRefs(parent.scope, parent.arity, program);
                        branch.cond_prep = try ASTNode.CondPrep.fromArity(arity, program);
                    }
                    if (branch.cond_arity == null)
                        if (a.stack.last().?.bits(program)) |b| {
                            assert(branch.cond_prep == .unchecked);
                            branch.cond_prep = if (b == 16) .dup_1_s else .dup_1_b;
                        };
                }

                const olda = a.*;
                var firsta: ?BlockAnalysis = null;
                var chosen: ?BlockAnalysis = null;

                for (cond.branches.items) |*branch| {
                    a.* = olda;
                    try branch.cond_prep.execute(a);
                    _ = try analyseBlock(program, parent, branch.cond, a, ctx);

                    // TODO: check cond conforms to arity
                    // (current code doesn't work)
                    //const arity = branch.cond_arity orelse
                    //    BlockAnalysis{
                    //    .args = VTList16.init(&[_]TypeInfo{tos.?}),
                    //    .stack = VTList16.init(&[_]TypeInfo{ tos.?, .Bool }),
                    //};
                    //try checkEndState(a, &arity, .CondCond, branch.cond_srcloc,
                    //                  false, program);

                    _ = a.stack.pop() catch unreachable;
                    const r = try analyseBlock(program, parent, branch.body, a, ctx);

                    if (r.continues) {
                        try checkEndState(a, ctx.loop.?.expected_arity, .LoopBody, ctx.loop.?.node.srcloc, false, program);
                    } else if (r.early_return) {
                        // TODO: don't we need to check against expected function return?
                    } else if (firsta) |firsta_| {
                        try checkEndState(a, &firsta_, .CondBody, branch.body_srcloc, false, program);
                        chosen = a.*;
                    } else {
                        chosen = a.*;
                        firsta = a.*;
                    }
                }

                if (cond.else_branch) |else_br| {
                    a.* = olda;
                    const r = try analyseBlock(program, parent, else_br, a, ctx);
                    if (r.continues) {
                        try checkEndState(a, ctx.loop.?.expected_arity, .LoopBody, ctx.loop.?.node.srcloc, false, program);
                    } else if (r.early_return) {
                        // TODO: don't we need to check against expected function return?
                    } else {
                        try checkEndState(a, &firsta.?, .CondBody, cond.else_srcloc.?, false, program);
                        chosen = a.*;
                    }
                }

                a.* = chosen orelse olda;

                // TODO: do branch arity checking
            },
            .Asm => |*i| {
                try analyseAsm(i, node.srcloc, a, ctx, program)
                    .mergeInto(a, program, node.srcloc);
            },
            .Value => |*v| {
                const stk = if (ctx.r_blk) &a.rstack else &a.stack;
                v.ret = ctx.r_blk;
                switch (v.val.typ) {
                    // .StaticPtr => |ind| a.stack.append(
                    //     program.statics.items[ind].type.ptrize16(program),
                    // ) catch unreachable,
                    .StaticPtr => |ind| {
                        stk.append((TypeInfo{ .Array = .{
                            .typ = program.btype(program.statics.items[ind].type),
                            .count = @intCast(program.statics.items[ind].count),
                        } }).ptrize16(program)) catch unreachable;
                        program.statics.items[ind].used = true;
                    },
                    else => stk.append(v.val.typ) catch unreachable,
                }
            },
            .VDecl => |*vd| {
                parent.scope.locals.append(.{
                    .name = vd.name,
                    .rtyp = try vd.utyp.resolveTypeRef(parent.scope, parent.arity, program),
                    .default = vd.default,
                    .declnode = node,
                }) catch unreachable;
                vd.localptr = parent.scope.locals.last().?;
                vd.localptr.?.inferLength(program);
            },
            .VRef => |*v| {
                v.localptr = parent.scope.findLocal(v.name, program, false) orelse
                    return program.aerr(error.UnknownLocal, node.srcloc, .{v.name});
                a.stack.append(v.localptr.?.rtyp.ptrize16(program)) catch unreachable;
            },
            .VDeref => |*v| {
                v.localptr = parent.scope.findLocal(v.name, program, false) orelse
                    return program.aerr(error.UnknownLocal, node.srcloc, .{v.name});
                const t = v.localptr.?.rtyp;
                if (t.size(program)) |sz| if (sz > 2)
                    return program.aerr(error.StructNotForStack, node.srcloc, .{t});
                a.stack.append(t) catch unreachable;
            },
            .Quote => |q| {
                const qdef = &q.def.node.Decl;
                if (!qdef.is_analysed) {
                    const resarity = try qdef.arity.?.resolveTypeRef(parent.scope, parent.arity, program);
                    for (0..resarity.args.len) |i|
                        qdef.analysis.stack.append(resarity.args.constSlice()[resarity.args.len - i - 1]) catch unreachable;
                    _ = try analyseBlock(program, qdef, qdef.body, &qdef.analysis, .{});
                    qdef.is_analysed = true;
                }

                a.stack.append(TypeInfo.ptr16(program, .{ .Fn = .{ .arity = &qdef.arity.? } }, 1)) catch unreachable;
                qdef.calls += 1;
            },
            // TODO: ptr8 (will require special handling in codegen -- can't
            // multiply u8 (ptr) w/ u16 (index) right?)
            //
            .GetIndex => |*gind| {
                assert(!ctx.r_blk);

                var target: TypeInfo = undefined;
                var indtype: ?TypeInfo = undefined;
                var known_target_len: ?u16 = null;
                var deref_target = false;

                if (gind.ind == .known) {
                    indtype = null;
                    target = a.stack.pop() catch
                        return program.aerr(error.StackUnderflow, node.srcloc, .{});
                    gind.order = .moot;
                } else if (a.stack.last().? != .U8 and a.stack.last().? != .U16) {
                    target = a.stack.pop() catch
                        return program.aerr(error.StackUnderflow, node.srcloc, .{});
                    indtype = a.stack.pop() catch
                        return program.aerr(error.StackUnderflow, node.srcloc, .{@as(usize, 2)});
                    gind.order = .ti;
                } else {
                    indtype = a.stack.pop() catch
                        return program.aerr(error.StackUnderflow, node.srcloc, .{});
                    target = a.stack.pop() catch
                        return program.aerr(error.StackUnderflow, node.srcloc, .{@as(usize, 2)});
                    gind.order = .it;
                }

                const childtype = switch (target) {
                    .Array => @panic("How on earth"), // Array on the stack??
                    .Ptr16 => |ptr| switch (program.ztype(ptr.typ)) {
                        .Array => |arr| b: {
                            const arrchild = program.ztype(arr.typ);
                            deref_target = arrchild == .Array;
                            known_target_len = arr.count;
                            break :b arrchild;
                        },
                        else => |t| t,
                    },
                    .Ptr8 => @panic("TODO"),
                    else => |t| return program.aerr(error.CannotGetIndex, node.srcloc, .{t}),
                };
                if (childtype.size(program)) |sz| {
                    if (sz == 0xFFFF) @panic("seriously");
                    gind.multiplier = sz;
                }

                if (gind.ind != .known) {
                    gind.ind = switch (indtype.?) {
                        .U16 => .stk_s,
                        .U8 => .stk_b,
                        else => if (indtype.?.isGeneric(program)) .stk_unresolved else {
                            return program.aerr(error.InvalidIndexType, node.srcloc, .{indtype.?});
                        },
                    };
                }

                if (gind.ind == .known and gind.multiplier != 0xFFFF) {
                    // Detect multiplication overflow
                    // https://stackoverflow.com/a/6472982
                    const x = (gind.ind.known >> 8) * (gind.multiplier & 0xFF);
                    const y = (gind.ind.known & 0xFF) * (gind.multiplier >> 8);
                    const bit = ((gind.ind.known >> 8) * (gind.multiplier >> 8)) +
                        (x >> 8) + (y >> 8);
                    if (bit != 0)
                        return program.aerr(error.IndexWouldOverflow, node.srcloc, .{ gind.ind.known, gind.multiplier });
                }

                if (gind.ind == .known and known_target_len != null) {
                    if (gind.ind.known >= known_target_len.?)
                        return program.aerr(error.IndexTooLarge, node.srcloc, .{ gind.ind.known, known_target_len.? });
                }

                const result = if (deref_target) childtype.ptrize16(program) else target;
                a.stack.append(result) catch unreachable;
            },
            .GetChild => |*gch| {
                assert(!ctx.r_blk);

                // error message: (replacing nuh uh)
                // Error: Need to know type at this point
                //  Hint: Try explicit cast.
                const b = a.stack.last() orelse
                    return program.aerr(error.StackUnderflow, node.srcloc, .{});
                const t = switch (b) {
                    // TODO: Ptr8 (it's easy, just change ptrize16 to be more generic)
                    .Ptr16 => |ptr| b: {
                        switch (program.ztype(ptr.typ)) {
                            .Struct => |s| {
                                if (ptr.ind > 1) {
                                    return program.aerr(error.CannotGetFieldMultiPtr, node.srcloc, .{program.ztype(ptr.typ)});
                                }
                                break :b s;
                            },
                            else => |typ| return program.aerr(error.CannotGetField, node.srcloc, .{typ}),
                        }
                    },
                    .Struct => |s| b: {
                        if (b.size(program)) |sz| if (sz > 2)
                            @panic("/dev/sda is on fire"); // How did this happen
                        break :b s;
                    },
                    else => return program.aerr(error.CannotGetField, node.srcloc, .{b}),
                };
                const tstruct = program.types.items[t].def.Struct;
                const i = for (tstruct.fields.items, 0..) |f, i| {
                    if (mem.eql(u8, f.name, gch.name)) break i;
                } else return program.aerr(error.NoSuchField, node.srcloc, .{ gch.name, TypeInfo{ .Struct = t } });
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
                    const generic = program.ztype(b.Ptr16.typ).isGeneric(program) or
                        tstruct.fields.items[i].offset == 0xFFFF;
                    gch.kind = if (generic) .unresolved else .{ .mem = .{
                        .offset = tstruct.fields.items[i].offset,
                        .is_short = b == .Ptr16,
                    } };
                    a.stack.append(tstruct.fields.items[i].type.ptrize16(program)) catch unreachable;
                }
            },
            .Breakpoint => |*brk| {
                brk.must_execute = ctx.loop == null and !ctx.conditional;
                brk.parent_test = parent;
                program.breakpoints.append(node) catch unreachable;

                switch (brk.type) {
                    .RCheckpoint => unreachable, // added by analyser only
                    .StdoutShouldEq => {},
                    .TosShouldNeqSos, .TosShouldEqSos => {
                        const tos = a.stack.pop() catch
                            return program.aerr(error.StackUnderflow, node.srcloc, .{});
                        const sos = a.stack.pop() catch
                            return program.aerr(error.StackUnderflow, node.srcloc, .{@as(usize, 2)});
                        if (!tos.doesInclude(sos, program) or
                            tos.bits(program) != sos.bits(program))
                        {
                            return program.aerr(error.TypeNotMatching, node.srcloc, .{ tos, sos });
                        }
                        switch (brk.type) {
                            .TosShouldEqSos => |*v| v.* = tos,
                            .TosShouldNeqSos => |*v| v.* = tos,
                            else => unreachable,
                        }
                    },
                    .TosShouldNeq, .TosShouldEq => |v| {
                        const tos = a.stack.pop() catch
                            return program.aerr(error.StackUnderflow, node.srcloc, .{});
                        if (!tos.doesInclude(v.typ, program) or
                            tos.bits(program) != v.typ.bits(program))
                        {
                            return program.aerr(error.TypeNotMatching, node.srcloc, .{ tos, v.typ });
                        }
                    },
                }
            },
            .Builtin => |*builtin| switch (builtin.type) {
                .SplitCast => |*c| {
                    const srcstk = if (ctx.r_blk) &a.rstack else &a.stack;

                    c.resolved1 = try c.original1.resolveTypeRef(parent.scope, parent.arity, program);
                    c.resolved2 = try c.original2.resolveTypeRef(parent.scope, parent.arity, program);

                    if (c.resolved1.bits(program)) |b| if (b != 8)
                        return program.aerr(error.CannotSplitIntoShort, node.srcloc, .{});

                    if (c.resolved2.bits(program)) |b| if (b != 8)
                        return program.aerr(error.CannotSplitIntoShort, node.srcloc, .{});

                    const into = srcstk.last() orelse
                        return program.aerr(error.StackUnderflow, node.srcloc, .{});

                    if (into.bits(program)) |b| if (b != 16)
                        return program.aerr(error.CannotSplitByte, node.srcloc, .{});

                    var casta = BlockAnalysis{};
                    const dststk = if (ctx.r_blk) &casta.rstack else &casta.stack;
                    const dstarg = if (ctx.r_blk) &casta.rargs else &casta.args;

                    dstarg.append(into) catch unreachable;
                    dststk.append(c.resolved1) catch unreachable;
                    dststk.append(c.resolved2) catch unreachable;
                    try casta.mergeInto(a, program, node.srcloc);

                    if (!into.isGeneric(program)) c.of = into;
                },
                .Make => |*make| {
                    assert(!ctx.r_blk);

                    make.resolved = try make.original.resolveTypeRef(parent.scope, parent.arity, program);
                    var b = BlockAnalysis{};
                    if (make.resolved != .Struct) {
                        return program.aerr(error.ExpectedStruct, node.srcloc, .{make.resolved});
                    }
                    if (make.resolved.size(program)) |sz| {
                        if (sz > 2) {
                            return program.aerr(error.StructNotForStack, node.srcloc, .{make.resolved});
                        }
                    }
                    const s = program.types.items[make.resolved.Struct];
                    for (s.def.Struct.fields.items) |field|
                        b.args.append(field.type) catch unreachable;
                    b.stack.append(make.resolved) catch unreachable;
                    try b.mergeInto(a, program, node.srcloc);
                },
                .SizeOf => |*sizeof| {
                    assert(!ctx.r_blk);

                    sizeof.resolved = try sizeof.original.resolveTypeRef(parent.scope, parent.arity, program);
                    const s = sizeof.resolved.size(program);
                    if (s == null or s.? <= 255) {
                        a.stack.append(.U8) catch unreachable;
                    } else if (s.? > 255) {
                        a.stack.append(.U16) catch unreachable;
                    } else unreachable;
                },
                .Here => a.stack.append(TypeInfo.ptrize16(.U8, program)) catch unreachable,
                .StaticsHere => a.stack.append(TypeInfo.ptrize16(.U8, program)) catch unreachable,
            },
            .Cast => |*c| {
                for (c.resolved.slice(), 0..) |*resolved, i|
                    resolved.* = try c.original.constSlice()[i]
                        .resolveTypeRef(parent.scope, parent.arity, program);

                var casta = BlockAnalysis{};
                const stk = if (ctx.r_blk) &a.rstack else &a.stack;
                const dststk = if (ctx.r_blk) &casta.rstack else &casta.stack;
                const dstarg = if (ctx.r_blk) &casta.rargs else &casta.args;
                c.ret = ctx.r_blk;

                for (c.resolved.constSlice(), 0..) |resolved, i| {
                    const find = stk.len - (c.resolved.len - i - 1) - 1;
                    const from = if (i >= stk.len) .Any else stk.slice()[find];
                    dstarg.append(from) catch unreachable;
                    dststk.append(resolved) catch unreachable;
                    if (!from.isGeneric(program))
                        c.from.slice()[i] = from;
                }

                try casta.mergeInto(a, program, node.srcloc);
            },
        }
    }

    return info;
}

// Sanity checks for function arities
fn checkFuncAritySanity(node: *ASTNode, p: *Program) !void {
    const decl = node.node.Decl;
    if (decl.is_noreturn and
        (decl.arity.?.stack.len > 0 or decl.arity.?.rstack.len > 0))
    {
        return p.aerr(error.NoreturnCannotReturn, node.srcloc, .{});
    }

    const lists = [_][]const TypeInfo{
        decl.arity.?.args.constSlice(),
        decl.arity.?.rargs.constSlice(),
        decl.arity.?.stack.constSlice(),
        decl.arity.?.rstack.constSlice(),
    };

    for (lists) |l| {
        for (l) |item| {
            if (item.size(p) == null) {
                return p.aerr(error.UnsizedArityItem, node.srcloc, .{item});
            }
        }
    }
}

// Ensure stack state matches what's expected at the end
fn checkEndState(
    a: *const BlockAnalysis,
    expect: *const BlockAnalysis,
    chktype: enum {
        FuncEnd,
        EntryEnd,
        WhenElseBranch,
        WhenBranch,
        LoopBody,
        CondCond,
        CondBody,
    },
    srcloc: Srcloc,
    dry_run: bool,
    program: *Program,
) !void {
    if (a.args.len != 0 or a.rargs.len != 0 or !expect.eqExactStacks(a)) {
        if (dry_run) {
            return switch (chktype) {
                .CondCond, .FuncEnd => error.StackMismatch,
                .EntryEnd => error.StackNotEmpty,
                .WhenElseBranch => error.StackBranching1,
                .CondBody, .WhenBranch => error.StackBranching2,
                .LoopBody => error.StackImbalanceLoop,
            };
        } else {
            return switch (chktype) {
                .CondCond, .FuncEnd => program.aerr(error.StackMismatch, srcloc, .{ a.*, expect.* }),
                .EntryEnd => program.aerr(error.StackNotEmpty, srcloc, .{}),
                .WhenElseBranch => program.aerr(error.StackBranching1, srcloc, .{ a.*, expect.* }),
                .CondBody, .WhenBranch => program.aerr(error.StackBranching2, srcloc, .{ a.*, expect.* }),
                .LoopBody => program.aerr(error.StackImbalanceLoop, srcloc, .{ a.*, expect.* }),
            };
        }
    }
}

pub fn postProcess(self: *Program) ErrorSet!void {
    const _S = struct {
        pub fn walkNodes(program: *Program, parent: ?*ASTNode, nodes: ASTNodeList) Error!void {
            var iter = nodes.iterator();
            while (iter.next()) |node|
                try walkNode(program, parent, node);
        }

        pub fn walkNode(program: *Program, parent: ?*ASTNode, node: *ASTNode) Error!void {
            switch (node.node) {
                .Decl => {},
                .Wild => |b| try walkNodes(program, parent, b.body),
                .Quote => |b| try walkNode(program, parent, b.def),
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
                else => {},
            }
        }

        pub fn registerLocals(scope: *Scope, program: *Program) !void {
            var iter = scope.locals.iterator();
            while (iter.next()) |local| if (local.ind == null) {
                program.statics.append(.{
                    .type = local.rtyp,
                    .count = 1,
                    .default = local.default,
                    .srcloc = local.declnode.srcloc,
                    .used = true,
                }) catch unreachable;
                local.ind = program.statics.items.len - 1;
            };
        }
    };

    for (self.defs.items) |def|
        if (def.node.Decl.calls > 0) {
            try _S.registerLocals(def.node.Decl.scope, self);
            try _S.walkNodes(self, def, def.node.Decl.body);
        };
    var imports = self.imports.valueIterator();
    while (imports.next()) |import|
        try _S.registerLocals(import.scope, self);
    try _S.registerLocals(self.global_scope, self);

    for (self.defs.items) |def|
        if (def.node.Decl.is_test and !def.node.Decl.is_noreturn) {
            def.node.Decl.body.append(.{
                .node = .{ .Breakpoint = .{
                    .type = .RCheckpoint,
                    .parent_test = &def.node.Decl,
                    .must_execute = true,
                } },
                .srcloc = def.srcloc,
            }) catch unreachable;
            self.breakpoints.append(def.node.Decl.body.last().?) catch unreachable;
        };

    // Determine whether to inline stuff
    var buf = common.Ins.List.init(common.gpa.allocator());

    for (self.defs.items) |def| {
        const d = &def.node.Decl;
        if (d.calls == 0) continue;
        assert(d.is_analysed);
        codegen.genNodeList(self, &buf, d.body, .{ .parent_decl = def }) catch unreachable;
        d.bytecode_size = buf.items.len;
        if (d.is_inline == .Auto)
            d.is_inline = switch (d.bytecode_size) {
                0...6 => .AutoYes,
                else => .AutoNo,
            };
        buf.shrinkRetainingCapacity(0);
    }
}

pub fn analyse(program: *Program, tests: bool) ErrorSet!void {
    // for (program.defs.items) |decl_node| {
    //     const decl = &decl_node.node.Decl;
    //     if (!decl.is_analysed) {
    //         analyseBlock(program, decl, decl.body, &decl_node.node.Decl.analysis);
    //         decl.is_analysed = true;
    //     }
    // }

    if (tests) {
        // Crash when using normal for iterating loop. Debugging revealed
        // no obvious cause -- memory was accessible, pointers were okay.
        // Segfault occurred in the for statement itself, weirdly enough.
        //
        // Not sure, but probably a miscompilation? idk
        //
        // 2024-09-18 (1:25 AM): Crash still occurs with Zig v0.13.0
        //
        for (0..program.defs.items.len) |i| {
            const d = program.defs.items[i];
            if (d.node.Decl.is_test) {
                const utest = &d.node.Decl;
                utest.calls += 1;

                assert(!utest.is_analysed);
                utest.is_analysed = true;
                const r = try analyseBlock(program, utest, utest.body, &utest.analysis, .{});
                utest.is_noreturn = r.no_return;
                try checkEndState(&utest.analysis, &BlockAnalysis{}, .EntryEnd, d.srcloc, false, program);
            }
        }
    } else {
        const entrypoint_def = program.global_scope.findDeclAny(program, "main").?;
        const entrypoint = &entrypoint_def.node.Decl;
        entrypoint.calls += 1;

        assert(!entrypoint.is_analysed);
        entrypoint.is_analysed = true;
        _ = try analyseBlock(program, entrypoint, entrypoint.body, &entrypoint.analysis, .{});
        try checkEndState(&entrypoint.analysis, &BlockAnalysis{}, .EntryEnd, entrypoint_def.srcloc, false, program);
    }

    try postProcess(program);
}

const testing = std.testing;

const core =
    \\
    \\ (word drop  (Any --) [ (asm "g" .Op/Opop) ])
    \\ (word =  ((AnySz $0) Any -- Bool) [ (asm "g" .Op/Oequ) ])
    \\ (word 0= (Any -- Bool) [ 0 (as $0) = ])
    \\
;

fn _testExpectErr(p: []const u8, e: Error) !void {
    const talloc = testing.allocator;

    var program = common.Program.init(talloc, "/") catch unreachable;
    defer program.deinit();

    var lexer = @import("lexer.zig").Lexer.init(&program, p, "<stdin>", talloc);
    const lexed = try lexer.lexList(.Root);
    defer @import("lexer.zig").Node.deinitMain(lexed, talloc);
    defer lexer.deinit();

    var parser = @import("parser.zig").Parser.init(&program, true, talloc);
    parser.parse(&lexed) catch unreachable;

    try testing.expectError(e, analyse(&program, false));
    try testing.expectEqual(program.errors.items.len, 1);
}

test "error on stack imbalance when returning" {
    try _testExpectErr("(word a (-- U8) [ 0 1 ]) (word main [ a ])", error.StackMismatch);
}

test "error on non-empty stack when returning from main/tests" {
    try _testExpectErr("(word main [ 0 ])", error.StackNotEmpty);
    try _testExpectErr("(word main [ 0s ])", error.StackNotEmpty);
}

test "error on when clause having different stack effects" {
    try _testExpectErr("(word main [ t (when [ 0 ] []) ])", error.StackBranching1);
    try _testExpectErr("(word main [ t (when [ 0 ] [ 0s ]) ])", error.StackBranching1);
    try _testExpectErr("(word main [ t (when [ 0s ] [ 0 ]) ])", error.StackBranching1);
    try _testExpectErr("(word main [ t (when [ ] [ 0 ]) ])", error.StackBranching1);
}

test "error on loop body stack effects" {
    inline for (&[_][]const u8{
        "(word main [ 0 (until [ 0= ] [ drop ]) ])",
        "(word main [ 0 (while [ 0= ] [ drop ]) ])",
        "(word main [ 0 (until [ 0= ] [ 0 ]) ])",
        "(word main [ 0 (while [ 0= ] [ 0 ]) ])",
    }) |s| {
        try _testExpectErr(core ++ s, error.StackImbalanceLoop);
    }
}

// test "error on mismatched stack effects in cond cond block" {
//     inline for (&[_][]const u8{
//         "(word main [ 0 (cond [ ( -- Bool) 0= ] []) ])",
//         "(word main [ 0 (cond [ ( U8 -- Bool) ] []) ])",
//         "(word main [ 0 (cond [ ( U8 U8 -- Bool) = = ] []) ])",
//     }) |s| {
//         try _testExpectErr(core ++ s, error.StackImbalanceLoop);
//     }
// }

test "error on cond bodies having differing stack effects" {
    inline for (&[_][]const u8{
        "(word main [ 0 (cond [ 0= ] [ 1 ] [ 1s ]) ])",
        "(word main [ 0 (cond [ 0= ] [] [ 1 = ] [ 0 ]) ])",
        "(word main [ 0 (cond [ 0= ] [] [ 1 = ] [] [ 2 = ] [ 0s ]) ])",
        "(word main [ 0 (cond [ 0= ] [ 9s ] [ 1 = ] [ 0 ]) ])",
    }) |s| {
        try _testExpectErr(core ++ s, error.StackBranching2);
    }
}

test "error on noreturn trying to return values" {
    inline for (&[_][]const u8{
        "(word main (--) [ foo ]) #noreturn (word foo (-- Bool) [ t ])",
    }) |s| {
        try _testExpectErr(core ++ s, error.NoreturnCannotReturn);
    }
}

test "error on arity items with no defined size" {
    inline for (&[_][]const u8{
        "(word main (--) [ f ]) (word f (-- Opaque) [ ])",
        "(word main (--) [ f ]) (word f (-- Any)    [ ])",
    }) |s| {
        try _testExpectErr(core ++ s, error.UnsizedArityItem);
    }
}

test "error on early-return when that doesn't satify arity requirements" {
    inline for (&[_][]const u8{
        "(word main (--) [ 0 t (when [ return ] [ drop ]) ])",
        "(word main (--) [ 0 t (when [ drop ] [ return ]) ])",
        "(word main (--) [ 0 t (when [ return ] [ return ]) ])",
    }) |s| {
        try _testExpectErr(core ++ s, error.StackMismatch);
    }
}

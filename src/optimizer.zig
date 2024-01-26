const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const assert = std.debug.assert;

//const ASTNode = @import("common.zig").ASTNode;
//const ASTNodeList = @import("common.zig").ASTNodeList;
//const Program = @import("common.zig").Program;
const Program = @import("common.zig").Program;
const Ins = @import("common.zig").Ins;
const Op = @import("common.zig").Op;
const OpTag = @import("common.zig").OpTag;
const StackBuffer = @import("buffer.zig").StackBuffer;

const WK_STACK = @import("common.zig").WK_STACK;
const RT_STACK = @import("common.zig").RT_STACK;

const gpa = &@import("common.zig").gpa;

pub const SETS = [_]PatRepSet{
    // zig fmt: off

    // #00 #00 => #0000
    s(.{ O(.Olit), R(), O(.Olit), R(),                 rOs(.Olit), rR(1), rR(3) }),

    // #0001 SWP => #0100
    s(.{ Os(.Olit), R(), R(), O(.Oswp),                rOs(.Olit), rR(2), rR(1) }),

    // SWP* SWP*  => <removed>
    // #00 POP    => <removed>
    // #0000 POP2 => <removed>
    s(.{ O(.Oswp), O(.Oswp),                                                    }),
    s(.{ Os(.Oswp), Os(.Oswp),                                                  }),
    s(.{ O(.Olit), R(), O(.Opop),                                               }),
    s(.{ Os(.Olit), R(), R(), Os(.Opop),                                        }),

    // #1234 POP     => #12
    // #1234 NIP     => #34
    // #1234 #56 NIP => #1256
    s(.{ Os(.Olit), R(), R(), O(.Opop),                        rO(.Olit), rR(1) }),
    s(.{ Os(.Olit), R(), R(), O(.Onip),                        rO(.Olit), rR(2) }),
    s(.{ Os(.Olit), R(), R(), O(.Olit), R(), O(.Onip), rOs(.Olit), rR(2), rR(4) }),

    // constant folding
    s(.{ Os(.Olit), R(), R(), O(.Oadd),           rO(.Olit), rMathRR(1, 2, '+') }),
    s(.{ Os(.Olit), R(), R(), O(.Osub),           rO(.Olit), rMathRR(1, 2, '-') }),
    s(.{ Os(.Olit), R(), R(), O(.Omul),           rO(.Olit), rMathRR(1, 2, '*') }),
    s(.{ Os(.Olit), R(), R(), O(.Odiv),           rO(.Olit), rMathRR(1, 2, '/') }),

    // TODO
    // - mul -> shift
    // - div -> shift
    // - ovr ovr -> dup2
    // - neq 00 eq -> eq
    // - eq 00 eq -> eq
    // - #0001 add -> inc
    // - #0002 add -> inc inc
    // - #ff neq neq -> inc (??)
    // - #0000 neq2 j*i -> ora
    // - #0000 gth2 j*i -> ora
    // - redundance
    //

    // zig fmt: on
};

pub const PatRepSet = struct {
    p: StackBuffer(Pattern, 8),
    r: StackBuffer(Replace, 8),
};

pub fn s(args: anytype) PatRepSet {
    comptime var new = PatRepSet{
        .p = StackBuffer(Pattern, 8).init(null),
        .r = StackBuffer(Replace, 8).init(null),
    };
    comptime var began_repl = false;
    const listinfo = @typeInfo(@TypeOf(args));
    comptime {
        inline for (listinfo.Struct.fields) |fieldinfo| {
            const field = @field(args, fieldinfo.name);
            if (fieldinfo.type == Pattern) {
                if (began_repl)
                    @compileError("Mixing patterns and replacements");
                new.p.append(field) catch unreachable;
            } else if (fieldinfo.type == Replace) {
                began_repl = true;
                new.r.append(field) catch unreachable;
            } else {
                @compileError("Invalid argument type " ++ @typeName(fieldinfo.type));
            }
        }
    }
    return new;
}

pub const Pattern = struct {
    pat: Type,
    short: bool = false,
    stk: usize = WK_STACK,
    keep: bool = false,

    pub const Type = union(enum) {
        O: OpTag, // Specific opcode
        R, // Any raw byte
        //B: u8, // Specific raw byte
    };

    pub fn matches(self: @This(), ins: Ins) bool {
        return switch (self.pat) {
            .R => ins.op == .Oraw,
            .O => |o| o == ins.op and
                ins.short == self.short and ins.stack == self.stk and ins.keep == self.keep,
        };
    }
};

// zig fmt: off
fn O(o: OpTag) Pattern { return .{ .pat = .{ .O = o }, .short = false }; }
fn Os(o: OpTag) Pattern { return .{ .pat = .{ .O = o }, .short = true }; }
fn R() Pattern { return .{ .pat = .R, .short = false }; }
// zig fmt: on

pub const Replace = struct {
    rpl: Type,

    pub const Type = union(enum) {
        I: struct {
            op: OpTag,
            k: bool = false,
            s: bool = false,
            r: bool = false,
        }, // Non-Oraw instruction
        R: usize, // Reference
        Math: struct {
            a: RefOrConst,
            b: RefOrConst,
            op: u21,
        }, // Constant math
    };

    pub const RefOrConst = union(enum) {
        ref: usize,
        val: u8,

        pub fn get(self: @This(), ins: []const Ins) u8 {
            return switch (self) {
                .ref => |i| ins[i].op.Oraw,
                .val => |v| v,
            };
        }
    };

    pub fn exec(self: @This(), ins: []const Ins) Ins {
        return switch (self.rpl) {
            .I => |i| Ins{
                .op = Op.fromTag(i.op) catch unreachable,
                .short = i.s,
                .keep = i.k,
                .stack = if (i.r) RT_STACK else WK_STACK,
            },
            .R => |r| ins[r],
            .Math => |m| Ins{ .op = .{ .Oraw = switch (m.op) {
                '+' => m.a.get(ins) +% m.b.get(ins),
                '-' => m.a.get(ins) -% m.b.get(ins),
                '*' => m.a.get(ins) *% m.b.get(ins),
                '/' => m.a.get(ins) / m.b.get(ins),
                else => unreachable,
            } }, .stack = WK_STACK },
        };
    }
};

// zig fmt: off
fn rO(o: OpTag) Replace { return .{.rpl = .{.I = .{ .op = o }}}; }
fn rOs(o: OpTag) Replace { return .{.rpl = .{.I = .{ .op = o, .s = true }}}; }
fn rMathRR(r1: usize, r2: usize, op: u21) Replace { return .{.rpl = .{.Math = .{ .a = .{ .ref = r1 }, .b = .{ .ref = r2 }, .op = op }}}; }
fn rR(r: usize) Replace { return .{ .rpl = .{.R = r} }; }
// zig fmt: on

pub fn optimize(p: *Program, p_ins: *Ins.List, only_safe: bool) !void {
    var ins = p_ins.items;
    var new = Ins.List.init(gpa.allocator());
    var did_something = false;

    while (ins.len > 0) {
        const maybe_matching_set = for (&SETS) |*set| {
            if (set.p.len > ins.len)
                continue;
            if (only_safe and set.p.len != set.r.len)
                continue;
            const p_match = for (set.p.constSlice(), 0..) |pat, i| {
                if (!pat.matches(ins[i])) break false;
            } else true;
            if (p_match) break set;
        } else null;

        if (maybe_matching_set) |matching| {
            did_something = true;
            for (matching.r.constSlice()) |rpl| {
                new.append(rpl.exec(ins[0..matching.p.len])) catch unreachable;
            }
            ins = ins[matching.p.len..];
        } else {
            new.append(ins[0]) catch unreachable;
            ins = ins[1..];
        }
    }

    p_ins.deinit();
    p_ins.* = new;

    if (did_something)
        try optimize(p, p_ins, only_safe);
}

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

    // - #00 #00 => #0000
    s(.{ O(.Olit), R(), O(.Olit), R(),                 rOs(.Olit), rR(1), rR(3) }),

    // - #0001 SWP => #0100
    s(.{ Os(.Olit), R(), R(), O(.Oswp),                rOs(.Olit), rR(2), rR(1) }),

    // - SWP* SWP*  => <removed>
    // - #00 POP    => <removed>
    // - #0000 POP2 => <removed>
    s(.{ O(.Oswp), O(.Oswp),                                                    }),
    s(.{ Os(.Oswp), Os(.Oswp),                                                  }),
    s(.{ O(.Olit), R(), O(.Opop),                                               }),
    s(.{ Os(.Olit), R(), R(), Os(.Opop),                                        }),

    // - #1234 POP     => #12
    // - #1234 NIP     => #34
    // - #1234 #56 NIP => #1256
    s(.{ Os(.Olit), R(), R(), O(.Opop),                        rO(.Olit), rR(1) }),
    s(.{ Os(.Olit), R(), R(), O(.Onip),                        rO(.Olit), rR(2) }),
    s(.{ Os(.Olit), R(), R(), O(.Olit), R(), O(.Onip), rOs(.Olit), rR(2), rR(4) }),

    // constant folding
    s(.{ Os(.Olit), R(), R(), O(.Oadd),           rO(.Olit), rMathRR(1, 2, '+') }),
    s(.{ Os(.Olit), R(), R(), O(.Osub),           rO(.Olit), rMathRR(1, 2, '-') }),
    s(.{ Os(.Olit), R(), R(), O(.Omul),           rO(.Olit), rMathRR(1, 2, '*') }),
    s(.{ Os(.Olit), R(), R(), O(.Odiv),           rO(.Olit), rMathRR(1, 2, '/') }),
    s(.{ Os(.Olit), R(), R(), O(.Oequ),           rO(.Olit), rMathRR(1, 2, '=') }),
    s(.{ Os(.Olit), R(), R(), O(.Oneq),           rO(.Olit), rMathRR(1, 2, '!') }),
    s(.{ Os(.Olit), R(), R(), O(.Ogth),           rO(.Olit), rMathRR(1, 2, '>') }),
    s(.{ Os(.Olit), R(), R(), O(.Olth),           rO(.Olit), rMathRR(1, 2, '<') }),

    // constant folding, shorts
    s(.{ Os(.Olit), R(), R(), Os(.Olit), R(), R(), Os(.Oadd),
         rOs(.Olit), rMathRR2A(1, 4, '+'), rMathRR2B(1, 4, '+') }),
    s(.{ Os(.Olit), R(), R(), Os(.Olit), R(), R(), Os(.Osub),
         rOs(.Olit), rMathRR2A(1, 4, '-'), rMathRR2B(1, 4, '-') }),
    s(.{ Os(.Olit), R(), R(), Os(.Olit), R(), R(), Os(.Omul),
         rOs(.Olit), rMathRR2A(1, 4, '*'), rMathRR2B(1, 4, '*') }),
    s(.{ Os(.Olit), R(), R(), Os(.Olit), R(), R(), Os(.Odiv),
         rOs(.Olit), rMathRR2A(1, 4, '/'), rMathRR2B(1, 4, '/') }),

    // - #00XX MUL2 -> #XX SFT
    // - #00XX DIV2 -> #XX SFT
    // (from uxnlin)
    // (non-short-mode rules omitted -- they don't save any space)
    //
    s(.{ Os(.Olit),B(0x00),B(0x02), Os(.Omul),  rO(.Olit), rB(0x10), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x04), Os(.Omul),  rO(.Olit), rB(0x20), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x08), Os(.Omul),  rO(.Olit), rB(0x30), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x10), Os(.Omul),  rO(.Olit), rB(0x40), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x20), Os(.Omul),  rO(.Olit), rB(0x50), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x40), Os(.Omul),  rO(.Olit), rB(0x60), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x80), Os(.Omul),  rO(.Olit), rB(0x70), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x01),B(0x00), Os(.Omul),  rO(.Olit), rB(0x80), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x02),B(0x00), Os(.Omul),  rO(.Olit), rB(0x90), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x04),B(0x00), Os(.Omul),  rO(.Olit), rB(0xa0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x08),B(0x00), Os(.Omul),  rO(.Olit), rB(0xb0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x10),B(0x00), Os(.Omul),  rO(.Olit), rB(0xc0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x20),B(0x00), Os(.Omul),  rO(.Olit), rB(0xd0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x40),B(0x00), Os(.Omul),  rO(.Olit), rB(0xe0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x80),B(0x00), Os(.Omul),  rO(.Olit), rB(0xf0), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x02), Os(.Odiv),  rO(.Olit), rB(0x01), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x04), Os(.Odiv),  rO(.Olit), rB(0x02), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x08), Os(.Odiv),  rO(.Olit), rB(0x03), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x10), Os(.Odiv),  rO(.Olit), rB(0x04), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x20), Os(.Odiv),  rO(.Olit), rB(0x05), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x40), Os(.Odiv),  rO(.Olit), rB(0x06), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x00),B(0x80), Os(.Odiv),  rO(.Olit), rB(0x07), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x01),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x08), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x02),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x09), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x04),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0a), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x08),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0b), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x10),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0c), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x20),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0d), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x40),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0e), rOs(.Osft) }),
    s(.{ Os(.Olit),B(0x80),B(0x00), Os(.Odiv),  rO(.Olit), rB(0x0f), rOs(.Osft) }),

    // keepifying instructions
    // - DUP <any-one-arg> -> $1k
    // - OVR OVR <any-two-args> -> $1k
    s(.{ O(.Odup), A1(),                                                  rK(1) }),
    s(.{ Os(.Odup), A2(),                                                 rK(1) }),
    s(.{ Os(.Odup), A1s(),                                                rK(1) }),
    s(.{ Os(.Oovr), Os(.Oovr), A2s(),                                     rK(2) }),

    // - NEQ 00 EQU -> EQU
    // - EQU 00 EQU -> NEQ
    // - NEQ 01 EQU -> NEQ
    // - EQU 01 EQU -> EQU
    s(.{ O(.Oneq),  O(.Olit), B(0x00), O(.Oequ),                      rO(.Oequ) }),
    s(.{ Os(.Oneq), O(.Olit), B(0x00), O(.Oequ),                     rOs(.Oequ) }),
    s(.{ O(.Oequ),  O(.Olit), B(0x00), O(.Oequ),                      rO(.Oneq) }),
    s(.{ Os(.Oequ), O(.Olit), B(0x00), O(.Oequ),                     rOs(.Oneq) }),
    s(.{ O(.Oneq),  O(.Olit), B(0x01), O(.Oequ),                      rO(.Oneq) }),
    s(.{ Os(.Oneq), O(.Olit), B(0x01), O(.Oequ),                     rOs(.Oneq) }),
    s(.{ O(.Oequ),  O(.Olit), B(0x01), O(.Oequ),                      rO(.Oequ) }),
    s(.{ Os(.Oequ), O(.Olit), B(0x01), O(.Oequ),                     rOs(.Oequ) }),

    // - #01   ADD  -> INC
    // - #02   ADD  -> INC INC
    s(.{ O(.Olit),  B(0x01), O(.Oadd),                                rO(.Oinc) }),
    s(.{ Os(.Olit), B(0x00), B(0x01), Os(.Oadd),                     rOs(.Oinc) }),
    s(.{ O(.Olit),  B(0x02), O(.Oadd),                     rO(.Oinc), rO(.Oinc) }),
    s(.{ Os(.Olit), B(0x00), B(0x02), Os(.Oadd),         rOs(.Oinc), rOs(.Oinc) }),

    // - #0003 ADD2 -> INC2 INC2 INC2
    s(.{ Os(.Olit), B(0x00),B(0x03), Os(.Oadd),rOs(.Oinc),rOs(.Oinc),rOs(.Oinc) }),

    // - OVR OVR -> DUP2
    // - NIP POP -> POP2
    // - POP POP -> POP2
    // - SWP POP -> NIP
    s(.{ O(.Oovr), O(.Oovr),                                         rOs(.Odup) }),
    s(.{ O(.Onip), O(.Opop),                                         rOs(.Opop) }),
    s(.{ O(.Opop), O(.Opop),                                         rOs(.Opop) }),
    s(.{ O(.Oswp), O(.Opop),                                          rO(.Onip) }),
    s(.{ Os(.Oswp), Os(.Opop),                                       rOs(.Onip) }),

    // zig fmt: on
};

pub const PatRepSet = struct {
    p: StackBuffer(Pattern, 8),
    r: StackBuffer(Replace, 8),
};

pub fn s(args: anytype) PatRepSet {
    @setEvalBranchQuota(2000);

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
        B: u8, // Specific raw byte
        R, // Any raw byte
        A1, // Any opcode taking one arg
        A2, // Any opcode taking two args, not incl Osft/Ojcn
    };

    pub fn matches(p: @This(), ins: Ins) bool {
        return switch (p.pat) {
            .R => ins.op == .Oraw,
            .B => |b| ins.op == .Oraw and ins.op.Oraw == b,
            .O => |o| o == ins.op and
                ins.short == p.short and ins.stack == p.stk and ins.keep == p.keep,
            .A1 => ins.op.argCount() == 1 and
                ins.op != .Ojci and
                ins.short == p.short and ins.stack == p.stk and ins.keep == p.keep,
            .A2 => ins.op.argCount() == 2 and
                ins.op != .Osft and ins.op != .Ojcn and
                ins.short == p.short and ins.stack == p.stk and ins.keep == p.keep,
        };
    }
};

// zig fmt: off
fn O(o: OpTag)  Pattern { return .{ .pat = .{ .O = o }                }; }
fn Os(o: OpTag) Pattern { return .{ .pat = .{ .O = o }, .short = true }; }
fn R()          Pattern { return .{ .pat = .R                         }; }
fn B(b: u8)     Pattern { return .{ .pat = .{ .B = b },               }; }
fn A1()         Pattern { return .{ .pat = .A1                        }; }
fn A2()         Pattern { return .{ .pat = .A2                        }; }
fn A1s()        Pattern { return .{ .pat = .A1,         .short = true }; }
fn A2s()        Pattern { return .{ .pat = .A2,         .short = true }; }
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
        B: u8, // Raw byte
        Math: MathTransform, // Constant math
        Math2A: MathTransform, // Constant math (short-mode), first byte
        Math2B: MathTransform, // Constant math (short-mode), second byte
        K: usize, // Keepified reference

        pub const MathTransform = struct {
            a: RefOrConst,
            b: RefOrConst,
            op: u21,
        };
    };

    pub const RefOrConst = union(enum) {
        ref: usize,
        val: u8,
        val2: u16,

        pub fn get(self: @This(), ins: []const Ins) u8 {
            return switch (self) {
                .ref => |i| ins[i].op.Oraw,
                .val => |v| v,
                .val2 => unreachable,
            };
        }

        pub fn get2(self: @This(), ins: []const Ins) u16 {
            return switch (self) {
                .ref => |i| @as(u16, @intCast(ins[i].op.Oraw)) << 8 |
                    ins[i + 1].op.Oraw,
                .val2 => |v| v,
                .val => unreachable,
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
            .Math => |m| Ins{ .op = .{ .Oraw = switch (m.op) {
                '+' => m.a.get(ins) +% m.b.get(ins),
                '-' => m.a.get(ins) -% m.b.get(ins),
                '*' => m.a.get(ins) *% m.b.get(ins),
                '/' => m.a.get(ins) / m.b.get(ins),
                '=' => @intFromBool(m.a.get(ins) == m.b.get(ins)),
                '!' => @intFromBool(m.a.get(ins) != m.b.get(ins)),
                '>' => @intFromBool(m.a.get(ins) > m.b.get(ins)),
                '<' => @intFromBool(m.a.get(ins) < m.b.get(ins)),
                else => unreachable,
            } }, .stack = WK_STACK },
            .Math2A, .Math2B => |m| b: {
                const rawres = switch (m.op) {
                    '+' => m.a.get2(ins) +% m.b.get2(ins),
                    '-' => m.a.get2(ins) -% m.b.get2(ins),
                    '*' => m.a.get2(ins) *% m.b.get2(ins),
                    '/' => m.a.get2(ins) / m.b.get2(ins),
                    '=' => @intFromBool(m.a.get2(ins) == m.b.get2(ins)),
                    '!' => @intFromBool(m.a.get2(ins) != m.b.get2(ins)),
                    '>' => @intFromBool(m.a.get2(ins) > m.b.get2(ins)),
                    '<' => @intFromBool(m.a.get2(ins) < m.b.get2(ins)),
                    else => unreachable,
                };
                const res = if (self.rpl == .Math2A) rawres >> 8 else rawres & 0xFF;
                break :b Ins{ .op = .{ .Oraw = @intCast(res) }, .stack = WK_STACK };
            },
            .B => |b| Ins{ .op = .{ .Oraw = b }, .stack = 0 },
            .R => |r| ins[r],
            .K => |r| Ins{
                .op = ins[r].op,
                .short = ins[r].short,
                .keep = true,
                .stack = ins[r].stack,
            },
        };
    }
};

// zig fmt: off
fn rO(o: OpTag) Replace { return .{.rpl = .{.I = .{ .op = o }}}; }
fn rOs(o: OpTag) Replace { return .{.rpl = .{.I = .{ .op = o, .s = true }}}; }
fn rMathRR(r1: usize, r2: usize, op: u21) Replace { return .{.rpl = .{.Math = .{ .a = .{ .ref = r1 }, .b = .{ .ref = r2 }, .op = op }}}; }
fn rMathRR2A(r1: usize, r2: usize, op: u21) Replace { return .{.rpl = .{.Math2A = .{ .a = .{ .ref = r1 }, .b = .{ .ref = r2 }, .op = op }}}; }
fn rMathRR2B(r1: usize, r2: usize, op: u21) Replace { return .{.rpl = .{.Math2B = .{ .a = .{ .ref = r1 }, .b = .{ .ref = r2 }, .op = op }}}; }
fn rR(r: usize) Replace { return .{ .rpl = .{.R = r} }; }
fn rB(b: usize) Replace { return .{ .rpl = .{.B = b} }; }
fn rK(r: usize) Replace { return .{ .rpl = .{.K = r} }; }
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
                // for (matching.r.constSlice(), 0..) |rpl, i| {
                //     std.log.info("{} -> {}", .{ ins[i], rpl.exec(ins[0..matching.p.len]) });
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

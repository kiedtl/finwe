const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const codegen = @import("codegen.zig");
const analyser = @import("analyser.zig");
const common = @This();

const LinkedList = @import("list.zig").LinkedList;
const StackBuffer = @import("buffer.zig").StackBuffer;

// ----------------------------------------------------------------------------

pub const WK_STACK = 0;
pub const RT_STACK = 1;
pub const STACK_SZ = 255;

pub var gpa = std.heap.GeneralPurposeAllocator(.{
    // Probably should enable this later on to track memory usage, if
    // allocations become too much
    .enable_memory_limit = false,

    .safety = true,

    // Probably would enable this later?
    .thread_safe = false,

    .never_unmap = false,
}){};

pub const String = std.ArrayList(u8);

pub const ASTNodeList = LinkedList(ASTNode);
//pub const ASTNodeList = std.ArrayList(ASTNode);
pub const ASTNodePtrList = std.ArrayList(*ASTNode);
//pub const ASTNodePtrList = LinkedList(*ASTNode);
//

pub const TypeFmt = struct {
    typ: TypeInfo,
    prog: *const Program,

    pub fn from(t: TypeInfo, p: *const Program) @This() {
        return .{ .typ = t, .prog = p };
    }

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        const s = @tagName(self.typ);

        switch (self.typ) {
            .TypeRef => |n| try writer.print("${}{s}", .{ n.n, if (n.r) "r" else "" }),
            .Struct, .EnumLit => |n| try writer.print("{s}", .{self.prog.types.items[n].name}),
            .AnyOf => |n| try writer.print("{s}<{}>", .{ s, TypeFmt.from(self.prog.ztype(n), self.prog) }),
            .AnySet => |n| {
                try writer.print("{s}<", .{s});
                for (n.set.constSlice()) |item|
                    try writer.print("{} ", .{TypeFmt.from(item, self.prog)});
                try writer.print(">", .{});
            },
            .Ptr8 => |n| try writer.print("{s}<{} @{}>", .{ s, TypeFmt.from(self.prog.ztype(n.typ), self.prog), n.ind }),
            .Ptr16 => |n| {
                for (0..n.ind) |_| try writer.print("@", .{});
                try writer.print("{}", .{TypeFmt.from(self.prog.ztype(n.typ), self.prog)});
            },
            .Array => |a| if (a.count) |c| {
                try writer.print("[{}]{s}<{}>", .{ c, s, TypeFmt.from(self.prog.ztype(a.typ), self.prog) });
            } else {
                try writer.print("{s}<{}>", .{ s, TypeFmt.from(self.prog.ztype(a.typ), self.prog) });
            },
            .Fn => |n| try writer.print("Fn{s}", .{analyser.AnalysisFmt.from(n.arity, self.prog)}),
            else => try writer.print("{s}", .{s}),
        }
    }
};

pub const TypeInfo = union(enum) {
    Type,
    Opaque,
    Bool,
    U8,
    U16,
    I8,
    I16,
    Char8,
    Char16,
    EnumLit: usize,
    Struct: usize,
    AnyPtr,
    AnyPtr16,
    Ptr8: Ptr,
    Ptr16: Ptr,
    Array: Array,
    Any,
    Any8,
    Any16,
    AnyDev,
    Dev8,
    Dev16,

    AnyOf: usize,
    AnySet: AnySet,

    StaticPtr: usize,
    Fn: Fn,

    Expr: Expr,

    AmbigEnumLit,
    TypeRef: TypeRef,
    Unresolved: Unresolved,

    pub const Tag = meta.Tag(@This());
    pub const List16 = @import("buffer.zig").StackBuffer(@This(), 16);

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        switch (self) {
            .TypeRef => |n| try writer.print("${}{s}", .{ n.n, if (n.r) "r" else "" }),
            .Struct, .EnumLit => |n| try writer.print("{s}<{}>", .{ @tagName(self), n }),
            .Ptr8, .Ptr16 => |n| try writer.print("{s}<{} @{}>", .{ @tagName(self), n.typ, n.ind }),
            .Array => |a| if (a.count) |c| {
                try writer.print("[{}]{s}<{}>", .{ c, @tagName(self), a.typ });
            } else {
                try writer.print("{s}<{}>", .{ @tagName(self), a.typ });
            },
            else => try writer.print("{s}", .{@tagName(self)}),
        }
        try writer.print("", .{});
    }

    pub const Fn = struct {
        arity: *analyser.BlockAnalysis,

        pub fn eq(a: Fn, b: Fn) bool {
            return a.arity.eqExact(b.arity);
        }
    };

    pub const TypeRef = struct {
        n: usize,
        r: bool = false,

        pub fn eq(a: TypeRef, b: TypeRef) bool {
            return a.n == b.n and a.r == b.r;
        }
    };

    pub const AnySet = struct {
        set: *StackBuffer(TypeInfo, 8),

        pub fn eq(a: AnySet, b: AnySet) bool {
            return a.set.eq(b.set.constSlice(), TypeInfo.eq);
        }
    };

    pub const Unresolved = struct {
        ident: []const u8,
        srcloc: Srcloc,
    };

    pub const Array = struct {
        typ: usize,
        count: ?u16, // null for size unknown at comptime

        pub fn eq(a: Array, b: Array) bool {
            return a.typ == b.typ and a.count == b.count;
        }
    };

    pub const Ptr = struct {
        typ: usize,
        ind: usize, // Always >=1. 1 == *Typ, 2 == **Typ, etc

        pub fn eq(a: Ptr, b: Ptr) bool {
            return a.typ == b.typ and a.ind == b.ind;
        }
    };

    pub const Expr = union(enum) {
        AnySz: usize,
        USz: usize,
        ISz: usize,
        AnyOf: usize,
        AnySet: AnySet,
        Child: usize,
        Of: Of,
        // Ptr8: usize,
        Ptr16: usize,
        Array: Array,
        FieldType: FieldType,
        Omit: Omit,
        Fn: Expr.Fn,

        pub const Tag = meta.Tag(@This());

        pub const Fn = struct {
            arity: *analyser.BlockAnalysis,
        };

        pub const Omit = struct {
            of: usize,
            field: []const u8,
        };

        pub const FieldType = struct {
            of: usize,
            field: []const u8,
        };

        pub const Of = struct {
            of: usize,
            // ptr because zig miscompiles otherwise
            // TxODO: remove ptr after a few releases
            //
            // Note 23y03: miscompile was due to ridiculously large size of
            // ASTNode struct (>26kb), I changed a few instances like this to
            // ptrs and brought it down to about 2.5kb. Probably for the best
            // that this remains a ptr.
            //
            args: *StackBuffer(usize, 16),
        };

        pub fn eq(a: @This(), b: @This()) bool {
            if (@as(Expr.Tag, a) != @as(Expr.Tag, b))
                return false;
            return switch (a) {
                .Of => |ar| ar.of == b.Of.of and
                    mem.eql(usize, ar.args.constSlice(), b.Of.args.constSlice()),
                .FieldType => |ft| ft.of == b.FieldType.of and
                    mem.eql(u8, ft.field, b.FieldType.field),
                .Ptr16 => |ar| ar == b.Ptr16,
                .Array => |ar| ar.eq(b.Array),
                .AnySz => |ar| ar == b.AnySz,
                .USz => |ar| ar == b.USz,
                .ISz => |ar| ar == b.ISz,
                .AnyOf => |ar| ar == b.AnyOf,
                .AnySet => |ar| ar.set.eq(b.AnySet.set.constSlice(), TypeInfo.eq),
                .Child => |ar| ar == b.Child,
                .Omit => |omit| omit.of == b.Omit.of and
                    mem.eql(u8, omit.field, b.Omit.field),
                .Fn => |func| func.arity.eqExact(b.Fn.arity),
            };
        }

        pub fn resolveGeneric(self: @This(), with: TypeInfo, program: *Program) @This() {
            return switch (self) {
                .FieldType => |ft| .{ .FieldType = .{
                    .of = program.btype(program.builtin_types.items[ft.of].resolveGeneric(with, program)),
                    .field = ft.field,
                } },
                .Of => |of| b: {
                    var new = of;
                    new.of = program.btype(
                        program.builtin_types.items[new.of].resolveGeneric(with, program),
                    );
                    for (new.args.slice()) |*arg| {
                        arg.* = program.btype(program.builtin_types.items[arg.*].resolveGeneric(with, program));
                    }
                    break :b .{ .Of = new };
                },
                .Ptr16 => |ar| .{ .Ptr16 = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .Array => |ar| .{ .Array = .{
                    .typ = program.btype(program.ztype(ar.typ).resolveGeneric(with, program)),
                    .count = ar.count,
                } },
                .AnySz => |ar| .{ .AnySz = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .USz => |ar| .{ .USz = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .ISz => |ar| .{ .ISz = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .AnyOf => |ar| .{ .AnyOf = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .AnySet => |ar| b: {
                    // FIXME: this is just wrong
                    // Resolving multiple to single value? Nonsense
                    var new = ar;
                    for (new.set.slice()) |*item| {
                        item.* = item.*.resolveGeneric(with, program);
                    }
                    break :b .{ .AnySet = new };
                },
                .Child => |ar| .{ .Child = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .Omit => |omit| .{ .Omit = .{
                    .of = program.btype(program.ztype(omit.of).resolveGeneric(with, program)),
                    .field = omit.field,
                } },
                .Fn => .{ .Fn = .{ .arity = with.Fn.arity } },
            };
        }

        pub fn getTypeRefs(self: @This(), buf: anytype, program: *Program) void {
            switch (self) {
                .FieldType => |f| program.ztype(f.of).getTypeRefs(buf, program),
                .Of => |of| {
                    program.ztype(of.of).getTypeRefs(buf, program);
                    for (of.args.constSlice()) |arg|
                        program.ztype(arg).getTypeRefs(buf, program);
                },
                .Ptr16 => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .Array => |ar| program.ztype(ar.typ).getTypeRefs(buf, program),
                .AnySz => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .USz => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .ISz => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .AnyOf => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .AnySet => |ar| {
                    for (ar.set.slice()) |item|
                        item.getTypeRefs(buf, program);
                },
                .Child => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .Omit => |ar| program.ztype(ar.of).getTypeRefs(buf, program),
                .Fn => |func| func.arity.getTypeRefs(buf, program),
            }
        }

        pub fn isGeneric(self: @This(), program: *Program) bool {
            return switch (self) {
                .FieldType => |f| program.ztype(f.of).isGeneric(program),
                .Of => |of| b: {
                    if (program.ztype(of.of).isGeneric(program))
                        break :b true;
                    for (of.args.constSlice()) |arg|
                        if (program.ztype(arg).isGeneric(program))
                            break :b true;
                    break :b false;
                },
                .Ptr16 => |ar| program.ztype(ar).isGeneric(program),
                .Array => |ar| program.ztype(ar.typ).isGeneric(program),
                .AnySz => |ar| program.ztype(ar).isGeneric(program),
                .USz => |ar| program.ztype(ar).isGeneric(program),
                .ISz => |ar| program.ztype(ar).isGeneric(program),
                .AnySet, .AnyOf => true,
                .Child => |ar| program.ztype(ar).isGeneric(program),
                .Omit => |ar| program.ztype(ar.of).isGeneric(program),
                .Fn => |func| func.arity.isGeneric(program),
            };
        }

        pub fn resolveTypeRef(
            self: @This(),
            scope: ?*Scope,
            arity: ?analyser.BlockAnalysis,
            program: *Program,
        ) analyser.Error!TypeInfo {
            return switch (self) {
                .FieldType => |ft| b: {
                    const arg_t = try program.builtin_types.items[ft.of].resolveTypeRef(scope, arity, program);
                    switch (arg_t) {
                        // .AnyOf => |anyof| {
                        //     const n = program.ztype(anyof).Struct;
                        //     const strct_def = program.types.items[n];

                        //     const fieldt = for (strct_def.def.Struct.fields.items) |f| {
                        //         if (mem.eql(u8, ft.field, f.name)) break f.type;
                        //     } else @panic("no such field");

                        //     return if (fieldt.isResolvable(program)) .Any else fieldt;
                        // },
                        .Struct => |n| {
                            const strct_def = program.types.items[n];

                            break :b for (strct_def.def.Struct.fields.items) |f| {
                                if (mem.eql(u8, ft.field, f.name)) break f.type;
                            } else @panic("no such field");
                        },
                        else => {
                            std.log.err("FieldType(): {}: invalid arg", .{TypeFmt.from(arg_t, program)});
                            @panic("whups");
                        },
                    }
                },
                .Ptr16 => |s| b: {
                    const arg_t = try program.ztype(s).resolveTypeRef(scope, arity, program);
                    break :b arg_t.ptrize16(program);
                },
                .Array => |s| b: {
                    const arg_t = try program.ztype(s.typ).resolveTypeRef(scope, arity, program);
                    if (arg_t == .Array and arg_t.Array.count == null and s.count != null)
                        @panic("Should be obvious, can't have unbounded array inside bounded");
                    break :b .{ .Array = .{ .typ = program.btype(arg_t), .count = s.count } };
                },
                .AnyOf => |anyof| b: {
                    const arg_t = try program.builtin_types.items[anyof].resolveTypeRef(scope, arity, program);
                    if (arg_t != .Struct or
                        program.types.items[arg_t.Struct].def.Struct.args == null)
                    {
                        @panic("bruh");
                    }

                    break :b .{ .AnyOf = program.btype(arg_t) };
                },
                .AnySet => |anyset| .{ .AnySet = anyset },
                .USz => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(scope, arity, program);
                    if (arg_t == .Dev8 or arg_t == .Dev16) {
                        break :b if (arg_t == .Dev8) .U8 else .U16;
                    } else if (arg_t.bits(program)) |b| {
                        break :b if (b == 16) .U16 else .U8;
                    } else {
                        break :b .Any;
                    }
                },
                .ISz => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(scope, arity, program);
                    if (arg_t == .Dev8 or arg_t == .Dev16) {
                        break :b if (arg_t == .Dev8) .I8 else .I16;
                    } else if (arg_t.bits(program)) |b| {
                        break :b if (b == 16) .I16 else .I8;
                    } else {
                        break :b .Any;
                    }
                },
                .AnySz => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(scope, arity, program);
                    if (arg_t == .Dev8 or arg_t == .Dev16) {
                        break :b if (arg_t == .Dev8) .Any8 else .Any16;
                    } else if (arg_t.bits(program)) |b| {
                        break :b if (b == 16) .Any16 else .Any8;
                    } else {
                        //break :b .{ .Expr = .{ .AnySz = program.btype(arg_t) } };
                        break :b .Any;
                    }
                },
                .Of => |of| b: {
                    const strct_typ = try program.builtin_types.items[of.of]
                        .resolveTypeRef(scope, arity, program);
                    if (strct_typ != .Struct)
                        @panic("invalid argument to Of()");

                    const strct_def = program.types.items[strct_typ.Struct];
                    if (strct_def.def.Struct.args == null)
                        @panic("invalid argument to Of()");
                    if (of.args.len != strct_def.def.Struct.args.?.len)
                        @panic("invalid argument count to Of()");

                    var new = strct_def.deepclone();
                    var calculate_offset = true;
                    for (new.def.Struct.args.?.slice(), 0..) |*arg, i| {
                        assert(arg.isGeneric(program));
                        const val = try program.ztype(of.args.slice()[i]).resolveTypeRef(scope, arity, program);
                        if (val.size(program) == null) {
                            calculate_offset = false;
                        }
                        if (arg.doesInclude(val, program)) {
                            arg.* = try val.resolveTypeRef(scope, arity, program);
                        } else {
                            std.log.err("Generic {} does not include {}", .{ arg, val });
                            @panic("whups");
                        }
                    }
                    const arglist = analyser.BlockAnalysis{ .args = new.def.Struct.args.? };
                    for (new.def.Struct.fields.items, 0..) |*field, i| {
                        field.type = try field.type.resolveTypeRef(scope, arglist, program);
                        common.UserType.checkStructField(field.type, i == new.def.Struct.fields.items.len - 1);
                    }
                    if (calculate_offset) {
                        var offset: u16 = 0;
                        for (new.def.Struct.fields.items, 0..) |*field, i| {
                            field.offset = offset;
                            offset += field.type.size(program) orelse {
                                if (field.type == .Array and
                                    i == new.def.Struct.fields.items.len - 1)
                                {
                                    // all is well
                                    continue;
                                } else unreachable;
                            };
                        }
                    }
                    new.def.Struct.args = null;
                    const ind = s: for (program.types.items, 0..) |usertype, i| {
                        if (usertype.def != .Struct)
                            continue;
                        if (!mem.eql(u8, usertype.name, new.name))
                            continue;
                        for (usertype.def.Struct.fields.items, 0..) |field, fi| {
                            if (field.offset != new.def.Struct.fields.items[fi].offset)
                                continue :s;
                            if (!field.type.eq(new.def.Struct.fields.items[fi].type))
                                continue :s;
                        }
                        break i;
                    } else br: {
                        program.types.append(new) catch unreachable;
                        break :br program.types.items.len - 1;
                    };
                    break :b .{ .Struct = ind };
                },
                // TODO: Childish (for only one level of Childification)
                .Child => |s| b: {
                    const arg_t = try program.ztype(s).resolveTypeRef(scope, arity, program);
                    const b = switch (arg_t) {
                        .AnyPtr, .AnyPtr16 => .Any,
                        .Ptr8, .Ptr16 => arg_t.deptrize(program),
                        .Array => |a| program.ztype(a.typ),
                        else => {
                            std.log.info("{}: invalid argument to Child", .{
                                TypeFmt.from(arg_t, program),
                            });
                            @panic("nah");
                        },
                    };

                    if ((arg_t == .Ptr16 or arg_t == .Ptr8) and
                        b == .Array)
                    {
                        // This was deptrize()'s job
                        //
                        //b = program.ztype(b.Array.typ);
                        //
                        //unreachable;
                    }

                    break :b b;
                },
                .Omit => |omit| b: {
                    const strct_typ = try program.ztype(omit.of).resolveTypeRef(scope, arity, program);
                    if (strct_typ != .Struct)
                        @panic("invalid argument to Omit()");

                    var new = program.types.items[strct_typ.Struct].deepclone();
                    for (new.def.Struct.fields.items, 0..) |field, i|
                        if (mem.eql(u8, field.name, omit.field)) {
                            _ = new.def.Struct.fields.orderedRemove(i);
                            break;
                        };

                    program.types.append(new) catch unreachable;
                    break :b .{ .Struct = program.types.items.len - 1 };
                },
                .Fn => |func| b: {
                    for (func.arity.args.constSlice()) |item|
                        if (item == .Type) @panic("Cannot have type args here");
                    for (func.arity.rargs.constSlice()) |item|
                        if (item == .Type) @panic("Cannot have type args here");

                    const new = gpa.allocator().create(analyser.BlockAnalysis) catch unreachable;
                    new.* = try func.arity.resolveTypeRefs(scope, arity, program);
                    break :b .{ .Fn = .{ .arity = new } };
                },
            };
        }
    };

    pub fn isResolvable(self: @This(), p: *Program) bool {
        return switch (self) {
            .AnyOf => |anyof| p.ztype(anyof).isResolvable(p),
            .AnySet => |anyset| for (anyset.set.constSlice()) |i| {
                if (i.isResolvable(p)) break true;
            } else false,
            .TypeRef, .Expr, .Unresolved => true,
            else => false,
        };
    }

    pub fn getTypeRefs(a: @This(), buf: anytype, program: *Program) void {
        switch (a) {
            .TypeRef => |r| buf.append(r) catch unreachable,
            .AnyOf => |anyof| program.ztype(anyof).getTypeRefs(buf, program),
            .AnySet => |anyset| for (anyset.set.constSlice()) |i|
                i.getTypeRefs(buf, program),
            .Expr => |e| e.getTypeRefs(buf, program),
            else => {},
        }
    }

    pub fn resolveTypeRef(a: @This(), p_scope: ?*Scope, arity: ?analyser.BlockAnalysis, program: *Program) analyser.Error!TypeInfo {
        const scope = p_scope orelse program.global_scope;
        const r: TypeInfo = switch (a) {
            .TypeRef => |r| b: {
                const args = if (r.r) &arity.?.rargs else &arity.?.args;
                break :b args.constSlice()[args.len - r.n - 1];
            },
            .AnyOf => |anyof| .{ .AnyOf = program.btype(try program.ztype(anyof).resolveTypeRef(scope, arity, program)) },
            // XXX: we don't clone here, hope it won't cause issues :P
            .AnySet => |*anyset| b: {
                for (anyset.set.slice()) |*item| {
                    item.* = try item.*.resolveTypeRef(scope, arity, program);
                }
                break :b a;
            },
            .Expr => |e| try e.resolveTypeRef(scope, arity, program),
            .Unresolved => |k| if (scope.findType(k.ident, program, true)) |n| b: {
                const def = program.types.items[n];
                break :b switch (def.def) {
                    .Enum => .{ .EnumLit = n },
                    .Struct => .{ .Struct = n },
                    .Alias => |alias| alias.val,
                    .Device => @panic("TODO: Devices (use Dev8/Dev16 for now?)"),
                };
            } else {
                return program.aerr(error.NoSuchType, k.srcloc, .{k.ident});
            },
            else => a,
        };
        return if (r.isResolvable(program)) try r.resolveTypeRef(scope, arity, program) else r;
    }

    pub fn eq(a: @This(), b: @This()) bool {
        if (@as(Tag, a) != @as(Tag, b)) return false;
        inline for (meta.fields(@This())) |field|
            if (mem.eql(u8, field.name, @tagName(a)))
                if (field.type == usize or field.type == void)
                    return @field(a, field.name) == @field(b, field.name)
                else if (field.type == TypeInfo.Unresolved)
                    return mem.eql(u8, @field(a, field.name).ident, @field(b, field.name).ident)
                else if (field.type == []const u8)
                    return mem.eql(u8, @field(a, field.name), @field(b, field.name))
                else
                    return @field(a, field.name).eq(@field(b, field.name));
        unreachable;
    }

    pub fn ptrize16(a: @This(), program: *Program) @This() {
        return switch (a) {
            .Ptr16 => |p| .{ .Ptr16 = .{ .typ = p.typ, .ind = p.ind + 1 } },
            else => TypeInfo.ptr16(program, a, 1),
        };
    }

    pub fn deptrize(self: @This(), program: *Program) @This() {
        const a: TypeInfo = switch (self) {
            .Ptr8 => |p| .{ .Ptr8 = .{ .typ = p.typ, .ind = p.ind - 1 } },
            .Ptr16 => |p| .{ .Ptr16 = .{ .typ = p.typ, .ind = p.ind - 1 } },
            else => unreachable,
        };
        const b = if (switch (a) {
            .Ptr16, .Ptr8 => |p| p.ind,
            else => unreachable,
        } == 0) switch (a) {
            .Ptr8, .Ptr16 => |p| program.ztype(p.typ),
            else => unreachable,
        } else a;
        return if (b == .Array) program.ztype(b.Array.typ) else b;
    }

    pub fn ptr16(program: *Program, to: TypeInfo, indirection: usize) TypeInfo {
        assert(indirection >= 1);
        const ind: ?usize = for (program.builtin_types.items, 0..) |typ, i| {
            if (to.eq(typ)) break i;
        } else null;
        if (ind) |_ind| {
            return .{ .Ptr16 = .{ .typ = _ind, .ind = indirection } };
        } else {
            program.builtin_types.append(to) catch unreachable;
            return .{ .Ptr16 = .{
                .typ = program.builtin_types.items.len - 1,
                .ind = indirection,
            } };
        }
    }

    pub fn resolveGeneric(self: @This(), with: @This(), program: *Program) @This() {
        return switch (self) {
            .Type, .AnySet, .AnyOf, .AnyDev, .AnyPtr16, .Any, .Any8, .Any16, .AnyPtr => with,
            .Expr => |e| .{ .Expr = e.resolveGeneric(with, program) },
            .Ptr16 => |p| program.ztype(p.typ).resolveGeneric(program.ztype(with.Ptr16.typ), program).ptrize16(program),
            .Ptr8 => |p| .{ .Ptr8 = .{ .typ = program.btype(program.builtin_types.items[p.typ].resolveGeneric(with, program)), .ind = p.ind } },
            .Array => |a| .{ .Array = .{ .typ = program.btype(program.ztype(a.typ).resolveGeneric(program.ztype(with.Array.typ), program)), .count = a.count } },
            else => self,
        };
    }

    pub fn isGeneric(self: @This(), program: *Program) bool {
        return switch (self) {
            .Type, .AnySet, .AnyOf, .AnyDev, .AnyPtr16, .Any, .Any8, .Any16, .AnyPtr, .TypeRef => true,
            .Expr => |e| e.isGeneric(program),
            .Ptr8, .Ptr16 => |ptr| program.ztype(ptr.typ).isGeneric(program),
            .Struct => |s| program.types.items[s].def.Struct.args != null,
            .Array => |arr| program.ztype(arr.typ).isGeneric(program),
            // Stack overflow when struct references itself (via Ptr etc)
            // .Struct => |s| b: {
            //     const fields = program.types.items[s].def.Struct.fields;
            //     var generic = false;
            //     for (fields.items) |field|
            //         if (field.type.isGeneric(program)) {
            //             generic = true;
            //         };
            //     break :b generic;
            // },
            else => false,
        };
    }

    pub fn doesInclude(self: @This(), other: @This(), program: *Program) bool {
        switch (self) {
            .Fn, .EnumLit, .AnySet, .AnyOf => {},
            else => if (@as(Tag, self) == @as(Tag, other)) return true,
        }
        return switch (self) {
            .Type => true,
            // TODO: pointer casting rules
            // .Ptr8, .Ptr16 => |ptr| switch (other) {
            //     .Ptr8, .Ptr16 => |otherptr| program.ztype(otherptr.typ).size(program) ==
            //         program.ztype(ptr.typ).size(program) or
            //         program.ztype(otherptr.typ).doesInclude(program.ztype(ptr.typ), program),
            //     else => return true, // TODO: make more strict
            // },
            .AnySet => |anyset| for (anyset.set.constSlice()) |i| {
                if (i.eq(other)) break true;
            } else false,
            .AnyOf => |anyof| mem.eql(
                u8,
                program.types.items[
                    program.ztype(anyof).Struct
                ].name,
                program.types.items[other.Struct].name,
            ),
            .Any => true,
            .AnyDev => other == .Dev8 or other == .Dev16,
            .Any8 => other.bits(program) != null and other.bits(program).? == 8,
            .Any16 => other.bits(program) != null and other.bits(program).? == 16,
            .AnyPtr => other == .Ptr8 or other == .Ptr16 or other == .StaticPtr,
            .AnyPtr16 => other == .Ptr16 or other == .StaticPtr,
            .EnumLit => |e| other == .EnumLit and e == other.EnumLit,
            .Char8 => other == .U8,
            .U8 => other == .Char8,
            .Fn => |func| other == .Fn and if (func.arity.conformGenericTo(&[_]TypeInfo{}, null, other.Fn.arity, null, program, true, false)) |_| true else |_| false,

            else => false, // self == other case already handled earlier
        };
    }

    pub fn size(self: @This(), program: *const Program) ?u16 {
        return switch (self) {
            .Opaque => null,
            .Struct => |s| b: {
                const tstruct = program.types.items[s].def.Struct;
                const last = tstruct.fields.items[tstruct.fields.items.len - 1];
                if (last.offset == 0xFFFF) return null;
                break :b last.offset + (last.type.size(program) orelse break :b null);
            },
            .Array => |a| b: {
                const tsize = program.ztype(a.typ).size(program) orelse break :b null;
                break :b tsize * (a.count orelse break :b null);
            },
            else => return if (self.bits(program)) |b| b / 8 else null,
        };
    }

    pub fn bits(self: @This(), program: *const Program) ?u5 {
        return switch (self) {
            .Type, .Opaque => null,
            .I8, .AnyDev, .Dev8, .Dev16, .U8, .Char8, .Ptr8, .Bool, .Any8 => 8,
            .I16, .AnyPtr16, .StaticPtr, .U16, .Char16, .Ptr16, .Any16 => 16,
            .AnyPtr, .Any => null,
            .EnumLit => |e| if (program.types.items[e].def.Enum.is_short) 16 else 8,
            .AnySet, .AnyOf => null,
            .Array => null,
            .Struct => b: {
                const sz = self.size(program) orelse return null;
                break :b if (sz <= 2) @as(u5, @intCast(sz)) * 8 else null;
            },
            // .Expr, .Quote, .AmbigEnumLit, .TypeRef => unreachable,
            .Unresolved, .Expr, .Fn, .TypeRef => null,
            .AmbigEnumLit => unreachable,
        };
    }
};

pub const Value = struct {
    typ: TypeInfo,
    val: V,

    pub const V = union(enum) {
        u8: u8,
        u16: u16,
        EnumLit: usize,
        Device: struct { dev_i: usize, field: usize },
        AmbigEnumLit: lexer.Node.EnumLit,
        None,
    };

    pub fn toU16(self: Value, program: *Program) u16 {
        return switch (self.typ) {
            .I16, .U16, .Char16, .Ptr16 => self.val.u16,
            .EnumLit => |e| b: {
                const v = program.types.items[e].def.Enum.fields.items[self.val.EnumLit];
                break :b v.value_b.? << 4 | v.value_a;
            },
            .AmbigEnumLit => unreachable,
            .Any, .Any8, .Any16, .AnyPtr, .AnyDev => unreachable,
            .StaticPtr => @panic("Codegen bug: string is static data, need UAL"),
            else => unreachable,
        };
    }

    pub fn toU8(self: Value, program: *Program) u8 {
        return switch (self.typ) {
            .Dev8, .Dev16 => b: {
                const t = program.types.items[self.val.Device.dev_i].def.Device;
                var v: u8 = t.start;
                for (0..self.val.Device.field) |i|
                    v += t.fields.items[i].type.bits(program).? / 8;
                break :b v;
            },
            .Bool, .I8, .U8, .Char8, .Ptr8 => self.val.u8,
            .EnumLit => |e| program.types.items[e].def.Enum.fields.items[self.val.EnumLit].value_a, // TODO: value_b
            .AmbigEnumLit => unreachable,
            .Any, .Any8, .Any16, .AnyPtr => unreachable,
            .StaticPtr => @panic("Codegen bug: string is static data, need UAL"),
            else => unreachable,
        };
    }
};

pub const ASTNode = struct {
    __prev: ?*ASTNode = null,
    __next: ?*ASTNode = null,

    node: ASTNode.Type,
    srcloc: Srcloc,
    romloc: usize = 0xFFFF,

    // incremented each time codegen runs over it, used to differentiate between
    // multiple version of generated bytecode (e.g. when inlining a func multiple
    // times)
    gen_id: usize = 0,

    pub const Tag = std.meta.Tag(ASTNode.Type);

    pub const Type = union(enum) {
        None, // Placeholder for removed ast values
        Import: Import,
        Decl: Decl, // word declaration
        Call: Call,
        Wild: Wild,
        RBlock: RBlock,
        Loop: Loop,
        When: When,
        Cond: Cond,
        Asm: Ins,
        Value: LitValue,
        Quote: Quote,
        Cast: Cast,
        VDecl: VDecl,
        VRef: VRef,
        VDeref: VDeref,
        GetChild: GetChild,
        GetIndex: GetIndex,
        TypeDef: TypeDef,
        Builtin: Builtin,
        Breakpoint: Breakpoint,
        Debug,
        Return,

        pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            if (comptime !mem.eql(u8, f, "")) @compileError("Unknown format: '" ++ f ++ "'");

            const s = @tagName(self);
            switch (self) {
                .Call => |c| try writer.print("Call<{s}, {}>", .{ c.name, c.variant }),
                .Wild => |w| try writer.print("Wild<{}>", .{w.arity}),
                .Loop => |l| switch (l.loop) {
                    .Until => |u| try writer.print("Until<{s}>", .{@tagName(u.cond_prep)}),
                    .While => |u| try writer.print("While<{s}>", .{@tagName(u.cond_prep)}),
                },
                .Asm => |a| try writer.print("Asm<{}>", .{a}),
                .Value => |v| try writer.print("Val<{}, {}>", .{ v.val.typ, v.val.val }),
                //.Cast => |c| try writer.print("Cast<{}, {} ${?}>", .{ c.of, c.to, c.ref }),
                else => try writer.print("{s}", .{s}),
            }
        }
    };

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) @compileError("Unknown format string: '" ++ f ++ "'");
        try writer.print("{}", .{self.node});
    }

    pub const LitValue = struct {
        val: Value,
        ret: bool = false,
    };

    pub const Import = struct {
        name: []const u8,
        path: []const u8,
        scope: *Scope,
        body: ASTNodeList,

        // Import into current namespace?
        is_defiling: bool = false,

        // Import that was already imported previously (into another
        // namspace)? Keep track of this so that walkNodes doesn't go
        // over import ast multiple times
        is_dupe: bool = false,
    };

    pub const Builtin = struct {
        type: union(enum) {
            SizeOf: SizeOf,
            Make: Make,
            SplitCast: SplitCast,
            Here,
            StaticsHere,
        },

        pub const SplitCast = struct {
            of: TypeInfo = .Any,
            resolved1: TypeInfo = .Any,
            original1: TypeInfo,
            resolved2: TypeInfo = .Any,
            original2: TypeInfo,
        };

        pub const Make = struct {
            original: TypeInfo,
            resolved: TypeInfo,
        };

        pub const SizeOf = struct {
            original: TypeInfo,
            resolved: TypeInfo,
        };
    };

    pub const TypeDef = struct {
        name: []const u8,
        def: union(enum) {
            Enum: EnumDef,
            Device: DeviceDef,
            Struct: StructDef,
            Alias: AliasDef,
        },
        is_private: bool = false,
        is_targ_dampe: bool = false,

        pub const AliasDef = struct {
            val: TypeInfo,
        };

        pub const Field = struct {
            name: []const u8,
            type: TypeInfo,
            srcloc: Srcloc,

            pub const AList = std.ArrayList(Field);
        };

        pub const EnumField = struct {
            name: []const u8,
            value: ?Value,
            srcloc: Srcloc,

            pub const AList = std.ArrayList(EnumField);
        };

        pub const DeviceDef = struct {
            start: u8,
            fields: Field.AList,
        };

        pub const StructDef = struct {
            args: ?TypeInfo.List16 = null,
            fields: Field.AList,
        };

        pub const EnumDef = struct {
            type: TypeInfo,
            fields: EnumField.AList,
        };
    };

    pub const GetChild = struct {
        name: []const u8,
        kind: union(enum) {
            unresolved,
            stk_one_s,
            stk_two_b: struct { ind: u1 },
            stk_one_b,
            mem: struct {
                offset: u16,
                is_short: bool,
            },
        } = .unresolved,
    };

    pub const GetIndex = struct {
        ind: union(enum) {
            known: u16,
            stk_s,
            stk_b,
            stk_unresolved,
        },
        multiplier: u16 = 0xFFFF,
        order: union(enum) {
            moot, // known index
            it, // ind then target
            ti, // target then ind
            unchecked,
        } = .unchecked,
    };

    pub const VDecl = struct {
        name: []const u8,
        localptr: ?*Local,
        utyp: TypeInfo,
        default: StaticData.AList,
    };

    pub const VRef = struct {
        name: []const u8,
        localptr: ?*Local = null,
    };

    pub const VDeref = struct {
        name: []const u8,
        localptr: ?*Local = null,
    };

    pub const Wild = struct {
        arity: analyser.BlockAnalysis,
        body: ASTNodeList,
    };

    pub const Cast = struct {
        ret: bool = false,
        from: *StackBuffer(TypeInfo, 4),
        resolved: *StackBuffer(TypeInfo, 4),
        original: *StackBuffer(TypeInfo, 4),
    };

    pub const Call = struct {
        name: []const u8,
        variant: usize = 0, // 0 if not generic
        args: StackBuffer(TypeInfo, 2) = StackBuffer(TypeInfo, 2).init(null),
        node: ?*ASTNode = null,
        goto: bool = false,
        is_method: bool = false,
        is_inline_override: bool = false,
    };

    pub const When = struct {
        yup: ASTNodeList,
        nah: ?ASTNodeList,
    };

    pub const Cond = struct {
        branches: Branch.AList,
        else_branch: ?ASTNodeList,
        else_srcloc: ?Srcloc,

        pub const Branch = struct {
            cond: ASTNodeList,
            cond_srcloc: Srcloc,
            cond_prep: CondPrep,
            cond_arity: ?analyser.BlockAnalysis = null,
            body: ASTNodeList,
            body_srcloc: Srcloc,

            pub const AList = std.ArrayList(Branch);
        };
    };

    pub const RBlock = struct {
        body: ASTNodeList,
    };

    pub const Loop = struct {
        loop: Loop.Type,
        body: ASTNodeList,

        pub const Type = union(enum) {
            Until: Basic,
            While: Basic,

            pub const Basic = struct {
                cond: ASTNodeList,
                cond_arity: ?analyser.BlockAnalysis = null,
                cond_prep: CondPrep,
            };
        };
    };

    pub const CondPrep = enum {
        unchecked,
        none,
        dup_1_b,
        dup_1_s,
        dup_2_bb,
        dup_2_bs,
        dup_2_sb,
        dup_2_ss,

        pub fn fromArity(cond_arity: *const analyser.BlockAnalysis, p: *Program) !CondPrep {
            if (cond_arity.stack.len != 1 or
                cond_arity.stack.last().? != .Bool)
            {
                @panic("Loop condition must return bool, the whole bool, and nothing but the bool");
            }

            switch (cond_arity.args.len) {
                0 => return .none,
                1 => if (cond_arity.args.last().?.size(p)) |sz| {
                    assert(sz <= 2);
                    return if (sz == 1) .dup_1_b else .dup_1_s;
                } else return .unchecked,
                2 => {
                    const a1 = cond_arity.args.constSlice()[1];
                    const a2 = cond_arity.args.constSlice()[0];
                    if (a1.size(p) != null and a2.size(p) != null) {
                        const a1s = a1.size(p).?;
                        const a2s = a2.size(p).?;
                        if (a1s == 1 and a2s == 1) {
                            return .dup_2_bb;
                        } else if (a1s == 2 and a2s == 1) {
                            return .dup_2_bs;
                        } else if (a1s == 1 and a2s == 2) {
                            return .dup_2_sb;
                        } else if (a1s == 2 and a2s == 2) {
                            return .dup_2_ss;
                        } else unreachable;
                    } else {
                        return .unchecked;
                    }
                },
                else => @panic("Loop condition can take maximum 2 args"),
            }
        }
        pub fn execute(self: CondPrep, a: *analyser.BlockAnalysis) !void {
            switch (self) {
                // no arity, best guess
                .unchecked => a.stack.append(a.stack.last().?) catch unreachable,
                .none => {},
                .dup_1_b, .dup_1_s => {
                    a.stack.append(a.stack.last().?) catch unreachable;
                },
                .dup_2_bb, .dup_2_sb, .dup_2_bs, .dup_2_ss => {
                    const a1 = a.stack.last().?;
                    const a2 = a.stack.constSlice()[a.stack.len - 2];
                    a.stack.append(a2) catch unreachable;
                    a.stack.append(a1) catch unreachable;
                },
            }
        }
    };

    pub const Decl = struct {
        name: []const u8,
        arity: ?analyser.BlockAnalysis = null,
        analysis: analyser.BlockAnalysis = analyser.BlockAnalysis{},
        variations: ASTNodePtrList,
        variant: usize = 0,
        variant_of: ?*ASTNode = null,
        calls: usize = 0,
        scope: *Scope,
        body: ASTNodeList,

        // What scope is it found in? not necessarily same as scope.parent
        // e.g. if method, then will be type's scope
        in_scope: *Scope,

        is_private: bool = false,
        is_noreturn: bool = false,
        is_method: ?TypeInfo = null,
        is_analysed: bool = false,
        is_test: bool = false,
        is_inline: Inline = .Auto,
        is_targ_dampe: bool = false,

        bytecode_size: usize = 0,
        folded_into: ?*ASTNode = null,

        pub const Inline = enum { Auto, AutoYes, AutoNo, Always, Never };

        pub fn skipGen(self: @This()) bool {
            return self.calls == 0 or self.is_inline == .Always or self.is_inline == .AutoYes;
        }
    };

    pub const Quote = struct {
        def: *ASTNode,
    };

    fn _deepcloneASTList(lst: ASTNodeList, parent: ?*Decl, program: *Program) ASTNodeList {
        var new = ASTNodeList.init(gpa.allocator());
        var itr = lst.iterator();
        while (itr.next()) |item| {
            new.append(item.deepclone(parent, program)) catch unreachable;
        }
        return new;
    }

    // Clone a Decl node, but also "register" it by adding it to list of variations
    // of original node, and add it to list of defs (program.defs).
    //
    pub fn deepcloneDecl(self: *@This(), astlst: *ASTNodeList, add_to_scope: bool, program: *Program) *@This() {
        var node = self.node;
        node.Decl.variations = ASTNodePtrList.init(gpa.allocator());
        node.Decl.body = _deepcloneASTList(node.Decl.body, &node.Decl, program);
        node.Decl.scope = node.Decl.scope.shallowclone();
        node.Decl.scope.resetChildParents();
        const new = ASTNode{
            .node = node,
            .srcloc = self.srcloc,
            .romloc = self.romloc,
        };
        const nptr = astlst.appendAndReturn(new) catch unreachable;
        self.node.Decl.variations.append(nptr) catch unreachable;
        nptr.node.Decl.variant = self.node.Decl.variations.items.len;
        nptr.node.Decl.variant_of = self;
        if (add_to_scope)
            nptr.node.Decl.in_scope.defs.append(nptr) catch unreachable;
        program.defs.append(nptr) catch unreachable;
        return nptr;
    }

    pub fn deepclone(self: @This(), parent: ?*Decl, program: *Program) @This() {
        var new = self.node;
        switch (new) {
            .TypeDef => unreachable,
            .When => {
                new.When.yup = _deepcloneASTList(new.When.yup, parent, program);
                if (new.When.nah) |n|
                    new.When.nah = _deepcloneASTList(n, parent, program);
            },
            .Cond => |*c| {
                if (c.else_branch != null)
                    c.else_branch = _deepcloneASTList(c.else_branch.?, parent, program);
                for (c.branches.items, 0..) |br, i| {
                    c.branches.items[i].body = _deepcloneASTList(br.body, parent, program);
                    c.branches.items[i].cond = _deepcloneASTList(br.cond, parent, program);
                }
            },
            .Loop => {
                new.Loop.body = _deepcloneASTList(new.Loop.body, parent, program);
                switch (new.Loop.loop) {
                    .Until => |u| new.Loop.loop.Until.cond = _deepcloneASTList(u.cond, parent, program),
                    .While => |u| new.Loop.loop.While.cond = _deepcloneASTList(u.cond, parent, program),
                }
            },
            .Decl => {
                // Technically should use deepcloneDecl for this. Still used
                // for nested decls (since deepcloneASTList doesn't
                // special-handle decls)
                //
                // It's okay to do this without "registering" in program.defs,
                // not sure why it's okay though <shrug>
                //
                // Best guess is because it's cloned anyway later when called.
                //
                // Anyway, wouldn't work because the calls have already been
                // resolved in parser post-processing, and cloning would break
                // those.
                //
                new.Decl.variations = ASTNodePtrList.init(gpa.allocator());
                new.Decl.body = _deepcloneASTList(new.Decl.body, &new.Decl, program);
                new.Decl.scope = new.Decl.scope.shallowclone();
            },
            .Quote => |*q| {
                q.def = q.def.deepcloneDecl(&program.ast, true, program);
            },
            .Wild => new.Wild.body = _deepcloneASTList(new.Wild.body, parent, program),
            .RBlock => new.RBlock.body = _deepcloneASTList(new.RBlock.body, parent, program),
            .Import => @panic("excuse me what"),
            .VDecl => {},
            .VDeref, .VRef => {},
            .Cast => {
                new.Cast.from = new.Cast.from.clone();
                new.Cast.original = new.Cast.original.clone();
                new.Cast.resolved = new.Cast.resolved.clone();
            },
            .GetIndex, .GetChild, .None, .Call, .Asm, .Value, .Debug, .Builtin, .Breakpoint, .Return => {},
        }

        return .{
            .node = new,
            .srcloc = self.srcloc,
            .romloc = self.romloc,
        };
    }
};

pub const Breakpoint = struct {
    type: Type,
    must_execute: bool = false,
    parent_test: ?*ASTNode.Decl = null,

    // context for test runner
    // FIXME: don't modify the breakpoint data directly
    is_executed: bool = false,

    pub const Type = union(enum) {
        TosShouldEq: Value,
        TosShouldEqSos: TypeInfo, // tos_type
        TosShouldNeq: Value,
        TosShouldNeqSos: TypeInfo, // tos_type
        StdoutShouldEq: String,
        RCheckpoint,
    };

    pub const AList = std.ArrayList(@This());
};

pub const Srcloc = struct {
    line: usize = 0,
    column: usize = 0,
    file: []const u8 = "<unknown>",

    pub fn format(self: @This(), comptime f: []const u8, _: fmt.FormatOptions, w: anytype) !void {
        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        try fmt.format(w, "{s}:{}:{}", .{ self.file, self.line, self.column });
    }
};

pub const Error = struct {
    ctx: Context = .{},
    e: Set,
    l: Srcloc,

    pub const Context = struct {
        usize1: ?usize = null,
        usize2: ?usize = null,
        lexnodetype1: ?meta.Tag(lexer.Node.NodeType) = null,
        lexnodetype2: ?meta.Tag(lexer.Node.NodeType) = null,
        string1: ?[]const u8 = null,
        string2: ?[]const u8 = null,
        finwetype1: ?TypeInfo = null,
        finwetype2: ?TypeInfo = null,
        ushort1: ?u16 = null,
        ushort2: ?u16 = null,
        analysis1: ?analyser.BlockAnalysis = null,
        analysis2: ?analyser.BlockAnalysis = null,
        parentype1: ?lexer.Lexer.Stack.Type = null,
        parentype2: ?lexer.Lexer.Stack.Type = null,

        pub fn from(tuple: anytype) Context {
            var new = Context{};
            const listinfo = @typeInfo(@TypeOf(tuple));
            comptime var typelist: [16]type = undefined;
            comptime var typelistlen = 0;
            inline for (listinfo.Struct.fields) |fieldinfo| {
                typelist[typelistlen] = fieldinfo.type;
                typelistlen += 1;
                comptime var n = 0;
                comptime for (typelist[0..typelistlen]) |t|
                    if (t == fieldinfo.type) {
                        n += 1;
                    };
                const nstr = switch (n) {
                    1 => "1",
                    2 => "2",
                    3 => "3",
                    4 => "4",
                    else => unreachable,
                };
                const typename = switch (fieldinfo.type) {
                    u16 => "ushort",
                    []const u8 => "string",
                    meta.Tag(lexer.Node.NodeType) => "lexnodetype",
                    TypeInfo => "finwetype",
                    analyser.BlockAnalysis => "analysis",
                    lexer.Lexer.Stack.Type => "parentype",
                    else => @typeName(fieldinfo.type),
                };
                @field(new, typename ++ nstr) = @field(tuple, fieldinfo.name);
            }
            return new;
        }
    };

    pub const Set = error{
        _Continue, // Internal control flow for walkNodes, should never be raised
    } || parser.Parser.ParserError || analyser.Error || lexer.Lexer.LexerError;
    pub const AList = std.ArrayList(@This());
};

pub const Local = struct {
    __prev: ?*@This() = null,
    __next: ?*@This() = null,

    name: []const u8,
    ind: ?usize = null,
    rtyp: TypeInfo,
    default: StaticData.AList,
    declnode: *ASTNode,

    pub const List = LinkedList(@This());

    pub fn inferLength(self: *Local, program: *Program) void {
        if (self.rtyp == .Array and self.rtyp.Array.count == null) {
            const artyp = program.ztype(self.rtyp.Array.typ);

            if (self.default.items.len == 0) {
                @panic("Need default for inferred array length");
            }

            var ctr: u16 = 0;
            for (self.default.items) |default| switch (default) {
                .String => |s| {
                    // TODO: add this back as a warning
                    // "You are potentially making a grave mistake! [U16] with
                    // a bunch of strings? really?"
                    //
                    //if (artyp != .Char8 and artyp != .U8)
                    //  @panic("Can only infer length of Char8/U8 array when given string");
                    ctr += @intCast(s.items.len + 1);
                },
                .Byte => ctr += 1,
                .Short => ctr += 2,
            };

            const typsz = artyp.size(program).?;
            if (ctr % typsz != 0)
                @panic("Data size in bytes not evenly divisible by local type size (hint: provide concrete array length)");

            ctr /= typsz;
            self.rtyp.Array.count = ctr;
        }
    }
};

pub const Static = struct {
    type: TypeInfo,
    count: usize,
    default: StaticData.AList,
    romloc: usize = 0xFFFF,
    srcloc: Srcloc,
    used: bool = false,

    pub const AList = std.ArrayList(@This());
};

pub const StaticData = union(enum) {
    String: String,
    Byte: u8,
    Short: u16,
    // Embed: struct {
    //     path: []const u8,
    //     // TODO: compress embeds
    //     // compress: bool,
    // },
    // TODO: struct literals
    // StructLit: struct {
    //     type: usize,
    //     values: std.ArrayList(Value),
    // },

    pub const AList = std.ArrayList(@This());
};

pub const Scope = struct {
    defs: ASTNodePtrList,
    locals: Local.List,
    imports: ASTNodePtrList,
    types: std.ArrayList(usize),
    parent: ?*Scope,

    pub const AList = std.ArrayList(Scope);

    pub fn shallowclone(self: *Scope) *Scope {
        const new = Scope.create(self.parent);
        new.defs.appendSlice(self.defs.items) catch unreachable;
        var iter = self.locals.iterator();
        while (iter.next()) |local|
            new.locals.append(local.*) catch unreachable;
        new.imports.appendSlice(self.imports.items) catch unreachable;
        new.types.appendSlice(self.types.items) catch unreachable;
        return new;
    }

    // When we clone a decl, we clone the old scope, so we need to set any
    // child decl's scope parents to the new scope
    //
    pub fn resetChildParents(self: *Scope) void {
        for (self.defs.items) |defnode| {
            defnode.node.Decl.scope.parent = self;
        }
    }

    pub fn create(parent: ?*Scope) *@This() {
        const p = gpa.allocator().create(@This()) catch unreachable;
        p.* = .{
            .defs = ASTNodePtrList.init(gpa.allocator()),
            .locals = Local.List.init(gpa.allocator()),
            .imports = ASTNodePtrList.init(gpa.allocator()),
            .types = std.ArrayList(usize).init(gpa.allocator()),
            .parent = parent,
        };
        return p;
    }

    pub fn findDeclForCaller(
        self: *Scope,
        program: *Program,
        name: []const u8,
        caller_node: *ASTNode,
        caller_parent_arity: ?analyser.BlockAnalysis,
        caller_scope: *Scope,
        caller_args: *const analyser.BlockAnalysis,
        r_stk: bool,
    ) !?*ASTNode {
        var searched_import_buf = ASTNodePtrList.init(gpa.allocator());
        defer searched_import_buf.deinit();
        var buf = ASTNodePtrList.init(gpa.allocator());
        defer buf.deinit();

        self._findDecl(name, &buf, &searched_import_buf, true, program);

        var type_args = @TypeOf(caller_args.args).init(null);
        for (caller_node.node.Call.args.constSlice()) |arg|
            type_args.append(try arg.resolveTypeRef(caller_scope, caller_parent_arity, program)) catch unreachable;

        for (buf.items) |potential| {
            if (potential.node.Decl.arity) |m_potential_arity| {
                var potential_arity = m_potential_arity;
                if (r_stk) potential_arity.reverse();
                _ = potential_arity.conformGenericTo(type_args.constSlice(), potential.node.Decl.scope.parent, caller_args, caller_node, program, true, false) catch continue;
            }
            return potential;
        }

        return buf.getLastOrNull();
    }

    pub fn findDeclAny(self: *Scope, program: *Program, name: []const u8) ?*ASTNode {
        var searched_import_buf = ASTNodePtrList.init(gpa.allocator());
        defer searched_import_buf.deinit();
        var buf = ASTNodePtrList.init(gpa.allocator());
        defer buf.deinit();
        self._findDecl(name, &buf, &searched_import_buf, true, program);
        return if (buf.items.len == 0) null else buf.items[0];
    }

    pub fn findDeclAll(self: *Scope, program: *Program, name: []const u8) ASTNodePtrList {
        var searched_import_buf = ASTNodePtrList.init(gpa.allocator());
        defer searched_import_buf.deinit();
        var buf = ASTNodePtrList.init(gpa.allocator());
        self._findDecl(name, &buf, &searched_import_buf, true, program);
        return buf;
    }

    fn _findDecl(self: *Scope, name: []const u8, buf: *ASTNodePtrList, searched_imports: *ASTNodePtrList, allow_private: bool, program: *Program) void {
        for (self.imports.items) |import| {
            const already_searched = for (searched_imports.items) |prev_import| {
                if (mem.eql(u8, prev_import.node.Import.name, import.node.Import.name))
                    break true;
            } else false;
            if (already_searched)
                continue;
            if (import.node.Import.is_defiling) {
                // std.log.info("[{x}] search import {s} ({x}) for {s}", .{
                //     @intFromPtr(self),
                //     import.node.Import.name,
                //     @intFromPtr(import.node.Import.scope),
                //     name,
                // });
                import.node.Import.scope._findDecl(name, buf, searched_imports, false, program);
                searched_imports.append(import) catch unreachable;
            } else if (mem.indexOfScalar(u8, name, '/')) |n| {
                if (mem.eql(u8, name[0..n], import.node.Import.name)) {
                    // std.log.info("[{x}] search import {s} ({x}) for {s}", .{
                    //     @intFromPtr(self),
                    //     import.node.Import.name,
                    //     @intFromPtr(import.node.Import.scope),
                    //     name,
                    // });
                    import.node.Import.scope._findDecl(name[n + 1 ..], buf, searched_imports, false, program);
                    searched_imports.append(import) catch unreachable;
                }
            }
        }
        // std.log.info("[{x}] search self for {s}", .{ @intFromPtr(self), name });
        for (self.defs.items) |def| {
            const decl = def.node.Decl;
            // std.log.info("    - ... {s}", .{decl.name});
            if (!decl.is_test and (allow_private or !decl.is_private) and
                (!decl.is_targ_dampe or program.flag_dampe) and
                mem.eql(u8, decl.name, name))
            {
                buf.append(def) catch unreachable;
            }
        }
        if (self.parent) |parent| {
            // std.log.info("[{x}] search parent {x} for {s}", .{
            //     @intFromPtr(self), @intFromPtr(parent), name,
            // });
            parent._findDecl(name, buf, searched_imports, allow_private, program);
        }
    }

    pub fn findType(self: *Scope, name: []const u8, p: *Program, allow_private: bool) ?usize {
        for (self.imports.items) |import| {
            if (import.node.Import.is_defiling) {
                if (import.node.Import.scope.findType(name, p, false)) |n|
                    return n;
            } else if (mem.indexOfScalar(u8, name, '/')) |n| {
                if (mem.eql(u8, name[0..n], import.node.Import.name))
                    if (import.node.Import.scope.findType(name[n + 1 ..], p, false)) |d|
                        return d;
            }
        }
        return for (self.types.items) |type_ind| {
            const typedef = p.types.items[type_ind];
            if ((allow_private or !typedef.is_private) and
                (!typedef.is_targ_dampe or p.flag_dampe) and
                mem.eql(u8, typedef.name, name))
            {
                break type_ind;
            }
        } else if (self.parent) |parent| b: {
            break :b parent.findType(name, p, allow_private);
        } else null;
    }

    pub fn findLocal(self: *Scope, name: []const u8, p: *Program, allow_private: bool) ?*Local {
        for (self.imports.items) |import| {
            if (import.node.Import.is_defiling) {
                if (import.node.Import.scope.findLocal(name, p, false)) |n|
                    return n;
            } else if (mem.indexOfScalar(u8, name, '/')) |n| {
                if (mem.eql(u8, name[0..n], import.node.Import.name))
                    if (import.node.Import.scope.findLocal(name[n + 1 ..], p, false)) |d|
                        return d;
            }
        }
        // std.log.info("[{x}] search self for {s}", .{ @intFromPtr(self), name });
        var iter = self.locals.iterator();
        return while (iter.next()) |local| {
            // std.log.info("    - ... {s}", .{local.name});
            if (mem.eql(u8, local.name, name)) break local;
        } else if (self.parent) |parent| b: {
            // std.log.info("[{x}] search parent {x} for {s}", .{
            //     @intFromPtr(self), @intFromPtr(parent), name,
            // });
            break :b parent.findLocal(name, p, allow_private);
        } else null;
    }
};

pub const Program = struct {
    compiler_dir_path: []u8,
    compiler_dir: std.fs.Dir,

    ast: ASTNodeList,
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
    statics: Static.AList,
    types: UserType.AList,
    builtin_types: std.ArrayList(TypeInfo),
    rng: std.rand.DefaultPrng,
    breakpoints: ASTNodePtrList,
    romloc_code_end: usize = 0,
    errors: Error.AList,
    global_scope: *Scope,

    // Keep track of which files evaluated, to avoid import infinite loop
    imports: ASTNodePtrList,

    flag_dampe: bool = false,

    // Stupid hack to allow "dry-run"'ing parser funcs without keeping errors
    // around
    forget_errors: bool = false,

    pub fn init(alloc: mem.Allocator) !@This() {
        const compiler_dir_path = std.fs.selfExeDirPathAlloc(alloc) catch
            return error.CannotOpenSelfDir;
        const compiler_dir = std.fs.openDirAbsolute(compiler_dir_path, .{}) catch
            return error.CannotOpenSelfDir;

        return Program{
            .compiler_dir_path = compiler_dir_path,
            .compiler_dir = compiler_dir,
            .ast = ASTNodeList.init(alloc),
            .defs = ASTNodePtrList.init(alloc),
            .macs = ASTNodePtrList.init(alloc),
            .statics = Static.AList.init(alloc),
            .types = UserType.AList.init(alloc),
            .builtin_types = std.ArrayList(TypeInfo).init(alloc),
            .errors = common.Error.AList.init(alloc),
            .rng = std.rand.DefaultPrng.init(@intCast(std.time.timestamp())),
            .breakpoints = ASTNodePtrList.init(alloc),
            .global_scope = Scope.create(null),
            .imports = ASTNodePtrList.init(alloc),
        };
    }

    pub fn walkNodes(self: *Program, parent: ?*ASTNode, nodes: ASTNodeList, ctx: anytype, func: *const fn (*ASTNode, ?*ASTNode, *Program, @TypeOf(ctx)) Error.Set!void) Error.Set!void {
        var iter = nodes.iterator();
        while (iter.next()) |node|
            walkNode(self, parent, node, ctx, func) catch |e| switch (e) {
                error._Continue => continue,
                else => return e,
            };
    }

    pub fn walkNode(self: *Program, parent: ?*ASTNode, node: *ASTNode, ctx: anytype, func: *const fn (*ASTNode, ?*ASTNode, *Program, @TypeOf(ctx)) Error.Set!void) Error.Set!void {
        try func(node, parent, self, ctx);
        switch (node.node) {
            .None, .Asm, .Cast, .Debug, .Breakpoint, .Builtin, .Return, .Call, .GetChild, .GetIndex, .VDecl, .VDeref, .VRef, .Value, .TypeDef => {},
            .Import => |b| try walkNodes(self, node, b.body, ctx, func),
            .Decl => |b| try walkNodes(self, node, b.body, ctx, func),
            .Wild => |b| try walkNodes(self, parent, b.body, ctx, func),
            .RBlock => |b| try walkNodes(self, parent, b.body, ctx, func),
            .Quote => |b| try walkNode(self, parent, b.def, ctx, func),
            .Loop => |d| {
                switch (d.loop) {
                    .Until => |u| try walkNodes(self, parent, u.cond, ctx, func),
                    .While => |u| try walkNodes(self, parent, u.cond, ctx, func),
                }
                try walkNodes(self, parent, d.body, ctx, func);
            },
            .When => |when| {
                try walkNodes(self, parent, when.yup, ctx, func);
                if (when.nah) |n| try walkNodes(self, parent, n, ctx, func);
            },
            .Cond => |cond| {
                for (cond.branches.items) |branch| {
                    try walkNodes(self, parent, branch.cond, ctx, func);
                    try walkNodes(self, parent, branch.body, ctx, func);
                }
                if (cond.else_branch) |branch|
                    try walkNodes(self, parent, branch, ctx, func);
            },
        }
    }

    pub fn addNativeType(self: *Program, comptime T: type, name: []const u8) void {
        switch (@typeInfo(T)) {
            .Enum => |info| {
                if (info.tag_type != u16 and info.tag_type != u8)
                    @compileError("Enum type must be either u8 or u16");
                var t = UserType{
                    .node = null,
                    .name = name,
                    .scope = Scope.create(null),
                    .def = .{ .Enum = .{
                        .is_short = info.tag_type == u16,
                        .fields = std.ArrayList(UserType.EnumField).init(gpa.allocator()),
                    } },
                };
                inline for (info.fields) |field| {
                    const va: u8 = if (info.tag_type == u16) @as(u16, @intCast(field.value)) & 0xFF else @intCast(field.value);
                    const vb: ?u8 = if (info.tag_type == u16) (@as(u16, @intCast(field.value)) >> 8) & 0xFF else null;
                    t.def.Enum.fields.append(.{ .name = field.name, .value_a = va, .value_b = vb }) catch unreachable;
                }
                self.types.append(t) catch unreachable;
            },
            else => @compileError("TODO"),
        }
    }

    pub inline fn ztype(self: *const Program, ind: usize) TypeInfo {
        return self.builtin_types.items[ind];
    }

    // Record type in builtin_types if not there otherwise find existing record
    pub fn btype(self: *Program, typ: TypeInfo) usize {
        return for (self.builtin_types.items, 0..) |t, i| {
            if (t.eq(typ)) break i;
        } else b: {
            self.builtin_types.append(typ) catch unreachable;
            break :b self.builtin_types.items.len - 1;
        };
    }

    pub fn perr(self: *Program, e: parser.Parser.ParserError, srcloc: common.Srcloc, args: anytype) parser.Parser.ParserError {
        if (!self.forget_errors)
            self.errors.append(.{ .e = e, .l = srcloc, .ctx = Error.Context.from(args) }) catch unreachable;
        return e;
    }

    pub fn aerr(self: *Program, e: analyser.Error, srcloc: common.Srcloc, args: anytype) analyser.Error {
        self.errors.append(.{ .e = e, .l = srcloc, .ctx = Error.Context.from(args) }) catch unreachable;
        return e;
    }
};

pub const UserType = struct {
    node: ?*ASTNode, // null means native (i.e. compiler internal)
    name: []const u8,
    def: Def,
    scope: *Scope, // for methods etc

    is_private: bool = false,
    is_targ_dampe: bool = false,

    pub const AList = std.ArrayList(UserType);

    pub const Def = union(enum) {
        Enum: Enum,
        Device: Device,
        Struct: Struct,
        Alias: Alias,
    };

    pub const Alias = struct {
        val: TypeInfo,
    };

    pub const Device = struct {
        start: u8,
        fields: DeviceField.AList,
    };

    pub const DeviceField = struct {
        name: []const u8,
        type: TypeInfo,
        // rw: RW,
        // pub const RW = enum { R, W, RW };

        pub const AList = std.ArrayList(@This());
    };

    pub const Struct = struct {
        args: ?TypeInfo.List16 = null,
        fields: StructField.AList,
    };

    pub const StructField = struct {
        name: []const u8,
        type: TypeInfo,
        offset: u16 = 0xFFFF,

        pub const AList = std.ArrayList(@This());
    };

    pub const Enum = struct {
        is_short: bool = false,
        fields: std.ArrayList(EnumField),
    };

    pub const EnumField = struct {
        name: []const u8,
        value_a: u8,
        value_b: ?u8,

        pub const AList = std.ArrayList(@This());
    };

    pub fn deepclone(self: @This()) @This() {
        var new = self;
        switch (new.def) {
            .Struct => |*s| s.fields = s.fields.clone() catch unreachable,
            .Device => @panic("TODO"),
            .Alias, .Enum => {},
        }
        return new;
    }

    pub fn checkStructField(t: TypeInfo, is_last: bool) void {
        if (t == .Array and t.Array.count == null and !is_last) {
            @panic("Unbounded array must be at end");
        } else if (t == .Opaque) {
            @panic("Struct cannot contain bare opaque");
        }
    }
};

pub const OpTag = enum(u8) {
    Xtua = 0xff,
    Xlbl = 0xfe,
    Oraw = 0xfd,
    Obrk = 0x00,
    Oinc = 0x01,
    Opop = 0x02,
    Onip = 0x03,
    Oswp = 0x04,
    Orot = 0x05,
    Odup = 0x06,
    Oovr = 0x07,
    Oequ = 0x08,
    Oneq = 0x09,
    Ogth = 0x0a,
    Olth = 0x0b,
    Ojmp = 0x0c,
    Ojcn = 0x0d,
    Ojsr = 0x0e,
    Osth = 0x0f,
    Olda = 0x14,
    Osta = 0x15,
    Odei = 0x16,
    Odeo = 0x17,
    Oadd = 0x18,
    Osub = 0x19,
    Omul = 0x1a,
    Odiv = 0x1b,
    Oand = 0x1c,
    Oora = 0x1d,
    Oeor = 0x1e,
    Osft = 0x1f,
    Ojci = 0x20,
    Ojmi = 0x40,
    Ojsi = 0x60,
    Olit = 0x80,
    // TODO: ldz/stz/ldr/str
};

pub const Op = union(OpTag) {
    Xtua: codegen.UA,
    Xlbl: codegen.UA,
    Oraw: u8,
    Obrk,
    Oinc,
    Opop,
    Onip,
    Oswp,
    Orot,
    Odup,
    Oovr,
    Oequ,
    Oneq,
    Ogth,
    Olth,
    Ojmp,
    Ojcn,
    Ojsr,
    Osth,
    Olda,
    Osta,
    Odei,
    Odeo,
    Oadd,
    Osub,
    Omul,
    Odiv,
    Oand,
    Oora,
    Oeor,
    Osft,
    Ojci,
    Ojmi,
    Ojsi,
    Olit,

    pub const Tag = meta.Tag(Op);

    pub fn fromTag(tag: Tag) !Op {
        return switch (tag) {
            .Xlbl, .Xtua, .Oraw => error.NeedsArg,
            else => b: inline for (meta.fields(Op)) |field| {
                if (field.type == void and @unionInit(Op, field.name, {}) == tag) {
                    break :b @unionInit(Op, field.name, {});
                }
            } else unreachable,
        };
    }

    pub fn argCount(self: @This()) usize {
        return switch (self) {
            .Xtua,
            .Xlbl,
            .Oraw,
            .Olit,
            .Obrk,
            .Ojmi,
            .Ojsi,
            => 0,
            .Ojci,
            .Ojmp,
            .Ojsr,
            .Osth,
            .Oinc,
            .Olda,
            .Odei,
            .Odup,
            .Opop,
            => 1,
            .Orot => 3,
            else => 2,
        };
    }
};

pub const Ins = struct {
    stack: usize,
    keep: bool = false,
    short: bool = false,
    op: Op,

    // Generic, may be either short or not
    // Will be lowered once analyser is done on calling function
    generic: bool = false,

    // Used for codegen
    labels: StackBuffer(Label, 8) = StackBuffer(Label, 8).init(null),

    pub const List = std.ArrayList(Ins);

    pub const Label = struct {
        for_ua: codegen.UA,
    };

    pub fn format(
        value: @This(),
        comptime f: []const u8,
        options: fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        if (comptime mem.eql(u8, f, "")) {
            //
        } else {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        if (value.op == .Oraw) {
            try fmt.format(writer, "{x:0>2}", .{value.op.Oraw});
            return;
        }

        const stk: []const u8 = if (value.stack == RT_STACK) "r" else "";
        const osz: []const u8 = if (value.short) "2" else "";
        const dup: []const u8 = if (value.keep) "k" else "";
        const str = @tagName(value.op);
        for (str[1..]) |char| try writer.writeByte(std.ascii.toUpper(char));
        try writer.print("{s}{s}{s}", .{ osz, dup, stk });
    }
};

// ----------------------------------------------------------------------------

test "Op.fromTag" {
    inline for (@typeInfo(Op.Tag).Enum.fields) |variant| {
        const e = @field(Op.Tag, variant.name);
        if (Op.fromTag(e)) |v| {
            try std.testing.expectEqual(e, v);
        } else |_| {}
    }
}

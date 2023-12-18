const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const fmt = std.fmt;
const assert = std.debug.assert;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
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
            .TypeRef => |n| try writer.print("{s}<{}>", .{ s, n }),
            .Struct, .EnumLit => |n| try writer.print("{s}<{s}>", .{ s, self.prog.types.items[n].name }),
            .AnyOf => |n| try writer.print("{s}<{}>", .{ s, TypeFmt.from(self.prog.ztype(n), self.prog) }),
            .Ptr8, .Ptr16 => |n| try writer.print("{s}<{} @{}>", .{ s, TypeFmt.from(self.prog.ztype(n.typ), self.prog), n.ind }),
            else => try writer.print("{s}", .{s}),
        }
    }
};

pub const TypeInfo = union(enum) {
    Bool,
    U8,
    U16,
    Char8,
    Char16,
    EnumLit: usize,
    Struct: usize,
    AnyPtr,
    AnyPtr16,
    Ptr8: Ptr,
    Ptr16: Ptr,
    Any,
    Any8,
    Any16,
    AnyDev,
    Dev8,
    Dev16,

    // A bit unique, it's like an Expr but it's a generic
    AnyOf: usize,

    StaticPtr: usize,
    Quote,

    Expr: Expr,

    AmbigEnumLit,
    TypeRef: usize,
    Unresolved: []const u8,

    pub const Tag = meta.Tag(@This());
    pub const List16 = @import("buffer.zig").StackBuffer(@This(), 16);

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) {
            @compileError("Unknown format string: '" ++ f ++ "'");
        }

        switch (self) {
            .Struct, .EnumLit, .TypeRef => |n| try writer.print("{s}<{}>", .{ @tagName(self), n }),
            .Ptr8, .Ptr16 => |n| try writer.print("{s}<{} @{}>", .{ @tagName(self), n.typ, n.ind }),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
        try writer.print("", .{});
    }

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
        AnyOf: usize,
        Child: usize,
        Of: Of,
        // Ptr8: usize,
        Ptr16: usize,
        FieldType: FieldType,

        pub const Tag = meta.Tag(@This());

        pub const FieldType = struct {
            of: usize,
            field: []const u8,
        };

        pub const Of = struct {
            of: usize,
            // ptr because zig miscompiles otherwise
            // TODO: remove ptr after a few releases
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
                .AnySz => |ar| ar == b.AnySz,
                .USz => |ar| ar == b.USz,
                .AnyOf => |ar| ar == b.AnyOf,
                .Child => |ar| ar == b.Child,
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
                .AnySz => |ar| .{ .AnySz = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .USz => |ar| .{ .USz = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .AnyOf => |ar| .{ .AnyOf = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
                .Child => |ar| .{ .Child = program.btype(program.builtin_types.items[ar].resolveGeneric(with, program)) },
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
                .AnySz => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .USz => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .AnyOf => |ar| program.ztype(ar).getTypeRefs(buf, program),
                .Child => |ar| program.ztype(ar).getTypeRefs(buf, program),
            }
        }

        pub fn isGeneric(self: @This(), program: *Program) bool {
            return switch (self) {
                .FieldType => |f| program.builtin_types.items[f.of].isGeneric(program),
                .Of => |of| b: {
                    if (program.builtin_types.items[of.of].isGeneric(program))
                        break :b true;
                    for (of.args.constSlice()) |arg|
                        if (program.builtin_types.items[arg].isGeneric(program))
                            break :b true;
                    break :b false;
                },
                .Ptr16 => |ar| program.builtin_types.items[ar].isGeneric(program),
                .AnySz => |ar| program.builtin_types.items[ar].isGeneric(program),
                .USz => |ar| program.builtin_types.items[ar].isGeneric(program),
                .AnyOf => |ar| program.builtin_types.items[ar].isGeneric(program),
                .Child => |ar| program.builtin_types.items[ar].isGeneric(program),
            };
        }

        pub fn resolveTypeRef(
            self: @This(),
            arity: ?analyser.BlockAnalysis,
            program: *Program,
        ) analyser.Error!TypeInfo {
            return switch (self) {
                .FieldType => |ft| b: {
                    const arg_t = try program.builtin_types.items[ft.of].resolveTypeRef(arity, program);
                    if (arg_t != .Struct) {
                        std.log.err("{}: invalid argument to FieldType()", .{TypeFmt.from(arg_t, program)});
                        @panic("whups");
                    }
                    const strct_def = program.types.items[arg_t.Struct];

                    break :b for (strct_def.def.Struct.fields.items) |f| {
                        if (mem.eql(u8, ft.field, f.name)) break f.type;
                    } else @panic("no such field");
                },
                .Ptr16 => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(arity, program);
                    break :b arg_t.ptrize16(program);
                },
                .AnyOf => |anyof| b: {
                    const arg_t = try program.builtin_types.items[anyof].resolveTypeRef(arity, program);
                    if (arg_t != .Struct or
                        program.types.items[arg_t.Struct].def.Struct.args == null)
                    {
                        @panic("bruh");
                    }

                    break :b .{ .AnyOf = program.btype(arg_t) };
                },
                .USz => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(arity, program);
                    if (arg_t == .Dev8 or arg_t == .Dev16) {
                        break :b if (arg_t == .Dev8) .U8 else .U16;
                    } else if (arg_t.bits(program)) |b| {
                        break :b if (b == 16) .U16 else .U8;
                    } else {
                        //break :b .{ .Expr = .{ .AnySz = program.btype(arg_t) } };
                        break :b .Any;
                    }
                },
                .AnySz => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(arity, program);
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
                        .resolveTypeRef(arity, program);
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
                        const val = program.builtin_types.items[of.args.slice()[i]];
                        if (val.size(program) == null)
                            calculate_offset = false;
                        if (arg.doesInclude(val, program)) {
                            arg.* = try val.resolveTypeRef(arity, program);
                        } else {
                            std.log.err("Generic {} does not include {}", .{ arg, val });
                            @panic("whups");
                        }
                    }
                    const arglist = analyser.BlockAnalysis{ .args = new.def.Struct.args.? };
                    for (new.def.Struct.fields.items) |*field| {
                        field.type = try field.type.resolveTypeRef(arglist, program);
                    }
                    if (calculate_offset) {
                        var offset: u8 = 0;
                        for (new.def.Struct.fields.items) |*field| {
                            field.offset = offset;
                            offset += field.type.size(program).?;
                        }
                    }
                    new.def.Struct.args = null;
                    program.types.append(new) catch unreachable;
                    break :b .{ .Struct = program.types.items.len - 1 };
                },
                .Child => |s| b: {
                    const arg_t = try program.builtin_types.items[s].resolveTypeRef(arity, program);
                    break :b switch (arg_t) {
                        .AnyPtr, .AnyPtr16 => .Any,
                        .Ptr8, .Ptr16 => arg_t.deptrize(program),
                        else => unreachable,
                    };
                },
            };
        }
    };

    pub fn isResolvable(self: @This(), p: *Program) bool {
        return switch (self) {
            .AnyOf => |anyof| p.ztype(anyof).isResolvable(p),
            .TypeRef, .Expr, .Unresolved => true,
            else => false,
        };
    }

    pub fn getTypeRefs(a: @This(), buf: anytype, program: *Program) void {
        switch (a) {
            .TypeRef => |r| buf.append(r) catch unreachable,
            .AnyOf => |anyof| program.ztype(anyof).getTypeRefs(buf, program),
            .Expr => |e| e.getTypeRefs(buf, program),
            else => {},
        }
    }

    pub fn resolveTypeRef(a: @This(), arity: ?analyser.BlockAnalysis, program: *Program) analyser.Error!TypeInfo {
        const r: TypeInfo = switch (a) {
            .TypeRef => |r| arity.?.args.constSlice()[arity.?.args.len - r - 1],
            .AnyOf => |anyof| .{ .AnyOf = program.btype(try program.ztype(anyof).resolveTypeRef(arity, program)) },
            .Expr => |e| try e.resolveTypeRef(arity, program),
            .Unresolved => |k| for (program.types.items, 0..) |t, i| {
                if (mem.eql(u8, t.name, k)) break switch (t.def) {
                    .Enum => .{ .EnumLit = i },
                    .Struct => .{ .Struct = i },
                    .Device => @panic("TODO: Devices (use Dev8/Dev16 for now?)"),
                };
            } else return error.NoSuchType,
            else => a,
        };
        return if (r.isResolvable(program)) try r.resolveTypeRef(arity, program) else r;
    }

    pub fn eq(a: @This(), b: @This()) bool {
        if (@as(Tag, a) != @as(Tag, b)) return false;
        inline for (meta.fields(@This())) |field|
            if (mem.eql(u8, field.name, @tagName(a)))
                if (field.type == usize or field.type == void)
                    return @field(a, field.name) == @field(b, field.name)
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

    pub fn deptrize(a: @This(), program: *Program) @This() {
        const r: TypeInfo = switch (a) {
            .Ptr8 => |p| .{ .Ptr8 = .{ .typ = p.typ, .ind = p.ind - 1 } },
            .Ptr16 => |p| .{ .Ptr16 = .{ .typ = p.typ, .ind = p.ind - 1 } },
            else => unreachable,
        };
        return if (r.Ptr16.ind == 0) switch (r) {
            .Ptr8, .Ptr16 => |p| program.ztype(p.typ),
            else => unreachable,
        } else r;
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
            .AnyOf, .AnyDev, .AnyPtr16, .Any, .Any8, .Any16, .AnyPtr => with,

            .Expr => |e| .{ .Expr = e.resolveGeneric(with, program) },
            //.Ptr16 => |p| program.ztype(p.typ).resolveGeneric(with, program).ptrize16(program),
            .Ptr16 => |p| program.ztype(p.typ).resolveGeneric(program.ztype(with.Ptr16.typ), program).ptrize16(program),
            .Ptr8 => |p| .{ .Ptr8 = .{ .typ = program.btype(program.builtin_types.items[p.typ].resolveGeneric(with, program)), .ind = p.ind } },
            //.Struct => @panic("uh oh"), // (Of) expr resolved too soon
            else => self,
        };
    }

    pub fn isGeneric(self: @This(), program: *Program) bool {
        return switch (self) {
            .AnyOf, .AnyDev, .AnyPtr16, .Any, .Any8, .Any16, .AnyPtr, .TypeRef => true,
            .Expr => |e| e.isGeneric(program),
            .Ptr8, .Ptr16 => |ptr| program.ztype(ptr.typ).isGeneric(program),
            .Struct => |s| program.types.items[s].def.Struct.args != null,
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

    pub fn doesInclude(self: @This(), other: @This(), program: *const Program) bool {
        switch (self) {
            .EnumLit, .AnyOf => {},
            else => if (@as(Tag, self) == @as(Tag, other)) return true,
        }
        return switch (self) {
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
            .AnyPtr => other == .Ptr8 or other == .Ptr16,
            .AnyPtr16 => other == .Ptr16 or other == .StaticPtr,
            .EnumLit => |e| other == .EnumLit and e == other.EnumLit,
            else => false, // self == other case already handled earlier
        };
    }

    pub fn size(self: @This(), program: *const Program) ?u8 {
        return switch (self) {
            .Struct => |s| b: {
                const tstruct = program.types.items[s].def.Struct;
                const last = tstruct.fields.items[tstruct.fields.items.len - 1];
                break :b last.offset + (last.type.size(program) orelse break :b null);
            },
            else => return if (self.bits(program)) |b| b / 8 else null,
        };
    }

    pub fn bits(self: @This(), program: *const Program) ?u5 {
        return switch (self) {
            .AnyDev, .Dev8, .Dev16, .U8, .Char8, .Ptr8, .Bool, .Any8 => 8,
            .AnyPtr16, .StaticPtr, .U16, .Char16, .Ptr16, .Any16 => 16,
            .AnyPtr, .Any => null,
            .EnumLit => |e| if (program.types.items[e].def.Enum.is_short) 16 else 8,
            .AnyOf => null,
            .Struct => b: {
                const sz = self.size(program) orelse return null;
                break :b if (sz <= 2) @as(u5, @intCast(sz)) * 8 else null;
            },
            // .Expr, .Quote, .AmbigEnumLit, .TypeRef => unreachable,
            .Unresolved, .Expr, .Quote, .TypeRef => null,
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
            .U16, .Char16, .Ptr16 => self.val.u16,
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
            .Bool, .U8, .Char8, .Ptr8 => self.val.u8,
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

    pub const Tag = std.meta.Tag(ASTNode.Type);

    pub const Type = union(enum) {
        None, // Placeholder for removed ast values
        Import: Import,
        Decl: Decl, // word declaration
        Call: Call,
        Wild: Wild,
        Loop: Loop,
        When: When,
        Cond: Cond,
        Asm: Ins,
        Value: Value,
        Quote: Quote,
        Cast: Cast,
        VDecl: VDecl,
        VRef: VRef,
        VDeref: VDeref,
        GetChild: GetChild,
        TypeDef: TypeDef,
        Here,
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
                .Value => |v| try writer.print("Val<{}, {}>", .{ v.typ, v.val }),
                .Cast => |c| try writer.print("Cast<{}, {} ${?}>", .{ c.of, c.to, c.ref }),
                else => try writer.print("{s}", .{s}),
            }
        }
    };

    pub fn format(self: @This(), comptime f: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (comptime !mem.eql(u8, f, "")) @compileError("Unknown format string: '" ++ f ++ "'");
        try writer.print("{}", .{self.node});
    }

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
        },

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
            Device: DeviceDef,
            Struct: StructDef,
        },

        pub const Field = struct {
            name: []const u8,
            type: TypeInfo,
            srcloc: Srcloc,

            pub const AList = std.ArrayList(Field);
        };

        pub const DeviceDef = struct {
            start: u8,
            fields: Field.AList,
        };

        pub const StructDef = struct {
            args: ?TypeInfo.List16 = null,
            fields: Field.AList,
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
                offset: u8,
                is_short: bool,
            },
        } = .unresolved,
    };

    pub const VDecl = struct {
        name: []const u8,
        lind: usize,
        utyp: TypeInfo,
        llen: usize,
    };

    pub const VRef = struct {
        name: []const u8,
        lind: ?usize = null, // Into decl.locals
        sind: ?usize = null, // Into program.statics, set in analysis postProcess
    };

    pub const VDeref = struct {
        name: []const u8,
        lind: ?usize = null, // Into decl.locals
        sind: ?usize = null, // Into program.statics, set in analysis postProcess
    };

    pub const Wild = struct {
        arity: analyser.BlockAnalysis,
        body: ASTNodeList,
    };

    pub const Cast = struct {
        of: TypeInfo = .Any,
        to: TypeInfo,
        ref: ?usize = null,
    };

    pub const Call = struct {
        name: []const u8,
        variant: usize = 0, // 0 if not generic
        node: ?*ASTNode = null,
        goto: bool = false,
    };

    pub const When = struct {
        yup: ASTNodeList,
        nah: ?ASTNodeList,
    };

    pub const Cond = struct {
        branches: Branch.List,
        else_branch: ?ASTNodeList,

        pub const Branch = struct {
            cond: ASTNodeList,
            body: ASTNodeList,

            pub const List = std.ArrayList(Branch);
        };
    };

    pub const Loop = struct {
        loop: Loop.Type,
        body: ASTNodeList,

        pub const Type = union(enum) {
            Until: struct {
                cond: ASTNodeList,
                cond_prep: enum { Unchecked, Dup, DupShort }, // TODO: Dup2, DupShort2, etc
            },
            While: struct {
                cond: ASTNodeList,
                cond_prep: enum { Unchecked, Dup, DupShort }, // TODO: Dup2, DupShort2, etc
            },
        };
    };

    pub const Decl = struct {
        name: []const u8,
        arity: ?analyser.BlockAnalysis = null,
        analysis: analyser.BlockAnalysis = analyser.BlockAnalysis{},
        variations: ASTNodePtrList,
        body: ASTNodeList,
        variant: usize = 0,
        calls: usize = 0,
        locals: StackBuffer(Local, 8) = StackBuffer(Local, 8).init(null),
        scope: *Scope,
        is_analysed: bool = false,
        is_test: bool = false,

        pub const Local = struct {
            name: []const u8,
            ind: usize,
            rtyp: TypeInfo,
            llen: usize,
        };
    };

    pub const Quote = struct {
        body: ASTNodeList,
    };

    fn _deepcloneASTList(lst: ASTNodeList, parent: ?*Decl, program: *Program) ASTNodeList {
        var new = ASTNodeList.init(gpa.allocator());
        var itr = lst.iterator();
        while (itr.next()) |item| {
            new.append(item.deepclone(parent, program)) catch unreachable;
        }
        return new;
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
                new.Decl.variations = ASTNodePtrList.init(gpa.allocator());
                new.Decl.body = _deepcloneASTList(new.Decl.body, &new.Decl, program);
            },
            .Quote => new.Quote.body = _deepcloneASTList(new.Quote.body, parent, program),
            .Wild => new.Wild.body = _deepcloneASTList(new.Wild.body, parent, program),
            .Import => @panic("excuse me what"),
            .VDecl => {},
            .VDeref, .VRef => {},
            .GetChild, .None, .Call, .Asm, .Value, .Cast, .Debug, .Builtin, .Breakpoint, .Here, .Return => {},
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
    romloc: usize = 0xFFFF,

    pub const Type = union(enum) {
        TosShouldEq: Value,
        // TosShouldNeq: Value,
    };

    pub const AList = std.ArrayList(@This());
};

pub const Srcloc = struct {
    line: usize = 0,
    column: usize = 0,
    // file: []const u8,
};

pub const Error = struct {
    e: Set,
    l: Srcloc,

    pub const Set = error{
        _Continue, // Internal control flow for walkNodes, should never be raised
    } || parser.Parser.ParserError || analyser.Error || lexer.Lexer.LexerError;
    pub const AList = std.ArrayList(@This());
};

pub const Static = struct {
    type: TypeInfo,
    count: usize,
    default: union(enum) {
        String: String,
        None,
    },
    romloc: usize = 0xFFFF,

    pub const AList = std.ArrayList(@This());
};

pub const Scope = struct {
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
    imports: ASTNodePtrList,
    parent: ?*Scope,

    pub const AList = std.ArrayList(Scope);

    pub fn create(parent: ?*Scope) *@This() {
        const p = gpa.allocator().create(@This()) catch unreachable;
        p.* = .{
            .defs = ASTNodePtrList.init(gpa.allocator()),
            .macs = ASTNodePtrList.init(gpa.allocator()),
            .imports = ASTNodePtrList.init(gpa.allocator()),
            .parent = parent,
        };
        return p;
    }

    pub fn findDecl(self: *Scope, name: []const u8) ?*ASTNode {
        for (self.imports.items) |import| {
            if (import.node.Import.is_defiling) {
                // std.log.info("[{x}] search import {s} ({x}) for {s}", .{
                //     @intFromPtr(self),
                //     import.node.Import.name,
                //     @intFromPtr(import.node.Import.scope),
                //     name,
                // });
                if (import.node.Import.scope.findDecl(name)) |n|
                    return n;
            } else {
                if (mem.indexOfScalar(u8, name, '/')) |n|
                    if (mem.eql(u8, name[0..n], import.node.Import.name))
                        if (import.node.Import.scope.findDecl(name[n + 1 ..])) |d|
                            return d;
            }
        }
        // std.log.info("[{x}] search self for {s}", .{ @intFromPtr(self), name });
        return for (self.defs.items) |def| {
            const decl = def.node.Decl;
            // std.log.info("    - ... {s}", .{decl.name});
            if (!decl.is_test and mem.eql(u8, decl.name, name))
                break def;
        } else if (self.parent) |parent| b: {
            // std.log.info("[{x}] search parent {x} for {s}", .{
            //     @intFromPtr(self), @intFromPtr(parent), name,
            // });
            break :b parent.findDecl(name);
        } else null;
    }

    pub fn findAny(self: *Scope, name: []const u8) ?*ASTNode {
        return self.findDecl(name);
    }
};

pub const Program = struct {
    ast: ASTNodeList,
    defs: ASTNodePtrList,
    macs: ASTNodePtrList,
    statics: Static.AList,
    types: UserType.AList,
    builtin_types: std.ArrayList(TypeInfo),
    rng: std.rand.DefaultPrng,
    breakpoints: common.Breakpoint.AList,
    romloc_code_end: usize = 0,
    errors: Error.AList,
    global_scope: *Scope,

    // Keep track of which files evaluated, to avoid import infinite loop
    imports: ASTNodePtrList,

    pub fn init(alloc: mem.Allocator) @This() {
        return Program{
            .ast = ASTNodeList.init(alloc),
            .defs = ASTNodePtrList.init(alloc),
            .macs = ASTNodePtrList.init(alloc),
            .statics = Static.AList.init(alloc),
            .types = UserType.AList.init(alloc),
            .builtin_types = std.ArrayList(TypeInfo).init(alloc),
            .errors = common.Error.AList.init(alloc),
            .rng = std.rand.DefaultPrng.init(@intCast(std.time.timestamp())),
            .breakpoints = common.Breakpoint.AList.init(alloc),
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
            .None, .Asm, .Cast, .Debug, .Breakpoint, .Builtin, .Here, .Return, .Call, .GetChild, .VDecl, .VDeref, .VRef, .Value, .TypeDef => {},
            .Import => |b| try walkNodes(self, node, b.body, ctx, func),
            .Decl => |b| try walkNodes(self, node, b.body, ctx, func),
            .Wild => |b| try walkNodes(self, parent, b.body, ctx, func),
            .Quote => |b| try walkNodes(self, parent, b.body, ctx, func),
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
                var t = UserType{ .node = null, .name = name, .def = .{ .Enum = .{
                    .is_short = info.tag_type == u16,
                    .fields = std.ArrayList(UserType.EnumField).init(gpa.allocator()),
                } } };
                inline for (info.fields) |field| {
                    const va: u8 = if (info.tag_type == u16) @as(u16, @intCast(field.value)) & 0xFF else @intCast(field.value);
                    const vb: ?u8 = if (info.tag_type == u16) (@as(u16, @intCast(field.value)) >> 4) & 0xFF else null;
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

    pub fn perr(self: *Program, e: parser.Parser.ParserError, srcloc: common.Srcloc) parser.Parser.ParserError {
        self.errors.append(.{ .e = e, .l = srcloc }) catch unreachable;
        return e;
    }

    pub fn aerr(self: *Program, e: analyser.Error, srcloc: common.Srcloc) analyser.Error {
        self.errors.append(.{ .e = e, .l = srcloc }) catch unreachable;
        return e;
    }
};

pub const UserType = struct {
    node: ?*ASTNode, // null means native (i.e. compiler internal)
    name: []const u8,
    def: Def,

    pub const AList = std.ArrayList(UserType);

    pub const Def = union(enum) {
        Enum: Enum,
        Device: Device,
        Struct: Struct,
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
        offset: u8 = 0,

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
    };

    pub fn deepclone(self: @This()) @This() {
        var new = self;
        switch (new.def) {
            .Struct => |*s| {
                var new_fields = StructField.AList.init(gpa.allocator());
                for (s.fields.items) |f| new_fields.append(f) catch unreachable;
                s.fields = new_fields;
            },
            .Device => @panic("TODO"),
            .Enum => {},
        }
        return new;
    }
};

pub const OpTag = enum(u16) {
    Oraw,
    Olit,
    Osr,
    Ojmp,
    Ojcn,
    Ojsr,
    Ozj,
    Ohalt,
    Onac,
    Onip,
    Oswp,
    Orot,
    Oovr,
    Odup,
    Oroll,
    Oinc,
    Odrop,
    Oeq,
    Oneq,
    Olt,
    Ogt,
    Oeor,
    Oand,
    Oora,
    Osft,
    Odmod,
    Omul,
    Odiv,
    Oadd,
    Osub,
    Osth,
    Olda,
    Osta,
    Odei,
    Odeo,
};

pub const Op = union(OpTag) {
    Oraw: u8,
    Olit,
    Osr: ?u8,
    Ojmp,
    Ojcn,
    Ojsr,
    Ozj: ?u8,
    Ohalt,
    Onac: []const u8,
    Onip,
    Oswp,
    Orot,
    Oovr,
    Odup,
    Oroll: ?u8,
    Oinc,
    Odrop,
    Oeq,
    Oneq,
    Olt,
    Ogt,
    Oeor,
    Oand,
    Oora,
    Osft,
    Odmod,
    Omul,
    Odiv,
    Oadd,
    Osub,
    Osth,
    Olda,
    Osta,
    Odei,
    Odeo,

    pub const Tag = meta.Tag(Op);

    pub fn fromTag(tag: Tag) !Op {
        return switch (tag) {
            .Oraw, .Onac => error.NeedsArg,
            .Olit => .Olit,
            .Osr => .{ .Osr = null },
            .Ojmp => .Ojmp,
            .Ojsr => .Ojsr,
            .Ojcn => .Ojcn,
            .Ozj => .{ .Ozj = null },
            .Ohalt => .Ohalt,
            .Onip => .Onip,
            .Oswp => .Oswp,
            .Orot => .Orot,
            .Oovr => .Oovr,
            .Odup => .Odup,
            .Oroll => .{ .Oroll = null },
            .Oinc => .Oinc,
            .Odrop => .Odrop,
            .Oeq => .Oeq,
            .Oneq => .Oneq,
            .Olt => .Olt,
            .Ogt => .Ogt,
            .Oeor => .Oeor,
            .Oand => .Oand,
            .Oora => .Oora,
            .Osft => .Osft,
            .Odmod => .Odmod,
            .Omul => .Omul,
            .Odiv => .Odiv,
            .Oadd => .Oadd,
            .Osub => .Osub,
            .Osth => .Osth,
            .Olda => .Olda,
            .Osta => .Osta,
            .Odei => .Odei,
            .Odeo => .Odeo,
        };
    }

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

        switch (value) {
            .Oraw => |l| try fmt.format(writer, "{}", .{l}),
            .Ozj => |j| try fmt.format(writer, "{?}", .{j}),
            .Osr => |j| try fmt.format(writer, "{?}", .{j}),
            .Onac => |n| try fmt.format(writer, "'{s}'", .{n}),
            .Oroll => |i| try fmt.format(writer, "{?}", .{i}),
            .Omul => |a| try fmt.format(writer, "{?}", .{a}),
            .Oadd => |a| try fmt.format(writer, "{?}", .{a}),
            .Osub => |a| try fmt.format(writer, "{?}", .{a}),
            else => try fmt.format(writer, "@", .{}),
        }
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

    pub const List = std.ArrayList(Ins);

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

        const stk: []const u8 = if (value.stack == RT_STACK) "r" else " ";
        const osz: []const u8 = if (value.short) "2" else " ";
        try fmt.format(writer, "[{s}{s}] {s: <5} {}", .{
            stk, osz, @tagName(value.op), value.op,
        });
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

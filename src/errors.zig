const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const assert = std.debug.assert;

const common = @import("common.zig");
const lexer = @import("lexer.zig");
const Program = common.Program;
const Error = common.Error;
const Srcloc = common.Srcloc;
const TypeFmt = common.TypeFmt;
const gpa = &common.gpa;
const AnalysisFmt = @import("analyser.zig").AnalysisFmt;

pub fn lexerNodeToString(self: lexer.Node.Tag) []const u8 {
    return switch (self) {
        .Char8 => "char8",
        .Char16 => "char16",
        .U8 => "u8",
        .U16 => "u16",
        .I8 => "i8",
        .I16 => "i16",
        .String => "string",
        .EnumLit => "enum literal",
        .Keyword => "identifier",
        .MethodCall => "method call",
        .Var => "variable",
        .VarNum => "numerical variable",
        .Child => "child-getter",
        .ChildNum => "indicer",
        .ChildAmbig => "child-getter",
        .List => "list",
        .Quote => "quote",
        .At => unreachable,
        .Metadata => "metadata",
        .T => "t",
        .Nil => "nil",
    };
}

pub fn printError(program: *Program, e: Error, lines: []const []const u8) void {
    var stderr = std.io.getStdErr().writer();

    for (e.l.line -| 3..e.l.line) |line|
        stderr.print("\x1b[38;5;15m{: >4} |\x1b[m {s}\n", .{ line + 1, lines[line] }) catch unreachable;
    for (0..e.l.column + 4 + 2) |_|
        stderr.print(" ", .{}) catch unreachable;
    stderr.print("\x1b[91;1m^\x1b[m\n", .{}) catch unreachable;
    stderr.print("\x1b[91m{s}:\x1b[37;1m ", .{@errorName(e.e)}) catch unreachable;
    switch (e.e) {
        // Lexer stuff
        error.InvalidEnumLiteral => stderr.print("Invalid enum literal", .{}) catch unreachable,
        // HINT: must be UTF-8
        error.InvalidCharLiteral => stderr.print("Invalid character literal", .{}) catch unreachable,
        error.InvalidUtf8 => stderr.print("Invalid UTF-8 sequence", .{}) catch unreachable,
        error.IncompleteEscapeSeq => stderr.print("Incomplete escape sequence", .{}) catch unreachable,
        error.InvalidEscapeSeq => stderr.print("Invalid escape sequence", .{}) catch unreachable,
        error.UnterminatedString => stderr.print("Missing end quote", .{}) catch unreachable,
        error.InvalidToken => stderr.print("Invalid token", .{}) catch unreachable,
        // HINT: arrays are not ringbuffers, idiot
        error.InvalidSignedIndex => stderr.print("Signed type cannot be index", .{}) catch unreachable,
        error.LoneSigil => stderr.print("Lone @ or # not allowed", .{}) catch unreachable,
        error.NestedMetadata => stderr.print("Multiple # tokens not allowed", .{}) catch unreachable,
        // HINT: GitHub issue ####
        error.QuotedMetadata => stderr.print("#[] syntax not implemented", .{}) catch unreachable,
        error.UnexpectedClosingParen => stderr.print("Expected {s}", .{
            e.ctx.parentype1.?.toString(),
        }) catch unreachable,

        // Parser stuff
        error.ExpectedItems => stderr.print("Not enough arguments (min {}, got {})", .{
            e.ctx.usize1.?, e.ctx.usize2.?,
        }) catch unreachable,
        error.UnexpectedItems => stderr.print("Too many arguments (max {}, got {})", .{
            e.ctx.usize1.?, e.ctx.usize2.?,
        }) catch unreachable,
        error.ExpectedNode => stderr.print("Expected {s}, got {s}", .{
            lexerNodeToString(e.ctx.lexnodetype1.?), lexerNodeToString(e.ctx.lexnodetype2.?),
        }) catch unreachable,
        error.ExpectedValue => stderr.print("Expected value, got {}", .{
            e.ctx.lexnodetype1.?,
        }) catch unreachable,
        error.ExpectedString => stderr.print("Expected string, got {}", .{
            e.ctx.lexnodetype1.?,
        }) catch unreachable,
        error.ExpectedNum => stderr.print("Expected number value, got {}", .{
            e.ctx.lexnodetype1.?,
        }) catch unreachable,
        error.InvalidType => if (e.ctx.string1) |str| {
            stderr.print("\"{s}\" is not a valid type", .{str}) catch unreachable;
        } else {
            stderr.print("Expression cannot be parsed into type", .{}) catch unreachable;
        },
        error.InvalidMetadata => if (e.ctx.string1) |str| {
            stderr.print("\"{s}\" is not a recognized metadata", .{str}) catch unreachable;
        } else {
            stderr.print("Expression is not valid metadata", .{}) catch unreachable;
        },
        error.InvalidKeyword => stderr.print("Invalid keyword \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.InvalidEnumField => stderr.print("No such field \"{s}\" in {s}", .{
            e.ctx.string1.?, e.ctx.string2.?,
        }) catch unreachable,
        error.InvalidImport => stderr.print("Couldn't find import \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.InvalidEmbed => stderr.print("Couldn't open \"{s}\" to embed: {}", .{
            e.ctx.string1.?, e.ctx.err1 orelse error.FileDoesNotExist,
        }) catch unreachable,
        // HINT (if str == u8,u16,i8,i16,etc): Did you mean <capitalized>?
        error.NoSuchType => stderr.print("No such type \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        // HINT: type does not have a defined size
        error.InvalidFieldType => stderr.print("Type {} cannot be in container field", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        // HINT: non-generic types cannot be used as template args
        error.InvalidStructArg => stderr.print("{} is not generic", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.StupidArraySyntax => stderr.print("Just use normal []/@[] array syntax you doofus", .{}) catch unreachable,
        error.MissingQuoteArity => stderr.print("Anonymous functions require explicit arity", .{}) catch unreachable,
        // HINT: enum type can only be either U16 or U8
        error.InvalidEnumType => stderr.print("Only U16/U8/I8/I16 may be used as enum type", .{}) catch unreachable,
        error.InvalidBreakpoint => stderr.print("Unknown breakpoint type \"{s}\"", .{
            e.ctx.string1.?,
        }) catch unreachable,
        // HINT: just flatten it already
        error.InvalidNestedDefault => stderr.print("Nested default value lists don't make sense here.", .{}) catch unreachable,

        // Parser + analyser
        error.UnknownLocal => stderr.print("No such variable \"{s}\" in scope", .{
            e.ctx.string1.?,
        }) catch unreachable,
        error.UnknownIdent => stderr.print("No such function \"{s}\" in scope", .{
            e.ctx.string1.?,
        }) catch unreachable,

        // Analyser
        error.GenericNotMatching => stderr.print("Arg {} not included in parameter {}", .{
            TypeFmt.from(e.ctx.finwetype1.?, program), TypeFmt.from(e.ctx.finwetype2.?, program),
        }) catch unreachable,
        error.TypeNotMatching => stderr.print("Arg {} doesn't match parameter {}", .{
            TypeFmt.from(e.ctx.finwetype1.?, program), TypeFmt.from(e.ctx.finwetype2.?, program),
        }) catch unreachable,
        error.CannotCallMethod => stderr.print("Cannot call method on type {}", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.StructNotForStack => stderr.print("Struct {} does not fit on stack", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.CannotGetIndex => stderr.print("{} cannot be indexed", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.InvalidIndexType => stderr.print("{} cannot be used as index, only u8/u16", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        // HINT: amazing you managed to get this error
        error.IndexWouldOverflow => stderr.print("Index of {} would overflow (type size is {})", .{
            e.ctx.ushort1.?, e.ctx.ushort2.?,
        }) catch unreachable,
        error.IndexTooLarge => stderr.print("Index {} larger than container length {}", .{
            e.ctx.ushort1.?, e.ctx.ushort2.?,
        }) catch unreachable,
        error.CannotGetFieldMultiPtr => stderr.print("Cannot get field of {} (too many indirections)", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.CannotGetField => stderr.print("Cannot get field of {}", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.NoSuchField => stderr.print("No \"{s}\" in {}", .{
            e.ctx.string1.?, TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.CannotSplitIntoShort => stderr.print("Split() targets must be byte-sized", .{}) catch unreachable,
        error.CannotSplitByte => stderr.print("Split() argument must be short-sized", .{}) catch unreachable,
        error.StackMismatch => stderr.print("Stack doesn't match arity: ({s} vs {s})", .{
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
        }) catch unreachable,
        error.StackNotEmpty => stderr.print("Main and tests must end with empty stack", .{}) catch unreachable,
        error.StackBranching1 => stderr.print("Stack different across branches ({s} vs {s})", .{
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
        }) catch unreachable,
        error.StackImbalanceLoop => stderr.print("Stack changes in loop body (loop: {s}; previous: {s})", .{
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
        }) catch unreachable,
        // HINT: add else branch
        error.StackBranching2 => stderr.print("Stack at end of when clause must not change (when: {s}; previous: {s})", .{
            AnalysisFmt.from(&e.ctx.analysis1.?, program),
            AnalysisFmt.from(&e.ctx.analysis2.?, program),
        }) catch unreachable,
        // HINT (if non-arity func): stack contents must be comptime-known at this point
        error.StackUnderflow => if (e.ctx.usize1) |index| {
            stderr.print("Stack underflow (at index {})", .{index}) catch unreachable;
        } else {
            stderr.print("Stack underflow", .{}) catch unreachable;
        },
        error.ExpectedStruct => stderr.print("Expected struct type, got {}", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        error.NakedBreak => stderr.print("Break cannot occur outside of a loop.", .{}) catch unreachable,
        error.NakedContinue => stderr.print("Continue cannot occur outside of a loop.", .{}) catch unreachable,
        error.NoreturnCannotReturn => stderr.print("<noreturn> words cannot return items.", .{}) catch unreachable,
        error.UnsizedArityItem => stderr.print("{} does not have a defined size.", .{
            TypeFmt.from(e.ctx.finwetype1.?, program),
        }) catch unreachable,
        // error.Template => stderr.print("ohno {} {}", .{
        //     e.ctx.usize1.?, e.ctx.usize2.?,
        // }) catch unreachable,
        else => stderr.print("TODO: error message", .{}) catch unreachable,
    }
    stderr.print("\x1b[m\n", .{}) catch unreachable;
    stderr.print("      \x1b[36mat \x1b[m{s}:\x1b[33m{}\x1b[m:\x1b[34m{}\x1b[m\n", .{
        e.l.file, e.l.line, e.l.column,
    }) catch unreachable;
}

pub fn printErrors(program: *Program, filename: []const u8) void {
    const Buf = struct {
        path: []const u8,
        buf: []u8,
        lines: std.ArrayList([]const u8),

        const AList = std.ArrayList(@This());

        pub fn addOrGet(self: *AList, path: []const u8) []const []const u8 {
            for (self.items) |buf| {
                if (mem.eql(u8, buf.path, path))
                    return buf.lines.items;
            }

            const file = std.fs.cwd().openFile(path, .{}) catch unreachable;
            const size = file.getEndPos() catch unreachable;
            defer file.close();

            var new: @This() = undefined;
            new.path = path;
            new.buf = gpa.allocator().alloc(u8, size) catch unreachable;
            _ = file.readAll(new.buf) catch unreachable;

            new.lines = std.ArrayList([]const u8).init(gpa.allocator());
            var iter = mem.splitScalar(u8, new.buf, '\n');
            while (iter.next()) |line|
                new.lines.append(line) catch unreachable;
            self.append(new) catch unreachable();

            return self.items[self.items.len - 1].lines.items;
        }
    };

    var bufs = Buf.AList.init(gpa.allocator());
    defer bufs.deinit();
    defer for (bufs.items) |buf| {
        buf.lines.deinit();
        gpa.allocator().free(buf.buf);
    };

    _ = Buf.addOrGet(&bufs, filename);

    for (program.errors.items) |err|
        printError(program, err, Buf.addOrGet(&bufs, err.l.file));
}

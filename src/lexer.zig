const std = @import("std");
const mem = std.mem;
const activeTag = std.meta.activeTag;
const assert = std.debug.assert;

const common = @import("common.zig");
const String = common.String;
pub const NodeList = std.ArrayList(Node);

pub const Node = struct {
    node: NodeType,
    location: common.Srcloc,

    pub const NodeType = union(enum) {
        Char8: u8,
        Char16: u16,
        U8: u8,
        U16: u16,
        String: String,
        EnumLit: EnumLit,
        Keyword: []const u8,
        Var: []const u8,
        VarNum: u8,
        Child: []const u8,
        ChildNum: u8,
        List: NodeList,
        Quote: NodeList,
        At: *Node,
        T,
        Nil,
    };

    pub const EnumLit = struct {
        of: ?[]const u8,
        v: []const u8,
    };

    pub fn deinitMain(list: NodeList, alloc: mem.Allocator) void {
        var n = Node{ .node = .{ .List = list }, .location = undefined };
        n.deinit(alloc);
    }

    pub fn deinit(self: *Node, alloc: mem.Allocator) void {
        switch (self.node) {
            .At => |node| {
                node.deinit(alloc);
                alloc.destroy(node);
            },
            .String => |str| str.deinit(),
            .Var, .Child, .Keyword => |data| alloc.free(data),
            .EnumLit => |data| {
                alloc.free(data.v);
                if (data.of) |d|
                    alloc.free(d);
            },
            .Quote, .List => |list| {
                for (list.items) |*li| li.deinit(alloc);
                list.deinit();
            },
            else => {},
        }
    }
};

pub const Lexer = struct {
    input: []const u8,
    alloc: mem.Allocator,
    index: usize = 0,
    stack: Stack.List, // Lexing stack, not program one
    line: usize = 1,
    column: usize = 0,
    file: []const u8,

    pub const Stack = struct {
        type: Type,

        pub const Type = enum { Root, Paren, Bracket };

        pub const List = std.ArrayList(Stack);
    };

    const Self = @This();

    pub const LexerError = error{
        NoMatchingParen,
        UnexpectedClosingParen,
        InvalidEnumLiteral,
        InvalidCharLiteral,
        InvalidUtf8,
        BadString,
    } || std.fmt.ParseIntError || std.mem.Allocator.Error;

    pub fn init(input: []const u8, filename: []const u8, alloc: mem.Allocator) Self {
        return .{
            .file = filename,
            .input = input,
            .alloc = alloc,
            .stack = Stack.List.init(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    pub fn lexWord(self: *Self, vtype: u21, word: []const u8) LexerError!Node.NodeType {
        return switch (vtype) {
            '$', 'k', ':', '.' => blk: {
                if (self.lexWord('#', word)) |node| {
                    break :blk switch (vtype) {
                        '$' => Node.NodeType{ .VarNum = node.U8 },
                        'k' => node,
                        '.' => error.InvalidEnumLiteral, // Cannot be numeric
                        ':' => Node.NodeType{ .ChildNum = node.U8 },
                        else => unreachable,
                    };
                } else |_| {
                    if (mem.eql(u8, word, "nil") or mem.eql(u8, word, "Nil") or
                        mem.eql(u8, word, "nah") or mem.eql(u8, word, "Nah"))
                    {
                        break :blk Node.NodeType{ .Nil = {} };
                    } else if (mem.eql(u8, word, "t") or mem.eql(u8, word, "T")) {
                        break :blk Node.NodeType{ .T = {} };
                    } else {
                        const enum_clarifier = mem.indexOfScalar(u8, word, '/');
                        if (vtype == '.' and enum_clarifier != null) {
                            if (enum_clarifier.? == 0) {
                                return error.InvalidEnumLiteral;
                            }
                            const a = try self.alloc.alloc(u8, word.len - enum_clarifier.? - 1);
                            const b = try self.alloc.alloc(u8, word.len - (word.len - enum_clarifier.?));
                            @memcpy(a, word[enum_clarifier.? + 1 ..]);
                            @memcpy(b, word[0..enum_clarifier.?]);
                            break :blk Node.NodeType{ .EnumLit = .{ .v = a, .of = b } };
                        } else {
                            const s = try self.alloc.alloc(u8, word.len);
                            @memcpy(s, word);
                            break :blk switch (vtype) {
                                'k' => Node.NodeType{ .Keyword = s },
                                '.' => Node.NodeType{ .EnumLit = .{ .v = s, .of = null } },
                                ':' => Node.NodeType{ .Child = s },
                                '$' => Node.NodeType{ .Var = s },
                                else => unreachable,
                            };
                        }
                    }
                }
            },
            // Never called by lexValue, only by lexWord to check if something
            // can be parsed as a number.
            '#' => blk: {
                var base: u8 = 10;
                var offset: usize = 0;

                if (mem.startsWith(u8, word, "0x")) {
                    base = 16;
                    offset = 2;
                } else if (mem.startsWith(u8, word, "0b")) {
                    base = 2;
                    offset = 2;
                } else if (mem.startsWith(u8, word, "0o")) {
                    base = 8;
                    offset = 2;
                }

                if (mem.endsWith(u8, word, "s")) {
                    const num = try std.fmt.parseInt(u16, word[offset .. word.len - 1], base);
                    break :blk Node.NodeType{ .U16 = num };
                } else {
                    const num = try std.fmt.parseInt(u8, word[offset..], base);
                    break :blk Node.NodeType{ .U8 = num };
                }
            },
            '\'' => blk: {
                const short = mem.endsWith(u8, word, "s");

                var utf8 = (std.unicode.Utf8View.init(if (short) word[0 .. word.len - 1] else word) catch return error.InvalidUtf8).iterator();
                const encoded_codepoint = utf8.nextCodepointSlice() orelse return error.InvalidCharLiteral;
                if (utf8.nextCodepointSlice()) |_| return error.InvalidCharLiteral;
                const codepoint = std.unicode.utf8Decode(encoded_codepoint) catch return error.InvalidUtf8;
                if (short) {
                    break :blk Node.NodeType{ .Char16 = @intCast(codepoint) };
                } else {
                    break :blk Node.NodeType{ .Char8 = @intCast(codepoint % 255) };
                }
            },
            else => @panic("what were you trying to do anyway"),
        };
    }

    pub fn moar(self: *Self) void {
        self.index += 1;
        if (self.index < self.input.len and
            self.input[self.index] == '\n')
        {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn lexString(self: *Self) LexerError!Node.NodeType {
        if (self.input[self.index] != '"') {
            return error.BadString; // ERROR: Invalid string
        }

        var buf = String.init(common.gpa.allocator());
        self.moar(); // skip beginning quote

        while (self.index < self.input.len) : (self.moar()) {
            switch (self.input[self.index]) {
                '"' => {
                    self.moar();
                    return Node.NodeType{ .String = buf };
                },
                '\\' => {
                    self.moar();
                    if (self.index == self.input.len) {
                        return error.BadString; // ERROR: incomplete escape sequence
                    }

                    // TODO: \xXX, \uXXXX, \UXXXXXXXX
                    const esc: u8 = switch (self.input[self.index]) {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        'a' => '\x07',
                        '0' => '\x00',
                        't' => '\t',
                        else => return error.BadString, // ERROR: invalid escape sequence
                    };

                    buf.append(esc) catch unreachable;
                },
                else => buf.append(self.input[self.index]) catch unreachable,
            }
        }

        return error.BadString; // ERROR: unterminated string
    }

    pub fn lexValue(self: *Self, vtype: u21) LexerError!Node.NodeType {
        if (vtype != 'k' and vtype != '#')
            self.moar();
        const oldi = self.index;

        const word_end = for (self.index..self.input.len) |ind| {
            switch (self.input[ind]) {
                0x09...0x0d, 0x20, '(', ')', '[', ']', '#' => break ind,
                else => {},
            }
        } else self.input.len;

        const word = self.input[oldi..word_end];
        assert(word.len > 0);

        for (oldi..word_end - 1) |_|
            self.moar();

        return self.lexWord(vtype, word);
    }

    fn lex(self: *Self) LexerError!?Node.NodeType {
        const ch = self.input[self.index];
        return switch (ch) {
            '#' => {
                while (self.index < self.input.len and self.input[self.index] != 0x0a)
                    self.moar();
                return null;
            },
            0x09...0x0d, 0x20 => null,
            '"' => try self.lexString(),
            '$', '.', ':', '\'' => try self.lexValue(ch),
            '@' => b: {
                if (self.index == self.input.len - 1) {
                    @panic("TODO: lexer: lone @");
                }

                self.moar();
                const ptr = try self.alloc.create(Node);
                ptr.* = Node{
                    .node = (try self.lex()) orelse @panic("TODO: lexer: lone @"),
                    .location = .{ .file = self.file, .line = self.line, .column = self.column },
                };
                break :b .{ .At = ptr };
            },
            '[' => Node.NodeType{ .Quote = try self.lexList(.Bracket) },
            '(' => Node.NodeType{ .List = try self.lexList(.Paren) },
            else => try self.lexValue('k'),
        };
    }

    pub fn lexList(self: *Self, mode: Stack.Type) LexerError!NodeList {
        try self.stack.append(.{ .type = mode });
        var res = NodeList.init(self.alloc);

        // Move past the first (/[ if we're parsing a list
        if (self.stack.items.len > 1) {
            switch (mode) {
                .Root => unreachable,
                .Paren => assert(self.input[self.index] == '('),
                .Bracket => assert(self.input[self.index] == '['),
            }
            self.moar();
        }

        while (self.index < self.input.len) : (self.moar()) {
            const ch = self.input[self.index];
            switch (ch) {
                ']', ')' => {
                    const expect: Stack.Type = if (ch == ']') .Bracket else .Paren;
                    if (self.stack.items.len <= 1 or
                        self.stack.items[self.stack.items.len - 1].type != expect)
                    {
                        return error.UnexpectedClosingParen;
                    }

                    _ = self.stack.pop();
                    return res;
                },
                else => res.append(Node{
                    .node = (try self.lex()) orelse continue,
                    .location = .{ .file = self.file, .line = self.line, .column = self.column },
                }) catch return error.OutOfMemory,
            }
        }

        return res;
    }
};

const testing = std.testing;

test "basic lexing" {
    const input = "0xfe 0xf1 0xf0s fum (test :foo bar 0xAB) (12 ['ë] :0)";
    var lexer = Lexer.init(input, std.testing.allocator);
    const result = try lexer.lexList(.Root);
    defer lexer.deinit();
    defer Node.deinitMain(result, std.testing.allocator);

    try testing.expectEqual(@as(usize, 6), result.items.len);

    try testing.expectEqual(activeTag(result.items[0].node), .U8);
    try testing.expectEqual(@as(u8, 0xfe), result.items[0].node.U8);

    try testing.expectEqual(activeTag(result.items[1].node), .U8);
    try testing.expectEqual(@as(u8, 0xf1), result.items[1].node.U8);

    try testing.expectEqual(activeTag(result.items[2].node), .U16);
    try testing.expectEqual(@as(u16, 0xf0), result.items[2].node.U16);

    try testing.expectEqual(activeTag(result.items[3].node), .Keyword);
    try testing.expectEqualSlices(u8, "fum", result.items[3].node.Keyword);

    try testing.expectEqual(activeTag(result.items[4].node), .List);
    {
        const list = result.items[4].node.List.items;

        try testing.expectEqual(activeTag(list[0].node), .Keyword);
        try testing.expectEqualSlices(u8, "test", list[0].node.Keyword);

        try testing.expectEqual(activeTag(list[1].node), .Child);
        try testing.expectEqualSlices(u8, "foo", list[1].node.Child);

        try testing.expectEqual(activeTag(list[2].node), .Keyword);
        try testing.expectEqualSlices(u8, "bar", list[2].node.Keyword);

        try testing.expectEqual(activeTag(list[3].node), .U8);
        try testing.expectEqual(@as(u8, 0xAB), list[3].node.U8);
    }

    try testing.expectEqual(activeTag(result.items[5].node), .List);
    {
        const list = result.items[5].node.List.items;

        try testing.expectEqual(activeTag(list[0].node), .U8);
        try testing.expectEqual(@as(u8, 12), list[0].node.U8);

        try testing.expectEqual(activeTag(list[1].node), .Quote);
        {
            const list2 = list[1].node.Quote.items;

            try testing.expectEqual(activeTag(list2[0].node), .Char8);
            try testing.expectEqual(@as(u21, 'ë'), list2[0].node.Char8);
        }

        try testing.expectEqual(activeTag(list[2].node), .ChildNum);
        try testing.expectEqual(@as(u8, 0), list[2].node.ChildNum);
    }
}

test "enum literals" {
    const input = ".foo .bar/baz";
    var lexer = Lexer.init(input, std.testing.allocator);
    const result = try lexer.lexList(.Root);
    defer lexer.deinit();
    defer Node.deinitMain(result, std.testing.allocator);

    try testing.expectEqual(@as(usize, 2), result.items.len);

    try testing.expectEqual(activeTag(result.items[0].node), .EnumLit);
    try testing.expect(mem.eql(u8, result.items[0].node.EnumLit.v, "foo"));
    try testing.expectEqual(result.items[0].node.EnumLit.of, null);

    try testing.expectEqual(activeTag(result.items[1].node), .EnumLit);
    try testing.expect(mem.eql(u8, result.items[1].node.EnumLit.v, "baz"));
    try testing.expect(mem.eql(u8, result.items[1].node.EnumLit.of.?, "bar"));
}

test "string literals" {
    const input = "\"foo\"";
    var lexer = Lexer.init(input, std.testing.allocator);
    const result = try lexer.lexList(.Root);
    defer lexer.deinit();
    defer Node.deinitMain(result, std.testing.allocator);

    try testing.expectEqual(@as(usize, 1), result.items.len);

    try testing.expectEqual(activeTag(result.items[0].node), .String);
    try testing.expect(mem.eql(u8, result.items[0].node.String.items, "foo"));
}

test "sigils lexing" {
    const input = "bar $baz @foo @(foo) (@foo) @[@(@foo)]";
    var lexer = Lexer.init(input, std.testing.allocator);
    const result = try lexer.lexList(.Root);
    defer lexer.deinit();
    defer Node.deinitMain(result, std.testing.allocator);

    try testing.expectEqual(@as(usize, 6), result.items.len);

    try testing.expectEqual(activeTag(result.items[0].node), .Keyword);
    try testing.expectEqualSlices(u8, "bar", result.items[0].node.Keyword);

    try testing.expectEqual(activeTag(result.items[1].node), .Var);
    try testing.expectEqualSlices(u8, "baz", result.items[1].node.Var);

    try testing.expectEqual(activeTag(result.items[2].node), .At);
    try testing.expectEqual(activeTag(result.items[2].node.At.node), .Keyword);
    try testing.expectEqualSlices(u8, "foo", result.items[2].node.At.node.Keyword);

    try testing.expectEqual(activeTag(result.items[3].node), .At);
    try testing.expectEqual(activeTag(result.items[3].node.At.node), .List);
    {
        const list = result.items[3].node.At.node.List.items;

        try testing.expectEqual(activeTag(list[0].node), .Keyword);
        try testing.expectEqualSlices(u8, "foo", list[0].node.Keyword);
    }

    try testing.expectEqual(activeTag(result.items[4].node), .List);
    {
        const list = result.items[4].node.List.items;

        try testing.expectEqual(activeTag(list[0].node), .At);
        try testing.expectEqual(activeTag(list[0].node.At.node), .Keyword);
        try testing.expectEqualSlices(u8, "foo", list[0].node.At.node.Keyword);
    }

    try testing.expectEqual(activeTag(result.items[5].node), .At);
    try testing.expectEqual(activeTag(result.items[5].node.At.node), .Quote);
    {
        const list = result.items[5].node.At.node.Quote.items;
        try testing.expectEqual(activeTag(list[0].node), .At);
        try testing.expectEqual(activeTag(list[0].node.At.node), .List);
        {
            const list2 = list[0].node.At.node.List.items;
            try testing.expectEqual(activeTag(list2[0].node), .At);
            try testing.expectEqual(activeTag(list2[0].node.At.node), .Keyword);
            try testing.expectEqualSlices(u8, "foo", list2[0].node.At.node.Keyword);
        }
    }
}

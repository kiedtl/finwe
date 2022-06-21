const std = @import("std");
const mem = std.mem;
const activeTag = std.meta.activeTag;
const assert = std.debug.assert;

const String = @import("common.zig").String;

pub const NodeList = std.ArrayList(Node);

pub const Node = struct {
    node: NodeType,
    location: usize,

    pub const NodeType = union(enum) {
        T,
        Nil,
        Number: f64,
        Codepoint: u21,
        String: String,
        EnumLit: []const u8,
        Keyword: []const u8,
        Child: []const u8,
        ChildNum: f64,
        List: NodeList,
        Quote: NodeList,
    };

    pub fn deinitMain(list: NodeList, alloc: mem.Allocator) void {
        (Node{ .node = .{ .List = list }, .location = 0 }).deinit(alloc);
    }

    pub fn deinit(self: *Node, alloc: mem.Allocator) void {
        switch (self.node) {
            .String => |str| str.deinit(),
            .Keyword => |data| alloc.free(data),
            .Child => |data| alloc.free(data),
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
    stack: Stack.List,

    pub const Stack = struct {
        type: Type,

        pub const Type = enum { Root, Paren, Bracket };

        pub const List = std.ArrayList(Stack);
    };

    const Self = @This();

    const LexerError = error{
        NoMatchingParen,
        UnexpectedClosingParen,
        InvalidEnumLiteral,
        InvalidCharLiteral,
        InvalidUtf8,
    } || std.fmt.ParseIntError || std.mem.Allocator.Error;

    pub fn init(input: []const u8, alloc: mem.Allocator) Self {
        return .{
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
            'k', ':', '.' => blk: {
                if (self.lexWord('#', word)) |node| {
                    break :blk switch (vtype) {
                        'k' => node,
                        '.' => error.InvalidEnumLiteral, // Cannot be numeric
                        ':' => Node.NodeType{ .ChildNum = node.Number },
                        else => unreachable,
                    };
                } else |_| {
                    if (mem.eql(u8, word, "nil")) {
                        break :blk Node.NodeType{ .Nil = {} };
                    } else if (mem.eql(u8, word, "t")) {
                        break :blk Node.NodeType{ .T = {} };
                    } else {
                        const s = try self.alloc.alloc(u8, word.len);
                        mem.copy(u8, s, word);
                        break :blk switch (vtype) {
                            'k' => Node.NodeType{ .Keyword = s },
                            '.' => Node.NodeType{ .EnumLit = s },
                            ':' => Node.NodeType{ .Child = s },
                            else => unreachable,
                        };
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

                if (base != 10) {
                    const num = try std.fmt.parseInt(usize, word[offset..], base);
                    break :blk Node.NodeType{
                        .Number = @intToFloat(f64, num),
                    };
                } else {
                    assert(offset == 0);
                    break :blk Node.NodeType{
                        .Number = try std.fmt.parseFloat(f64, word),
                    };
                }
            },
            '\'' => blk: {
                var utf8 = (std.unicode.Utf8View.init(word) catch return error.InvalidUtf8).iterator();
                const encoded_codepoint = utf8.nextCodepointSlice() orelse return error.InvalidCharLiteral;
                if (utf8.nextCodepointSlice()) |_| return error.InvalidCharLiteral;
                const codepoint = std.unicode.utf8Decode(encoded_codepoint) catch return error.InvalidUtf8;
                break :blk Node.NodeType{ .Codepoint = codepoint };
            },
            else => @panic("what were you trying to do anyway"),
        };
    }

    pub fn lexValue(self: *Self, vtype: u21) LexerError!Node.NodeType {
        if (vtype != 'k' and vtype != '#')
            self.index += 1;
        const oldi = self.index;

        while (self.index < self.input.len) : (self.index += 1) {
            switch (self.input[self.index]) {
                0x09...0x0d, 0x20, '(', ')', '[', ']', '#' => break,
                else => {},
            }
        }

        const word = self.input[oldi..self.index];
        assert(word.len > 0);

        // lex() expects index to point to last non-word char, so move index back
        self.index -= 1;

        return self.lexWord(vtype, word);
    }

    pub fn lex(self: *Self, mode: Stack.Type) LexerError!NodeList {
        try self.stack.append(.{ .type = mode });
        var res = NodeList.init(self.alloc);

        // Move past the first (/[ if we're parsing a list
        if (self.stack.items.len > 1) {
            switch (mode) {
                .Root => unreachable,
                .Paren => assert(self.input[self.index] == '('),
                .Bracket => assert(self.input[self.index] == '['),
            }
            self.index += 1;
        }

        while (self.index < self.input.len) : (self.index += 1) {
            const ch = self.input[self.index];

            var vch = self.index;
            var v: Node.NodeType = switch (ch) {
                '#' => {
                    while (self.index < self.input.len and self.input[self.index] != 0x0a)
                        self.index += 1;
                    continue;
                },
                0x09...0x0d, 0x20 => continue,
                '.', ':', '\'' => try self.lexValue(ch),
                '[' => Node.NodeType{ .Quote = try self.lex(.Bracket) },
                '(' => Node.NodeType{ .List = try self.lex(.Paren) },
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
                else => try self.lexValue('k'),
            };

            res.append(Node{
                .node = v,
                .location = vch,
            }) catch return error.OutOfMemory;
        }

        return res;
    }
};

const testing = std.testing;

test "basic lexing" {
    const input = "0xfe 0xf1 0xf0 fum (test :foo bar 0xBEEF) (12 ['ë] :0)";
    var lexer = Lexer.init(input, std.testing.allocator);
    var result = try lexer.lex(.Root);
    defer lexer.deinit();
    defer Node.deinitMain(result, std.testing.allocator);

    try testing.expectEqual(@as(usize, 6), result.items.len);

    try testing.expectEqual(activeTag(result.items[0].node), .Number);
    try testing.expectEqual(@as(f64, 0xfe), result.items[0].node.Number);

    try testing.expectEqual(activeTag(result.items[1].node), .Number);
    try testing.expectEqual(@as(f64, 0xf1), result.items[1].node.Number);

    try testing.expectEqual(activeTag(result.items[2].node), .Number);
    try testing.expectEqual(@as(f64, 0xf0), result.items[2].node.Number);

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

        try testing.expectEqual(activeTag(list[3].node), .Number);
        try testing.expectEqual(@as(f64, 0xBEEF), list[3].node.Number);
    }

    try testing.expectEqual(activeTag(result.items[5].node), .List);
    {
        const list = result.items[5].node.List.items;

        try testing.expectEqual(activeTag(list[0].node), .Number);
        try testing.expectEqual(@as(f64, 12), list[0].node.Number);

        try testing.expectEqual(activeTag(list[1].node), .Quote);
        {
            const list2 = list[1].node.Quote.items;

            try testing.expectEqual(activeTag(list2[0].node), .Codepoint);
            try testing.expectEqual(@as(u21, 'ë'), list2[0].node.Codepoint);
        }

        try testing.expectEqual(activeTag(list[2].node), .ChildNum);
        try testing.expectEqual(@as(f64, 0), list[2].node.ChildNum);
    }
}

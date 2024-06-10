const std = @import("std");
const builtin = @import("builtin");

fn isWhitespace(c: u8) bool {
    return switch (c) {
        ' ', '\n', '\r', '\t' => true,
        else => false,
    };
}

// A very crude 'string intern' pool
pub const StringPool = struct {
    const PoolError = error{
        NoActiveString,
        StringActive,
        StringTooLong,
        StringPoppedOutOfExistence,
    };

    const maxBufferSize = 4096;
    const initialStringSize = 256;

    pub const Buffer = struct {
        buffer: []u8,
        stream: std.io.FixedBufferStream([]u8),
    };

    allocator: std.mem.Allocator,
    buffers: std.SinglyLinkedList(Buffer),
    maybeActiveStringOffset: ?usize = null,
    association: std.StringHashMap([]const u8),

    pub fn makeBufferNode(allocator: std.mem.Allocator, size: usize) !*std.SinglyLinkedList(Buffer).Node {
        const initialBuffer = try allocator.alloc(u8, size);
        const stream = std.io.fixedBufferStream(initialBuffer);

        const node = try allocator.create(std.SinglyLinkedList(Buffer).Node);
        node.data = .{ .buffer = initialBuffer, .stream = stream };

        return node;
    }

    pub fn init(allocator: std.mem.Allocator) !StringPool {
        var ll = std.SinglyLinkedList(Buffer){};
        ll.prepend(try makeBufferNode(allocator, maxBufferSize));

        return .{
            .allocator = allocator,
            .buffers = ll,
            .association = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: StringPool) void {
        while (self.buffers.popFirst()) |node| {
            self.allocator.free(node.data.buffer);
            self.allocator.destroy(node);
        }
        self.maybeActiveStringOffset = null;
        self.association.deinit();
    }

    fn activeStringOffset(self: *StringPool) !usize {
        return self.maybeActiveStringOffset orelse return PoolError.NoActiveString;
    }

    fn activeWriteStream(self: *StringPool) !*std.io.FixedBufferStream([]u8) {
        if (self.buffers.first) |first| return &first.data.stream;

        const newNode = try makeBufferNode(self.allocator, maxBufferSize);
        self.buffers.prepend(newNode);
        return &newNode.data.stream;
    }

    fn expandForString(self: *StringPool, stringLength: usize) !void {
        const writeStream = try self.activeWriteStream();
        if (try writeStream.getEndPos() - try writeStream.getPos() < stringLength) {
            const newNode = try makeBufferNode(self.allocator, maxBufferSize);
            self.buffers.prepend(newNode);
        }
    }

    pub fn startString(self: *StringPool) !void {
        if (self.maybeActiveStringOffset != null) return PoolError.StringActive;
        try self.expandForString(initialStringSize);

        const writeStream = try self.activeWriteStream();
        self.maybeActiveStringOffset = try writeStream.getPos();
    }

    pub fn addToString(self: *StringPool, c: u8) !void {
        const stringOffset = try self.activeStringOffset();
        var writeStream = try self.activeWriteStream();
        if (try writeStream.getPos() >= writeStream.buffer.len) {
            // make a new (larger) buffer in the linked list and
            // bring the string over to it
            const fitCapacityNode = try makeBufferNode(self.allocator, writeStream.buffer.len * 2);
            self.buffers.prepend(fitCapacityNode);
            const newStringSlice = fitCapacityNode.data.buffer[0..(try writeStream.getPos() - stringOffset)];
            @memcpy(newStringSlice, try self.stringSoFar());
            writeStream = &fitCapacityNode.data.stream;
        }
        try writeStream.writer().writeByte(c);
    }

    pub fn popFromString(self: *StringPool) !void {
        const stringOffset = try self.activeStringOffset();
        var writeStream = try self.activeWriteStream();
        if (try writeStream.getPos() <= stringOffset) // we've popped more characters than are in the string
            return PoolError.StringPoppedOutOfExistence;
        try writeStream.seekBy(-1);
    }

    pub fn stringSoFar(self: *StringPool) ![]const u8 {
        const stringOffset = try self.activeStringOffset();
        var writeStream = try self.activeWriteStream();
        return writeStream.buffer[stringOffset..try writeStream.getPos()];
    }

    pub fn finishString(self: *StringPool) ![]const u8 {
        const stringOffset = try self.activeStringOffset();
        var writeStream = try self.activeWriteStream();
        const str = writeStream.buffer[stringOffset..try writeStream.getPos()];
        if (self.association.get(str)) |existingString| return existingString;
        try self.association.put(str, str);
        self.maybeActiveStringOffset = null;
        return str;
    }

    pub fn reset(self: *StringPool) !void {
        const stringOffset = try self.activeStringOffset();
        var writeStream = try self.activeWriteStream();
        try writeStream.seekTo(stringOffset);
        self.maybeActiveStringOffset = null;
    }
};

pub const Lexer = struct {
    pub const Src = struct { start: usize, end: usize };
    pub const LexError = error{BadlyFormedComment};

    pub const Token = struct {
        pub const Kind = enum(i9) {
            ident = -1,
            text = -2,
            comment = -3,
            open_elem = '<',
            close_elem = '>',
            end_children = '/',
            string = '"',
            eq = '=',
            instruction = '?',
            _,
        };

        kind: Kind,
        contents: []const u8,
        source: Src,
    };

    pub fn TokenIterator(comptime Reader: type) type {
        return struct {
            const _TokenIterator = @This();

            allocator: std.mem.Allocator,
            reader: Reader,
            stringPool: StringPool,

            caret: usize = 0,

            maybeLast: ?u8 = null,
            maybeBuffered: ?u8 = null,
            inside_tag: bool = false,
            inside_comment: bool = false,

            pub fn init(allocator: std.mem.Allocator, reader: Reader, stringPool: StringPool) _TokenIterator {
                return .{
                    .allocator = allocator,
                    .reader = reader,
                    .stringPool = stringPool,
                };
            }

            pub fn deinit(self: _TokenIterator) void {
                self.stringPool.deinit();
            }

            fn readNextCharacter(self: *_TokenIterator) !u8 {
                self.caret += 1;
                if (self.maybeBuffered) |buffered| {
                    self.maybeBuffered = null;
                    try self.stringPool.addToString(buffered);
                    return buffered;
                }
                const nextCharacter = try self.reader.readByte();
                self.maybeLast = nextCharacter;
                try self.stringPool.addToString(nextCharacter);
                return nextCharacter;
            }

            fn peekNextCharacter(self: *_TokenIterator) !u8 {
                if (self.maybeBuffered) |buffered| return buffered;
                self.maybeBuffered = try self.reader.readByte();
                return self.maybeBuffered.?;
            }

            fn moveBackCharacter(self: *_TokenIterator) !void {
                if (self.maybeBuffered != null) unreachable;
                self.maybeBuffered = self.maybeLast orelse unreachable;
                self.maybeLast = null;
                self.caret -= 1;
                try self.stringPool.popFromString();
            }

            pub fn next(self: *_TokenIterator) !?Token {
                if (self.inside_tag) {
                    // whitespace is irrelevant inside tags
                    while (isWhitespace(try self.peekNextCharacter())) {
                        _ = self.readNextCharacter() catch |e| switch (e) {
                            StringPool.PoolError.NoActiveString => {},
                            else => return e,
                        };
                    }
                }

                self.stringPool.reset() catch |e| switch (e) {
                    StringPool.PoolError.NoActiveString => {},
                    else => return e,
                };

                try self.stringPool.startString();

                return (if (self.inside_tag) self.nextInTag() else self.nextText()) catch |e| switch (e) {
                    error.EndOfStream => return null,
                    else => return e,
                };
            }

            fn nextText(self: *_TokenIterator) !?Token {
                const nextChar = try self.readNextCharacter();
                switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                    Token.Kind.text,
                    Token.Kind.ident,
                    Token.Kind.comment,
                    => unreachable,
                    Token.Kind.open_elem => |tag| {
                        if (try self.peekNextCharacter() == '!') {
                            _ = try self.readNextCharacter();
                            if (try self.readNextCharacter() != '-') return LexError.BadlyFormedComment;
                            if (try self.readNextCharacter() != '-') return LexError.BadlyFormedComment;
                            return try self.readComment();
                        }
                        self.inside_tag = true;
                        return try self.readSingle(tag);
                    },
                    else => return try self.readText(),
                }
            }

            fn nextInTag(self: *_TokenIterator) !?Token {
                const nextChar = try self.readNextCharacter();
                switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                    Token.Kind.text,
                    Token.Kind.ident,
                    Token.Kind.comment,
                    => unreachable,
                    Token.Kind.open_elem,
                    Token.Kind.end_children,
                    Token.Kind.eq,
                    Token.Kind.instruction,
                    => |tag| return try self.readSingle(tag),
                    Token.Kind.close_elem => {
                        self.inside_tag = false;
                        return try self.readSingle(.close_elem);
                    },
                    Token.Kind.string => return try self.readString(),
                    _ => return try self.readIdent(),
                }
            }

            fn createTokenWithContents(self: *_TokenIterator, kind: Token.Kind, start: usize, end: usize) !Token {
                return Token{
                    .kind = kind,
                    .contents = try self.stringPool.finishString(),
                    .source = .{ .start = start, .end = end },
                };
            }

            fn createEmptyToken(self: *_TokenIterator, kind: Token.Kind, start: usize, end: usize) !Token {
                try self.stringPool.reset();
                return Token{
                    .kind = kind,
                    .contents = "",
                    .source = .{ .start = start, .end = end },
                };
            }

            fn readSingle(self: *_TokenIterator, kind: Token.Kind) !Token {
                return self.createEmptyToken(kind, self.caret, self.caret + 1);
            }

            fn readString(self: *_TokenIterator) !Token {
                const start = self.caret;
                try self.stringPool.reset();
                try self.stringPool.startString();
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.string => break,
                        else => {},
                    }
                }
                _ = try self.stringPool.popFromString();
                return self.createTokenWithContents(.string, start, self.caret);
            }

            fn readIdent(self: *_TokenIterator) !Token {
                const start = self.caret;
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    if (isWhitespace(nextChar)) {
                        try self.moveBackCharacter();
                        break;
                    }
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.text,
                        Token.Kind.ident,
                        Token.Kind.comment,
                        => unreachable,
                        .open_elem, .close_elem, .end_children, .string, .eq, .instruction => {
                            try self.moveBackCharacter();
                            break;
                        },
                        _ => continue,
                    }
                }
                return self.createTokenWithContents(.ident, start, self.caret);
            }

            fn readText(self: *_TokenIterator) !Token {
                const start = self.caret;
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.text,
                        Token.Kind.ident,
                        Token.Kind.comment,
                        => unreachable,
                        .open_elem => {
                            try self.moveBackCharacter();
                            break;
                        },
                        .close_elem, .end_children, .string, .eq, .instruction => continue,
                        _ => continue,
                    }
                }
                return self.createTokenWithContents(.text, start, self.caret);
            }

            pub fn readComment(self: *_TokenIterator) !Token {
                const start = self.caret;
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.text,
                        Token.Kind.ident,
                        Token.Kind.comment,
                        => unreachable,
                        .close_elem => {
                            if (std.mem.endsWith(u8, try self.stringPool.stringSoFar(), "-->")) {
                                try self.stringPool.popFromString(); // remove last 3 comments
                                try self.stringPool.popFromString();
                                try self.stringPool.popFromString();
                                break;
                            }
                            continue;
                        },
                        .open_elem, .end_children, .string, .eq, .instruction => continue,
                        _ => continue,
                    }
                }
                return self.createTokenWithContents(.text, start, self.caret);
            }
        };
    }

    pub fn tokenIterator(allocator: std.mem.Allocator, reader: anytype) !TokenIterator(@TypeOf(reader)) {
        const stringPool = try StringPool.init(allocator);
        return TokenIterator(@TypeOf(reader)).init(allocator, reader, stringPool);
    }
};

test Lexer {
    const str = "<?xml?><span class=\"p-4 m-2 gap-4\">hello</span>";
    var stream = std.io.fixedBufferStream(str);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var lexer = try Lexer.tokenIterator(arena.allocator(), stream.reader());
    try std.testing.expectEqual(Lexer.Token.Kind.open_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.instruction, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.ident, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.instruction, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.close_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.open_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.ident, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.ident, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.eq, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.string, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.close_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.text, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.open_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.end_children, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.ident, (try lexer.next()).?.kind);
    try std.testing.expectEqual(Lexer.Token.Kind.close_elem, (try lexer.next()).?.kind);
    try std.testing.expectEqual(str.len, lexer.caret);
}

pub const Header = struct {
    version: []const u8,
    encoding: []const u8,
};

pub const Document = struct {
    pub const AstError = error{ UnexpectedEof, UnexpectedToken, InvalidClosingTag };

    pub fn Builder(comptime Reader: type) type {
        return struct {
            const _Builder = @This();
            const TokenIterator = Lexer.TokenIterator(Reader);

            allocator: std.mem.Allocator,
            lexer: TokenIterator,

            maybeLast: ?Lexer.Token = null,
            maybeBuffered: ?Lexer.Token = null,

            fn moveNext(self: *_Builder) !Lexer.Token {
                if (self.maybeBuffered) |buffered| {
                    self.maybeBuffered = null;
                    return buffered;
                }
                const next = try self.lexer.next() orelse return AstError.UnexpectedEof;
                self.maybeLast = next;
                return next;
            }

            fn moveBack(self: *_Builder) void {
                if (self.maybeBuffered != null) unreachable;
                self.maybeBuffered = self.maybeLast orelse unreachable;
            }

            pub fn consume(self: *_Builder, kind: Lexer.Token.Kind) !Lexer.Token {
                const next = try self.moveNext();
                if (next.kind != kind) {
                    return AstError.UnexpectedToken;
                }
                return next;
            }

            pub fn consumeMaybe(self: *_Builder, kind: Lexer.Token.Kind) !?Lexer.Token {
                const next = try self.moveNext();
                if (next.kind != kind) {
                    self.moveBack();
                    return null;
                }
                return next;
            }

            pub fn buildAttribute(self: *_Builder) !?Node.Element.Attribute {
                const nameToken = try self.consumeMaybe(.ident) orelse return null;
                if (try self.consumeMaybe(.eq) != null) {
                    const val = try self.moveNext();
                    switch (val.kind) {
                        .ident, .string, .text => {
                            return .{ .name = nameToken.contents, .value = val.contents };
                        },
                        else => return AstError.UnexpectedToken,
                    }
                }
                return .{ .name = nameToken.contents, .value = "" };
            }

            pub fn buildInnerTag(self: *_Builder) !Node.Element {
                const tagName = try self.consume(.ident);
                var attributes = std.ArrayList(Node.Element.Attribute).init(self.allocator);
                while (try self.buildAttribute()) |attr| {
                    try attributes.append(attr);
                }
                const isSingle = try self.consumeMaybe(.end_children) != null;
                _ = try self.consume(.close_elem);
                return .{
                    .tagName = tagName.contents,
                    .attributes = try attributes.toOwnedSlice(),
                    .is_single = isSingle,
                    .children = &.{},
                };
            }

            pub fn buildComment(self: *_Builder) []const u8 {
                if (try self.consumeMaybe(.comment_bang) == null) return null;
                _ = try self.consume(.comment_line);
                _ = try self.consume(.comment_line);
            }

            pub fn buildEndingTag(self: *_Builder) !?[]const u8 {
                if (try self.consumeMaybe(.end_children) == null) return null;
                const tagName = try self.consume(.ident);
                _ = try self.consume(.close_elem);
                return tagName.contents;
            }

            pub fn buildChildren(self: *_Builder, maybeOpenTag: ?[]const u8) ![]Node {
                var children = std.ArrayList(Node).init(self.allocator);
                while (true) {
                    const nextToken = self.moveNext() catch |e| {
                        switch (e) {
                            AstError.UnexpectedEof => {
                                if (maybeOpenTag == null) return children.toOwnedSlice();
                                return e;
                            },
                            else => return e,
                        }
                    };
                    switch (nextToken.kind) {
                        .open_elem => {
                            if (try buildEndingTag(self)) |tagName| {
                                const openTag = maybeOpenTag orelse return AstError.InvalidClosingTag;
                                if (tagName.ptr == openTag.ptr) {
                                    return children.toOwnedSlice();
                                }
                                return AstError.InvalidClosingTag;
                            }

                            if (try self.consumeMaybe(.instruction)) |_| { // cba parsing instructions right now lol!
                                const tagName = try self.consume(.ident);
                                _ = tagName;
                                // var attributes = std.ArrayList(Node.Element.Attribute).init(self.allocator);
                                while (try self.buildAttribute()) |attr| {
                                    // try attributes.append(attr);
                                    _ = attr;
                                }
                                _ = try self.consume(.instruction);
                                _ = try self.consume(.close_elem);
                                continue;
                            }

                            var element = try self.buildInnerTag();
                            if (!element.is_single) {
                                element.children = try self.buildChildren(element.tagName);
                            }
                            try children.append(.{ .element = element });
                        },
                        .text => {
                            try children.append(.{ .text = nextToken.contents });
                        },
                        .comment => {
                            try children.append(.{ .comment = nextToken.contents });
                        },
                        else => return AstError.UnexpectedToken,
                    }
                }
            }
        };
    }

    pub const Node = union(enum(u2)) {
        pub const Element = struct {
            pub const Attribute = struct { name: []const u8, value: []const u8 };

            tagName: []const u8,
            attributes: []Attribute,
            is_single: bool,
            children: []Node,

            pub fn attributeValueByName(self: Element, name: []const u8) ?[]const u8 {
                return for (self.attributes) |_attr| {
                    if (std.mem.eql(u8, _attr.name, name)) {
                        break _attr.value;
                    }
                } else null;
            }

            pub fn attr(self: Element, name: []const u8) ?[]const u8 {
                return self.attributeValueByName(name);
            }

            pub fn elementByTagName(self: Element, tagName: []const u8) ?Element {
                return for (self.children) |node| {
                    switch (node) {
                        .element => |elemNode| {
                            if (std.mem.eql(u8, elemNode.tagName, tagName)) {
                                break elemNode;
                            }
                        },
                        else => {},
                    }
                } else null;
            }

            pub fn elem(self: Element, tagName: []const u8) ?Element {
                return self.elementByTagName(tagName);
            }

            pub fn elementsByTagNameAlloc(self: Element, allocator: std.mem.Allocator, tagName: []const u8) ![]Element {
                if (self.is_single or self.children.len == 0) return &.{};
                var arr = std.ArrayList(Element).init(allocator);
                for (self.children) |node| {
                    switch (node) {
                        .element => |elemNode| {
                            if (std.mem.eql(u8, elemNode.tagName, tagName)) {
                                try arr.append(elemNode);
                            }
                        },
                        else => {},
                    }
                }
                return try arr.toOwnedSlice();
            }

            pub fn elemsAlloc(self: Element, allocator: std.mem.Allocator, tagName: []const u8) ![]Element {
                return self.elementsByTagNameAlloc(allocator, tagName);
            }

            pub fn elementByAttributeValue(self: Element, attributeName: []const u8, attributeValue: []const u8) ?Element {
                return for (self.children) |node| {
                    switch (node) {
                        .element => |elemNode| {
                            const val = elemNode.attributeValueByName(attributeName) orelse continue;
                            if (std.mem.eql(u8, val, attributeValue)) {
                                break elemNode;
                            }
                        },
                        else => {},
                    }
                } else null;
            }

            pub fn textAlloc(self: Element, allocator: std.mem.Allocator) ![]const u8 {
                var arr = std.ArrayList(u8).init(allocator);
                for (self.children) |node| {
                    switch (node) {
                        .text => |content| {
                            try arr.appendSlice(content);
                        },
                        else => {},
                    }
                }
                return try arr.toOwnedSlice();
            }

            fn populateValueTypeShapeImpl(self: Element, comptime T: type, comptime shape: *const Shape, allocator: std.mem.Allocator, val: *T) !void {
                const resolvedTypeInfo = comptime resolveNullTypeInfo(T);
                switch (shape.*) {
                    .maybe => |maybeChild| {
                        if (@typeInfo(T) != .Optional)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with many elements. Hint: field must be optional");

                        try self.populateValueTypeShapeImpl(T, maybeChild, allocator, val);
                    },
                    .single => |singleShape| {
                        const foundElem = self.elementByTagName(singleShape.tagName) orelse if (@typeInfo(T) == .Optional) {
                            val.* = null;
                            return;
                        } else {
                            return PopulateError.MissingChild;
                        };
                        val.* = try foundElem.createValueTypeShape(T, singleShape.child, allocator);
                    },
                    .many => |manyShape| {
                        if (resolvedTypeInfo == null or resolvedTypeInfo.? != .Pointer or resolvedTypeInfo.?.Pointer.size != .Slice)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with many elements. Hint: type must be a slice");

                        const filtered = try self.elementsByTagNameAlloc(allocator, manyShape.tagName);
                        val.* = try allocator.alloc(resolvedTypeInfo.?.Pointer.child, filtered.len);
                        errdefer allocator.free(val.*);
                        for (0.., filtered) |i, element| {
                            (val.*)[i] = try element.createValueTypeShape(resolvedTypeInfo.?.Pointer.child, manyShape.child, allocator);
                        }
                    },
                    .attr => |attrShape| {
                        if (T != []const u8 and T != ?[]const u8)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element content. Hint: type must be '[]const u8'");
                        const originalValue = self.attributeValueByName(attrShape.attributeName) orelse if (@typeInfo(T) == .Optional) {
                            val.* = null;
                            return;
                        } else {
                            return PopulateError.MissingAttribute;
                        };
                        val.* = try allocator.alloc(u8, originalValue.len);
                        @memcpy(val.*, originalValue);
                    },
                    .children => |children| {
                        if (@TypeOf(T) != type)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element children. Hint: type must a struct");
                        inline for (children) |field| {
                            @field(val.*, field.fieldName) = try self.createValueTypeShape(@TypeOf(@field(val.*, field.fieldName)), field.shape, allocator);
                        }
                    },
                    .content => |contentType| {
                        if (T != []const u8 and T != ?[]const u8)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element content. Hint: type must be '[]const u8'");
                        val.* = try self.textAlloc(allocator);
                        switch (contentType) {
                            .verbatim => {},
                            .trim => {
                                val.* = std.mem.trim(u8, val.*, &std.ascii.whitespace);
                            },
                        }
                    },
                }
            }

            pub fn populateValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator, val: *T) !void {
                try self.populateValueTypeShapeImpl(T, processChild(shape), allocator, val);
            }

            pub fn populateValueType(self: Element, comptime T: type, allocator: std.mem.Allocator, val: *T) !void {
                try self.populateValueTypeShape(T, @field(T, XmlShapeIdentifier), allocator, val);
            }

            pub fn createValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator) !T {
                var a: T = undefined;
                switch (@typeInfo(T)) {
                    .Struct => {
                        inline for (std.meta.fields(T)) |field| {
                            if (field.default_value) |defaultValue| {
                                @field(a, field.name) = @as(*field.type, @ptrCast(@alignCast(defaultValue))).*;
                            }
                        }
                    },
                    else => {},
                }
                try self.populateValueTypeShape(T, shape, allocator, &a);
                return a;
            }

            pub fn createValue(self: Element, comptime T: type, allocator: std.mem.Allocator) !T {
                return self.createValueTypeShape(T, @field(T, XmlShapeIdentifier), allocator);
            }

            pub fn createValueShape(self: Element, comptime shape: anytype, allocator: std.mem.Allocator) !ShapeType(shape) {
                return self.createValueTypeShape(ShapeType(shape), allocator, shape);
            }
        };

        element: Element,
        text: []const u8,
        comment: []const u8,
    };

    root: Node.Element,

    fn builderFromStream(allocator: std.mem.Allocator, reader: anytype) !Builder(@TypeOf(reader)) {
        return Builder(@TypeOf(reader)){ .allocator = allocator, .lexer = try Lexer.tokenIterator(allocator, reader) };
    }

    pub fn fromReader(allocator: std.mem.Allocator, reader: anytype) !Document {
        var builder = try Document.builderFromStream(allocator, reader);
        return Document{ .root = .{ .tagName = "", .attributes = &.{}, .is_single = false, .children = try builder.buildChildren(null) } };
    }

    pub fn populateValueTypeShape(self: Document, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator, val: *T) !void {
        try self.root.populateValueTypeShape(T, shape, allocator, val);
    }

    pub fn populateValueType(self: Document, comptime T: type, allocator: std.mem.Allocator, val: *T) !void {
        try self.root.populateValueType(T, allocator, val);
    }

    pub fn createValueTypeShape(self: Document, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator) !T {
        return try self.root.createValueTypeShape(T, shape, allocator);
    }

    pub fn createValue(self: Document, comptime T: type, allocator: std.mem.Allocator) !T {
        return self.root.createValue(T, allocator);
    }

    pub fn createValueShape(self: Document, comptime shape: anytype, allocator: std.mem.Allocator) !ShapeType(shape) {
        return self.root.createValueTypeShape(ShapeType(shape), shape, allocator);
    }
};

test "Ast" {
    const str = "<span class=\"among us\">hello!!!<p></p><span></span></span><img src=\"hello\"/>";
    var stream = std.io.fixedBufferStream(str);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const document = try Document.fromReader(arena.allocator(), stream.reader());

    try std.testing.expectEqual(2, document.root.children.len);
    try std.testing.expectEqualStrings("span", document.root.children[0].element.tagName);
    try std.testing.expectEqual(1, document.root.children[0].element.attributes.len);
    try std.testing.expectEqualStrings("class", document.root.children[0].element.attributes[0].name);
    try std.testing.expectEqualStrings("among us", document.root.children[0].element.attributes[0].value);
    try std.testing.expectEqual(3, document.root.children[0].element.children.len);
    try std.testing.expectEqualStrings("hello!!!", document.root.children[0].element.children[0].text);
    try std.testing.expectEqualStrings("p", document.root.children[0].element.children[1].element.tagName);
    try std.testing.expectEqualStrings("span", document.root.children[0].element.children[2].element.tagName);
    try std.testing.expectEqualStrings("img", document.root.children[1].element.tagName);
    try std.testing.expectEqualStrings("src", document.root.children[1].element.attributes[0].name);
    try std.testing.expectEqualStrings("hello", document.root.children[1].element.attributes[0].value);
}

pub const OwnedDocument = struct {
    arena: std.heap.ArenaAllocator,
    doc: Document,

    pub fn deinit(self: OwnedDocument) void {
        self.arena.deinit();
    }
};

pub fn parseXml(allocator: std.mem.Allocator, reader: anytype) !OwnedDocument {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();
    const document = try Document.fromReader(arena.allocator(), reader);
    return .{ .arena = arena, .doc = document };
}

pub fn parseXmlFull(allocator: std.mem.Allocator, source: []const u8) !OwnedDocument {
    var stream = std.io.fixedBufferStream(source);
    return try parseXml(allocator, stream.reader());
}

pub const Shape = union(enum) {
    pub const Single = struct {
        tagName: []const u8,
        child: *const Shape,
    };

    pub const Many = struct {
        tagName: []const u8,
        child: *const Shape,
    };

    pub const Attr = struct {
        attributeName: []const u8,
    };

    pub const Child = struct {
        fieldName: [:0]const u8,
        shape: *const Shape,
    };

    pub const ContentMode = enum(u1) {
        verbatim,
        trim,
    };

    maybe: *const Shape,
    single: Single,
    many: Many,
    attr: Attr,
    children: []const Child,
    content: ContentMode,
};

pub fn ShapeType(comptime shape: anytype) type {
    return switch (processChild(shape).*) {
        .maybe => |maybeShape| ?ShapeType(maybeShape),
        .single => |singleShape| ShapeType(singleShape.child),
        .many => |manyShape| []ShapeType(manyShape.child),
        .attr => []const u8,
        .children => |childrenShape| blk: {
            var fields: []const std.builtin.Type.StructField = &.{};
            for (childrenShape) |child| {
                fields = fields ++ &[_]std.builtin.Type.StructField{
                    std.builtin.Type.StructField{
                        .name = child.fieldName,
                        .type = ShapeType(child.shape),
                        .alignment = @alignOf(ShapeType(child.shape)),
                        .default_value = null,
                        .is_comptime = false,
                    },
                };
            }
            break :blk @Type(
                std.builtin.Type{
                    .Struct = .{
                        .backing_integer = null,
                        .decls = &.{},
                        .fields = fields,
                        .is_tuple = false,
                        .layout = .auto,
                    },
                },
            );
        },
        .content => []const u8,
    };
}

pub const XmlShapeIdentifier = "XmlShape";

pub fn processChild(comptime child: anytype) *const Shape {
    if (@TypeOf(child) == []const u8) return &Shape{ .attr = .{.child} }; // strings are alias for attribute name
    if (@TypeOf(child) == *const Shape) return child; // accept any shape instances too
    if (@TypeOf(child) == type) {
        return processChild(@field(child, XmlShapeIdentifier));
    }

    switch (@typeInfo(@TypeOf(child))) {
        .EnumLiteral => {
            if (child.* == .content) return &Shape{.content};
        },
        .Struct => {
            var children: []const Shape.Child = &.{};
            for (std.meta.fields(@TypeOf(child))) |field| {
                children = children ++ &[_]Shape.Child{Shape.Child{ .fieldName = field.name, .shape = @field(child, field.name) }};
            }
            return &Shape{ .children = children };
        },
        else => @compileError("Invalid XML shape: " ++ @typeName(@TypeOf(child.*))),
    }
}

pub fn maybe(comptime child: anytype) *const Shape {
    return &Shape{ .maybe = processChild(child) };
}
pub fn @"?"(comptime child: anytype) *const Shape {
    return maybe(child);
}

pub fn singleElement(comptime tagName: []const u8, comptime child: anytype) *const Shape {
    return &Shape{ .single = .{ .tagName = tagName, .child = processChild(child) } };
}
pub fn @"<>"(comptime tagName: []const u8, comptime child: anytype) *const Shape {
    return singleElement(tagName, child);
}
pub fn @"?<>"(comptime tagName: []const u8, comptime child: anytype) *const Shape {
    return maybe(singleElement(tagName, child));
}

pub fn manyElements(comptime tagName: []const u8, comptime child: anytype) *const Shape {
    return &Shape{ .many = .{ .tagName = tagName, .child = processChild(child) } };
}
pub fn @"[]"(comptime tagName: []const u8, comptime child: anytype) *const Shape {
    return manyElements(tagName, child);
}

pub fn attribute(comptime attributeName: []const u8) *const Shape {
    return &Shape{ .attr = .{ .attributeName = attributeName } };
}
pub fn @"$"(comptime attributeName: []const u8) *const Shape {
    return attribute(attributeName);
}
pub fn @"?$"(comptime attributeName: []const u8) *const Shape {
    return maybe(attribute(attributeName));
}

pub fn elementContent(contentMode: Shape.ContentMode) *const Shape {
    return &Shape{ .content = contentMode };
}
pub fn @"*"(contentMode: Shape.ContentMode) *const Shape {
    return elementContent(contentMode);
}

const PopulateError = error{ MissingAttribute, MissingChild };

pub fn resolveNullTypeInfo(comptime T: type) ?std.builtin.Type {
    if (@typeInfo(T) == .Optional) {
        return @typeInfo(@typeInfo(T).Optional.child);
    }
    return @typeInfo(T);
}

pub const Pet = struct {
    pub const XmlShape = .{
        .name = elementContent(.trim),
        .colour = maybe(attribute("colour")),
        .animal = attribute("animal"),
    };

    name: []const u8,
    colour: ?[]const u8,
    animal: []const u8,
};

pub const Person = struct {
    pub const XmlShape = .{
        .name = elementContent(.trim),
        .age = attribute("age"),
        .pets = singleElement("pets", manyElements("pet", Pet)),
    };

    name: []const u8,
    age: []const u8,
    pets: []Pet,
};

pub const Register = struct {
    pub const XmlShape = .{
        .people = singleElement("people", manyElements("person", Person)),
    };

    people: []Person,
};

test "create xml values" {
    const xml =
        \\<people>
        \\    <person age="probably like 30 something">
        \\        Andrew Kelley
        \\
        \\        <pets>
        \\            <pet animal="ziguana">Zero</pet>
        \\            <pet animal="ziguana">Ziggy</pet>
        \\        </pets>
        \\    </person>
        \\    <person age="24">
        \\        Edward the Confessor
        \\
        \\        <pets>
        \\            <pet animal="dog" colour="red">Barney</pet>
        \\            <pet animal="cat" colour="white">Whitepaws</pet>
        \\        </pets>
        \\    </person>
        \\</people>
    ;

    var stream = std.io.fixedBufferStream(xml);
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const document = try Document.fromReader(arena.allocator(), stream.reader());

    const person = try document.createValue(Register, arena.allocator());

    try std.testing.expectEqual(person.people.len, 2);

    try std.testing.expectEqualStrings(person.people[0].age, "probably like 30 something");
    try std.testing.expectEqualStrings(person.people[0].name, "Andrew Kelley");
    try std.testing.expectEqual(person.people[0].pets.len, 2);
    try std.testing.expectEqualStrings(person.people[0].pets[0].animal, "ziguana");
    try std.testing.expectEqual(person.people[0].pets[0].colour, null);
    try std.testing.expectEqualStrings(person.people[0].pets[0].name, "Zero");
    try std.testing.expectEqualStrings(person.people[0].pets[1].animal, "ziguana");
    try std.testing.expectEqual(person.people[0].pets[1].colour, null);
    try std.testing.expectEqualStrings(person.people[0].pets[1].name, "Ziggy");

    try std.testing.expectEqualStrings(person.people[1].age, "24");
    try std.testing.expectEqualStrings(person.people[1].name, "Edward the Confessor");
    try std.testing.expectEqual(person.people[1].pets.len, 2);
    try std.testing.expectEqualStrings(person.people[1].pets[0].animal, "dog");
    try std.testing.expectEqualStrings(person.people[1].pets[0].colour.?, "red");
    try std.testing.expectEqualStrings(person.people[1].pets[0].name, "Barney");
    try std.testing.expectEqualStrings(person.people[1].pets[1].animal, "cat");
    try std.testing.expectEqualStrings(person.people[1].pets[1].colour.?, "white");
    try std.testing.expectEqualStrings(person.people[1].pets[1].name, "Whitepaws");
}

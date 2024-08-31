const std = @import("std");
const builtin = @import("builtin");

fn isWhitespace(c: u8) bool {
    return switch (c) {
        ' ', '\n', '\r', '\t' => true,
        else => false,
    };
}

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
            str: std.ArrayList(u8),

            caret: usize = 0,

            maybeLast: ?u8 = null,
            maybeBuffered: ?u8 = null,
            inside_tag: bool = false,
            inside_comment: bool = false,

            pub fn init(allocator: std.mem.Allocator, reader: Reader) !_TokenIterator {
                return .{
                    .allocator = allocator,
                    .reader = reader,
                    .str = try std.ArrayList(u8).initCapacity(allocator, 4096),
                };
            }

            pub fn deinit(self: _TokenIterator) void {
                self.str.deinit();
            }

            fn readNextCharacter(self: *_TokenIterator) !u8 {
                self.caret += 1;
                if (self.maybeBuffered) |buffered| {
                    self.maybeBuffered = null;
                    return buffered;
                }
                const nextCharacter = try self.reader.readByte();
                self.maybeLast = nextCharacter;
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
            }

            pub fn next(self: *_TokenIterator) !?Token {
                if (self.inside_tag) {
                    // whitespace is irrelevant inside tags
                    while (isWhitespace(try self.peekNextCharacter())) {
                        _ = try self.readNextCharacter();
                    }
                }

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
                            return try self.readComment(nextChar);
                        }
                        self.inside_tag = true;
                        return try self.readSingle(tag);
                    },
                    else => return try self.readText(nextChar),
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
                    _ => return try self.readIdent(nextChar),
                }
            }

            fn readSingle(self: *_TokenIterator, kind: Token.Kind) !Token {
                return Token{
                    .kind = kind,
                    .contents = &.{},
                    .source = .{ .start = self.caret, .end = self.caret + 1 },
                };
            }

            fn readString(self: *_TokenIterator) !Token {
                const start = self.caret;
                self.str.clearRetainingCapacity();
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.string => break,
                        else => {
                            try self.str.append(nextChar);
                        },
                    }
                }
                const contents = try self.allocator.alloc(u8, self.str.items.len);
                @memcpy(contents, self.str.items);
                return Token{
                    .kind = .string,
                    .contents = contents,
                    .source = .{ .start = start, .end = self.caret },
                };
            }

            fn readIdent(self: *_TokenIterator, char: u8) !Token {
                const start = self.caret;
                self.str.clearRetainingCapacity();
                try self.str.append(char);
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    if (isWhitespace(nextChar)) break;
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.text,
                        Token.Kind.ident,
                        Token.Kind.comment,
                        => unreachable,
                        .open_elem, .close_elem, .end_children, .string, .eq, .instruction => {
                            try self.moveBackCharacter();
                            break;
                        },
                        _ => {},
                    }
                    try self.str.append(nextChar);
                }
                const contents = try self.allocator.alloc(u8, self.str.items.len);
                @memcpy(contents, self.str.items);
                return Token{
                    .kind = .ident,
                    .contents = contents,
                    .source = .{ .start = start, .end = self.caret },
                };
            }

            fn readText(self: *_TokenIterator, char: u8) !Token {
                const start = self.caret;
                self.str.clearRetainingCapacity();
                try self.str.append(char);
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
                        .close_elem, .end_children, .string, .eq, .instruction => {},
                        _ => {},
                    }
                    try self.str.append(nextChar);
                }
                const contents = try self.allocator.alloc(u8, self.str.items.len);
                @memcpy(contents, self.str.items);
                return Token{
                    .kind = .text,
                    .contents = contents,
                    .source = .{ .start = start, .end = self.caret },
                };
            }

            pub fn readComment(self: *_TokenIterator, char: u8) !Token {
                const start = self.caret;
                self.str.clearRetainingCapacity();
                try self.str.append(char);
                while (true) {
                    const nextChar = try self.readNextCharacter();
                    switch (@as(Token.Kind, @enumFromInt(nextChar))) {
                        Token.Kind.text,
                        Token.Kind.ident,
                        Token.Kind.comment,
                        => unreachable,
                        .close_elem => {
                            if (std.mem.bytesToValue(u16, self.str.items[self.str.items.len - 2 ..]) == comptime std.mem.bytesToValue(u16, "--")) {
                                _ = self.str.pop();
                                _ = self.str.pop();
                                break;
                            }
                        },
                        .open_elem, .end_children, .string, .eq, .instruction => {},
                        _ => {},
                    }
                    try self.str.append(nextChar);
                }
                const contents = try self.allocator.alloc(u8, self.str.items.len);
                @memcpy(contents, self.str.items);
                return Token{
                    .kind = .comment,
                    .contents = contents,
                    .source = .{ .start = start, .end = self.caret },
                };
            }
        };
    }

    pub fn tokenIterator(allocator: std.mem.Allocator, reader: anytype) !TokenIterator(@TypeOf(reader)) {
        return try TokenIterator(@TypeOf(reader)).init(allocator, reader);
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
                                if (std.mem.eql(u8, tagName, openTag)) {
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

            const PopulateError = error{ MissingAttribute, MissingChild, NoOptionsUsed };

            fn populateValueTypeShapeImpl(self: Element, comptime T: type, comptime path: []const *const Shape, allocator: std.mem.Allocator, val: *T) (std.mem.Allocator.Error || PopulateError)!void {
                const shape = path[path.len - 1];
                const resolvedTypeInfo = @typeInfo(resolveNullType(T));
                const isNullable = @typeInfo(T) == .optional;
                switch (shape.*) {
                    .maybe => |maybeChild| {
                        if (@typeInfo(T) != .optional)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with an optional value. Hint: field must be optional");

                        try self.populateValueTypeShapeImpl(T, path ++ .{maybeChild}, allocator, val);
                    },
                    .single => |singleShape| {
                        const foundElem = self.elementByTagName(singleShape.tagName) orelse if (@typeInfo(T) == .optional) {
                            val.* = null;
                            return;
                        } else {
                            return PopulateError.MissingChild;
                        };
                        val.* = try foundElem.createValueTypeShapeImpl(T, path ++ .{singleShape.child}, allocator);
                    },
                    .many => |manyShape| {
                        if (resolvedTypeInfo != .pointer or resolvedTypeInfo.pointer.size != .Slice)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with many elements. Hint: type must be a slice");

                        const filtered = try self.elementsByTagNameAlloc(allocator, manyShape.tagName);
                        const out = try allocator.alloc(resolvedTypeInfo.pointer.child, filtered.len);
                        errdefer allocator.free(out);
                        for (0.., filtered) |i, element| {
                            out[i] = try element.createValueTypeShapeImpl(resolvedTypeInfo.pointer.child, path ++ .{manyShape.child}, allocator);
                        }
                        val.* = out;
                    },
                    .attr => |attributeName| {
                        if (T != []const u8 and T != ?[]const u8)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element content. Hint: type must be '[]const u8'");
                        const originalValue = self.attributeValueByName(attributeName) orelse if (isNullable) {
                            val.* = null;
                            return;
                        } else {
                            return PopulateError.MissingAttribute;
                        };
                        const allocated = try allocator.alloc(u8, originalValue.len);
                        @memcpy(allocated, originalValue);
                        val.* = allocated;
                    },
                    .children => |children| {
                        if (@TypeOf(T) != type)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element children. Hint: type must a struct");
                        var out: T = undefined;
                        inline for (children) |field| {
                            @field(out, field.fieldName) = try self.createValueTypeShapeImpl(@TypeOf(@field(val.*, field.fieldName)), path ++ .{field.shape}, allocator);
                        }
                        val.* = out;
                    },
                    .pattern => |orderedShapes| {
                        var i: usize = 0;
                        if (resolvedTypeInfo != .@"struct" or !resolvedTypeInfo.@"struct".is_tuple)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with pattern. Hint: type must be a tuple");
                        if (self.children.len < orderedShapes.len) {
                            val.* = if (isNullable) null else return PopulateError.MissingChild;
                            return;
                        }
                        val.* = while (i < self.children.len - orderedShapes.len) : (i += 1) {
                            const childrenSlice = self.children[i .. i + orderedShapes.len];
                            var tuple: resolveNullType(T) = undefined;
                            var done = true;
                            blk: inline for (0.., orderedShapes) |j, orderedShape| {
                                const fakeSubElement = Element{ .tagName = self.tagName, .attributes = self.attributes, .children = childrenSlice[j .. j + 1], .is_single = self.is_single };
                                if (try fakeSubElement.createValueTypeShapeImpl(?resolvedTypeInfo.@"struct".fields[j].type, path ++ .{orderedShape}, allocator)) |branchVal| {
                                    tuple[j] = branchVal;
                                } else {
                                    done = false; // does not match
                                    break :blk;
                                }
                            }
                            if (done) break tuple;
                        } else if (isNullable) null else return PopulateError.MissingChild;
                    },
                    .disj => |disjShapes| {
                        const differentTypes = comptime blk: {
                            var shapeTypes: []const type = &.{};
                            for (disjShapes) |branch| {
                                shapeTypes = shapeTypes ++ &[_]type{ShapeType(branch)};
                            }
                            break :blk !std.mem.allEqual(type, shapeTypes, shapeTypes[0]);
                        };
                        if (differentTypes or resolvedTypeInfo == .@"union") {
                            if (resolvedTypeInfo != .@"union")
                                @compileError("Cannot populate type " ++ @typeName(T) ++ " with disjunction where the types may be different. Hint: type must be a union");
                            const unionFields = resolvedTypeInfo.@"union".fields;
                            if (unionFields.len != disjShapes.len) {
                                @compileError("Cannot populate type " ++ @typeInfo(T) ++ "with the given disjunction, as there a mismatched number of union fields to branches");
                            }
                            inline for (0.., disjShapes) |i, branchShape| {
                                if (try self.createValueTypeShapeImpl(?unionFields[i].type, path ++ .{branchShape}, allocator)) |branchVal| {
                                    val.* = @unionInit(T, unionFields[i].name, branchVal);
                                    return;
                                }
                            }
                        } else {
                            inline for (disjShapes) |branchShape| {
                                if (try self.createValueTypeShapeImpl(?T, path ++ .{branchShape}, allocator)) |branchVal| {
                                    val.* = branchVal;
                                    return;
                                }
                            }
                        }
                        if (isNullable) {
                            val.* = null;
                            return;
                        }
                        return PopulateError.NoOptionsUsed;
                    },
                    .link => |steps| {
                        if (resolvedTypeInfo != .pointer) @compileError("Cannot populate type " ++ @typeName(T) ++ " with a deferred shape. Hint: type must be a pointer to avoid dependency loops");
                        const allocated = try allocator.create(resolvedTypeInfo.pointer.child);
                        allocated.* = try self.createValueTypeShapeImpl(resolvedTypeInfo.pointer.child, path[0 .. path.len - steps], allocator);
                        val.* = allocated;
                    },
                    .content => |contentType| {
                        if (T != []const u8 and T != ?[]const u8)
                            @compileError("Cannot populate type " ++ @typeName(T) ++ " with element content. Hint: type must be '[]const u8'");
                        var allocated = try self.textAlloc(allocator);
                        switch (contentType) {
                            .verbatim => {},
                            .trim => {
                                allocated = std.mem.trim(u8, allocated, &std.ascii.whitespace);
                            },
                        }
                        val.* = allocated;
                    },
                }
            }

            fn createValueTypeShapeImpl(self: Element, comptime T: type, comptime path: []const *const Shape, allocator: std.mem.Allocator) !T {
                var a: T = undefined;
                switch (@typeInfo(T)) {
                    .@"struct" => {
                        inline for (std.meta.fields(T)) |field| {
                            if (field.default_value) |defaultValue| {
                                @field(a, field.name) = @as(*field.type, @ptrCast(@alignCast(defaultValue))).*;
                            }
                        }
                    },
                    else => {},
                }
                try self.populateValueTypeShapeImpl(T, path, allocator, &a);
                return a;
            }

            pub fn populateValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator, val: *T) !void {
                try self.populateValueTypeShapeImpl(T, &.{processChild(shape)}, allocator, val);
            }

            pub fn populateValueType(self: Element, comptime T: type, allocator: std.mem.Allocator, val: *T) !void {
                try self.populateValueTypeShape(T, @field(T, XmlShapeIdentifier), allocator, val);
            }

            pub fn createValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator) !T {
                var a: T = undefined;
                switch (@typeInfo(T)) {
                    .@"struct" => {
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
    attr: []const u8,
    children: []const Child,
    pattern: []const *const Shape,
    disj: []const *const Shape,
    link: usize,
    content: ContentMode,
};

pub fn ShapeTypeImpl(comptime shape: anytype, comptime path: []const *const Shape) type {
    const childPath = path ++ .{shape};
    return switch (processChild(shape).*) {
        .maybe => |maybeShape| ?ShapeTypeImpl(maybeShape, childPath),
        .single => |singleShape| ShapeTypeImpl(singleShape.child, childPath),
        .many => |manyShape| []ShapeTypeImpl(manyShape.child, childPath),
        .attr => []const u8,
        .children => |childrenShape| blk: {
            var fields: []const std.builtin.Type.StructField = &.{};
            for (childrenShape) |child| {
                fields = fields ++ &[_]std.builtin.Type.StructField{
                    std.builtin.Type.StructField{
                        .name = child.fieldName,
                        .type = ShapeTypeImpl(child.shape, childPath),
                        .alignment = @alignOf(ShapeTypeImpl(child.shape, childPath)),
                        .default_value = null,
                        .is_comptime = false,
                    },
                };
            }
            break :blk @Type(
                std.builtin.Type{
                    .@"struct" = .{
                        .backing_integer = null,
                        .decls = &.{},
                        .fields = fields,
                        .is_tuple = false,
                        .layout = .auto,
                    },
                },
            );
        },
        .pattern => |orderedShapes| blk: {
            var fields: []const std.builtin.Type.StructField = &.{};
            for (0.., orderedShapes) |i, child| {
                fields = fields ++ &[_]std.builtin.Type.StructField{
                    std.builtin.Type.StructField{
                        .name = std.fmt.comptimePrint("{}", .{i}),
                        .type = ShapeTypeImpl(child, childPath),
                        .alignment = @alignOf(ShapeTypeImpl(child, childPath)),
                        .default_value = null,
                        .is_comptime = false,
                    },
                };
            }
            break :blk @Type(
                std.builtin.Type{
                    .@"struct" = .{
                        .backing_integer = null,
                        .decls = &.{},
                        .fields = fields,
                        .is_tuple = true,
                        .layout = .auto,
                    },
                },
            );
        },
        .disj => |disjShapes| blk: {
            var shapeTypes: []type = &.{};
            for (disjShapes) |branch| {
                shapeTypes = shapeTypes ++ &[_]type{ShapeTypeImpl(branch, childPath)};
            }
            if (std.mem.allEqual(type, shapeTypes, shapeTypes[0])) {
                return shapeTypes[0];
            }
            var fields: []std.builtin.Type.UnionField = &.{};
            for (0.., shapeTypes) |i, shapeType| {
                fields = fields ++ &[]std.builtin.Type.UnionField{std.builtin.Type.UnionField{ .name = std.fmt.comptimePrint("{s}", .{i}), .type = shapeType, .alignment = @alignOf(shapeType) }};
            }
            break :blk @Type(
                std.builtin.Type{
                    .@"union" = .{ .tag_type = null, .layout = .auto, .decls = &.{}, .fields = fields },
                },
            );
        },
        .link => *anyopaque, // todo: try to reference the actual previous type
        .content => []const u8,
    };
}

pub fn ShapeType(comptime shape: anytype) type {
    return ShapeTypeImpl(shape, &.{});
}

pub const XmlShapeIdentifier = "XmlShape";

pub fn processChild(comptime child: anytype) *const Shape {
    if (@TypeOf(child) == []const u8) return &Shape{ .attr = .{.child} }; // strings are alias for attribute name
    if (@TypeOf(child) == *const Shape) return child; // accept any shape instances too
    if (@TypeOf(child) == type) {
        return processChild(@field(child, XmlShapeIdentifier));
    }

    switch (@typeInfo(@TypeOf(child))) {
        .enum_literal => {
            if (child.* == .content) return &Shape{.content};
        },
        .@"struct" => {
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
    const shape = processChild(child);
    if (shape.* == .maybe) @compileError("Cannot nest 'maybe'");
    return &Shape{ .maybe = shape };
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
    return &Shape{ .attr = attributeName };
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

pub fn pattern(comptime children: anytype) *const Shape {
    var shapes: []const *const Shape = &.{};
    for (children) |child| {
        const shape = processChild(child);
        if (shape.* == .many or (shape.* == .maybe and shape.maybe.* == .many))
            @compileError("Cannot have 'many elements' shape in a pattern");
        shapes = shapes ++ &[_]*const Shape{shape};
    }
    return &Shape{ .pattern = shapes };
}
pub fn @"%"(comptime children: anytype) *const Shape {
    return pattern(children);
}

pub fn disjunction(comptime branches: anytype) *const Shape {
    if (branches.len < 2) @compileError("Disjunction needs at least two branches");

    var shapes: []const *const Shape = &.{};
    for (branches) |branch| {
        shapes = shapes ++ &[_]*const Shape{processChild(branch)};
    }
    return &.{ .disj = shapes };
}
pub fn @"or"(comptime branches: anytype) *const Shape {
    return disjunction(branches);
}

pub fn link(steps: usize) *const Shape {
    return &.{ .link = steps };
}

fn resolveNullType(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .optional => |opt| opt.child,
        else => T,
    };
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

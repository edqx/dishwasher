const std = @import("std");
const Scanner = @import("./Scanner.zig");

const Error = error{XmlDefect};

pub const Diagnostics = struct {
    pub const Range = struct {
        start: usize,
        end: usize,
    };

    pub const Defect = struct {
        pub const Kind = enum {
            missing_tag_name,
            tag_never_opened,
            tag_never_closed,
        };

        kind: Kind,
        range: Range,
    };

    defects: std.ArrayList(Defect),

    pub fn init(allocator: std.mem.Allocator) Diagnostics {
        return .{
            .defects = std.ArrayList(Defect).init(allocator),
        };
    }

    pub fn deinit(self: Diagnostics) void {
        self.defects.deinit();
    }

    fn buildRange(tokens: []const Scanner.Token) Range {
        std.debug.assert(tokens.len > 0);

        var min: usize = std.math.maxInt(usize);
        var max: usize = 0;
        for (tokens) |token| {
            if (token.start_pos < min) min = token.start_pos;
            if (token.end_pos > max) max = token.end_pos;
        }

        std.debug.assert(max != std.math.maxInt(usize));
        std.debug.assert(max >= min);

        return .{ .start = min, .end = max };
    }

    pub fn reportDefect(self: *Diagnostics, defectKind: Defect.Kind, tokens: []const Scanner.Token) !void {
        try self.defects.append(.{
            .kind = defectKind,
            .range = buildRange(tokens),
        });
    }

    pub fn hasDefect(self: Diagnostics) bool {
        return self.defects.items.len > 0;
    }
};

pub const Tree = struct {
    pub const Owned = struct {
        arena: std.heap.ArenaAllocator,
        tree: Tree,

        pub fn deinit(self: Owned) void {
            self.arena.deinit();
        }
    };

    pub const Node = union(enum) {
        pub const Elem = struct {
            pub const Attr = struct {
                name: []const u8,
                value: ?[]const u8,
            };

            tag_name: []const u8,
            attributes: []const Attr,
            tree: ?Tree,

            // Get an attribute given its name.
            pub fn attributeByName(self: Elem, needle: []const u8) ?Attr {
                return for (self.attributes) |attribute| {
                    if (std.mem.eql(u8, attribute.name, needle)) {
                        break attribute;
                    }
                } else null;
            }

            // Alias for attributeByName
            pub fn attr(self: Elem, needle: []const u8) ?Attr {
                return try self.attributeByName(needle);
            }

            // Get the value of an attribute given its name. Note that if the
            // attribute has no value, e.g., <button disabled> this will
            // still return null. Use attr or attributeByName in those
            // cases.
            pub fn attributeValueByName(self: Elem, needle: []const u8) ?[]const u8 {
                return (self.attributeByName(needle) orelse return null).value;
            }

            // Alias for attributeValueByName
            pub fn attrValue(self: Elem, needle: []const u8) ?[]const u8 {
                return try self.attributeValueByName(needle);
            }
        };

        pub const Text = struct {
            contents: []const u8,

            // Return the text without any whitespace at the beginning or end.
            pub fn trimmed(self: Text) []const u8 {
                return std.mem.trim(u8, self.contents, &std.ascii.whitespace);
            }
        };

        pub const Comment = struct {
            contents: []const u8,
        };

        elem: Elem,
        text: Text,
        comment: Comment,
    };

    children: []const Node,

    // Find an element child by its tag name
    pub fn elementByTagName(self: Tree, needle: []const u8) ?Node.Elem {
        return for (self.children) |child| {
            switch (child) {
                .elem => |elem_child| {
                    if (std.mem.eql(u8, elem_child.tag_name, needle)) {
                        break elem_child;
                    }
                },
                else => {},
            }
        } else null;
    }

    // Alias for elementByTagName
    pub fn elem(self: Tree, needle: []const u8) ?Node.Elem {
        return try self.elementByTagName(needle);
    }

    // Allocate a slice for all of the element children of a given tag name
    // To free the returned slice, you can just call allocator.free(elements)
    // where 'elements' is the returned slice.
    pub fn elementsByTagNameAlloc(self: Tree, allocator: std.mem.Allocator, needle: []const u8) ![]Node.Elem {
        var out = std.ArrayList(Node.Elem).init(allocator);
        errdefer out.deinit();

        for (self.children) |child| {
            switch (child) {
                .elem => |elem_child| {
                    if (std.mem.eql(u8, elem_child.tag_name, needle)) {
                        try out.append(elem_child);
                    }
                },
                else => {},
            }
        }

        return out.toOwnedSlice();
    }

    // Alias for elementsByTagName
    pub fn elemsAlloc(self: Tree, allocator: std.mem.Allocator, needle: []const u8) ![]Node.Elem {
        return try self.elementsByTagNameAlloc(allocator, needle);
    }

    // Get an element by the value of one of its attributes
    pub fn elementByAttributeValue(self: Tree, needle_name: []const u8, needle_value: []const u8) ?Node.Elem {
        return for (self.children) |child| {
            switch (child) {
                .elem => |elem_child| {
                    const attribute = elem_child.attributeByName(needle_name) orelse continue;
                    if (std.mem.eql(u8, attribute.value orelse continue, needle_value)) {
                        break elem_child;
                    }
                },
                else => {},
            }
        } else null;
    }

    // Alias for elementByAttributeValue
    pub fn elemByAttr(self: Tree, needle_name: []const u8, needle_value: []const u8) ?Node.Elem {
        return try self.elementByAttributeValue(needle_name, needle_value);
    }

    // Return the inner text (not including the elements) of the tree. Note that the
    // result will be entirely unformatted. Free with allocator.free(result);
    pub fn concatTextAlloc(self: Tree, allocator: std.mem.Allocator) ![]const u8 {
        var content_length: usize = 0;
        for (self.children) |child| {
            switch (child) {
                .text => |text_child| content_length += text_child.contents.len,
                else => {},
            }
        }
        const combined = try allocator.alloc(u8, content_length);
        errdefer allocator.free(combined);

        var cursor: usize = 0;
        for (self.children) |child| {
            switch (child) {
                .text => |text_child| {
                    @memcpy(combined[cursor .. cursor + text_child.contents.len], text_child.contents);
                    cursor += text_child.contents.len;
                },
                else => {},
            }
        }

        return combined;
    }

    // Return the inner text (not including the elements) of the tree but without
    // any whitespace at the start or end. Free with allocator.free(result).
    pub fn concatTextTrimmedAlloc(self: Tree, allocator: std.mem.Allocator) ![]const u8 {
        const combined = try self.concatTextAlloc(allocator);
        defer allocator.free(combined);

        const trimmed = std.mem.trim(u8, combined, &std.ascii.whitespace);
        return try allocator.dupe(u8, trimmed);
    }

    pub fn concatTextComptime(self: Tree) []const u8 {
        var out: []const u8 = &.{};
        for (self.children) |child| {
            switch (child) {
                .text => |text_child| {
                    out = out ++ text_child.contents;
                },
                else => {},
            }
        }
        return out;
    }

    pub fn concatTextTrimmedComptime(self: Tree) []const u8 {
        const combined = self.concatTextComptime();
        return std.mem.trim(u8, combined, &std.ascii.whitespace);
    }
};

pub fn StateMachine(comptime Builder: type) type {
    return struct {
        pub const State = union(enum) {
            const ElemTag = struct {
                open_token: Scanner.Token,
                tag_name: []const u8,
            };

            default: void,
            elem_tag: ElemTag,
        };

        const StateMachineT = @This();

        builder: *Builder,
        state: State = .default,

        pub fn feedToken(self: *StateMachineT, token: Scanner.Token) !void {
            switch (self.state) {
                .default => switch (token.kind) {
                    .element_open => {
                        if (token.inner.len == 0) {
                            try self.builder.reportDefectOrExit(.missing_tag_name, &.{token});
                        }

                        const copied_token: Scanner.Token = try self.builder.copyToken(token);
                        try self.builder.buildAttributes();
                        self.state = .{ .elem_tag = .{
                            .open_token = copied_token,
                            .tag_name = copied_token.inner,
                        } };
                    },
                    .element_children_end => {
                        const stack_size: usize = self.builder.getStackSize();

                        if (stack_size == 1) {
                            try self.builder.reportDefectOrExit(.tag_never_opened, &.{token});
                            return;
                        }

                        const open_token: Scanner.Token = try self.builder.getOpenToken() orelse unreachable;
                        const children = try self.builder.getOwnedChildren();

                        try self.builder.popStack();

                        if (!std.mem.eql(u8, open_token.inner, token.inner)) {
                            try self.builder.reportDefectOrExit(.tag_never_opened, &.{token});
                        }

                        try self.builder.setElementTree(.{ .children = children });
                    },
                    .comment_open => {
                        try self.builder.addComment();
                    },
                    .comment_close => {
                        try self.builder.closeComment();
                    },
                    .meta_attribute => {},
                    .meta_attribute_value => {},
                    .doctype => {},
                    .text_chunk => {
                        try self.builder.appendTextChunk(token.inner);
                    },
                    else => unreachable,
                },
                .elem_tag => |*tag_details| {
                    switch (token.kind) {
                        .element_close, .element_self_end => {
                            const attributes = try self.builder.getAttributesOwned();

                            try self.builder.addNode(.{ .elem = .{
                                .tag_name = tag_details.tag_name,
                                .attributes = attributes,
                                .tree = null,
                            } });
                            if (token.kind == .element_close) {
                                try self.builder.pushStack(tag_details.open_token);
                            }
                            self.state = .default;
                        },
                        .element_attribute => {
                            try self.builder.appendAttribute(token.inner);
                        },
                        .element_attribute_value => {
                            try self.builder.setLastAttributeValue(token.inner);
                        },
                        else => unreachable,
                    }
                },
            }
        }

        pub fn finalise(self: *StateMachineT) !Tree {
            while (self.builder.getStackSize() > 1) {
                const open_token = try self.builder.getOpenToken() orelse unreachable;
                try self.builder.reportDefectOrExit(.tag_never_closed, &.{open_token});
                try self.builder.popStack();
            }

            return .{ .children = try self.builder.getOwnedChildren() };
        }
    };
}

pub fn stateMachine(builder: anytype) StateMachine(@typeInfo(@TypeOf(builder)).pointer.child) {
    return .{ .builder = builder };
}

pub const RuntimeBuilder = struct {
    const TempTree = struct {
        maybe_open_token: ?Scanner.Token,
        children: std.ArrayList(Tree.Node),
    };

    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,
    maybe_diagnostics: ?*Diagnostics,

    stack: std.ArrayList(TempTree),
    attributes: ?std.ArrayList(Tree.Node.Elem.Attr),

    pub fn init(
        temp_allocator: std.mem.Allocator,
        data_allocator: std.mem.Allocator,
        maybe_diagnostics: ?*Diagnostics,
    ) !RuntimeBuilder {
        const root: TempTree = .{
            .maybe_open_token = null,
            .children = std.ArrayList(Tree.Node).init(data_allocator),
        };
        var stack = try std.ArrayList(TempTree).initCapacity(temp_allocator, 8);
        try stack.append(root);
        return .{
            .temp_allocator = temp_allocator,
            .data_allocator = data_allocator,
            .maybe_diagnostics = maybe_diagnostics,
            .stack = stack,
            .attributes = null,
        };
    }

    pub fn deinit(self: *RuntimeBuilder) void {
        self.stack.deinit();
        if (self.attributes) |*attr| {
            attr.deinit();
            self.attributes = null;
        }
    }

    fn reportDefectOrExit(self: *RuntimeBuilder, defectKind: Diagnostics.Defect.Kind, tokens: []const Scanner.Token) !void {
        const diagnostics = self.maybe_diagnostics orelse return Error.XmlDefect;
        try diagnostics.reportDefect(defectKind, tokens);
    }

    pub fn copyToken(self: *RuntimeBuilder, token: Scanner.Token) !Scanner.Token {
        var copied = token;
        copied.inner = try self.data_allocator.dupe(u8, token.inner);
        return copied;
    }

    pub fn getOpenToken(self: *RuntimeBuilder) !?Scanner.Token {
        std.debug.assert(self.stack.items.len > 0);
        return self.stack.getLast().maybe_open_token;
    }

    pub fn getOwnedChildren(self: *RuntimeBuilder) ![]const Tree.Node {
        std.debug.assert(self.stack.items.len > 0);
        return try self.stack.items[self.stack.items.len - 1].children.toOwnedSlice();
    }

    pub fn getStackSize(self: *RuntimeBuilder) usize {
        return self.stack.items.len;
    }

    pub fn pushStack(self: *RuntimeBuilder, open_token: Scanner.Token) !void {
        try self.stack.append(.{
            .maybe_open_token = open_token,
            .children = std.ArrayList(Tree.Node).init(self.data_allocator),
        });
    }

    pub fn popStack(self: *RuntimeBuilder) !void {
        std.debug.assert(self.stack.items.len > 0);
        _ = self.stack.pop();
    }

    pub fn addNode(self: *RuntimeBuilder, node: Tree.Node) !void {
        std.debug.assert(self.stack.items.len > 0);
        const last = &self.stack.items[self.stack.items.len - 1];
        try last.children.append(node);
    }

    pub fn setElementTree(self: *RuntimeBuilder, tree: Tree) !void {
        std.debug.assert(self.stack.items.len > 0);
        const last = &self.stack.items[self.stack.items.len - 1];
        const last_child = &last.children.items[last.children.items.len - 1];
        std.debug.assert(last_child.* == .elem);
        std.debug.assert(last_child.elem.tree == null);
        last_child.elem.tree = tree;
    }

    pub fn addComment(self: *RuntimeBuilder) !void {
        std.debug.assert(self.stack.items.len > 0);
        const last = &self.stack.items[self.stack.items.len - 1];
        try last.children.append(.{ .comment = .{
            .contents = &.{},
        } });
    }

    pub fn closeComment(self: *RuntimeBuilder) !void {
        std.debug.assert(self.stack.items.len > 0);
        const last = &self.stack.items[self.stack.items.len - 1];
        std.debug.assert(last.children.items.len > 0);
        std.debug.assert(last.children.items[last.children.items.len - 1] == .comment);
        try last.children.append(.{ .text = .{
            .contents = &.{},
        } });
    }

    pub fn appendTextChunk(self: *RuntimeBuilder, text_content: []const u8) !void {
        std.debug.assert(self.stack.items.len > 0);
        const last = &self.stack.items[self.stack.items.len - 1];
        if (last.children.items.len > 0) {
            const last_node = &last.children.items[last.children.items.len - 1];
            switch (last_node.*) {
                inline .text, .comment => |*text_node| {
                    const previous_contents = text_node.contents;
                    defer self.data_allocator.free(previous_contents);
                    const concat = try self.data_allocator.alloc(u8, text_node.contents.len + text_content.len);
                    @memcpy(concat[0..text_node.contents.len], text_node.contents);
                    @memcpy(concat[text_node.contents.len..], text_content);
                    text_node.contents = concat;
                    return;
                },
                else => {},
            }
        }
        try last.children.append(.{ .text = .{
            .contents = try self.data_allocator.dupe(u8, text_content),
        } });
    }

    pub fn buildAttributes(self: *RuntimeBuilder) !void {
        std.debug.assert(self.attributes == null);
        self.attributes = std.ArrayList(Tree.Node.Elem.Attr).init(self.data_allocator);
    }

    pub fn getAttributesOwned(self: *RuntimeBuilder) ![]const Tree.Node.Elem.Attr {
        std.debug.assert(self.attributes != null);
        defer self.attributes = null;
        return self.attributes.?.toOwnedSlice();
    }

    pub fn appendAttribute(self: *RuntimeBuilder, attr_name: []const u8) !void {
        std.debug.assert(self.attributes != null);
        try self.attributes.?.append(.{
            .name = try self.data_allocator.dupe(u8, attr_name),
            .value = null,
        });
    }

    pub fn setLastAttributeValue(self: *RuntimeBuilder, attr_value: []const u8) !void {
        std.debug.assert(self.attributes != null);
        std.debug.assert(self.attributes.?.items.len > 0);

        self.attributes.?.items[self.attributes.?.items.len - 1].value =
            try self.data_allocator.dupe(u8, attr_value);
    }
};

pub const ComptimeBuilder = struct {
    const TempTree = struct {
        maybe_open_token: ?Scanner.Token,
        children: []const Tree.Node,
    };

    stack: []const TempTree = &.{.{
        .maybe_open_token = null,
        .children = &.{},
    }},
    attributes: ?[]const Tree.Node.Elem.Attr = null,

    pub fn deinit(self: ComptimeBuilder) void {
        self.stack.deinit();
    }

    fn reportDefectOrExit(self: ComptimeBuilder, defectKind: Diagnostics.Defect.Kind, tokens: []const Scanner.Token) !void {
        _ = self;
        _ = defectKind;
        _ = tokens;
        return Error.XmlDefect;
    }

    pub fn copyToken(_: *ComptimeBuilder, token: Scanner.Token) !Scanner.Token {
        return token;
    }

    pub fn buildAttributes(self: *ComptimeBuilder) !void {
        std.debug.assert(self.attributes == null);
        self.attributes = &.{};
    }

    pub fn getOpenToken(self: *ComptimeBuilder) !?Scanner.Token {
        std.debug.assert(self.stack.len > 0);
        return self.stack[self.stack.len - 1].maybe_open_token;
    }

    pub fn getOwnedChildren(self: *ComptimeBuilder) ![]const Tree.Node {
        std.debug.assert(self.stack.len > 0);
        return self.stack[self.stack.len - 1].children;
    }

    pub fn getStackSize(self: *ComptimeBuilder) usize {
        return self.stack.len;
    }

    pub fn pushStack(self: *ComptimeBuilder, open_token: Scanner.Token) !void {
        self.stack = self.stack ++ .{.{
            .maybe_open_token = open_token,
            .children = &.{},
        }};
    }

    pub fn popStack(self: *ComptimeBuilder) !void {
        std.debug.assert(self.stack.len > 0);
        self.stack = self.stack[0 .. self.stack.len - 1];
    }

    pub fn addNode(self: *ComptimeBuilder, node: Tree.Node) !void {
        std.debug.assert(self.stack.len > 0);
        const last = self.stack[self.stack.len - 1];
        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{
            .{
                .maybe_open_token = last.maybe_open_token,
                .children = last.children[0..last.children.len] ++ .{node},
            },
        };
    }

    pub fn setElementTree(self: *ComptimeBuilder, tree: Tree) !void {
        std.debug.assert(self.stack.len > 0);
        const last = self.stack[self.stack.len - 1];
        const last_child = last.children[last.children.len - 1];
        std.debug.assert(last_child == .elem);
        std.debug.assert(last_child.elem.tree == null);
        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
            .maybe_open_token = last.maybe_open_token,
            .children = last.children[0 .. last.children.len - 1] ++ .{
                .{ .elem = .{
                    .tag_name = last_child.elem.tag_name,
                    .attributes = last_child.elem.attributes,
                    .tree = tree,
                } },
            },
        }};
    }

    pub fn addComment(self: *ComptimeBuilder) !void {
        std.debug.assert(self.stack.len > 0);
        const last = self.stack[self.stack.len - 1];
        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
            .maybe_open_token = last.maybe_open_token,
            .children = last.children[0..last.children.len] ++ .{.{
                .comment = .{ .contents = &.{} },
            }},
        }};
    }

    pub fn closeComment(self: *ComptimeBuilder) !void {
        std.debug.assert(self.stack.len > 0);
        const last = self.stack[self.stack.len - 1];
        std.debug.assert(last.children.len > 0);
        std.debug.assert(last.children[last.children.len - 1] == .comment);
        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
            .maybe_open_token = last.maybe_open_token,
            .children = last.children[0..last.children.len] ++ .{.{
                .text = .{ .contents = &.{} },
            }},
        }};
    }

    pub fn appendTextChunk(self: *ComptimeBuilder, text_content: []const u8) !void {
        std.debug.assert(self.stack.len > 0);
        const last = self.stack[self.stack.len - 1];
        if (last.children.len > 0) {
            const last_node = last.children[last.children.len - 1];
            switch (last_node) {
                inline .text, .comment => |text_node, tag| {
                    const previous_contents: []const u8 = if (text_node.contents.len > 0) text_node.contents else &.{};
                    const contents = previous_contents ++ text_content;
                    self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                        .maybe_open_token = last.maybe_open_token,
                        .children = last.children[0 .. last.children.len - 1] ++ .{
                            switch (tag) {
                                .text => .{ .text = .{ .contents = contents } },
                                .comment => .{ .comment = .{ .contents = contents } },
                                else => unreachable,
                            },
                        },
                    }};
                    return;
                },
                else => {},
            }
        }

        const temp_children: []const Tree.Node = if (last.children.len > 0) last.children else &.{};
        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
            .maybe_open_token = last.maybe_open_token,
            .children = temp_children ++ .{.{
                .text = .{ .contents = text_content },
            }},
        }};
    }

    pub fn getAttributesOwned(self: *ComptimeBuilder) ![]const Tree.Node.Elem.Attr {
        std.debug.assert(self.attributes != null);
        defer self.attributes = null;
        return self.attributes.?;
    }

    pub fn appendAttribute(self: *ComptimeBuilder, attr_name: []const u8) !void {
        std.debug.assert(self.attributes != null);
        self.attributes = self.attributes.? ++ .{.{
            .name = attr_name,
            .value = null,
        }};
    }

    pub fn setLastAttributeValue(self: *ComptimeBuilder, attr_value: []const u8) !void {
        std.debug.assert(self.attributes != null);
        std.debug.assert(self.attributes.?.len > 0);

        const last_attr = self.attributes.?[self.attributes.?.len - 1];
        self.attributes = self.attributes.?[0 .. self.attributes.?.len - 1] ++ .{.{
            .name = last_attr.name,
            .value = attr_value,
        }};
    }
};

fn parseFromSliceImpl(allocator: std.mem.Allocator, slice: []const u8, maybe_diagnostics: ?*Diagnostics) !Tree.Owned {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var scanner = Scanner.fromSlice(slice);
    var builder = try RuntimeBuilder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    var state = stateMachine(&builder);
    while (try scanner.next()) |token| {
        try state.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try state.finalise(),
    };
}

pub fn parseFromSliceDiagnostics(allocator: std.mem.Allocator, slice: []const u8, diagnostics: *Diagnostics) !Tree.Owned {
    const fromSliceDiagnostics = try parseFromSliceImpl(allocator, slice, diagnostics);
    return fromSliceDiagnostics;
}

pub fn fromSlice(allocator: std.mem.Allocator, slice: []const u8) !Tree.Owned {
    return parseFromSliceImpl(allocator, slice, null);
}

fn fromReaderImpl(allocator: std.mem.Allocator, reader: anytype, maybe_diagnostics: ?*Diagnostics) !Tree.Owned {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var xmlReader = Scanner.staticBufferReader(reader);
    var builder = try RuntimeBuilder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    var state = stateMachine(&builder);
    while (try xmlReader.next()) |token| {
        try state.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try state.finalise(),
    };
}

pub fn fromReaderDiagnostics(allocator: std.mem.Allocator, reader: anytype, diagnostics: *Diagnostics) !Tree.Owned {
    const result = try fromReaderImpl(allocator, reader, diagnostics);
    return result;
}

pub fn fromReader(allocator: std.mem.Allocator, reader: anytype) !Tree.Owned {
    return fromReaderImpl(allocator, reader, null);
}

pub fn fromSliceComptime(comptime slice: []const u8) Tree {
    return comptime blk: {
        var scanner = Scanner.fromSlice(slice);
        var builder = ComptimeBuilder{};
        var state = stateMachine(&builder);

        while (try scanner.next()) |token| {
            try state.feedToken(token);
        }

        break :blk try state.finalise();
    };
}

const test_buf = "<div betrayed-by=\"judas\">jesus <p>christ</p> lord <amen/></div>";
pub fn expectTestTreeValid(tree: Tree) !void {
    try std.testing.expectEqual(tree.children.len, 1);
    try std.testing.expect(tree.children[0] == .elem);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tag_name, "div");
    try std.testing.expectEqual(tree.children[0].elem.attributes.len, 1);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.attributes[0].name, "betrayed-by");
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.attributes[0].value.?, "judas");
    try std.testing.expect(tree.children[0].elem.tree != null);
    try std.testing.expectEqual(tree.children[0].elem.tree.?.children.len, 4);

    try std.testing.expect(tree.children[0].elem.tree.?.children[0] == .text);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tree.?.children[0].text.contents, "jesus ");

    try std.testing.expect(tree.children[0].elem.tree.?.children[1] == .elem);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tree.?.children[1].elem.tag_name, "p");
    try std.testing.expect(tree.children[0].elem.tree.?.children[0] == .text);
    try std.testing.expectEqual(tree.children[0].elem.tree.?.children[1].elem.attributes.len, 0);
    try std.testing.expect(tree.children[0].elem.tree.?.children[1].elem.tree != null);
    try std.testing.expectEqual(tree.children[0].elem.tree.?.children[1].elem.tree.?.children.len, 1);
    try std.testing.expect(tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0] == .text);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0].text.contents, "christ");

    try std.testing.expect(tree.children[0].elem.tree.?.children[2] == .text);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tree.?.children[2].text.contents, " lord ");

    try std.testing.expect(tree.children[0].elem.tree.?.children[3] == .elem);
    try std.testing.expectEqualSlices(u8, tree.children[0].elem.tree.?.children[3].elem.tag_name, "amen");
    try std.testing.expectEqual(tree.children[0].elem.tree.?.children[3].elem.attributes.len, 0);
    try std.testing.expect(tree.children[0].elem.tree.?.children[3].elem.tree == null);
}

test fromReader {
    var fba = std.io.fixedBufferStream(test_buf);

    const parsed = try fromReader(std.testing.allocator, fba.reader());
    defer parsed.deinit();

    try expectTestTreeValid(parsed.tree);
}

test fromReaderDiagnostics {
    const buf = "<div><p></p>";
    var fba = std.io.fixedBufferStream(buf);

    var diagnostics = Diagnostics.init(std.testing.allocator);
    defer diagnostics.deinit();

    const parsed = try fromReaderDiagnostics(std.testing.allocator, fba.reader(), &diagnostics);
    defer parsed.deinit();

    try std.testing.expectEqual(diagnostics.defects.items.len, 1);
}

test fromSliceComptime {
    @setEvalBranchQuota(8192);
    const tree = fromSliceComptime(test_buf);
    try expectTestTreeValid(tree);
}

test Tree {
    const tree: Tree = .{
        .children = &.{
            .{ .elem = .{
                .tag_name = "person",
                .attributes = &.{
                    .{ .name = "name", .value = "Jonas" },
                    .{ .name = "age", .value = "18" },
                },
                .tree = null,
            } },
            .{ .text = .{ .contents = "\n    this is a gap!!!    \n    \n    \n    " } },
            .{ .elem = .{
                .tag_name = "goat",
                .attributes = &.{},
                .tree = null,
            } },
            .{ .elem = .{
                .tag_name = "person",
                .attributes = &.{
                    .{ .name = "name", .value = "Kyle" },
                    .{ .name = "age", .value = "24" },
                },
                .tree = null,
            } },
        },
    };

    const jonas = tree.elementByTagName("person");
    const kyle = tree.elementByAttributeValue("name", "Kyle");

    try std.testing.expect(jonas != null);
    try std.testing.expect(kyle != null);
    try std.testing.expectEqualSlices(u8, "person", jonas.?.tag_name);
    try std.testing.expectEqualSlices(u8, "person", kyle.?.tag_name);
    try std.testing.expectEqualSlices(u8, "Jonas", jonas.?.attributes[0].value.?);
    try std.testing.expectEqualSlices(u8, "Kyle", kyle.?.attributes[0].value.?);

    const jonas_name = tree.children[0].elem.attributeValueByName("name");
    try std.testing.expect(jonas_name != null);
    try std.testing.expectEqualSlices(u8, "Jonas", jonas_name.?);

    const all_people = try tree.elementsByTagNameAlloc(std.testing.allocator, "person");
    defer std.testing.allocator.free(all_people);

    try std.testing.expectEqual(2, all_people.len);
    try std.testing.expectEqualDeep(jonas.?, all_people[0]);
    try std.testing.expectEqualDeep(kyle.?, all_people[1]);

    const text = try tree.concatTextAlloc(std.testing.allocator);
    defer std.testing.allocator.free(text);
    const text2 = try tree.concatTextTrimmedAlloc(std.testing.allocator);
    defer std.testing.allocator.free(text2);

    try std.testing.expectEqualSlices(u8, "\n    this is a gap!!!    \n    \n    \n    ", text);
    try std.testing.expectEqualSlices(u8, "this is a gap!!!", text2);
}

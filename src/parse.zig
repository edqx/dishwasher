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
            MissingTagName,
            TagNeverOpened,
            TagNeverClosed,
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

pub const Builder = struct {
    pub const State = union(enum) {
        const ElemTag = struct {
            open_token: Scanner.Token,
            tag_name: []const u8,
            attributes: std.ArrayList(Tree.Node.Elem.Attr),
        };

        default: void,
        elem_tag: ElemTag,
    };

    const TempTree = struct {
        maybe_open_token: ?Scanner.Token,

        children: std.ArrayList(Tree.Node),
    };

    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,
    maybe_diagnostics: ?*Diagnostics,

    state: State,
    stack: std.ArrayList(TempTree),

    pub fn init(
        temp_allocator: std.mem.Allocator,
        data_allocator: std.mem.Allocator,
        maybe_diagnostics: ?*Diagnostics,
    ) !Builder {
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
            .state = .default,
            .stack = stack,
        };
    }

    pub fn deinit(self: Builder) void {
        self.stack.deinit();
    }

    fn reportDefectOrExit(self: *Builder, defectKind: Diagnostics.Defect.Kind, tokens: []const Scanner.Token) !void {
        const diagnostics = self.maybe_diagnostics orelse return Error.XmlDefect;
        try diagnostics.reportDefect(defectKind, tokens);
    }

    pub fn feedToken(self: *Builder, token: Scanner.Token) !void {
        std.debug.assert(self.stack.items.len > 0);
        switch (self.state) {
            .default => {
                switch (token.kind) {
                    .element_open => {
                        if (token.inner.len == 0) {
                            try self.reportDefectOrExit(.MissingTagName, &.{token});
                        }

                        var copied_token = token;
                        copied_token.inner = try self.data_allocator.dupe(u8, token.inner);
                        errdefer self.data_allocator.free(copied_token.inner);

                        self.state = .{ .elem_tag = .{
                            .open_token = copied_token,
                            .tag_name = copied_token.inner,
                            .attributes = std.ArrayList(Tree.Node.Elem.Attr).init(self.data_allocator),
                        } };
                    },
                    .element_children_end => {
                        var last = &self.stack.items[self.stack.items.len - 1];

                        if (self.stack.items.len == 1) {
                            try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                            return;
                        }
                        std.debug.assert(last.maybe_open_token != null);
                        const open_token = last.maybe_open_token.?;
                        const children = try last.children.toOwnedSlice();
                        _ = self.stack.pop();

                        last = &self.stack.items[self.stack.items.len - 1];

                        if (!std.mem.eql(u8, open_token.inner, token.inner)) {
                            std.log.info("{s} != {s}", .{ open_token.inner, token.inner });
                            try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                        }

                        std.debug.assert(last.children.items.len > 0);

                        const last_child = &last.children.items[last.children.items.len - 1];
                        std.debug.assert(last_child.* == .elem);

                        last_child.elem.tree = .{ .children = children };
                    },
                    .comment_open => {
                        var last = &self.stack.items[self.stack.items.len - 1];
                        try last.children.append(.{ .comment = .{
                            .contents = &.{},
                        } });
                    },
                    .comment_close => {
                        var last = &self.stack.items[self.stack.items.len - 1];
                        std.debug.assert(last.children.items.len > 0);
                        std.debug.assert(last.children.items[last.children.items.len - 1] == .comment);
                        try last.children.append(.{ .text = .{
                            .contents = &.{},
                        } });
                    },
                    .meta_attribute => {},
                    .meta_attribute_value => {},
                    .doctype => {},
                    .text_chunk => {
                        var last = &self.stack.items[self.stack.items.len - 1];
                        if (last.children.items.len > 0) {
                            const last_node = &last.children.items[last.children.items.len - 1];
                            switch (last_node.*) {
                                inline .text, .comment => |*text_node| {
                                    const previous_contents = text_node.contents;
                                    defer self.data_allocator.free(previous_contents);
                                    var concat = try self.data_allocator.alloc(u8, text_node.contents.len + token.inner.len);
                                    @memcpy(concat[0..text_node.contents.len], text_node.contents);
                                    @memcpy(concat[text_node.contents.len..], token.inner);
                                    text_node.contents = concat;
                                    return;
                                },
                                else => {},
                            }
                        }
                        try last.children.append(.{ .text = .{
                            .contents = try self.data_allocator.dupe(u8, token.inner),
                        } });
                    },
                    else => unreachable,
                }
            },
            .elem_tag => |*tag_details| {
                switch (token.kind) {
                    .element_close, .element_self_end => {
                        const last = &self.stack.items[self.stack.items.len - 1];
                        try last.children.append(.{ .elem = .{
                            .tag_name = tag_details.tag_name,
                            .attributes = try tag_details.attributes.toOwnedSlice(),
                            .tree = null,
                        } });
                        if (token.kind == .element_close) {
                            try self.stack.append(.{
                                .maybe_open_token = tag_details.open_token,
                                .children = std.ArrayList(Tree.Node).init(self.data_allocator),
                            });
                        }
                        self.state = .default;
                    },
                    .element_attribute => {
                        try tag_details.attributes.append(.{
                            .name = try self.data_allocator.dupe(u8, token.inner),
                            .value = null,
                        });
                    },
                    .element_attribute_value => {
                        std.debug.assert(tag_details.attributes.items.len > 0);

                        tag_details.attributes.items[tag_details.attributes.items.len - 1].value =
                            try self.data_allocator.dupe(u8, token.inner);
                    },
                    else => unreachable,
                }
            },
        }
    }

    pub fn finalise(self: *Builder) !Tree {
        std.debug.assert(self.stack.items.len > 0);

        if (self.stack.items.len > 1) {
            for (self.stack.items[1..]) |subtree| {
                std.debug.assert(subtree.maybe_open_token != null);
                try self.reportDefectOrExit(.TagNeverClosed, &.{subtree.maybe_open_token.?});
            }
        }

        var root = self.stack.pop();

        return .{
            .children = try root.children.toOwnedSlice(),
        };
    }
};

pub const ComptimeBuilder = struct {
    pub const State = union(enum) {
        const ElemTag = struct {
            open_token: Scanner.Token,
            tag_name: []const u8,
            attributes: []const Tree.Node.Elem.Attr,
        };

        default: void,
        elem_tag: ElemTag,
    };

    const TempTree = struct {
        maybe_open_token: ?Scanner.Token,
        children: []const Tree.Node,
    };

    state: State = .default,
    stack: []const TempTree = &.{.{
        .maybe_open_token = null,
        .children = &.{},
    }},

    pub fn deinit(self: ComptimeBuilder) void {
        self.stack.deinit();
    }

    fn reportDefectOrExit(self: ComptimeBuilder, defectKind: Diagnostics.Defect.Kind, tokens: []const Scanner.Token) !void {
        const diagnostics = self.maybe_diagnostics orelse return Error.XmlDefect;
        try diagnostics.reportDefect(defectKind, tokens);
    }

    pub fn feedToken(self: *ComptimeBuilder, token: Scanner.Token) !void {
        std.debug.assert(self.stack.len > 0);
        switch (self.state) {
            .default => {
                switch (token.kind) {
                    .element_open => {
                        if (token.inner.len == 0) {
                            try self.reportDefectOrExit(.MissingTagName, &.{token});
                        }

                        self.state = .{ .elem_tag = .{
                            .open_token = token,
                            .tag_name = token.inner,
                            .attributes = &.{},
                        } };
                    },
                    .element_children_end => {
                        const last = self.stack[self.stack.len - 1];

                        if (self.stack.len == 1) {
                            try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                            return;
                        }
                        std.debug.assert(last.maybe_open_token != null);
                        const open_token = last.maybe_open_token.?;
                        const children = last.children;

                        const second_last = self.stack[self.stack.len - 2];

                        if (!std.mem.eql(u8, open_token.inner, token.inner)) {
                            try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                        }

                        std.debug.assert(second_last.children.len > 0);

                        const last_child = second_last.children[second_last.children.len - 1];
                        std.debug.assert(last_child == .elem);

                        self.stack = self.stack[0 .. self.stack.len - 2] ++ .{.{
                            .maybe_open_token = second_last.maybe_open_token,
                            .children = second_last.children[0 .. second_last.children.len - 1] ++ .{
                                .{ .elem = .{
                                    .tag_name = last_child.elem.tag_name,
                                    .attributes = last_child.elem.attributes,
                                    .tree = .{ .children = children },
                                } },
                            },
                        }};
                    },
                    .comment_open => {
                        const last = self.stack[self.stack.len - 1];
                        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                            .maybe_open_token = last.maybe_open_token,
                            .children = last.children[0..last.children.len] ++ .{.{
                                .comment = .{ .contents = token.inner },
                            }},
                        }};
                    },
                    .comment_close => {
                        const last = self.stack[self.stack.len - 1];
                        std.debug.assert(last.children.len > 0);
                        std.debug.assert(last.children[last.children.len - 1] == .comment);
                        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                            .maybe_open_token = last.maybe_open_token,
                            .children = last.children[0..last.children.len] ++ .{.{
                                .text = .{ .contents = token.inner },
                            }},
                        }};
                    },
                    .meta_attribute => {},
                    .meta_attribute_value => {},
                    .doctype => {},
                    .text_chunk => {
                        const last = self.stack[self.stack.len - 1];
                        if (last.children.len > 0) {
                            const last_node = last.children[last.children.len - 1];
                            switch (last_node) {
                                inline .text, .comment => |text_node, tag| {
                                    const contents = text_node.contents ++ token.inner;
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
                                },
                                else => {},
                            }
                        }

                        const children: []const Tree.Node = if (last.children.len == 0) &.{} else last.children;
                        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                            .maybe_open_token = last.maybe_open_token,
                            .children = children ++ .{.{
                                .text = .{ .contents = token.inner },
                            }},
                        }};
                    },
                    else => unreachable,
                }
            },
            .elem_tag => |tag_details| {
                switch (token.kind) {
                    .element_close => {
                        const last = self.stack[self.stack.len - 1];
                        self.state = .default;
                        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{
                            .{
                                .maybe_open_token = last.maybe_open_token,
                                .children = last.children ++ .{.{
                                    .elem = .{
                                        .tag_name = tag_details.tag_name,
                                        .attributes = tag_details.attributes,
                                        .tree = null,
                                    },
                                }},
                            },
                            .{
                                .maybe_open_token = tag_details.open_token,
                                .children = &.{},
                            },
                        };
                    },
                    .element_self_end => {
                        const last = self.stack[self.stack.len - 1];
                        self.state = .default;
                        self.stack = self.stack[0 .. self.stack.len - 1] ++ .{
                            .{
                                .maybe_open_token = last.maybe_open_token,
                                .children = last.children ++ .{.{
                                    .elem = .{
                                        .tag_name = tag_details.tag_name,
                                        .attributes = tag_details.attributes,
                                        .tree = null,
                                    },
                                }},
                            },
                        };
                    },
                    .element_attribute => {
                        self.state = .{
                            .elem_tag = .{
                                .open_token = tag_details.open_token,
                                .tag_name = tag_details.tag_name,
                                .attributes = tag_details.attributes ++ .{.{
                                    .name = token.inner,
                                    .value = null,
                                }},
                            },
                        };
                    },
                    .element_attribute_value => {
                        std.debug.assert(tag_details.attributes.len > 0);

                        const last_attribute = tag_details.attributes[tag_details.attributes.len - 1];

                        self.state = .{
                            .elem_tag = .{
                                .open_token = tag_details.open_token,
                                .tag_name = tag_details.tag_name,
                                .attributes = tag_details.attributes[0 .. tag_details.attributes.len - 1] ++ .{.{
                                    .name = last_attribute.name,
                                    .value = token.inner,
                                }},
                            },
                        };
                    },
                    else => unreachable,
                }
            },
        }
    }

    pub fn finalise(self: *ComptimeBuilder) !Tree {
        std.debug.assert(self.stack.len > 0);

        if (self.stack.len > 1) {
            for (self.stack[1..]) |subtree| {
                std.debug.assert(subtree.maybe_open_token != null);
                try self.reportDefectOrExit(.TagNeverClosed, &.{subtree.maybe_open_token.?});
            }
        }

        return .{
            .children = self.stack[self.stack.len - 1].children,
        };
    }
};

fn parseFromSliceImpl(allocator: std.mem.Allocator, slice: []const u8, maybe_diagnostics: ?*Diagnostics) !Tree.Owned {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var scanner = Scanner.fromSlice(slice);
    var builder = try Builder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    while (try scanner.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
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
    var builder = try Builder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    while (try xmlReader.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
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

        while (try scanner.next()) |token| {
            try builder.feedToken(token);
        }

        break :blk try builder.finalise();
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

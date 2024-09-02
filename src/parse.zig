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

pub const Builder = struct {
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
                attributes: []Attr,
                tree: ?Tree,
            };

            pub const Text = struct {
                contents: []const u8,

                pub fn trimmed(self: Text) []const u8 {
                    _ = self;
                }
            };

            pub const Comment = struct {
                contents: []const u8,
            };

            elem: Elem,
            text: Text,
            comment: Comment,
        };

        children: []Node,
    };

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

                        self.state = .{ .elem_tag = .{
                            .open_token = token,
                            .tag_name = try self.data_allocator.dupe(u8, token.inner),
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
                                    defer self.data_allocator.free(text_node.contents);
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
    pub const Tree = struct {
        pub const Node = union(enum) {
            pub const Elem = struct {
                pub const Attr = struct {
                    name: []const u8,
                    value: ?[]const u8,
                };

                tag_name: []const u8,
                attributes: []const Attr,
                tree: ?Tree,
            };

            pub const Text = struct {
                contents: []const u8,

                pub fn trimmed(self: Text) []const u8 {
                    _ = self;
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
    };

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

    maybe_diagnostics: ?*Diagnostics = null,

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

    pub fn getNextState(comptime self: ComptimeBuilder, token: Scanner.Token) !ComptimeBuilder {
        std.debug.assert(self.stack.len > 0);
        switch (self.state) {
            .default => {
                switch (token.kind) {
                    .element_open => {
                        if (token.inner.len == 0) {
                            try self.reportDefectOrExit(.MissingTagName, &.{token});
                        }

                        return .{
                            .state = .{ .elem_tag = .{
                                .open_token = token,
                                .tag_name = token.inner,
                                .attributes = &.{},
                            } },
                            .stack = self.stack,
                        };
                    },
                    .element_children_end => {
                        const last = self.stack[self.stack.len - 1];

                        if (self.stack.len == 1) {
                            try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                            return self;
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

                        return .{
                            .state = self.state,
                            .stack = self.stack[0 .. self.stack.len - 2] ++ .{.{
                                .maybe_open_token = second_last.maybe_open_token,
                                .children = second_last.children[0 .. second_last.children.len - 1] ++ .{
                                    .{ .elem = .{
                                        .tag_name = last_child.elem.tag_name,
                                        .attributes = last_child.elem.attributes,
                                        .tree = .{ .children = children },
                                    } },
                                },
                            }},
                        };
                    },
                    .comment_open => {
                        const last = self.stack[self.stack.len - 1];
                        return .{
                            .state = self.state,
                            .stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                                .maybe_open_token = last.maybe_open_token,
                                .children = last.children[0..last.children.len] ++ .{.{
                                    .comment = .{ .contents = token.inner },
                                }},
                            }},
                        };
                    },
                    .comment_close => {
                        const last = self.stack[self.stack.len - 1];
                        std.debug.assert(last.children.len > 0);
                        std.debug.assert(last.children[last.children.len - 1] == .comment);
                        return .{
                            .state = self.state,
                            .stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                                .maybe_open_token = last.maybe_open_token,
                                .children = last.children[0..last.children.len] ++ .{.{
                                    .text = .{ .contents = token.inner },
                                }},
                            }},
                        };
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
                                    return .{
                                        .state = self.state,
                                        .stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                                            .maybe_open_token = last.maybe_open_token,
                                            .children = last.children[0 .. last.children.len - 1] ++ .{
                                                switch (tag) {
                                                    .text => .{ .text = .{ .contents = contents } },
                                                    .comment => .{ .comment = .{ .contents = contents } },
                                                },
                                            },
                                        }},
                                    };
                                },
                                else => {},
                            }
                        }

                        const children: []const Tree.Node = if (last.children.len == 0) &.{} else last.children;
                        return .{
                            .state = self.state,
                            .stack = self.stack[0 .. self.stack.len - 1] ++ .{.{
                                .maybe_open_token = last.maybe_open_token,
                                .children = children ++ .{.{
                                    .text = .{ .contents = token.inner },
                                }},
                            }},
                        };
                    },
                    else => unreachable,
                }
            },
            .elem_tag => |tag_details| {
                switch (token.kind) {
                    .element_close => {
                        const last = self.stack[self.stack.len - 1];
                        return .{
                            .state = .default,
                            .stack = self.stack[0 .. self.stack.len - 1] ++ .{
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
                            },
                        };
                    },
                    .element_self_end => {
                        const last = self.stack[self.stack.len - 1];
                        return .{
                            .state = .default,
                            .stack = self.stack[0 .. self.stack.len - 1] ++ .{
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
                            },
                        };
                    },
                    .element_attribute => {
                        return .{
                            .state = .{
                                .elem_tag = .{
                                    .open_token = tag_details.open_token,
                                    .tag_name = tag_details.tag_name,
                                    .attributes = tag_details.attributes ++ .{.{
                                        .name = token.inner,
                                        .value = null,
                                    }},
                                },
                            },
                            .stack = self.stack,
                        };
                    },
                    .element_attribute_value => {
                        std.debug.assert(tag_details.attributes.len > 0);

                        const last_attribute = tag_details.attributes[tag_details.attributes.len - 1];

                        return .{
                            .state = .{
                                .elem_tag = .{
                                    .open_token = tag_details.open_token,
                                    .tag_name = tag_details.tag_name,
                                    .attributes = tag_details.attributes[0 .. tag_details.attributes.len - 1] ++ .{.{
                                        .name = last_attribute.name,
                                        .value = token.inner,
                                    }},
                                },
                            },
                            .stack = self.stack,
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

fn parseFromSliceImpl(allocator: std.mem.Allocator, slice: []const u8, maybe_diagnostics: ?*Diagnostics) !Builder.Tree.Owned {
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

pub fn parseFromSliceDiagnostics(allocator: std.mem.Allocator, slice: []const u8, diagnostics: *Diagnostics) !Builder.Tree.Owned {
    const fromSliceDiagnostics = try parseFromSliceImpl(allocator, slice, diagnostics);
    return fromSliceDiagnostics;
}

pub fn fromSlice(allocator: std.mem.Allocator, slice: []const u8) !Builder.Tree.Owned {
    return parseFromSliceImpl(allocator, slice, null);
}

fn fromReaderImpl(allocator: std.mem.Allocator, reader: anytype, maybe_diagnostics: ?*Diagnostics) !Builder.Tree.Owned {
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

pub fn fromReaderDiagnostics(allocator: std.mem.Allocator, reader: anytype, diagnostics: *Diagnostics) !Builder.Tree.Owned {
    const result = try fromReaderImpl(allocator, reader, diagnostics);
    return result;
}

pub fn fromReader(allocator: std.mem.Allocator, reader: anytype) !Builder.Tree.Owned {
    return fromReaderImpl(allocator, reader, null);
}

pub fn comptimeFromSlice(comptime slice: []const u8) ComptimeBuilder.Tree {
    return comptime blk: {
        var scanner = Scanner.fromSlice(slice);
        var builder = ComptimeBuilder{};

        while (try scanner.next()) |token| {
            builder = try builder.getNextState(token);
        }

        break :blk try builder.finalise();
    };
}

test fromReader {
    {
        const buf = "<div betrayed-by=\"judas\">jesus <p>christ</p> lord <amen/></div>";
        var fba = std.io.fixedBufferStream(buf);

        const parsed = try fromReader(std.testing.allocator, fba.reader());
        defer parsed.deinit();

        try std.testing.expectEqual(parsed.tree.children.len, 1);
        try std.testing.expect(parsed.tree.children[0] == .elem);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tag_name, "div");
        try std.testing.expectEqual(parsed.tree.children[0].elem.attributes.len, 1);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.attributes[0].name, "betrayed-by");
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.attributes[0].value.?, "judas");
        try std.testing.expect(parsed.tree.children[0].elem.tree != null);
        try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children.len, 4);

        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[0] == .text);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[0].text.contents, "jesus ");

        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1] == .elem);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[1].elem.tag_name, "p");
        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[0] == .text);
        try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[1].elem.attributes.len, 0);
        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1].elem.tree != null);
        try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children.len, 1);
        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0] == .text);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0].text.contents, "christ");

        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[2] == .text);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[2].text.contents, " lord ");

        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[3] == .elem);
        try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[3].elem.tag_name, "amen");
        try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[3].elem.attributes.len, 0);
        try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[3].elem.tree == null);
    }

    {
        const buf = "<div><p></p>";
        var fba = std.io.fixedBufferStream(buf);

        var diagnostics = Diagnostics.init(std.testing.allocator);
        defer diagnostics.deinit();

        const parsed = try fromReaderDiagnostics(std.testing.allocator, fba.reader(), &diagnostics);
        defer parsed.deinit();

        try std.testing.expectEqual(diagnostics.defects.items.len, 1);
    }
}

test comptimeFromSlice {
    const tree = comptimeFromSlice("<div betrayed-by=\"judas\">jesus <p>christ</p> lord <amen/></div>");

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

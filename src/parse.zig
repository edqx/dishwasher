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

pub const OwnedTree = struct {
    arena: std.heap.ArenaAllocator,
    tree: Tree,

    pub fn deinit(self: OwnedTree) void {
        self.arena.deinit();
    }
};

pub const TreeBuilder = struct {
    const TempTree = struct {
        maybe_open_token: ?Scanner.Token,
        maybe_new_elem_tag_name: ?[]const u8 = null,
        maybe_new_elem_attributes: ?std.ArrayList(Tree.Node.Elem.Attr) = null,

        children: std.ArrayList(Tree.Node),
    };

    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,
    maybe_diagnostics: ?*Diagnostics,

    stack: std.ArrayList(TempTree),

    pub fn init(
        temp_allocator: std.mem.Allocator,
        data_allocator: std.mem.Allocator,
        maybe_diagnostics: ?*Diagnostics,
    ) !TreeBuilder {
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
        };
    }

    pub fn deinit(self: TreeBuilder) void {
        self.stack.deinit();
    }

    fn reportDefectOrExit(self: *TreeBuilder, defectKind: Diagnostics.Defect.Kind, tokens: []const Scanner.Token) !void {
        const diagnostics = self.maybe_diagnostics orelse return Error.XmlDefect;
        try diagnostics.reportDefect(defectKind, tokens);
    }

    pub fn feedToken(self: *TreeBuilder, token: Scanner.Token) !void {
        std.debug.assert(self.stack.items.len > 0);
        switch (token.kind) {
            .element_open => {
                if (token.inner.len == 0) {
                    try self.reportDefectOrExit(.MissingTagName, &.{token});
                }

                var last = &self.stack.items[self.stack.items.len - 1];
                last.maybe_new_elem_tag_name = try self.data_allocator.dupe(u8, token.inner);
                last.maybe_new_elem_attributes = std.ArrayList(Tree.Node.Elem.Attr).init(self.data_allocator);
                try self.stack.append(.{
                    .maybe_open_token = token,
                    .children = std.ArrayList(Tree.Node).init(self.data_allocator),
                });
            },
            .element_close, .element_close_self => {
                var last = &self.stack.items[self.stack.items.len - 1];
                std.debug.assert(last.maybe_new_elem_tag_name == null);
                std.debug.assert(last.maybe_new_elem_attributes == null);

                if (self.stack.items.len == 1) {
                    try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                    return;
                }
                const children = try last.children.toOwnedSlice();
                _ = self.stack.pop();
                last = &self.stack.items[self.stack.items.len - 1];
                std.debug.assert(last.maybe_new_elem_tag_name != null);
                std.debug.assert(last.maybe_new_elem_attributes != null);

                if (token.kind == .element_close) {
                    if (!std.mem.eql(u8, last.maybe_new_elem_tag_name.?, token.inner)) {
                        try self.reportDefectOrExit(.TagNeverOpened, &.{token});
                    }
                }

                try last.children.append(.{ .elem = .{
                    .tag_name = last.maybe_new_elem_tag_name.?,
                    .attributes = try last.maybe_new_elem_attributes.?.toOwnedSlice(),
                    .tree = switch (token.kind) {
                        .element_close => .{ .children = children },
                        .element_close_self => null,
                        else => unreachable,
                    },
                } });

                last.maybe_new_elem_tag_name = null;
                last.maybe_new_elem_attributes = null;
            },
            .element_attribute => {
                std.debug.assert(self.stack.items.len > 1);
                var last2 = &self.stack.items[self.stack.items.len - 2];
                std.debug.assert(last2.maybe_new_elem_attributes != null);

                try last2.maybe_new_elem_attributes.?.append(.{
                    .name = try self.data_allocator.dupe(u8, token.inner),
                    .value = null,
                });
            },
            .element_attribute_value => {
                std.debug.assert(self.stack.items.len > 1);
                var last2 = &self.stack.items[self.stack.items.len - 2];
                std.debug.assert(last2.maybe_new_elem_attributes != null);
                const attributes = &last2.maybe_new_elem_attributes.?;
                std.debug.assert(attributes.items.len > 0);

                attributes.items[attributes.items.len - 1].value =
                    try self.data_allocator.dupe(u8, token.inner);
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
        }
    }

    pub fn finalise(self: *TreeBuilder) !Tree {
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

fn parseFromSliceImpl(allocator: std.mem.Allocator, slice: []const u8, maybe_diagnostics: ?*Diagnostics) !OwnedTree {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var scanner = Scanner.fromSlice(slice);
    var builder = try TreeBuilder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    while (try scanner.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
    };
}

pub fn parseFromSliceDiagnostics(allocator: std.mem.Allocator, slice: []const u8, diagnostics: *Diagnostics) !OwnedTree {
    const fromSliceDiagnostics = try parseFromSliceImpl(allocator, slice, diagnostics);
    return fromSliceDiagnostics;
}

pub fn fromSlice(allocator: std.mem.Allocator, slice: []const u8) !OwnedTree {
    return parseFromSliceImpl(allocator, slice, null);
}

fn fromReaderImpl(allocator: std.mem.Allocator, reader: anytype, maybe_diagnostics: ?*Diagnostics) !OwnedTree {
    var arena = std.heap.ArenaAllocator.init(allocator);
    errdefer arena.deinit();

    var xmlReader = Scanner.staticBufferReader(reader);
    var builder = try TreeBuilder.init(allocator, arena.allocator(), maybe_diagnostics);
    defer builder.deinit();

    while (try xmlReader.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
    };
}

pub fn fromReaderDiagnostics(allocator: std.mem.Allocator, reader: anytype, diagnostics: *Diagnostics) !OwnedTree {
    const result = try fromReaderImpl(allocator, reader, diagnostics);
    return result;
}

pub fn fromReader(allocator: std.mem.Allocator, reader: anytype) !OwnedTree {
    return fromReaderImpl(allocator, reader, null);
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

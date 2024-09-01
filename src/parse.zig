const std = @import("std");
const Scanner = @import("./Scanner.zig");

pub const Tree = struct {
    pub const Node = union(enum) {
        pub const Elem = struct {
            pub const Attr = struct {
                name: []const u8,
                value: ?[]const u8,
            };

            tagName: []const u8,
            attributes: []Attr,
            tree: ?Tree,
        };

        pub const Text = struct {
            contents: []const u8,

            pub fn trimmed(self: Text) []const u8 {
                _ = self;
            }
        };

        elem: Elem,
        text: Text,
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
        new_elem_tag_name: ?[]const u8 = null,
        new_elem_attributes: ?std.ArrayList(Tree.Node.Elem.Attr) = null,

        children: std.ArrayList(Tree.Node),
    };

    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,

    stack: std.ArrayList(TempTree),

    pub fn init(temp_allocator: std.mem.Allocator, data_allocator: std.mem.Allocator) !TreeBuilder {
        const root: TempTree = .{
            .children = std.ArrayList(Tree.Node).init(data_allocator),
        };
        var stack = try std.ArrayList(TempTree).initCapacity(temp_allocator, 8);
        try stack.append(root);
        return .{
            .temp_allocator = temp_allocator,
            .data_allocator = data_allocator,
            .stack = stack,
        };
    }

    pub fn deinit(self: TreeBuilder) void {
        self.stack.deinit();
    }

    pub fn feedToken(self: *TreeBuilder, token: Scanner.Token) !void {
        std.debug.assert(self.stack.items.len > 0);
        switch (token.kind) {
            .element_open => {
                var last = &self.stack.items[self.stack.items.len - 1];
                last.new_elem_tag_name = try self.data_allocator.dupe(u8, token.inner);
                last.new_elem_attributes = std.ArrayList(Tree.Node.Elem.Attr).init(self.data_allocator);
                try self.stack.append(.{
                    .children = std.ArrayList(Tree.Node).init(self.data_allocator),
                });
            },
            .element_close, .element_close_self => {
                var last = &self.stack.items[self.stack.items.len - 1];
                std.debug.assert(last.new_elem_tag_name == null);
                std.debug.assert(last.new_elem_attributes == null);

                const children = try last.children.toOwnedSlice();
                _ = self.stack.pop();
                if (self.stack.items.len == 0) {
                    std.debug.panic("Unexpected closing tag on root", .{});
                }
                last = &self.stack.items[self.stack.items.len - 1];
                std.debug.assert(last.new_elem_tag_name != null);
                std.debug.assert(last.new_elem_attributes != null);

                if (token.kind == .element_close) {
                    std.debug.assert(std.mem.eql(u8, last.new_elem_tag_name.?, token.inner));
                }

                try last.children.append(.{ .elem = .{
                    .tagName = last.new_elem_tag_name.?,
                    .attributes = try last.new_elem_attributes.?.toOwnedSlice(),
                    .tree = switch (token.kind) {
                        .element_close => .{ .children = children },
                        .element_close_self => null,
                        else => unreachable,
                    },
                } });

                last.new_elem_tag_name = null;
                last.new_elem_attributes = null;
            },
            .element_attribute => {
                std.debug.assert(self.stack.items.len > 1);
                var last2 = &self.stack.items[self.stack.items.len - 2];
                std.debug.assert(last2.new_elem_attributes != null);

                try last2.new_elem_attributes.?.append(.{
                    .name = try self.data_allocator.dupe(u8, token.inner),
                    .value = null,
                });
            },
            .element_attribute_value => {
                std.debug.assert(self.stack.items.len > 1);
                var last2 = &self.stack.items[self.stack.items.len - 2];
                std.debug.assert(last2.new_elem_attributes != null);
                const attributes = &last2.new_elem_attributes.?;
                std.debug.assert(attributes.items.len > 0);

                attributes.items[attributes.items.len - 1].value =
                    try self.data_allocator.dupe(u8, token.inner);
            },
            .meta_attribute => {},
            .meta_attribute_value => {},
            .doctype => {},
            .text_chunk => {
                var last = &self.stack.items[self.stack.items.len - 1];
                if (last.children.items.len > 0) {
                    const last_node = &last.children.items[last.children.items.len - 1];
                    switch (last_node.*) {
                        .text => |*text_node| {
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
        std.debug.assert(self.stack.items.len == 1);

        var root = self.stack.pop();
        std.debug.assert(root.new_elem_tag_name == null);
        std.debug.assert(root.new_elem_attributes == null);

        return .{
            .children = try root.children.toOwnedSlice(),
        };
    }
};

pub fn parseFromSlice(allocator: std.mem.Allocator, slice: []const u8) !OwnedTree {
    var arena = std.heap.ArenaAllocator.init(allocator);
    var scanner = Scanner.fromSlice(slice);
    var builder = try TreeBuilder.init(allocator, arena.allocator());
    defer builder.deinit();

    while (try scanner.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
    };
}

pub fn parseFromReader(allocator: std.mem.Allocator, reader: anytype) !OwnedTree {
    var arena = std.heap.ArenaAllocator.init(allocator);
    var xmlReader = Scanner.staticBufferReader(reader);
    var builder = try TreeBuilder.init(allocator, arena.allocator());
    defer builder.deinit();

    while (try xmlReader.next()) |token| {
        try builder.feedToken(token);
    }

    return .{
        .arena = arena,
        .tree = try builder.finalise(),
    };
}

test parseFromReader {
    const buf = "<div betrayed-by=\"judas\">jesus <p>christ</p> lord <amen/></div>";
    var fba = std.io.fixedBufferStream(buf);

    const parsed = try parseFromReader(std.testing.allocator, fba.reader());
    defer parsed.deinit();

    try std.testing.expectEqual(parsed.tree.children.len, 1);
    try std.testing.expect(parsed.tree.children[0] == .elem);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tagName, "div");
    try std.testing.expectEqual(parsed.tree.children[0].elem.attributes.len, 1);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.attributes[0].name, "betrayed-by");
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.attributes[0].value.?, "judas");
    try std.testing.expect(parsed.tree.children[0].elem.tree != null);
    try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children.len, 4);

    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[0] == .text);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[0].text.contents, "jesus ");

    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1] == .elem);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[1].elem.tagName, "p");
    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[0] == .text);
    try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[1].elem.attributes.len, 0);
    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1].elem.tree != null);
    try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children.len, 1);
    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0] == .text);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[1].elem.tree.?.children[0].text.contents, "christ");

    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[2] == .text);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[2].text.contents, " lord ");

    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[3] == .elem);
    try std.testing.expectEqualSlices(u8, parsed.tree.children[0].elem.tree.?.children[3].elem.tagName, "amen");
    try std.testing.expectEqual(parsed.tree.children[0].elem.tree.?.children[3].elem.attributes.len, 0);
    try std.testing.expect(parsed.tree.children[0].elem.tree.?.children[3].elem.tree == null);
}

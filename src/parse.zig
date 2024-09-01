const std = @import("std");
const Scanner = @import("./Scanner.zig");

pub const Tree = struct {
    pub const Node = union(enum) {
        pub const Elem = struct {
            tagName: []const u8,
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
    data_allocator: std.mem.Allocator,
    elem_stack: std.ArrayList(Tree.Node.Elem),

    children: std.ArrayList(Tree.Node),

    pub fn init(temp_allocator: std.mem.Allocator, data_allocator: std.mem.Allocator) TreeBuilder {
        return .{
            .arena = data_allocator,
            .stack = std.ArrayList(Tree.Node.Elem).init(temp_allocator),
            .children = std.ArrayList(Tree.Node).init(temp_allocator),
        };
    }

    pub fn deinit(self: TreeBuilder) void {
        self.stack.deinit();
        self.children.deinit();
    }

    pub fn feedToken(token: Scanner.Token) !void {}
};

pub fn parseFromSlice(allocator: std.mem.Allocator, slice: []const u8) !OwnedTree {
    const arena = std.heap.ArenaAllocator.init(allocator);
    var scanner = Scanner.fromSlice(slice);
}

pub fn parseFromReader(allocator: std.mem.Allocator, reader: anytype) !OwnedTree {
    const arena = std.heap.ArenaAllocator.init(allocator);
    var xmlReader = Scanner.staticBufferReader(reader);
}

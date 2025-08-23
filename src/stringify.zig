const std = @import("std");
const Tree = @import("./parse.zig").Tree;

const Options = struct {
    const IndentStyle = union(enum) {
        none: void,
        tabs: void,
        spaces: usize,
    };

    const TextFormattingStyle = union(enum) {
        verbatim: void,
        max_width: usize,
    };

    indent_style: IndentStyle = .{
        .spaces = 4,
    },

    // Whether or not text content should be trimmed and aligned as if whitespace
    // was limited to 1 (one) space.
    format_text_content: TextFormattingStyle = .{ .max_width = 80 },
};

fn writeTextFormatted(indentation: usize, text: []const u8, max_width: usize) !void {}

fn writeTreeImpl(tree: Tree, options: Options, writer: anytype, depth: usize) !void {
    for (tree.children) |child| {
        switch (child) {
            .elem => |elem_child| {},
            .text => |text_child| {
                if (options.format_text_content) {
                    writeTextFormatted(text_child.contents, options, writer);
                } else {
                    writer.write(text_child.contents);
                }
            },
            .comment => |comment_child| {},
        }
    }
}

pub fn writeTree(tree: Tree, options: Options, writer: anytype) !void {
    try writeTreeImpl(tree, options, writer, 0);
}

test writeTree {
    var tree: Tree = .{ .children = &.{} };
    var array = std.array_list.Managed(u8).init(std.testing.allocator);
    defer array.deinit();

    writeTree(tree, .{}, array.writer());
}

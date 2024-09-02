const std = @import("std");
const parse = @import("./parse.zig");

const Tree = parse.Builder.Tree;

pub const ContentError = error{
    MissingAttribute,
    UnexpectedAttributeValue,
    MissingAttributeValue,
    MissingChild,
    MissingOption,
    MissingPatternMatch,
};

pub fn PopulateShapeHeirarchy(comptime T: type, comptime shapes: anytype) type {
    const last_shape = shapes[shapes.len - 1];

    const dest_type_info: std.builtin.Type = @typeInfo(T);
    const ShapeType = @TypeOf(last_shape);
    const shape_type_info = @typeInfo(ShapeType);

    const shape_print = std.fmt.comptimePrint("{}", .{last_shape});

    return struct {
        pub const OwnedDocument = struct {
            owned_tree: Tree.Owned,
            value: T,

            pub fn deinit(self: OwnedDocument) void {
                self.owned_tree.deinit();
            }
        };

        pub fn fromTreeImpl(
            allocator: std.mem.Allocator,
            tree: Tree,
            attributes: []const Tree.Node.Elem.Attr,
            val: *T,
        ) (std.mem.Allocator.Error || ContentError)!void {
            switch (shape_type_info) {
                .type => {
                    if (last_shape == Tree) {
                        if (T != Tree) {
                            @compileError("Shape " ++ @typeName(Tree) ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be the Tree type");
                        }
                        val.* = tree;
                        return;
                    }
                    if (dest_type_info == .pointer and dest_type_info.pointer.size == .One) {
                        const child_type = dest_type_info.pointer.child;
                        val.* = try allocator.create(child_type);
                        errdefer allocator.destroy(val.*);

                        val.*.* = try Populate(last_shape).initFromTreeImpl(
                            allocator,
                            tree,
                            attributes,
                        );
                        return;
                    }
                    if (T != last_shape) {
                        @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T));
                    }
                    val.* = try Populate(last_shape).initFromTreeImpl(
                        allocator,
                        tree,
                        attributes,
                    );
                },
                .@"struct" => |struct_info| {
                    if (struct_info.is_tuple) {
                        if (struct_info.fields.len == 2 and last_shape[0] == .attribute) {
                            if (T != []const u8 and T != []u8) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a string type");
                            }

                            val.* = for (attributes) |attribute| {
                                if (std.mem.eql(u8, attribute.name, last_shape[1])) {
                                    break attribute.value orelse return ContentError.MissingAttributeValue;
                                }
                            } else return ContentError.MissingAttribute;
                        } else if (struct_info.fields.len == 2 and last_shape[0] == .attribute_exists) {
                            if (T != bool) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a boolean type");
                            }

                            val.* = for (attributes) |attribute| {
                                if (std.mem.eql(u8, attribute.name, last_shape[1])) {
                                    break true;
                                }
                            } else if (T == bool) false;
                        } else if (struct_info.fields.len == 2 and last_shape[0] == .maybe) {
                            if (dest_type_info != .optional) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be optional");
                            }

                            const ChildType = dest_type_info.optional.child;

                            const child_shape = last_shape[1];

                            val.* = PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                allocator,
                                tree,
                                attributes,
                            ) catch |e| switch (e) {
                                error.MissingAttribute,
                                error.UnexpectedAttributeValue,
                                error.MissingAttributeValue,
                                error.MissingChild,
                                error.MissingOption,
                                error.MissingPatternMatch,
                                => null,
                                else => return e,
                            };
                        } else if (struct_info.fields.len == 3 and last_shape[0] == .elements) {
                            if (dest_type_info != .pointer or dest_type_info.pointer.size != .Slice) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a slice type");
                            }

                            const ChildType = dest_type_info.pointer.child;

                            const tag_name = last_shape[1];
                            const child_shape = last_shape[2];

                            var elems = std.ArrayList(ChildType).init(allocator);

                            for (tree.children) |child| {
                                switch (child) {
                                    .elem => |elem_child| {
                                        if (std.mem.eql(u8, elem_child.tag_name, tag_name)) {
                                            try elems.append(try PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                                allocator,
                                                elem_child.tree orelse .{ .children = &.{} },
                                                elem_child.attributes,
                                            ));
                                        }
                                    },
                                    else => {},
                                }
                            }

                            val.* = try elems.toOwnedSlice();
                        } else if (struct_info.fields.len == 3 and last_shape[0] == .element) {
                            const tag_name = last_shape[1];
                            const child_shape = last_shape[2];

                            const elem: Tree.Node.Elem = for (tree.children) |child| {
                                switch (child) {
                                    .elem => |elem_child| {
                                        if (std.mem.eql(u8, elem_child.tag_name, tag_name)) {
                                            break elem_child;
                                        }
                                    },
                                    else => {},
                                }
                            } else return ContentError.MissingChild;

                            try PopulateShape(T, child_shape).fromTreeImpl(
                                allocator,
                                elem.tree orelse .{ .children = &.{} },
                                elem.attributes,
                                val,
                            );
                        } else if (struct_info.fields.len > 2 and last_shape[0] == .one_of) {
                            if (dest_type_info != .@"union") {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a union type");
                            }

                            const union_fields = dest_type_info.@"union".fields;
                            const num_child_shapes = struct_info.fields.len - 1;

                            if (union_fields.len != num_child_shapes) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", mismatched number of branches");
                            }

                            val.* = inline for (union_fields, 0..num_child_shapes) |field, i| {
                                const shape = last_shape[1 + i];
                                const child_shape_type_info = @typeInfo(@TypeOf(shape));
                                if (child_shape_type_info == .enum_literal and shape == .none) {
                                    if (field.type != void) {
                                        @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be null");
                                    }
                                    break @unionInit(T, field.name, {});
                                }
                                const maybe_found = PopulateShape(field.type, shape).initFromTreeImpl(
                                    allocator,
                                    tree,
                                    attributes,
                                ) catch |e| switch (e) {
                                    error.MissingAttribute,
                                    error.UnexpectedAttributeValue,
                                    error.MissingAttributeValue,
                                    error.MissingChild,
                                    error.MissingOption,
                                    error.MissingPatternMatch,
                                    => null,
                                    else => return e,
                                };
                                if (maybe_found) |found| {
                                    break @unionInit(T, field.name, found);
                                }
                            } else return ContentError.MissingOption;
                        } else {
                            @compileError("Unknown shape: " ++ shape_print);
                        }
                    } else {
                        if (dest_type_info != .@"struct") {
                            @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a struct type");
                        }

                        const struct_fields = dest_type_info.@"struct".fields;
                        const shape_fields = struct_info.fields;

                        inline for (shape_fields) |shape_field| {
                            const base_field = comptime for (struct_fields) |field| {
                                if (std.mem.eql(u8, field.name, shape_field.name)) {
                                    break field;
                                }
                            } else @compileError("Missing field '" ++ shape_field.name ++ "' on base type " ++ @typeName(T));

                            const shape_field_val = @field(last_shape, shape_field.name);
                            @field(val.*, shape_field.name) = try PopulateShape(base_field.type, shape_field_val)
                                .initFromTreeImpl(allocator, tree, attributes);
                        }
                    }
                },
                .enum_literal => {
                    if (last_shape == .content or last_shape == .content_trimmed) {
                        if (T != []const u8 and T != []u8) {
                            @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a string type");
                        }

                        var content_length: usize = 0;
                        for (tree.children) |child| {
                            switch (child) {
                                .text => |text_child| content_length += text_child.contents.len,
                                else => {},
                            }
                        }
                        const combined = try allocator.alloc(u8, content_length);
                        errdefer allocator.free(combined);

                        var cursor: usize = 0;
                        for (tree.children) |child| {
                            switch (child) {
                                .text => |text_child| {
                                    @memcpy(combined[cursor .. cursor + text_child.contents.len], text_child.contents);
                                    cursor += text_child.contents.len;
                                },
                                else => {},
                            }
                        }

                        if (last_shape == .content_trimmed) {
                            defer allocator.free(combined);

                            const trimmed = std.mem.trim(u8, combined, &std.ascii.whitespace);
                            errdefer allocator.free(trimmed);

                            val.* = try allocator.dupe(u8, trimmed);
                        } else {
                            val.* = combined;
                        }
                        return;
                    }
                },
                else => @compileError("Unknown destination type " ++ shape_print),
            }
        }

        pub fn initFromTreeImpl(allocator: std.mem.Allocator, tree: Tree, attributes: []const Tree.Node.Elem.Attr) !T {
            var val: T = undefined;
            switch (dest_type_info) {
                .@"struct" => |structInfo| {
                    inline for (structInfo.fields) |field| {
                        if (field.default_value) |default_value| {
                            @field(val, field.name) = @as(*field.type, @alignCast(@ptrCast(default_value))).*;
                        }
                    }
                },
                else => {},
            }
            try fromTreeImpl(allocator, tree, attributes, &val);
            return val;
        }

        pub fn fromTreeOwned(allocator: std.mem.Allocator, tree: Tree, val: *T) !void {
            try fromTreeImpl(allocator, tree, &.{}, val);
        }

        pub fn initFromTreeOwned(allocator: std.mem.Allocator, tree: Tree) !T {
            return try initFromTreeImpl(allocator, tree, &.{});
        }

        pub fn fromReader(allocator: std.mem.Allocator, reader: anytype, val: *T) !std.heap.ArenaAllocator {
            var owned_tree = try parse.fromReader(allocator, reader);
            try fromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree, val);
            return owned_tree.arena;
        }

        pub fn initFromReader(allocator: std.mem.Allocator, reader: anytype) !OwnedDocument {
            var owned_tree = try parse.fromReader(allocator, reader);
            const value = try initFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree);
            return .{ .owned_tree = owned_tree, .value = value };
        }

        pub fn fromSlice(allocator: std.mem.Allocator, slice: []const u8, val: *T) !std.heap.ArenaAllocator {
            var owned_tree = try parse.fromSlice(allocator, slice);
            try fromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree, val);
            return owned_tree.arena;
        }

        pub fn initFromSlice(allocator: std.mem.Allocator, slice: []const u8) !OwnedDocument {
            var owned_tree = try parse.fromSlice(allocator, slice);
            const value = initFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree);
            return .{ .owned_tree = owned_tree, .value = value };
        }
    };
}

pub fn PopulateShape(comptime T: type, comptime shape: anytype) type {
    return PopulateShapeHeirarchy(T, .{shape});
}

pub fn Populate(comptime T: type) type {
    if (T == Tree) {
        return PopulateShape(T, Tree);
    }

    if (@hasDecl(T, "xml_shape")) {
        return PopulateShape(T, T.xml_shape);
    }

    @compileError("Type " ++ @typeName(T) ++ " needs an 'xml_shape' declaration to automatically deserialise from XML");
}

const Person = struct {
    pub const xml_shape = .{
        .name = .content_trimmed,
        .age = .{ .attribute, "age" },
        .jobs = .{ .elements, "job", .{
            .start_date = .{ .attribute, "start_date" },
            .end_date = .{ .attribute, "end_date" },
            .title = .content_trimmed,
            .fired = .attribute_exists,
        } },
        .location = .{
            .one_of,
            .{ .element, "house", .content },
            .{ .element, "work", .content },
            .none,
        },
        .apprentice = .{ .maybe, .{ .element, "apprentice", Person } },
        .children = .{ .elements, "child", Person },
        .custom_data = .{ .maybe, .{ .element, "custom_data", Tree } },
    };

    name: []const u8,
    age: []const u8,
    jobs: []struct {
        start_date: []const u8,
        end_date: []const u8,
        title: []const u8,
        fired: bool,
    },
    location: union(enum) {
        house: []const u8,
        work: []const u8,
        none: void,
    },
    apprentice: ?*Person,
    children: []Person,
    custom_data: ?Tree,
};

pub const Document = struct {
    pub const xml_shape = .{
        .people = .{ .elements, "person", Person },
    };

    people: []Person,
};

test Populate {
    const buf =
        \\<!-- not biblically accurate... -->
        \\<person age="42">
        \\    Judas
        \\    <job start_date="2019" end_date="2022">software engineeer</job>
        \\    <job start_date="2022" end_date="2023">dishwasher</job>
        \\    <job start_date="2023" end_date="-">door-to-door salesman</job>
        \\    <house>Nazereth</house>
        \\    <custom_data>
        \\        <temperature unit="celcius">36</temperature>
        \\        <favourite_color>blue</favourite_color>
        \\    </custom_data>
        \\</person>
        \\<person age="40">
        \\    Paul
        \\    <job start_date="40" end_date="-">apostle</job>
        \\    <work>Tarsus</work>
        \\    <child age="20">
        \\        John
        \\        <apprentice age="18">
        \\            Jude
        \\        </apprentice>
        \\        <child age="2">
        \\            Jamie
        \\            <work>baby places</work>
        \\        </child>
        \\    </child>
        \\</person>
    ;

    var owned_tree = try parse.fromSlice(std.testing.allocator, buf);
    defer owned_tree.deinit();

    const populate: Document = try Populate(Document).initFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree);

    try std.testing.expectEqual(populate.people.len, 2);
    try std.testing.expectEqualSlices(u8, populate.people[0].name, "Judas");
    try std.testing.expectEqualSlices(u8, populate.people[0].age, "42");
    try std.testing.expectEqual(populate.people[0].jobs.len, 3);
    try std.testing.expect(populate.people[0].location == .house);
    try std.testing.expectEqualSlices(u8, populate.people[0].location.house, "Nazereth");
    try std.testing.expectEqualSlices(u8, populate.people[1].name, "Paul");
    try std.testing.expectEqualSlices(u8, populate.people[1].age, "40");
    try std.testing.expectEqual(populate.people[1].jobs.len, 1);
    try std.testing.expect(populate.people[1].location == .work);
    try std.testing.expectEqualSlices(u8, populate.people[1].location.work, "Tarsus");

    try std.testing.expect(populate.people[0].custom_data != null);
    try std.testing.expectEqual(populate.people[0].custom_data.?.children.len, 5);
    try std.testing.expect(populate.people[0].custom_data.?.children[1] == .elem);
    try std.testing.expectEqual(populate.people[0].custom_data.?.children[1].elem.attributes.len, 1);
    try std.testing.expectEqualSlices(u8, populate.people[0].custom_data.?.children[1].elem.attributes[0].name, "unit");
    try std.testing.expect(populate.people[0].custom_data.?.children[1].elem.attributes[0].value != null);
    try std.testing.expectEqualSlices(u8, populate.people[0].custom_data.?.children[1].elem.attributes[0].value.?, "celcius");
}

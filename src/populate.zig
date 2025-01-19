const std = @import("std");
const parse = @import("./parse.zig");

const Tree = parse.Tree;

pub const ContentError = error{
    MissingAttribute,
    UnexpectedAttributeValue,
    MissingAttributeValue,
    MissingChild,
    MissingOption,
    MissingPatternMatch,
};

pub const Mode = enum {
    compile_time,
    run_time,
};

fn PopulateShape(comptime T: type, comptime shape: anytype) type {
    const dest_type_info: std.builtin.Type = @typeInfo(T);
    const ShapeType = @TypeOf(shape);
    const shape_type_info = @typeInfo(ShapeType);

    const shape_print = std.fmt.comptimePrint("{}", .{shape});
    const cannot_be_applied = "Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T);

    return struct {
        pub const OwnedDocument = struct {
            owned_tree: Tree.Owned,
            value: T,

            pub fn deinit(self: OwnedDocument) void {
                self.owned_tree.deinit();
            }
        };

        pub fn deinit(allocator: std.mem.Allocator, val: T) void {
            switch (shape_type_info) {
                .type => {
                    if (shape == Tree) {
                        return;
                    }
                    if (dest_type_info == .pointer and dest_type_info.pointer.size == .One) {
                        Populate(shape).deinit(allocator, val.*);
                        allocator.destroy(val);
                        return;
                    }
                    Populate(shape).deinit(allocator, val);
                },
                .@"struct" => |struct_info| {
                    if (struct_info.is_tuple) {
                        if (struct_info.fields.len == 2 and shape[0] == .attribute) {
                            allocator.free(val);
                        } else if (struct_info.fields.len == 2 and shape[0] == .attribute_exists) {
                            // no free
                        } else if (struct_info.fields.len == 2 and shape[0] == .maybe) {
                            const ChildType = dest_type_info.optional.child;
                            const child_shape = shape[1];
                            if (val) |non_null| {
                                PopulateShape(ChildType, child_shape).deinit(allocator, non_null);
                            }
                        } else if (struct_info.fields.len == 3 and shape[0] == .elements) {
                            const ChildType = dest_type_info.pointer.child;
                            const child_shape = shape[2];

                            var i = val.len;
                            while (i > 0) {
                                i -= 1;
                                PopulateShape(ChildType, child_shape).deinit(allocator, val[i]);
                            }
                            allocator.free(val);
                        } else if (struct_info.fields.len == 3 and shape[0] == .element) {
                            const child_shape = shape[2];
                            PopulateShape(T, child_shape).deinit(allocator, val);
                        } else if (struct_info.fields.len > 2 and shape[0] == .one_of) {
                            const union_fields = dest_type_info.@"union".fields;
                            inline for (union_fields, 0..) |field, i| {
                                const child_shape = shape[1 + i];
                                if (std.mem.eql(u8, @tagName(val), field.name)) {
                                    PopulateShape(field.type, child_shape)
                                        .deinit(allocator, @field(val, field.name));
                                }
                            }
                        } else {
                            @compileError("Unknown shape: " ++ shape_print);
                        }
                    } else {
                        const struct_fields = dest_type_info.@"struct".fields;
                        const shape_fields = struct_info.fields;

                        comptime var i: usize = shape_fields.len;
                        inline while (i > 0) {
                            i -= 1;
                            const shape_field = shape_fields[i];
                            const base_field = comptime for (struct_fields) |field| {
                                if (std.mem.eql(u8, field.name, shape_field.name)) {
                                    break field;
                                }
                            } else @compileError("Missing field '" ++ shape_field.name ++ "' on base type " ++ @typeName(T));

                            const shape_field_val = @field(shape, shape_field.name);
                            PopulateShape(base_field.type, shape_field_val)
                                .deinit(allocator, @field(val, base_field.name));
                        }
                    }
                },
                .enum_literal => {
                    if (shape == .content or shape == .content_trimmed) {
                        allocator.free(val);
                    }
                },
                else => @compileError("Unknown shape type " ++ shape_print),
            }
        }

        fn fromTreeImpl(
            comptime mode: Mode,
            allocator: std.mem.Allocator,
            tree: Tree,
            attributes: []const Tree.Node.Elem.Attr,
            val: *T,
        ) (std.mem.Allocator.Error || ContentError)!void {
            switch (shape_type_info) {
                .type => {
                    if (shape == Tree) {
                        if (T != Tree) {
                            @compileError(cannot_be_applied ++ ", must be the Tree type");
                        }
                        val.* = tree;
                        return;
                    }
                    if (dest_type_info == .pointer and dest_type_info.pointer.size == .One) {
                        const child_type = dest_type_info.pointer.child;
                        switch (mode) {
                            .compile_time => {
                                if (!dest_type_info.pointer.is_const) {
                                    @compileError(cannot_be_applied ++ ", must be a const pointer when populating at compile time");
                                }
                                val.* = &try Populate(shape).initFromTreeImpl(
                                    mode,
                                    allocator,
                                    tree,
                                    attributes,
                                );
                            },
                            .run_time => {
                                const out = try allocator.create(child_type);
                                errdefer allocator.destroy(out);

                                out.* = try Populate(shape).initFromTreeImpl(
                                    mode,
                                    allocator,
                                    tree,
                                    attributes,
                                );

                                val.* = out;
                            },
                        }
                        return;
                    }
                    if (T != shape) {
                        @compileError(cannot_be_applied);
                    }
                    val.* = try Populate(shape).initFromTreeImpl(
                        mode,
                        allocator,
                        tree,
                        attributes,
                    );
                },
                .@"struct" => |struct_info| {
                    if (struct_info.is_tuple) {
                        if (struct_info.fields.len == 2 and shape[0] == .attribute) {
                            if (T != []const u8 and T != []u8) {
                                @compileError(cannot_be_applied ++ ", must be a string type");
                            }

                            val.* = for (attributes) |attribute| {
                                if (std.mem.eql(u8, attribute.name, shape[1])) {
                                    const value = attribute.value orelse return ContentError.MissingAttributeValue;
                                    break switch (mode) {
                                        .compile_time => value,
                                        .run_time => try allocator.dupe(u8, value),
                                    };
                                }
                            } else return ContentError.MissingAttribute;
                        } else if (struct_info.fields.len == 2 and shape[0] == .attribute_exists) {
                            if (T != bool) {
                                @compileError(cannot_be_applied ++ ", must be a boolean type");
                            }

                            val.* = for (attributes) |attribute| {
                                if (std.mem.eql(u8, attribute.name, shape[1])) {
                                    break true;
                                }
                            } else if (T == bool) false;
                        } else if (struct_info.fields.len == 2 and shape[0] == .maybe) {
                            if (dest_type_info != .optional) {
                                @compileError(cannot_be_applied ++ ", must be optional");
                            }

                            const ChildType = dest_type_info.optional.child;
                            const child_shape = shape[1];

                            val.* = PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                mode,
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
                        } else if (struct_info.fields.len == 3 and shape[0] == .elements) {
                            if (dest_type_info != .pointer or dest_type_info.pointer.size != .Slice) {
                                @compileError(cannot_be_applied ++ ", must be a slice type");
                            }

                            const ChildType = dest_type_info.pointer.child;

                            const tag_name = shape[1];
                            const child_shape = shape[2];

                            switch (mode) {
                                .compile_time => {
                                    if (!dest_type_info.pointer.is_const) {
                                        @compileError(cannot_be_applied ++ ", must be a const slice when populating at compile time");
                                    }

                                    var result: []const ChildType = &.{};

                                    for (tree.children) |child| {
                                        switch (child) {
                                            .elem => |elem_child| {
                                                if (std.mem.eql(u8, elem_child.tag_name, tag_name)) {
                                                    result = result ++ .{try PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                                        mode,
                                                        allocator,
                                                        elem_child.tree orelse .{ .children = &.{} },
                                                        elem_child.attributes,
                                                    )};
                                                }
                                            },
                                            else => {},
                                        }
                                    }
                                    val.* = result;
                                },
                                .run_time => {
                                    var result = std.ArrayList(ChildType).init(allocator);
                                    errdefer result.deinit();
                                    errdefer {
                                        for (result.items) |item| {
                                            PopulateShape(ChildType, child_shape).deinit(allocator, item);
                                        }
                                    }

                                    for (tree.children) |child| {
                                        switch (child) {
                                            .elem => |elem_child| {
                                                if (std.mem.eql(u8, elem_child.tag_name, tag_name)) {
                                                    try result.append(try PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                                        mode,
                                                        allocator,
                                                        elem_child.tree orelse .{ .children = &.{} },
                                                        elem_child.attributes,
                                                    ));
                                                }
                                            },
                                            else => {},
                                        }
                                    }

                                    val.* = try result.toOwnedSlice();
                                },
                            }
                        } else if (struct_info.fields.len == 3 and shape[0] == .element) {
                            const tag_name = shape[1];
                            const child_shape = shape[2];

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
                                mode,
                                allocator,
                                elem.tree orelse .{ .children = &.{} },
                                elem.attributes,
                                val,
                            );
                        } else if (struct_info.fields.len > 2 and shape[0] == .one_of) {
                            if (dest_type_info != .@"union") {
                                @compileError(cannot_be_applied ++ ", must be a union type");
                            }

                            const union_fields = dest_type_info.@"union".fields;
                            const num_child_shapes = struct_info.fields.len - 1;

                            if (union_fields.len != num_child_shapes) {
                                @compileError(cannot_be_applied ++ ", mismatched number of branches");
                            }

                            val.* = inline for (union_fields, 0..num_child_shapes) |field, i| {
                                const child_shape = shape[1 + i];
                                const child_shape_type_info = @typeInfo(@TypeOf(child_shape));
                                if (child_shape_type_info == .enum_literal and child_shape == .none) {
                                    if (field.type != void) {
                                        @compileError(cannot_be_applied ++ ", must be null");
                                    }
                                    break @unionInit(T, field.name, {});
                                }
                                const maybe_found: ?field.type = PopulateShape(field.type, child_shape).initFromTreeImpl(
                                    mode,
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
                            @compileError(cannot_be_applied ++ ", must be a struct type");
                        }

                        const struct_fields = dest_type_info.@"struct".fields;
                        const shape_fields = struct_info.fields;

                        var i: usize = 0;
                        errdefer {
                            switch (mode) {
                                .compile_time => {},
                                .run_time => {
                                    // we have to reverse all of the fields by deinitialising
                                    // them. the problem is that the number of fields that were
                                    // initialised is runtime-known, whereas the fields themselves
                                    // need to be comptime known. so this code is weird.
                                    comptime var j: usize = shape_fields.len;
                                    inline while (j > 0) {
                                        j -= 1;
                                        if (j < i) {
                                            const shape_field = shape_fields[j];
                                            const base_field = comptime for (struct_fields) |field| {
                                                if (std.mem.eql(u8, field.name, shape_field.name)) {
                                                    break field;
                                                }
                                            } else @compileError("Missing field '" ++ shape_field.name ++ "' on base type " ++ @typeName(T));

                                            const shape_field_val = @field(shape, shape_field.name);
                                            PopulateShape(base_field.type, shape_field_val)
                                                .deinit(allocator, @field(val, base_field.name));
                                        }
                                    }
                                },
                            }
                        }

                        inline for (shape_fields) |shape_field| {
                            defer i += 1;

                            const base_field = comptime for (struct_fields) |field| {
                                if (std.mem.eql(u8, field.name, shape_field.name)) {
                                    break field;
                                }
                            } else @compileError(cannot_be_applied ++ ", missing field '" ++ shape_field.name);

                            const shape_field_val = @field(shape, shape_field.name);
                            @field(val.*, base_field.name) = try PopulateShape(base_field.type, shape_field_val)
                                .initFromTreeImpl(mode, allocator, tree, attributes);
                        }
                    }
                },
                .enum_literal => {
                    if (shape == .content or shape == .content_trimmed) {
                        if (T != []const u8 and T != []u8) {
                            @compileError(cannot_be_applied ++ ", must be a string type");
                        }

                        switch (mode) {
                            .compile_time => {
                                val.* = switch (shape) {
                                    .content => tree.concatTextComptime(),
                                    .content_trimmed => tree.concatTextTrimmedComptime(),
                                    else => unreachable,
                                };
                            },
                            .run_time => {
                                val.* = switch (shape) {
                                    .content => try tree.concatTextAlloc(allocator),
                                    .content_trimmed => try tree.concatTextTrimmedAlloc(allocator),
                                    else => unreachable,
                                };
                            },
                        }
                    }
                },
                else => @compileError("Unknown shape type " ++ shape_print),
            }
        }

        fn initFromTreeImpl(
            comptime mode: Mode,
            allocator: std.mem.Allocator,
            tree: Tree,
            attributes: []const Tree.Node.Elem.Attr,
        ) !T {
            var val: T = undefined;
            switch (dest_type_info) {
                .@"struct" => |structInfo| {
                    inline for (structInfo.fields) |field| {
                        if (field.default_value_ptr) |default_value| {
                            @field(val, field.name) = @as(*field.type, @constCast(@alignCast(@ptrCast(default_value)))).*;
                        }
                    }
                },
                else => {},
            }
            try fromTreeImpl(mode, allocator, tree, attributes, &val);
            return val;
        }

        pub fn fromTreeComptime(comptime tree: Tree, val: *T) !void {
            try fromTreeImpl(.compile_time, undefined, tree, &.{}, val);
        }

        pub fn initFromTreeComptime(comptime tree: Tree) T {
            return comptime try initFromTreeImpl(.compile_time, undefined, tree, &.{});
        }

        pub fn fromTreeOwned(allocator: std.mem.Allocator, tree: Tree, val: *T) !void {
            try fromTreeImpl(.run_time, allocator, tree, &.{}, val);
        }

        pub fn initFromTreeOwned(allocator: std.mem.Allocator, tree: Tree) !T {
            return try initFromTreeImpl(.run_time, allocator, tree, &.{});
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

        pub fn fromSliceComptime(slice: []const u8, val: *T) !void {
            const tree = comptime try parse.fromSliceComptime(slice);
            fromTreeComptime(tree, val);
        }

        pub fn initFromSlice(allocator: std.mem.Allocator, slice: []const u8) !OwnedDocument {
            var owned_tree = try parse.fromSlice(allocator, slice);
            const value = initFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree);
            return .{ .owned_tree = owned_tree, .value = value };
        }

        pub fn initFromSliceComptime(comptime slice: []const u8) T {
            const tree = comptime parse.fromSliceComptime(slice);
            return comptime initFromTreeComptime(tree);
        }
    };
}

pub fn ShapeTypeFromType(comptime T: type) type {
    if (T == Tree) {
        return type;
    }

    if (@hasDecl(T, "xml_shape")) {
        return @TypeOf(T.xml_shape);
    }

    @compileError("Type " ++ @typeName(T) ++ " needs an 'xml_shape' declaration to deserialise from XML");
}

pub fn shapeFromType(comptime T: type) ShapeTypeFromType(T) {
    if (T == Tree) {
        return Tree;
    }

    if (@hasDecl(T, "xml_shape")) {
        return T.xml_shape;
    }
}

pub fn Populate(comptime T: type) type {
    return PopulateShape(T, shapeFromType(T));
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
    jobs: []const struct {
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
    apprentice: ?*const Person,
    children: []const Person,
    custom_data: ?Tree,
};

const Document = struct {
    pub const xml_shape = .{
        .people = .{ .elements, "person", Person },
    };

    people: []const Person,
};

const test_buf =
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

pub fn expectTestBufDocumentValid(document: Document) !void {
    try std.testing.expectEqual(document.people.len, 2);
    try std.testing.expectEqualSlices(u8, document.people[0].name, "Judas");
    try std.testing.expectEqualSlices(u8, document.people[0].age, "42");
    try std.testing.expectEqual(document.people[0].jobs.len, 3);
    try std.testing.expect(document.people[0].location == .house);
    try std.testing.expectEqualSlices(u8, document.people[0].location.house, "Nazereth");
    try std.testing.expectEqualSlices(u8, document.people[1].name, "Paul");
    try std.testing.expectEqualSlices(u8, document.people[1].age, "40");
    try std.testing.expectEqual(document.people[1].jobs.len, 1);
    try std.testing.expect(document.people[1].location == .work);
    try std.testing.expectEqualSlices(u8, document.people[1].location.work, "Tarsus");

    try std.testing.expect(document.people[0].custom_data != null);
    try std.testing.expectEqual(document.people[0].custom_data.?.children.len, 5);
    try std.testing.expect(document.people[0].custom_data.?.children[1] == .elem);
    try std.testing.expectEqual(document.people[0].custom_data.?.children[1].elem.attributes.len, 1);
    try std.testing.expectEqualSlices(u8, document.people[0].custom_data.?.children[1].elem.attributes[0].name, "unit");
    try std.testing.expect(document.people[0].custom_data.?.children[1].elem.attributes[0].value != null);
    try std.testing.expectEqualSlices(u8, document.people[0].custom_data.?.children[1].elem.attributes[0].value.?, "celcius");
}

test Populate {
    var owned_tree = try parse.fromSlice(std.testing.allocator, test_buf);
    defer owned_tree.deinit();

    const document: Document = try Populate(Document).initFromTreeOwned(std.testing.allocator, owned_tree.tree);
    defer Populate(Document).deinit(std.testing.allocator, document);

    try expectTestBufDocumentValid(document);
}

test "comptime populate" {
    @setEvalBranchQuota(8192);
    const tree = comptime parse.fromSliceComptime(test_buf);
    const document: Document = comptime Populate(Document).initFromTreeComptime(tree);

    try expectTestBufDocumentValid(document);
}

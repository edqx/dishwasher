const std = @import("std");
const parse = @import("./parse.zig");

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
            owned_tree: parse.OwnedTree,
            value: T,

            pub fn deinit(self: OwnedDocument) void {
                self.owned_tree.deinit();
            }
        };

        pub fn populateFromTreeImpl(
            allocator: std.mem.Allocator,
            tree: parse.Tree,
            attributes: []parse.Tree.Node.Elem.Attr,
            val: *T,
        ) !void {
            if (T == parse.Tree) {
                val.* = tree;
                return;
            }

            switch (shape_type_info) {
                .type => {
                    if (T != last_shape) {
                        @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeInfo(T));
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
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeInfo(T) + ", must be optional");
                            }

                            const ChildType = dest_type_info.optional.child;

                            const child_shape = last_shape[1];

                            val.* = PopulateShape(ChildType, child_shape).initFromTreeImpl(
                                allocator,
                                tree,
                                attributes,
                            ) catch |e| switch (e) {
                                ContentError => null,
                                else => return e,
                            };
                        } else if (struct_info.fields.len == 3 and last_shape[0] == .elements) {
                            if (dest_type_info != .pointer or dest_type_info.pointer.size != .Slice) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeInfo(T) + ", must be a slice type");
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

                            const elem: parse.Tree.Node.Elem = for (tree.children) |child| {
                                switch (child) {
                                    .elem => |elem_child| {
                                        if (std.mem.eql(u8, elem_child.tag_name, tag_name)) {
                                            break elem_child;
                                        }
                                    },
                                }
                            } else return ContentError.MissingChild;

                            val.* = PopulateShape(T, child_shape).initFromTreeImpl(
                                allocator,
                                elem.tree,
                                elem.attributes,
                            );
                        } else if (struct_info.fields.len > 2 and last_shape[0] == .one_of) {
                            if (dest_type_info != .@"union") {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a union type");
                            }

                            const union_fields = dest_type_info.@"union".fields;
                            const child_shapes = last_shape[1..];

                            if (union_fields.len != child_shapes.len) {
                                @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", mismatched number of branches");
                            }

                            val.* = for (union_fields, child_shapes) |field, shape| {
                                if (shape == .none) {
                                    if (field.type != void) {
                                        @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be null");
                                    }
                                    break @unionInit(T, field.name, {});
                                }
                                break @unionInit(T, field.name, PopulateShape(field.type, shape).initFromTreeImpl(
                                    allocator,
                                    tree,
                                    attributes,
                                ) catch |e| switch (e) {
                                    ContentError => continue,
                                    else => return e,
                                });
                            } else return ContentError.MissingOption;
                        } else { // pattern
                            // if (dest_type_info != .@"struct" or !dest_type_info.@"struct".is_tuple) {
                            //     @compileError("Shape " ++ shape_print ++ " cannot be applied to type " ++ @typeName(T) ++ ", must be a tuple type");
                            // }

                            // const struct_fields = dest_type_info.@"struct".fields;
                            // const shape_fields = struct_info.fields;

                            // if (struct_fields.len != shape_fields.len) {
                            //     @compileError("Pattern expected " ++ std.fmt.comptimePrint("{}", .{shape_fields.len}) ++ " fields in destination tuple, got " ++ std.fmt.comptimePrint("{}", .{struct_fields.len}));
                            // }

                            // if (tree.children.len < struct_fields.len) {
                            //     return ContentError.MissingPatternMatch;
                            // }

                            // for (0..tree.children.len - struct_fields.len + 1) |i| {
                            //     var out: T = undefined;
                            //     var flag = true;
                            //     inline for (0.., struct_fields, shape_fields) |j, struct_field, shape_field| {
                            //         out[j] = PopulateShape(struct_field.type, shape_field).initFromTreeImpl(
                            //             allocator,
                            //             .{ .children = &.{tree.children[i + j]} },
                            //             .{},
                            //         ) catch |e| switch (e) {
                            //             ContentError => {
                            //                 flag = false;
                            //                 break;
                            //             },
                            //             else => return e,
                            //         };
                            //     }
                            //     if (flag) {
                            //         val.* = out;
                            //         break;
                            //     }
                            // }
                            @compileError("Pattern not currently supported");
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

        pub fn initFromTreeImpl(allocator: std.mem.Allocator, tree: parse.Tree, attributes: []parse.Tree.Node.Elem.Attr) !T {
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
            try populateFromTreeImpl(allocator, tree, attributes, &val);
            return val;
        }

        pub fn populateFromTreeOwned(allocator: std.mem.Allocator, tree: parse.Tree, val: *T) !void {
            try populateFromTreeImpl(allocator, tree, &.{}, val);
        }

        pub fn initFromTreeOwned(allocator: std.mem.Allocator, tree: parse.Tree) !T {
            return try initFromTreeImpl(allocator, tree, &.{});
        }

        pub fn populateFromReader(allocator: std.mem.Allocator, reader: anytype, val: *T) !void {
            var owned_tree = try parse.fromReader(allocator, reader);
            try populateFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree, val);
        }

        pub fn initFromReader(allocator: std.mem.Allocator, reader: anytype) !OwnedDocument {
            var owned_tree = try parse.fromReader(allocator, reader);
            const value = initFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree);
            return .{ .owned_tree = owned_tree, .value = value };
        }

        pub fn populateFromSlice(allocator: std.mem.Allocator, slice: []const u8, val: *T) !void {
            var owned_tree = try parse.fromSlice(allocator, slice);
            try populateFromTreeOwned(owned_tree.arena.allocator(), owned_tree.tree, val);
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
    if (T == parse.Tree) {
        return PopulateShape(T, parse.Tree);
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
    };

    name: []const u8,
    age: []const u8,
    jobs: []struct {
        start_date: []const u8,
        end_date: []const u8,
        title: []const u8,
        fired: bool,
    },
};

pub const Document = struct {
    pub const xml_shape = .{
        .people = .{ .elements, "person", Person },
    };

    people: []Person,
};

test Populate {
    const buf =
        \\<person age="42">
        \\    Judas
        \\    <job start_date="2019" end_date="505">software engineeer</job>
        \\    <job start_date="2022" end_date="2023">dishwasher</job>
        \\    <job start_date="2023" end_date="-">door-to-door salesman</job>
        \\</person>
    ;

    var owned_tree = try parse.parseFromSlice(std.testing.allocator, buf);
    defer owned_tree.deinit();

    const populate = try Populate(parse.Tree).initFromTree(owned_tree.arena.allocator(), owned_tree.tree);

    // std.debug.print("document: {s}\n", .{populate.people[0].jobs[0].start_date});
    std.debug.print("populate {s}", .{populate.children[0].elem.tag_name});
}

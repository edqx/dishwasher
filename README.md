# DISHWASHER (An XML parser)

### Disclaimer: This XML parser is **NOT** compliant with the XML spec!

A relatively crude, but also relatively fast, XML parser for [Zig](https://ziglang.org) 0.13.0.

I needed a faster XML parser than the ones that were available, and I didn't need it to be spec compliant, so I wrote this and releasing it in case it is useful to anyone else.

### Benchmarks
Here are the results on my computer in different optimisation modes (i9-14900kf) when parsing the [OpenGL XML Spec document](https://github.com/KhronosGroup/OpenGL-Registry/blob/main/xml/gl.xml) fully, averaged over 100 runs for each (rounded to 2sf):
| Optimise Mode | Min | Max | Avg |
|-|-|-|-|
| `Debug` | `0.190s` | `0.500s` | `0.320s` |
| `ReleaseSafe` | `0.018s` | `0.038s` | `0.024s` |
| `ReleaseSmall` | `0.026s` | `0.150s` | `0.046s` |
| `ReleaseFast` | `0.018s` | `0.140s` | `0.031s` |

### Usage
#### Example
You can load an entire document and inspect the resulting tree with some utility methods on the elements:
```zig
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var ownedDocument = try dishwasher.parseXmlFull(gpa.allocator(), @embedFile("./gl-spec.xml"));
// You OWN the memory allocated in the result, use OwnedDocument.deinit once finished with it
defer ownedDocument.deinit();

const register = ownedDocument.doc.root.elementByTagName("registry") orelse unreachable;
const typesSet = register.elementByTagName("extensions") orelse unreachable;
const t = typesSet.elementByAttributeValue("name", "GL_EXT_direct_state_access") orelse unreachable;

try std.io.getStdOut().writer().print("comment: {s}", .{t.attributeValueByName("comment") orelse unreachable});
```

Alternatively, you can create a schema for the document in Zig to populate or create a value of a certain type from a document. Since there's no way to attach metadata to fields in structs, we'll need to create a schema using some helpful builder functions from Dishwasher:
```zig
pub const World = struct {
    pub const Province = struct {
        pub const XmlShape = .{
            .name = dw.attribute("name"),
            .description = dw.elementContent(.verbatim)
        };

        name: []const u8,
        description: []const u8
    };

    pub const Country = struct {
        pub const XmlShape = .{
            .name = dw.attribute("name"),
            .provinces = dw.manyElements("province", Province)
        };

        name: []const u8,
        provinces: []Province
    };

    pub const XmlShape = .{
        .countries = dw.singleElement("countries", dw.manyElements("country", Country)),
    };

    countries: []Country
};
...
// Dishwasher will take the "XmlShape" declaration in each struct to understand how to read them
// from the document.
var world: World = undefined;
try ownedDocument.doc.populateValueType(World, arena.allocator(), &world);
// Note: strings are copied from the document, so while you may deinitialise the document, you still OWN
// the resulting value, and moreover are responsible for freeing the strings. Use an arena
// to quickly and safely dispose of the entire value with all of the copied strings.

std.log.info("num countries: {}", .{ world.countries.len });
```

If you want to skip the types, Dishwasher can infer the final struct type given any shape:
```zig
pub const worldSchema = .{
    .countries = dw.singleElement("countries", dw.manyElements("country", .{
        .name = dw.attribute("name"),
        .provinces = dw.manyElements("province", .{
            .name = dw.attribute("name"),
            .description = dw.elementContent(.verbatim),
        }),
    })),
};
...
const world: dw.ShapeType(worldSchema) = try ownedDocument.doc.createValueShape(worldSchema, arena.allocator());

std.log.info("num countries: {}", .{ world.countries.len });
```
With an alternative syntax:
```zig
pub const worldSchema = .{
    .countries = dw.@"<>"("countries", dw.@"[]"("country", .{
        .name = dw.@"$"("name"),
        .provinces = dw.@"[]"("province", .{
            .name = dw.@"$"("name"),
            .description = dw.@"*"(.verbatim),
        }),
    })),
};
...
const world: dw.ShapeType(worldSchema) = try ownedDocument.doc.createValueShape(worldSchema, arena.allocator());

std.log.info("num countries: {}", .{ world.countries.len });
```

[Check out the tests](https://github.com/edqx/dishwasher/blob/master/src/main.zig) for more examples.

#### API

```zig
pub fn parseXml(allocator: std.mem.Allocator, reader: anytype) !OwnedDocument;
pub fn parseXmlFull(allocator: std.mem.Allocator, source: []const u8) !OwnedDocument;

pub fn maybe(comptime child: anytype) *const Shape;
// or
pub fn @"?"(comptime child: anytype) *const Shape;

pub fn singleElement(comptime tagName: []const u8, comptime child: anytype) *const Shape;
// or
pub fn @"<>"(comptime tagName: []const u8, comptime child: anytype) *const Shape;
pub fn @"?<>"(comptime tagName: []const u8, comptime child: anytype) *const Shape;

pub fn manyElements(comptime tagName: []const u8, comptime child: anytype) *const Shape;
// or
pub fn @"[]"(comptime tagName: []const u8, comptime child: anytype) *const Shape;

pub fn attribute(comptime attributeName: []const u8) *const Shape;
// or
pub fn @"$"(comptime tagName: []const u8) *const Shape;
pub fn @"?$"(comptime tagName: []const u8) *const Shape;

pub fn elementContent(contentMode: Shape.ContentMode) *const Shape;
// or
pub fn @"*"(contentMode: Shape.ContentMode) *const Shape;

pub fn pattern(comptime children: anytype) *const Shape;
// or
pub fn @"%"(comptime children: anytype) *const Shape;

pub fn disjunction(comptime branches: anytype) *const Shape;
// or
pub fn @"or"(comptime branches: anytype) *const Shape;

pub const OwnedDocument = struct {
    doc: Document,

    pub fn deinit(self: OwnedDocument) void;
};

pub const Document = struct {
    root: Node.Element,

    pub fn populateValueTypeShape(self: Document, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator, val: *T) !void;
    pub fn populateValueType(self: Document, comptime T: type, allocator: std.mem.Allocator, val: *T) !void;
    pub fn createValueTypeShape(self: Document, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator) !T;
    pub fn createValue(self: Document, comptime T: type, allocator: std.mem.Allocator) !T;
    pub fn createValueShape(self: Document, comptime shape: anytype, allocator: std.mem.Allocator) !ShapeType(shape);
};

pub const Node = union {
    pub const Element = struct {
        pub const Attribute = struct {
            name: []const u8,
            value: []const u8,
        };

        tagName: []const u8,

        pub fn attributeValueByName(self: Element, name: []const u8) ?[]const u8;
        // or
        pub fn attr(self: Element, name: []const u8) ?[]const u8;

        pub fn elementByTagName(self: Element, tagName: []const u8) ?Element;
        // or
        pub fn elem(self: Element, tagName: []const u8) ?Element;

        pub fn elementsByTagNameAlloc(self: Element, allocator: std.mem.Allocator, tagName: []const u8) ![]Element;
        // or
        pub fn elems(self: Element, allocator: std.mem.Allocator, tagName: []const u8) ![]Element;

        pub fn elementByAttributeValue(self: Element, attributeName: []const u8, attributeValue: []const u8) ?Element;
        pub fn textAlloc(self: Element, allocator: std.mem.Allocator) ![]const u8;
        
        pub fn populateValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator, val: *T) !void;
        pub fn populateValueType(self: Element, comptime T: type, allocator: std.mem.Allocator, val: *T) !void;
        pub fn createValueTypeShape(self: Element, comptime T: type, comptime shape: anytype, allocator: std.mem.Allocator) !T;
        pub fn createValue(self: Element, comptime T: type, allocator: std.mem.Allocator) !T;
        pub fn createValueShape(self: Element, comptime shape: anytype, allocator: std.mem.Allocator) !ShapeType(shape);
    }

    element: Element,
    text: []const u8,
    comment: []const u8,
};

pub const Shape = union(enum) {
    pub const ContentMode = enum(u1) {
        verbatim,
        trim,
    };
};
```

# DISHWASHER (An XML parser)

### Disclaimer: This XML parser is **NOT** compliant with the XML spec!

A relatively crude, but also relatively fast, XML parser for [Zig](https://ziglang.org) 0.12.0.

I needed a faster XML parser than the ones that were available, and I didn't need it to be spec compliant, so I wrote this and releasing it in case it is useful to anyone else.

### Benchmarks
Not many, but it can parse the full [OpenGL XML Spec document](https://github.com/KhronosGroup/OpenGL-Registry/blob/main/xml/gl.xml) in around 25 milliseconds on my computer (i9-14900kf).

### Usage
#### Example
```zig
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var ownedDocument = try dishwasher.parseXmlFull(gpa.allocator(), @embedFile("./gl-spec.xml"));
// You OWN the memory allocated in the result, use OwnedDocument.deinit once finished with it
defer ownedDocument.deinit();

const register = ownedDocument.doc.root.elementByTagName("registry") orelse unreachable;
const typesSet = register.elementByTagName("extensions") orelse unreachable;
const t = typesSet.elementByAttributeValue("name", "GL_EXT_direct_state_access") orelse unreachable;

try std.io.getStdout().writer().print("comment: {s}", .{t.attributeValueByName("comment") orelse unreachable});
```

#### API

```zig
pub fn parseXml(allocator: std.mem.Allocator, reader: anytype) !OwnedDocument;
pub fn parseXmlFull(allocator: std.mem.Allocator, source: []const u8) !OwnedDocument;

pub const OwnedDocument = struct {
    arena: std.heap.ArenaAllocator,
    doc: Document,

    pub fn deinit(self: OwnedDocument) void;
};

pub const Document = struct {
    root: Node.Element,
};

pub const Node = union {
    pub const Element = struct {
        pub const Attribute = struct {
            name: []const u8,
            value: []const u8,
        };

        tagName: []const u8,
        attributes: []Attribute,
        is_single: bool,
        children: []Node,

        pub fn attributeValueByName(self: Element, name: []const u8) ?[]const u8;
        pub fn elementByTagName(self: Element, tagName: []const u8) ?Element;
        pub fn elementsByTagName(self: Element, allocator: std.mem.Allocator, tagName: []const u8) ![]Element;
        pub fn elementByAttributeValue(self: Element, attributeName: []const u8, attributeValue: []const u8) ?Element;
        pub fn textContent(self: Element, allocator: std.mem.Allocator) ![]const u8;
    }

    element: Element,
    text: []const u8,
    comment: []const u8,
};
```
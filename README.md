# Dishwasher

A fairly fast XML parser for [Zig](https://ziglang.org).

Note that this parser isn't strictly spec-compliant, however it will probably
work with most well-formed xml documents.

## Features
- [x] Pretty speedy
- [x] Reader API-friendly
- [x] Can populate structs
- [x] Can populate dynamic values
- [x] Compile-time parsing
- [x] Diagnostics for malformed documents
- [ ] Stringification (coming soon)

### Benchmarks

Here are the results from the given benchmarks on my pc (i9-14900kf) in different
optimisation modes, when parsing the [OpenGL XML Spec document](https://github.com/KhronosGroup/OpenGL-Registry/blob/main/xml/gl.xml)
fully.

| Mode | Min | Max | Avg |
|------|-----|-----|-----|
| `Debug` | `100ms` | `280ms` | `131ms` |
| `ReleaseSafe` | `13ms` | `25ms` | `15ms` | 
| `ReleaseSmall` | `18ms` | `50ms` | `30ms` |
| `ReleaseFast` | `7ms` | `27ms` | `13ms` |

_All times are averaged over 100 runs, rounded to the nearest 2sf._

## Documentation

Generate documentation for dishwasher with `zig build docs`, will output browser files at `zig-out/docs`. You can serve
this however you like, for example with Python: `python -m http.server` or with NodeJS: `npx serve`.

## Usage

Dishwasher has 4 APIs, 3 of which will be most useful.

- [Parsing API](#parsing-api) - for parsing an entire XML document at runtime.
- [Populate API](#populate-api) - for mapping an XML document to a given struct.
- [Comptime Parsing and Populate API](#comptime-parsing-and-populate-api) - for parsing an entire XML document at compile time.
- [Scanner API](#scanner-api) - for iterating through XML symbols from a slice or reader.

### Parsing API
Dishwasher lets you parse an XML document from either an entire slice or a
reader into a tree-like structure that represents all nodes.

All of the parse methods create an arena which is returned back to you so that
you can deinitialise it when you no longer need the data.

#### Parse from a slice
```zig
const owned_tree = dishwasher.parse.fromSlice(allocator, xml_text);
defer owned_tree.deinit(); // all strings and lists will be free'd

std.debug.assert(owned_tree.tree.children[0] == .elem);
```

#### Parse from a reader
```zig
const owned_tree = dishwasher.parse.fromReader(allocator, file.reader());
defer owned_tree.deinit();

std.debug.assert(owned_tree.tree.children[0] == .elem);
```

#### Diagnostics
You can also get basic information about invalid documents using the parse
diagnostics struct, and passing it into either `parse.fromSliceDiagnostics`
or `parse.fromReaderDiagnostics`.
```zig
var diagnostics = dishwasher.parse.Diagnostics.init(allocator);
defer diagnostics.deinit();

const parsed = try dishwasher.parse.fromReaderDiagnostics(allocator, file.reader(), &diagnostics);
defer parsed.deinit();

for (diagnostics.defects.items) |defect| {
  std.debug.print("{} from {}..{}", .{ defect.kind, defect.range.start, defect.range.end });
}
```

#### Tree API
The returned tree has the following signature:
```zig
const Tree = struct {
    pub const Node = union(enum) {
        pub const Elem = struct {
            pub const Attr = struct {
                name: []const u8,
                value: ?[]const u8,
            };

            tag_name: []const u8,
            attributes: []const Attr,
            tree: ?Tree,

            // Get an attribute given its name.
            pub fn attributeByName(self: Elem, needle: []const u8) ?Attr;
            pub fn attr(self: Elem, needle: []const u8) ?Attr;

            // Get the value of an attribute given its name. Note that if the
            // attribute has no value, e.g., <button disabled> this will
            // still return null. Use attr or attributeByName in those
            // cases.
            pub fn attributeValueByName(self: Elem, needle: []const u8) ?[]const u8;
            pub fn attrValue(self: Elem, needle: []const u8) ?[]const u8;
        };

        pub const Text = struct {
            contents: []const u8,

            // Return the text without any whitespace at the beginning or end.
            pub fn trimmed(self: Text) []const u8;
        }

        pub const Comment = struct {
            contents: []const u8,
        };

        elem: Elem,
        text: text,
        comment: Comment,
    };

    children: []const Node,

    // Find an element child by its tag name
    pub fn elementByTagName(self: Tree, needle: []const u8) ?Node.Elem;
    pub fn elem(self: Tree, needle: []const u8) ?Node.Elem;
    
    // Allocate a slice for all of the element children of a given tag name
    // To free the returned slice, you can just call allocator.free(elements)
    // where 'elements' is the returned slice.
    pub fn elementsByTagNameAlloc(self: Tree, allocator: std.mem.Allocator, needle: []const u8) ![]Node.Elem;
    pub fn elemsAlloc(self: Tree, allocator: std.mem.Allocator, needle: []const u8) ![]Node.Elem;

    // Get an element by the value of one of its attributes
    pub fn elementByAttributeValue(self: Tree, needle_name: []const u8, needle_value: []const u8) ?Node.Elem;
    pub fn elemByAttr(self: Tree, needle_name: []const u8, needle_value: []const u8) ?Node.Elem;

    // Return the inner text (not including the elements) of the tree. Note that the
    // result will be entirely unformatted.
    pub fn concatTextAlloc(self: Tree, allocator: std.mem.Allocator) ![]const u8;
    // Return the inner text (not including the elements) of the tree but without
    // any whitespace at the start or end.
    pub fn concatTextTrimmedAlloc(self: Tree, allocator: std.mem.Allocator) ![]const u8;
}
```

### Populate API
Often, it's useful to be able to populate a given struct with values from an XML
document. That is, 'reading' the document into the struct.

Dishwasher comes with a shaping API so you can dictate how the document should
be read into the struct. Simply declare an `xml_shape` on the struct:

```zig
const Job = struct {
  title: []const u8,
  start_date: []const u8,
  end_date: []const u8,
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
};

pub const Register = struct {
    pub const xml_shape = .{
        .people = .{ .elements, "person", Person },
    };

    people: []Person,
};

const register = try diswasher.Populate(Register).initFromSlice(allocator, xml_text);
defer register.deinit();

// register.value: Register
```

Alternatively, you can populate an existing struct:
```zig
var register: Register = undefined;
const arena = try dishwasher.Populate(Register).fromSlice(allocator, xml_text, &register);
defer arena.deinit();
```

If you want to own all of your values yourself, use:
```zig
const owned_tree = dishwasher.parse.fromReader(allocator, file.reader());
defer owned_tree.deinit();

const register = try dishwasher.Populate(Register).initFromTreeOwned(allocator, owned_tree.tree);
// free 'register' values yourself..
```

#### Dynamic values
If some field accepts any sort of XML document shape, you can instruct it
to accept a `parse.Tree`:

```zig
const Register = struct {
  pub const xml_shape = .{
    .people = .{ .elements, "person", dishwasher.parse.Tree },
  };

  people: []dishwasher.parse.Tree,
};
```

> [!NOTE]
> Note that the tree is not duplicated for you, so if you use the `initFromTreeOwned` method, the values in the tree
> will still belong to the arena that was initialised for the tree.

#### Free struct
If you use the `initFromTreeOwned` population method, you can free all of the values in an arena-friendly way with:
```zig
dishwasher.Populate(Register).deinit(allocator, register);
```

### Comptime Parsing and Populate API
It could be useful to parse an XML document at compile time, for example
for some inline code generation. While comptime doesn't have allocators,
there's a custom API for this:

```zig
const tree = dishwasher.parse.fromSliceComptime(xml_text);
```

Check out the [Tree API](#tree-api) to know what to do with the returned value.

#### Comptime Populate API
If you want to populate a struct at compile time, you can use the `*Comptime` methods
on the `Populate` struct:
```zig
const register = dishwasher.Populate(Register).initFromSliceComptime(xml_text);
```

> [!NOTE]
> Remember that for the target struct, all pointers need to be `*const T` or
> `[]const T`.

### Scanner API
If you want low-level access to the iterator for lexing an XML document,
you can use the Scanner API, which accepts a slice buffer:
```zig
var xmlScanner = Scanner.fromSlice(xml_text);
while (try xmlScanner.next()) |token| {
  std.debug.print("token kind: ", .{ token.kind });
}
```

If you have a reader and no access to the entire slice, the Reader API
can connect any reader to the Scanner API so you can lex from a reader:
```zig
var xmlReader = Scanner.staticBufferReader(file.reader());
while (try xmlScanner.next()) |token| {
  std.debug.print("token kind: ", .{ token.kind });
}
```

> [!NOTE]
> If you're given an error about running out of buffer space, try increase
> the reader buffer size with
> ```zig
> const buffer_size = 4096;
> const XmlFileReader = zigScanner.StaticBufferReader(@TypeOf(file.reader()), buffer_size);
> const xmlReader = XmlFileReader.init(file.reader());
> ```

#### Comptime Scanner
The scanner works during compile time without any modification.

## License
All dishwasher code is under the MIT license.

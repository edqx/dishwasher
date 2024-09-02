# Dishwasher

A fairly fast XML parser for [Zig](https://ziglang.org).

Note that this parser isn't strictly spec-compliant, however it will probably
work with most well-formed xml documents.

## Features
- [x] Reader API-friendly
- [x] Compile-time parsing
- [x] Can populate structs
- [ ] Stringification (coming soon)

### Benchmarks

Here are the results from the given benchmarks on my laptop in different
optimisation modes, when parsing the [OpenGL XML Spec document](https://github.com/KhronosGroup/OpenGL-Registry/blob/main/xml/gl.xml)
fully.

| Mode | Min | Max | Avg |
|------|-----|-----|-----|
| `Debug` | `240ms` | `290ms` | `260ms` |
| `ReleaseSafe` | `31ms` | `44ms` | `34ms` | 
| `ReleaseSmall` | `39ms` | `49ms` | `43ms` |
| `ReleaseFast` | `17ms` | `25ms` | `20ms` |

_All times are averaged over 100 runs, rounded to the nearest 2sf._

## Usage

Dishwasher has 4 APIs, 3 of which will be most useful.

- [Parsing API](#parsing-api) - for parsing an entire XML document at runtime.
- [Populate API](#populate-api) - for mapping an XML document to a given struct.
- [Comptime Parsing API](#comptime-parsing-api) - for parsing an entire XML document at compile time.
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

### Populate API
Often, it's useful to be able to populate a given struct with values from an XML
document. That is, 'reading' the document into the struct.

Dishwasher comes with a shaping API so you can dictate how the document should
be read into the struct. Simple declare an `xml_shape` on the struct:

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
            .{ .element, "residence", .content },
        },
    };

    name: []const u8,
    age: []const u8,
    jobs: []Job,
    location: union(enum) {
        house: []const u8,
        residence: []const u8,
    },
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

If some field accepts any sort of XML document shape, you can instruct it
to accept a `parse.Builder.Tree`:

```zig
const Register = struct {
  pub const xml_shape = .{
    .people = .{ .elements, "person", dishwasher.parse.Builder.Tree },
  };

  people: []dishwasher.parse.Builder.Tree,
};
```

### Comptime Parsing API
It could be useful to parse an XML document at compile time, for example
for some inline code generation. While comptime doesn't have allocators,
there's a custom API for this:

```ts
const tree = dishwasher.parse.comptimeFromSlice(xml_text);
```

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

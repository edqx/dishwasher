const std = @import("std");

pub const Scanner = struct {
    pub const Error = error{
        NoSpaceLeft,
        UnexpectedEof,
    };

    pub const State = union {
        pub const Tag = struct {
            pub const Kind = enum {
                element,
                meta,
                doctype,
            };

            pub const Inner = union {
                next_attribute: void,
                attribute_value: void,
            };

            kind: Kind,
            state: Inner,
        };

        pub const Cdata = struct {};

        default: void,
        tag: Tag,
        cdata: Cdata,
    };

    pub const Token = struct {
        pub const Kind = enum {
            element_open,
            element_close,
            element_attribute,
            element_attribute_value,
            meta_attribute,
            meta_attribute_value,
            doctype,
            text,
        };

        kind: Kind,
        inner: []const u8,
    };

    buffer: []const u8,
    end_of_input: bool,
    cursor: usize,

    state: State,

    pub fn peekBytes(self: *Scanner, numBytes: usize) ![]const u8 {
        const remaining = self.buffer.len - self.cursor;
        if (remaining < numBytes) {
            return if (self.end_of_input) Error.UnexpectedEof else Error.NoSpaceLeft;
        }
        return self.buffer[self.cursor .. self.cursor + numBytes];
    }

    pub fn peekChar(self: *Scanner, ahead: usize) !u8 {
        return (try self.peekBytes(ahead))[ahead - 1];
    }

    pub fn next(self: *Scanner) !?Token {
        switch (self.state) {
            .default => {
                if (std.mem.eql(u8, try self.peekBytes(4), "<!--")) {}
                if (std.mem.eql(u8, try self.peekBytes(9), "<!DOCTYPE")) {
                    self.cursor += 9;
                    self.state = .{
                        .tag = .{
                            .kind = .doctype,
                            .state = .next_attribute,
                        },
                    };
                    return try self.next();
                }
                if (std.mem.eql(u8, try self.peekBytes(5), "<?xml")) {
                    self.cursor += 5;
                    self.state = .{
                        .tag = .{
                            .kind = .meta,
                            .state = .next_attribute,
                        },
                    };
                    return try self.next();
                }
                if (std.mem.eql(u8, try self.peekBytes(1), "<")) {
                    var tag_name_length: usize = 0;
                    while (true) : (tag_name_length += 1) {
                        const char = try self.peekChar(1 + tag_name_length + 1);
                        switch (char) {
                            ' ', '\n', '\r' => break,
                            '/', '>' => break,
                        }
                    }
                    const tagName = self.buffer[self.cursor .. self.cursor + 1 + tag_name_length];
                    self.cursor += 1 + tag_name_length;
                    self.state = .{
                        .tag = .{
                            .kind = .element,
                            .state = .next_attribute,
                        },
                    };
                    return .{
                        .kind = .element_open,
                        .inner = tagName,
                    };
                }
            },
            .tag => |tagDetails| {
                _ = tagDetails;
            },
            .cdata => |cDataDetails| {
                _ = cDataDetails;
            },
        }
    }
};

pub fn Reader(comptime ReaderType: type, comptime bufferSize: usize) type {
    return struct {
        const _Reader = @This();

        buffer: [bufferSize]u8,
        reader: ReaderType,
        scanner: Scanner,

        pub fn init(inner: ReaderType) !_Reader {
            return _Reader{
                .buffer = undefined,
                .reader = inner,
                .scanner = .{ .buffer = &.{}, .cursor = 0 },
            };
        }

        pub fn next(self: *_Reader) !?Scanner.Token {
            while (true) {
                return self.scanner.next() catch |e| switch (e) {
                    error.NoSpaceLeft => {
                        self.refillBuffer();
                        continue;
                    },
                    else => return e,
                };
            }
        }

        fn refillBuffer(self: *_Reader) !void {
            const remaining = self.scanner.buffer.len - self.scanner.cursor;
            std.mem.copyForwards(u8, self.buffer[0..remaining], self.buffer[self.scanner.cursor..]);
            const bytesRead = try self.reader.read(&self.buffer[remaining..]);
            self.scanner.buffer = self.buffer[0..bytesRead];
            self.scanner.end_of_input = bytesRead != bufferSize;
        }
    };
}

pub fn reader(inner: anytype) !Reader(@TypeOf(inner), 4096) {
    return try Reader(@TypeOf(inner), 4096).init(inner);
}

test reader {
    var fba = std.io.fixedBufferStream(&.{});

    _ = try reader(fba.reader());
}

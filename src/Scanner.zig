const std = @import("std");

const Scanner = @This();

pub const Error = error{
    NoSpaceLeft,
    UnexpectedEof,

    UnexpectedToken,
};

pub const State = union(enum) {
    pub const TagState = union(enum) {
        next_attribute: void,
        attribute_value: void,
    };

    pub const Cdata = struct {};

    default: void,
    comment: void,
    meta: TagState,
    elem: TagState,
    cdata: Cdata,
};

pub const Token = struct {
    pub const Kind = enum {
        element_open,
        element_close,
        element_close_self,
        element_attribute,
        element_attribute_value,
        comment_open,
        comment_close,
        meta_attribute,
        meta_attribute_value,
        doctype,
        text_chunk,
    };

    kind: Kind,
    inner: []const u8,
    start_pos: usize,
    end_pos: usize,
};

buffer: []const u8,
end_of_input: bool,
cursor: usize,
global_cursor: usize,

state: State,

pub fn fromSlice(slice: []const u8) Scanner {
    return .{
        .buffer = slice,
        .end_of_input = true,
        .cursor = 0,
        .global_cursor = 0,
        .state = .default,
    };
}

fn peekBytes(self: *Scanner, numBytes: usize) ![]const u8 {
    if (self.buffer.len < self.cursor + numBytes) {
        return if (self.end_of_input) return self.buffer[self.cursor..] else Error.NoSpaceLeft;
    }
    return self.buffer[self.cursor .. self.cursor + numBytes];
}

fn peekChar(self: *Scanner, ahead: usize) !?u8 {
    const bytes = try self.peekBytes(ahead);
    if (bytes.len < ahead) return null;
    return bytes[ahead - 1];
}

fn advanceCursor(self: *Scanner, bytes: usize) usize {
    defer self.global_cursor += bytes;
    defer self.cursor += bytes;
    return self.cursor;
}

fn setCursor(self: *Scanner, pos: usize) void {
    self.cursor = pos;
    self.global_cursor = pos;
}

pub fn next(self: *Scanner) !?Token {
    var start_pos = self.global_cursor;
    switch (self.state) {
        .default => {
            if (try self.peekChar(1) == '<') {
                if (std.mem.eql(u8, try self.peekBytes(4), "<!--")) {
                    const initial_pos = self.advanceCursor(4);
                    errdefer self.setCursor(initial_pos);

                    self.state = .comment;
                    errdefer self.state = .default;

                    return .{
                        .kind = .comment_open,
                        .inner = &.{},
                        .start_pos = start_pos,
                        .end_pos = self.global_cursor,
                    };
                }

                if (std.mem.eql(u8, try self.peekBytes(9), "<!DOCTYPE")) {
                    const initial_pos = self.advanceCursor(9);
                    errdefer self.setCursor(initial_pos);
                    self.state = .{ .elem = .next_attribute };
                    errdefer self.state = .default;

                    return try self.next();
                }
                if (std.mem.eql(u8, try self.peekBytes(5), "<?xml")) {
                    const initial_pos = self.advanceCursor(5);
                    errdefer self.setCursor(initial_pos);

                    self.state = .{ .meta = .next_attribute };
                    errdefer self.state = .default;

                    return try self.next();
                }

                if (std.mem.eql(u8, try self.peekBytes(2), "</")) {
                    var tag_name_length: usize = 0;
                    while (true) : (tag_name_length += 1) {
                        const char = try self.peekChar(2 + tag_name_length + 1) orelse return Error.UnexpectedEof;
                        switch (char) {
                            '>' => break,
                            else => {},
                        }
                    }
                    const tag_name = self.buffer[self.cursor + 2 .. self.cursor + 2 + tag_name_length];
                    const initial_pos = self.advanceCursor(2 + tag_name_length + 1); // +1 for the closing tag
                    errdefer self.setCursor(initial_pos);

                    self.state = .default;
                    errdefer self.state = .default;

                    return .{
                        .kind = .element_close,
                        .inner = tag_name,
                        .start_pos = start_pos,
                        .end_pos = self.global_cursor,
                    };
                }

                if (std.mem.eql(u8, try self.peekBytes(1), "<")) {
                    var tag_name_length: usize = 0;
                    while (true) : (tag_name_length += 1) {
                        const char = try self.peekChar(1 + tag_name_length + 1) orelse return Error.UnexpectedEof;
                        switch (char) {
                            ' ', '\n', '\r', '/', '>' => break,
                            else => {},
                        }
                    }
                    const tag_name = self.buffer[self.cursor + 1 .. self.cursor + 1 + tag_name_length];
                    const initial_pos = self.advanceCursor(1 + tag_name_length);
                    errdefer self.setCursor(initial_pos);

                    self.state = .{ .elem = .next_attribute };
                    errdefer self.state = .default;

                    return .{
                        .kind = .element_open,
                        .inner = tag_name,
                        .start_pos = start_pos,
                        .end_pos = self.global_cursor,
                    };
                }
            }

            var text_chunk_length: usize = 0;
            while (true) : (text_chunk_length += 1) {
                const char = self.peekChar(text_chunk_length + 1) catch |e| {
                    switch (e) {
                        error.NoSpaceLeft => break,
                        else => return e,
                    }
                } orelse break;
                switch (char) {
                    '<' => break,
                    else => {},
                }
            }

            if (text_chunk_length == 0) return null;

            const text_chunk = self.buffer[self.cursor .. self.cursor + text_chunk_length];
            const initial_pos = self.advanceCursor(text_chunk_length);
            errdefer self.setCursor(initial_pos);

            return .{
                .kind = .text_chunk,
                .inner = text_chunk,
                .start_pos = start_pos,
                .end_pos = self.global_cursor,
            };
        },
        .comment => {
            if (std.mem.eql(u8, try self.peekBytes(3), "-->")) {
                const initial_pos = self.advanceCursor(3);
                errdefer self.setCursor(initial_pos);

                self.state = .default;
                errdefer self.state = .comment;

                return .{
                    .kind = .comment_close,
                    .inner = &.{},
                    .start_pos = start_pos,
                    .end_pos = self.global_cursor,
                };
            }

            var text_chunk_length: usize = 0;
            while (true) : (text_chunk_length += 1) {
                if (std.mem.endsWith(u8, try self.peekBytes(text_chunk_length + 3), "-->")) {
                    break;
                }
                _ = self.peekChar(text_chunk_length + 1) catch |e| {
                    switch (e) {
                        error.NoSpaceLeft => break,
                        else => return e,
                    }
                } orelse break;
            }

            if (text_chunk_length == 0) return null;

            const text_chunk = self.buffer[self.cursor .. self.cursor + text_chunk_length];
            const initial_pos = self.advanceCursor(text_chunk_length);
            errdefer self.setCursor(initial_pos);

            return .{
                .kind = .text_chunk,
                .inner = text_chunk,
                .start_pos = start_pos,
                .end_pos = self.global_cursor,
            };
        },
        inline .elem, .meta => |elem_state| {
            var num_whitespace: usize = 0;
            while (true) : (num_whitespace += 1) {
                const char = try self.peekChar(num_whitespace + 1) orelse return Error.UnexpectedEof;
                switch (char) {
                    ' ', '\n', '\r' => {},
                    else => break,
                }
            }
            const initial_pos = self.advanceCursor(num_whitespace);
            errdefer self.setCursor(initial_pos);

            const initial_state = self.state;

            start_pos = self.global_cursor;
            const firstChar = try self.peekChar(1) orelse return Error.UnexpectedEof;
            switch (self.state) {
                .elem => {
                    if (std.mem.eql(u8, try self.peekBytes(2), "/>")) {
                        const initial_pos_2 = self.advanceCursor(2);
                        errdefer self.setCursor(initial_pos_2);

                        self.state = .default;

                        return .{
                            .kind = .element_close_self,
                            .inner = &.{},
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }

                    if (firstChar == '>') {
                        const initial_pos_2 = self.advanceCursor(1);
                        errdefer self.setCursor(initial_pos_2);

                        self.state = .default;
                        errdefer self.state = initial_state;

                        return try self.next();
                    }
                },
                .meta => {
                    if (std.mem.eql(u8, try self.peekBytes(2), "?>")) {
                        const initial_pos_2 = self.advanceCursor(2);
                        errdefer self.setCursor(initial_pos_2);

                        self.state = .default;
                        errdefer self.state = initial_state;

                        return try self.next();
                    }
                },
                else => unreachable,
            }

            switch (elem_state) {
                .next_attribute => {
                    var attr_name_length: usize = 0;
                    while (true) : (attr_name_length += 1) {
                        const char = try self.peekChar(attr_name_length + 1) orelse return Error.UnexpectedEof;
                        switch (char) {
                            ' ', '\n', '\r', '=', '/', '>' => break,
                            else => {},
                        }
                    }
                    if (attr_name_length == 0)
                        return Error.UnexpectedToken;

                    const attr_name = self.buffer[self.cursor .. self.cursor + attr_name_length];
                    const initial_pos_2 = self.advanceCursor(attr_name_length);
                    errdefer self.setCursor(initial_pos_2);

                    var num_whitespace_2: usize = 0;
                    while (true) : (num_whitespace_2 += 1) {
                        const char = try self.peekChar(num_whitespace_2 + 1) orelse return Error.UnexpectedEof;
                        switch (char) {
                            ' ', '\n', '\r' => {},
                            else => break,
                        }
                    }
                    const initial_pos_3 = self.advanceCursor(num_whitespace_2);
                    errdefer self.setCursor(initial_pos_3);

                    const name_end_pos = self.global_cursor;

                    const has_value = (try self.peekChar(1) orelse return Error.UnexpectedEof) == '=';
                    const initial_pos_4 = self.advanceCursor(if (has_value) 1 else 0);
                    errdefer self.setCursor(initial_pos_4);

                    self.state = switch (self.state) {
                        .elem => .{ .elem = if (has_value) .attribute_value else .next_attribute },
                        .meta => .{ .meta = if (has_value) .attribute_value else .next_attribute },
                        else => unreachable,
                    };
                    errdefer self.state = initial_state;

                    return .{
                        .kind = switch (self.state) {
                            .elem => .element_attribute,
                            .meta => .meta_attribute,
                            else => unreachable,
                        },
                        .inner = attr_name,
                        .start_pos = start_pos,
                        .end_pos = name_end_pos,
                    };
                },
                .attribute_value => {
                    if (firstChar == '"') {
                        const initial_pos_2 = self.advanceCursor(1);
                        errdefer self.setCursor(initial_pos_2);

                        var attr_value_length: usize = 0;
                        while (true) : (attr_value_length += 1) {
                            const char = try self.peekChar(attr_value_length + 1) orelse return Error.UnexpectedEof;
                            switch (char) {
                                '"' => break,
                                else => {},
                            }
                        }
                        const attr_value = self.buffer[self.cursor .. self.cursor + attr_value_length];
                        const initial_pos_3 = self.advanceCursor(attr_value_length + 1);
                        errdefer self.setCursor(initial_pos_3);

                        self.state = switch (self.state) {
                            .elem => .{ .elem = .next_attribute },
                            .meta => .{ .meta = .next_attribute },
                            else => unreachable,
                        };
                        errdefer self.state = initial_state;

                        return .{
                            .kind = switch (self.state) {
                                .elem => .element_attribute_value,
                                .meta => .meta_attribute_value,
                                else => unreachable,
                            },
                            .inner = attr_value,
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }

                    var attr_value_length: usize = 0;
                    while (true) : (attr_value_length += 1) {
                        const char = try self.peekChar(attr_value_length + 1) orelse return Error.UnexpectedEof;
                        switch (char) {
                            ' ', '\n', '\r', '/', '>' => break,
                            else => {},
                        }
                    }
                    if (attr_value_length == 0)
                        return Error.UnexpectedToken;

                    const attr_value = self.buffer[self.cursor .. self.cursor + attr_value_length];
                    const initial_pos_2 = self.advanceCursor(attr_value_length);
                    errdefer self.setCursor(initial_pos_2);

                    return .{
                        .kind = switch (self.state) {
                            .elem => .element_attribute_value,
                            .meta => .meta_attribute_value,
                            else => unreachable,
                        },
                        .inner = attr_value,
                        .start_pos = start_pos,
                        .end_pos = self.global_cursor,
                    };
                },
            }
        },
        .cdata => |cdata_details| {
            _ = cdata_details;
        },
    }
    return null;
}

pub fn StaticBufferReader(comptime ReaderType: type, comptime bufferSize: usize) type {
    return struct {
        const _Reader = @This();

        const ReaderError = error{BufferNotLargeEnough};

        buffer: [bufferSize]u8,
        reader: ReaderType,
        scanner: Scanner,

        pub fn init(inner: ReaderType) _Reader {
            return _Reader{
                .buffer = undefined,
                .reader = inner,
                .scanner = .{
                    .buffer = &.{},
                    .cursor = 0,
                    .global_cursor = 0,
                    .state = .default,
                    .end_of_input = false,
                },
            };
        }

        pub fn next(self: *_Reader) !?Scanner.Token {
            return self.scanner.next() catch |e| switch (e) {
                error.NoSpaceLeft => {
                    try self.refillBuffer();
                    return self.scanner.next() catch |e2| switch (e2) {
                        error.NoSpaceLeft => ReaderError.BufferNotLargeEnough,
                        else => return e2,
                    };
                },
                else => return e,
            };
        }

        fn refillBuffer(self: *_Reader) !void {
            const remaining = self.scanner.buffer.len - self.scanner.cursor;
            std.mem.copyForwards(u8, self.buffer[0..remaining], self.buffer[self.scanner.cursor .. self.scanner.cursor + remaining]);
            const bytesRead = try self.reader.read(self.buffer[remaining..]);
            self.scanner.buffer = self.buffer[0 .. remaining + bytesRead];
            self.scanner.cursor = 0;
            self.scanner.end_of_input = bytesRead != bufferSize - remaining;
        }
    };
}

pub fn staticBufferReader(inner: anytype) StaticBufferReader(@TypeOf(inner), 1024) {
    return StaticBufferReader(@TypeOf(inner), 1024).init(inner);
}

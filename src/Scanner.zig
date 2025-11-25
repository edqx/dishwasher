const std = @import("std");

const Scanner = @This();

pub const Error = error{
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
        element_children_end,
        element_self_end,
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

// adapted from https://ziglang.org/documentation/master/std/#std.Io.Reader.peekDelimiterInclusive
pub fn peekDelimiterAnyInclusive(r: *std.Io.Reader, delimiters: []const u8) std.Io.Reader.DelimiterError![]u8 {
    {
        const contents = r.buffer[0..r.end];
        const seek = r.seek;
        if (std.mem.indexOfAnyPos(u8, contents, seek, delimiters)) |end| {
            @branchHint(.likely);
            return contents[seek .. end + 1];
        }
    }
    while (true) {
        const content_len = r.end - r.seek;
        if (r.buffer.len - content_len == 0) break;
        try r.fillMore();
        const seek = r.seek;
        const contents = r.buffer[0..r.end];
        if (std.mem.indexOfAnyPos(u8, contents, seek + content_len, delimiters)) |end| {
            return contents[seek .. end + 1];
        }
    }
    var failing_writer = std.Io.Writer.failing;
    while (r.vtable.stream(r, &failing_writer, .limited(1))) |n| {
        std.debug.assert(n == 0);
    } else |err| switch (err) {
        error.WriteFailed => return error.StreamTooLong,
        error.ReadFailed => |e| return e,
        error.EndOfStream => |e| return e,
    }
}

// adapted from https://ziglang.org/documentation/master/std/#std.Io.Reader.peekDelimiterExclusive
fn peekDelimiterAnyExclusive(r: *std.Io.Reader, delimiter: []const u8) std.Io.Reader.DelimiterError![]u8 {
    const result = peekDelimiterAnyInclusive(r, delimiter) catch |err| switch (err) {
        error.EndOfStream => {
            const remaining = r.buffer[r.seek..r.end];
            if (remaining.len == 0) return error.EndOfStream;
            return remaining;
        },
        else => |e| return e,
    };
    return result[0 .. result.len - 1];
}

// adapted from https://ziglang.org/documentation/master/std/#std.Io.Reader.takeDelimiterExclusive
fn takeDelimiterAnyExclusive(r: *std.Io.Reader, delimiter: []const u8) std.Io.Reader.DelimiterError![]u8 {
    const result = try peekDelimiterAnyExclusive(r, delimiter);
    r.toss(result.len);
    return result;
}

reader: *std.Io.Reader,

global_cursor: usize = 0,
state: State = .default,

fn canPeekBytes(self: *Scanner, num_bytes: usize) !bool {
    self.reader.fill(num_bytes) catch |e| switch (e) {
        error.EndOfStream => return false,
        else => return e,
    };
    return true;
}

pub fn next(self: *Scanner) !?Token {
    return self.nextImpl() catch |e| switch (e) {
        error.EndOfStream => return null,
        else => return e,
    };
}

fn nextImpl(self: *Scanner) !?Token {
    var start_pos = self.global_cursor;
    switch (self.state) {
        .default => {
            if (try self.reader.peekByte() == '<') {
                if (std.mem.eql(u8, try self.reader.peek(1), "<")) {
                    self.reader.toss(1);

                    if (std.mem.eql(u8, try self.reader.peek(1), "/")) {
                        self.reader.toss(1);

                        const tag_name = try self.reader.takeDelimiterExclusive('>');
                        self.state = .default;

                        self.reader.toss(1);

                        return .{
                            .kind = .element_children_end,
                            .inner = tag_name,
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }

                    if (try self.canPeekBytes(3) and std.mem.eql(u8, try self.reader.peek(3), "!--")) {
                        self.reader.toss(3);
                        self.state = .comment;

                        return .{
                            .kind = .comment_open,
                            .inner = &.{},
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }

                    if (try self.canPeekBytes(4) and std.mem.eql(u8, try self.reader.peek(4), "?xml")) {
                        self.reader.toss(4);
                        self.state = .{ .meta = .next_attribute };

                        return try self.nextImpl();
                    }

                    if (try self.canPeekBytes(8) and std.mem.eql(u8, try self.reader.peek(8), "!DOCTYPE")) {
                        self.reader.toss(8);
                        self.state = .{ .elem = .next_attribute };

                        return try self.nextImpl();
                    }

                    const tag_name = try takeDelimiterAnyExclusive(self.reader, " \n\r/>");
                    if (tag_name.len == 0) return error.UnexpectedToken;
                    self.state = .{ .elem = .next_attribute };

                    return .{
                        .kind = .element_open,
                        .inner = tag_name,
                        .start_pos = start_pos,
                        .end_pos = self.global_cursor,
                    };
                }
            }

            if (self.reader.buffered().len == 0) {
                try self.reader.fillMore();
            }

            const text_chunk = self.reader.peekDelimiterExclusive('<') catch |e| switch (e) {
                error.StreamTooLong => try self.reader.peek(self.reader.buffered().len),
                else => return e,
            };

            self.reader.toss(text_chunk.len);

            return .{
                .kind = .text_chunk,
                .inner = text_chunk,
                .start_pos = start_pos,
                .end_pos = self.global_cursor,
            };
        },
        .comment => {
            if (std.mem.eql(u8, try self.reader.peek(3), "-->")) {
                self.reader.toss(3);
                self.state = .default;

                return .{
                    .kind = .comment_close,
                    .inner = &.{},
                    .start_pos = start_pos,
                    .end_pos = self.global_cursor,
                };
            }

            if (try self.reader.peekByte() == '-') {
                return .{
                    .kind = .text_chunk,
                    .inner = try self.reader.take(1),
                    .start_pos = start_pos,
                    .end_pos = self.global_cursor,
                };
            }

            const text_chunk = self.reader.peekDelimiterExclusive('-') catch |e| switch (e) {
                error.StreamTooLong => try self.reader.peek(self.reader.buffered().len),
                else => return e,
            };

            self.reader.toss(text_chunk.len);

            return .{
                .kind = .text_chunk,
                .inner = text_chunk,
                .start_pos = start_pos,
                .end_pos = self.global_cursor,
            };
        },
        inline .elem, .meta => |elem_state| {
            // remove whitespace
            while (true) {
                const char = try self.reader.peekByte();
                switch (char) {
                    ' ', '\n', '\r' => self.reader.toss(1),
                    else => break,
                }
            }

            start_pos = self.global_cursor;
            switch (self.state) {
                .elem => {
                    if (std.mem.eql(u8, try self.reader.peek(2), "/>")) {
                        self.reader.toss(2);
                        self.state = .default;

                        return .{
                            .kind = .element_self_end,
                            .inner = &.{},
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }

                    if (try self.reader.peekByte() == '>') {
                        self.reader.toss(1);
                        self.state = .default;

                        return .{
                            .kind = .element_close,
                            .inner = &.{},
                            .start_pos = start_pos,
                            .end_pos = self.global_cursor,
                        };
                    }
                },
                .meta => {
                    if (std.mem.eql(u8, try self.reader.peek(2), "?>")) {
                        self.reader.toss(2);
                        self.state = .default;

                        return try self.nextImpl();
                    }
                },
                else => unreachable,
            }

            switch (elem_state) {
                .next_attribute => {
                    const attr_name = try takeDelimiterAnyExclusive(self.reader, " \n\r=/>");
                    if (attr_name.len == 0) return error.UnexpectedToken;

                    // remove whitespace
                    while (true) {
                        const char = try self.reader.peekByte();
                        switch (char) {
                            ' ', '\n', '\r' => self.reader.toss(1),
                            else => break,
                        }
                    }

                    const name_end_pos = self.global_cursor;

                    const has_value = try self.reader.peekByte() == '=';
                    if (has_value) {
                        self.reader.toss(1);
                    }

                    self.state = switch (self.state) {
                        .elem => .{ .elem = if (has_value) .attribute_value else .next_attribute },
                        .meta => .{ .meta = if (has_value) .attribute_value else .next_attribute },
                        else => unreachable,
                    };

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
                    const peek = try self.reader.peekByte();
                    if (peek == '"' or peek == '\'') {
                        self.reader.toss(1);

                        const attr_value = try self.reader.takeDelimiterExclusive(peek);
                        self.reader.toss(1);

                        self.state = switch (self.state) {
                            .elem => .{ .elem = .next_attribute },
                            .meta => .{ .meta = .next_attribute },
                            else => unreachable,
                        };

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

                    const attr_value = try takeDelimiterAnyExclusive(self.reader, " \n\r/>?");
                    if (attr_value.len == 0) return error.UnexpectedToken;

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

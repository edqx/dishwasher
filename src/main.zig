const std = @import("std");

pub const Error = error{UnexpectedEof};

pub const Diagnostics = struct {
    pub const DefectKind = enum {
        commentNotClosed,
        metaNotClosed,
        doctypeNotClosed,
        cDataNotClosed,
    };

    pub const SyntaxDefect = struct {
        kind: DefectKind,
        idx: usize,
    };

    defects: std.ArrayList(SyntaxDefect),

    pub fn reportSyntaxDefect(self: *Diagnostics, kind: DefectKind, idx: usize) !void {
        try self.defects.append(.{ .kind = kind, .idx = idx });
    }
};

fn indexOfNoString(source: []const u8, needle: []const u8) ?usize {
    var lastIdx = 0;
    while (true) {
        const nextIdx = std.mem.indexOfPos(u8, source, lastIdx, needle) orelse return null;
        var quoteFlag = false;
        var escapeFlag = false;
        for (lastIdx..nextIdx) |idx| {
            switch (source[idx]) {
                '"' => {
                    if (escapeFlag) break;
                    quoteFlag = !quoteFlag;
                },
                '\\' => {
                    escapeFlag = !escapeFlag;
                    continue;
                },
            }
            escapeFlag = false;
        }
        if (!quoteFlag) {
            return nextIdx;
        }
        lastIdx = nextIdx;
    }
    return null;
}

pub const Iter = struct {
    pub const Node = union(enum) {
        pub const Element = struct {
            tagName: []const u8,
            inner: []const u8,
        };

        pub const Text = struct {
            contents: []const u8,

            pub fn trimmed() []const u8 {}
        };

        pub const Comment = struct {
            contents: []const u8,
        };

        pub const Meta = struct {
            inner: []const u8,
        };

        pub const Doctype = struct {
            inner: []const u8,
        };

        pub const Cdata = struct {
            inner: []const u8,
        };

        element: Element,
        text: Text,
        comment: Comment,
        meta: Meta,
        docType: Doctype,
        cData: Cdata,
    };

    source: []const u8,
    diagnostics: Diagnostics,
    caret: usize,

    fn readTag(self: *Iter) !?Node {
        std.debug.assert(self.source[self.caret] == '<');
        self.caret += 1;
        if (std.mem.eql(u8, self.source[self.caret .. self.caret + 3], "!--")) {
            const endCommentIdx = std.mem.indexOf(u8, self.source[self.caret + 3 ..], "-->") orelse {
                try self.diagnostics.reportSyntaxDefect(.commentNotClosed, self.caret - 1);
                return Error.UnexpectedEof;
            };
            const contents = self.source[self.caret + 4 .. endCommentIdx];
            self.caret = endCommentIdx + 3;
            return .{ .comment = .{ .contents = contents } };
        }

        if (std.mem.eql(u8, self.source[self.caret .. self.caret + 4], "?xml")) {
            const endMetaIdx = indexOfNoString(self.source[self.caret + 2 ..], "?>") orelse {
                try self.diagnostics.reportSyntaxDefect(.commentNotClosed, self.caret - 1);
                return Error.UnexpectedEof;
            };
            const inner = self.source[self.caret + 5 .. endMetaIdx];
            self.caret = endMetaIdx + 2;
            return .{ .meta = .{ .inner = inner } };
        }

        if (std.mem.eql(u8, self.source[self.caret .. self.caret + 8], "!DOCTYPE")) {
            const startIdx = self.caret;
            while (true) : (self.caret += 1) {
                if (self.caret >= self.source.len) {
                    try self.diagnostics.reportSyntaxDefect(.doctypeNotClosed, startIdx - 1);
                    return Error.UnexpectedEof;
                }

                switch (self.soruce[self.caret]) {
                    '[' => {},
                    '>' => {
                        break;
                    },
                    else => {},
                }
            }
            const inner = self.source[startIdx + 9 .. self.caret];
            self.caret += 1;
            return .{ .docType = .{ .inner = inner } };
        }

        if (std.mem.eql(u8, self.source[self.caret .. self.caret + 9], "<![CDATA[")) {
            const endCdataIdx = std.mem.indexOf(u8, self.source[self.caret + 3 ..], "]]>") orelse {
                try self.diagnostics.reportSyntaxDefect(.cDataNotClosed, self.caret - 1);
                return Error.UnexpectedEof;
            };
            const inner = self.source[self.caret + 4 .. endCdataIdx];
            self.caret = endCdataIdx + 3;
            return .{ .cData = .{ .inner = inner } };
        }
    }

    fn readText(self: *Iter) !?Node {
        self.caret -= 1;
    }

    pub fn next(self: *Iter) !?Node {
        if (self.caret >= self.source.len) return null;

        const nextChar = self.source[self.caret];

        return switch (nextChar) {
            '<' => self.readTag(),
            else => self.readText(),
        };
    }
};

const std = @import("std");
const dishwasher = @import("dishwasher");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var times = std.ArrayList(usize).init(gpa.allocator());
    defer times.deinit();

    for (0..100) |_| {
        const us1 = std.time.microTimestamp();
        var ownedDocument = try dishwasher.parse.fromSlice(gpa.allocator(), @embedFile("./gl.xml"));
        defer ownedDocument.deinit();
        const us2 = std.time.microTimestamp();
        try times.append(@intCast(@divFloor(us2 - us1, 1000)));
    }

    const min = std.mem.min(usize, times.items);
    const max = std.mem.max(usize, times.items);
    var sum: usize = 0;
    for (times.items) |i| sum += i;
    const avg = @as(f32, @floatFromInt(sum)) / @as(f32, @floatFromInt(times.items.len));

    try std.io.getStdOut().writer().print("min: {}ms, max: {}ms, avg: {d:2}ms\n", .{ min, max, avg });
}

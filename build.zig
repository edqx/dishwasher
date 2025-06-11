const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dishwasher = b.addModule("dishwasher", .{
        .root_source_file = b.path("src/dishwasher.zig"),
        .target = target,
        .optimize = optimize,
    });

    const dishwasher_tests = b.addTest(.{
        .root_module = dishwasher,
    });

    const run_dishwasher_tests = b.addRunArtifact(dishwasher_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_dishwasher_tests.step);

    const docs_lib = b.addLibrary(.{
        .name = "dishwasher",
        .root_module = dishwasher,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs_lib.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
        .exclude_extensions = &.{".html"},
    });

    const install_html = b.addInstallFile(b.path("docs/index.html"), "docs/index.html");

    const docs_step = b.step("docs", "Generate docs");
    docs_step.dependOn(&install_docs.step);
    docs_step.dependOn(&install_html.step);
}

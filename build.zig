const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dishwasher = b.addModule("dishwasher", .{
        .root_source_file = b.path("src/dishwasher.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Doc
    const docs = b.addLibrary(.{ .name = "dishwasher", .root_module = dishwasher });
    const install_docs = b.addInstallDirectory(.{
        .source_dir = docs.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
        // Seems a bit drastic but by default only index.html is installed
        // and I override it below. Maybe there is a cleaner way ?
        .exclude_extensions = &.{".html"},
    });
    const install_html = b.addInstallFile(b.path("docs/index.html"), "docs/index.html");
    const install_logo = b.addInstallFile(b.path("docs/logo.svg"), "docs/logo.svg");
    const docs_step = b.step("docs", "Build documentation");
    docs_step.dependOn(&install_docs.step);
    docs_step.dependOn(&install_html.step);
    docs_step.dependOn(&install_logo.step);

    // Unit Tests
    const exe_unit_tests = b.addTest(.{
        .root_module = dishwasher,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}

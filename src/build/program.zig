//! Compile the emitted user IR to an object with `zig cc`, cache it by
//! content, and (optionally) emit the inspection artifacts derived from the
//! same IR.

const std = @import("std");
const Build = @import("context.zig").Build;
const hashing = @import("../utils/hashing.zig");

// Cache key for the object produced from the emitted user IR.
pub fn userObjectKey(
    ll_source: []const u8,
    toolchain: []const u8,
    target_arg: []const u8,
    cpu_key: []const u8,
    clang_flag: []const u8,
    include_dirs: []const []const u8,
) hashing.Digest {
    var kb = hashing.KeyBuilder.init("doxa-user-object-v2");
    kb.addBytes(toolchain);
    kb.addBytes(target_arg);
    kb.addBytes(cpu_key);
    kb.addBytes(clang_flag);
    for (include_dirs) |dir| kb.addBytes(dir);
    kb.addBytes(ll_source);
    return kb.finish();
}

pub fn compile(build: *const Build) ![]u8 {
    build.profiler.begin("cc-object");
    defer build.profiler.end();

    const io = build.io;
    const allocator = build.allocator;
    const ll_source = try std.Io.Dir.cwd().readFileAlloc(io, build.ir_path, allocator, .unlimited);
    defer allocator.free(ll_source);
    const key = userObjectKey(
        ll_source,
        build.toolchain,
        build.target_arg,
        build.target.cpuKey(build.native_cpu_key.bytes()),
        build.config.opt.clangFlag(),
        build.include_dirs,
    );
    if (build.artifacts.contains(key, build.obj_ext)) return build.artifacts.pathAlloc(key, build.obj_ext);

    const staged = try build.artifacts.stagingPathAlloc(key, build.obj_ext);
    defer allocator.free(staged);

    build.profiler.begin("cc-compile");
    defer build.profiler.end();
    var args = std.array_list.Managed([]const u8).init(allocator);
    defer args.deinit();
    try args.appendSlice(&[_][]const u8{ build.zig_exe_path, "cc", "-Wno-override-module", "-Wno-unused-command-line-argument", "-c", build.ir_path, "-o", staged });
    try build.target.appendTargetArg(&args);
    try build.target.appendCpuArg(&args);
    // Match the C twin's `-ffp-contract=off`: Doxa never forms FMAs, and
    // refusing contraction keeps float workloads bit-identical.
    try args.append("-ffp-contract=off");
    try args.append(build.config.opt.clangFlag());
    var include_flags = std.array_list.Managed([]const u8).init(allocator);
    defer {
        for (include_flags.items) |f| allocator.free(f);
        include_flags.deinit();
    }
    for (build.include_dirs) |dir| {
        const flag = try std.fmt.allocPrint(allocator, "-I{s}", .{dir});
        try include_flags.append(flag);
        try args.append(flag);
    }
    var child = try std.process.spawn(io, .{
        .argv = args.items,
        .cwd = .{ .path = build.cache_dir },
        .stdout = .inherit,
        .stderr = .inherit,
    });
    const term = try child.wait(io);
    switch (term) {
        .exited => |code| if (code != 0) {
            build.reporter.reportBuildStepFailure("compiling the program object", code);
            return error.Unexpected;
        },
        else => {
            build.reporter.reportBuildStepFailure("compiling the program object", null);
            return error.Unexpected;
        },
    }
    return build.artifacts.publish(key, build.obj_ext);
}

pub fn emitInspections(build: *const Build) !void {
    if (build.config.emit_opt_ir) {
        build.profiler.begin("emit-opt-ir");
        defer build.profiler.end();
        try emitInspectionArtifact(build, .opt_ir);
    }
    if (build.config.emit_asm) {
        build.profiler.begin("emit-asm");
        defer build.profiler.end();
        try emitInspectionArtifact(build, .asm_);
    }
}

const EmitKind = enum { opt_ir, asm_ };

// Run the vendored `zig cc` over the already-emitted `<stem>.ll` to produce an
// inspection artifact (optimized LLVM IR or target assembly) at the same
// optimization level the object build uses. Non-fatal: a failure here warns but
// does not abort the compile.
fn emitInspectionArtifact(build: *const Build, kind: EmitKind) !void {
    const allocator = build.allocator;
    var args = std.array_list.Managed([]const u8).init(allocator);
    defer args.deinit();

    const in_name = try std.fmt.allocPrint(allocator, "{s}.ll", .{build.stem});
    defer allocator.free(in_name);
    const out_ext = switch (kind) {
        .opt_ir => "opt.ll",
        .asm_ => "s",
    };
    const out_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ build.stem, out_ext });
    defer allocator.free(out_name);

    try args.appendSlice(&[_][]const u8{ build.zig_exe_path, "cc", "-Wno-override-module", "-Wno-unused-command-line-argument", "-S" });
    if (kind == .opt_ir) try args.append("-emit-llvm");
    try args.appendSlice(&[_][]const u8{ in_name, "-o", out_name });

    try build.target.appendTargetArg(&args);
    try build.target.appendCpuArg(&args);
    // Keep the inspected artifact on the same footing as the object build:
    // bit-identical float results come from refusing FMA contraction on both.
    try args.append("-ffp-contract=off");
    try args.append(build.config.opt.clangFlag());

    var child = try std.process.spawn(build.io, .{
        .argv = args.items,
        .cwd = .{ .path = build.config.cache_dir },
        .stdout = .inherit,
        .stderr = .inherit,
    });
    const term = try child.wait(build.io);
    switch (term) {
        .exited => |code| if (code != 0) {
            std.debug.print("Warning: could not emit inspection artifact {s} (zig cc exited with code {d}); continuing\n", .{ out_name, code });
            return;
        },
        else => {
            std.debug.print("Warning: could not emit inspection artifact {s} (zig cc terminated unexpectedly); continuing\n", .{out_name});
            return;
        },
    }
    std.debug.print("Wrote {s}/{s}\n", .{ build.config.cache_dir, out_name });
}

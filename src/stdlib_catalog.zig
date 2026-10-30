//! The standard-library catalog as a module for tools outside `src/`
//! (`scripts/gen_stdlib_docs.zig`). A module reaches only files under its
//! root's directory, and the catalog imports `common/`, so the root is here.

pub const catalog = @import("stdlib/catalog.zig");

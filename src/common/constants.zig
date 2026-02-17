const MB = 1024 * 1024;
pub const MAX_SOURCE_FILE_BYTES: usize = 1 * MB;

pub const EXIT_CODE_USAGE: u8 = 1;
pub const EXIT_CODE_RUNTIME: u8 = 2;
pub const MAX_LSP_FILE_BYTES: usize = 4 * MB;
pub const MAX_TEST_OUTPUT_BYTES: usize = 8 * MB;
pub const MAX_STDIN_READ_BYTES: usize = 1 * MB;

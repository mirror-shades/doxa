//! An imported file that does not compile: the error is reported here.
pub fn twice(x: i64) i64 {
    return x * nope;
}

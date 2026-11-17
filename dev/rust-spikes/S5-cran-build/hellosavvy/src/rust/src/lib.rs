use savvy::savvy;

/// Add two numbers in Rust
///
/// @param x,y Numbers.
/// @returns Their sum.
/// @export
#[savvy]
fn hello_add(x: f64, y: f64) -> savvy::Result<savvy::Sexp> {
    savvy::OwnedRealSexp::try_from_scalar(x + y).map(Into::into)
}

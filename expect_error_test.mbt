///|
fn[T] expect_error(f : () -> T raise) -> Unit raise {
  try f() catch {
    _ => ()
  } noraise {
    _ => fail("expected expression to raise")
  }
}

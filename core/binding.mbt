///|
/// Create a binding value with an id and value.
///
/// # Example
/// ```mbt check
/// test "Binding::new" {
///   let binding = Binding::new(1, Void)
///   inspect(binding.id(), content="1")
/// }
/// ```
pub fn Binding::new(id : Int, value : Value) -> Binding {
  { id, value }
}

///|
/// Read the binding id.
///
/// # Example
/// ```mbt check
/// test "Binding::id" {
///   let binding = Binding::new(7, Void)
///   inspect(binding.id(), content="7")
/// }
/// ```
pub fn Binding::id(self : Binding) -> Int {
  self.id
}

///|
/// Read the binding value.
///
/// # Example
/// ```mbt check
/// test "Binding::value" {
///   let binding = Binding::new(1, Void)
///   match binding.value() {
///     Void => ()
///     _ => fail("expected void")
///   }
/// }
/// ```
pub fn Binding::value(self : Binding) -> Value {
  self.value
}

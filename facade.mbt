///|
/// Re-export core runtime types used by the public API.
pub using @core {
  type Datum,
  type EvalError,
  type ParseError,
  type Primitive,
  type Value,
}

///|
/// Re-export parser entrypoints for convenience.
pub using @parser {
  parse_number_token,
  parse_program,
}

///|
/// Evaluate a full Scheme program and return the final value.
///
/// # Example
/// ```mbt check
/// test "facade eval" {
///   let value = eval_program("(+ 1 2)")
///   inspect(value_to_string(value), content="3")
/// }
/// ```
pub fn eval_program(src : String) -> Value raise {
  @eval.eval_program(src)
}

///|
/// Evaluate a full Scheme program and return all top-level results.
///
/// # Example
/// ```mbt check
/// test "facade eval all" {
///   let values = eval_program_all("(define x 1) (+ x 2)")
///   inspect(values.length(), content="2")
/// }
/// ```
pub fn eval_program_all(src : String) -> Array[Value] raise {
  @eval.eval_program_all(src)
}

///|
/// Register an in-memory source for (include ...) by path.
///
/// # Example
/// ```mbt check
/// test "facade include" {
///   register_include_source("mem.scm", "(+ 1 2)")
///   let value = eval_program("(include \"mem.scm\")")
///   inspect(value_to_string(value), content="3")
/// }
/// ```
pub fn register_include_source(path : String, content : String) -> Unit {
  @eval.register_include_source(path, content)
}

///|
/// Render a runtime value using Scheme write/display rules.
///
/// # Example
/// ```mbt check
/// test "facade value to string" {
///   let value = eval_program("(+ 1 2)")
///   inspect(value_to_string(value), content="3")
/// }
/// ```
pub fn value_to_string(value : Value) -> String {
  @runtime.value_to_string(value)
}

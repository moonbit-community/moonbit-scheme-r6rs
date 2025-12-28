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
  parse_number_token_with_radix,
  parse_program,
}

///|
/// Evaluate a full Scheme program and return the final value.
pub fn eval_program(src : String) -> Value raise {
  @eval.eval_program(src)
}

///|
/// Evaluate a full Scheme program and return all top-level results.
pub fn eval_program_all(src : String) -> Array[Value] raise {
  @eval.eval_program_all(src)
}

///|
/// Register an in-memory source for (include ...) by path.
pub fn register_include_source(path : String, content : String) -> Unit {
  @eval.register_include_source(path, content)
}

///|
/// Render a runtime value using Scheme write/display rules.
pub fn value_to_string(value : Value) -> String {
  @runtime.value_to_string(value)
}

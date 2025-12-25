///|
pub using @core {
  type Datum,
  type EvalError,
  type ParseError,
  type Primitive,
  type Value,
}
pub using @parser {parse_number_token, parse_number_token_with_radix, parse_program}

///|
pub fn eval_program(src : String) -> Value raise {
  @eval.eval_program(src)
}

///|
pub fn eval_program_all(src : String) -> Array[Value] raise {
  @eval.eval_program_all(src)
}

///|
pub fn value_to_string(value : Value) -> String {
  @eval.value_to_string(value)
}

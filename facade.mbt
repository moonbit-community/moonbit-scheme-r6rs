///|
pub using @core {
  type Datum,
  type EvalError,
  type ParseError,
  type Primitive,
  type Value,
}

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

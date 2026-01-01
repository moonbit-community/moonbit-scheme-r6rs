///|
/// Parse all top-level datums from a source string.
///
/// # Example
/// ```mbt check
/// test "facade parse program" {
///   let forms = parse_program("#\\a")
///   match forms {
///     [Char('a'), ..] => ()
///     _ => fail("expected char datum")
///   }
/// }
/// ```
///
/// # Example
/// ```mbt check
/// test "facade parse program fold case" {
///   let forms = parse_program("ABC", fold_case=true)
///   match forms {
///     [Symbol("abc"), ..] => ()
///     _ => fail("expected folded symbol")
///   }
/// }
/// ```
pub fn parse_program(src : String, fold_case? : Bool) -> Array[@core.Datum] raise @core.ParseError {
  match fold_case {
    Some(value) => @parser.parse_program(src, fold_case=value)
    None => @parser.parse_program(src)
  }
}

///|
/// Parse a numeric token using default radix and exactness rules.
///
/// # Example
/// ```mbt check
/// test "facade parse number token" {
///   match parse_number_token("10") {
///     Some(Int(10)) => ()
///     _ => fail("expected integer")
///   }
///   match parse_number_token("ff", radix=16) {
///     Some(Int(255)) => ()
///     _ => fail("expected 255")
///   }
/// }
/// ```
pub fn parse_number_token(tok : String, radix? : Int) -> @core.Datum? {
  match radix {
    Some(value) => @parser.parse_number_token(tok, radix=value)
    None => @parser.parse_number_token(tok)
  }
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
pub fn eval_program(src : String) -> @core.Value raise {
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
pub fn eval_program_all(src : String) -> Array[@core.Value] raise {
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
pub fn value_to_string(value : @core.Value) -> String {
  @runtime.value_to_string(value)
}

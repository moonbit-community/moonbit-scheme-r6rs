///|
let include_registry : Ref[Map[String, String]] = Ref::new({})

///|
/// Register an in-memory source for (include ...) by path.
///
/// # Example
/// ```mbt check
/// test "include registry" {
///   register_include_source("mem.scm", "(+ 1 2)")
///   let value = eval_program("(include \"mem.scm\")")
///   inspect(@runtime.value_to_string(value), content="3")
/// }
/// ```
pub fn register_include_source(path : String, content : String) -> Unit {
  include_registry.val[path] = content
}

///|
fn include_path_from_arg(arg : @core.Datum) -> String raise @core.EvalError {
  match @runtime.datum_unlabel(arg) {
    String(path) => path.val
    _ => raise @core.EvalError("type error: string expected")
  }
}

///|
fn include_read_text(path : String) -> String raise @core.EvalError {
  match include_registry.val.get(path) {
    Some(content) => content
    None => raise @core.EvalError("include: unknown file: \{path}")
  }
}

///|
fn include_parse_file(
  path : String,
  fold_case : Bool,
) -> Array[@core.Datum] raise @core.EvalError {
  let content = include_read_text(path)
  @parser.parse_program(content, fold_case=fold_case) catch {
    _ => raise @core.EvalError("include: parse error: \{path}")
  }
}

///|
fn include_forms(
  args : Array[@core.Datum],
  fold_case : Bool,
) -> Array[@core.Datum] raise @core.EvalError {
  let forms : Array[@core.Datum] = []
  for arg in args {
    let path = include_path_from_arg(arg)
    let parsed = include_parse_file(path, fold_case)
    forms.push_iter(parsed.iter())
  }
  forms
}

///|
fn eval_include_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
  fold_case : Bool,
) -> MachineState raise @core.EvalError {
  let forms = include_forms(args, fold_case)
  eval_sequence_state(forms, env, kont, handlers)
}

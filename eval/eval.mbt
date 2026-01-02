///|
fn register_builtin_libraries(env : @core.Env) -> Unit raise @core.EvalError {
  let exports = env_export_map(env)
  for name in base_syntax_keywords() {
    exports[name] = @runtime.make_binding(@core.Value::SyntaxKeyword(name))
  }
  register_library_entry("rnrs.base", Some([6]), exports)
  let eval_exports : Map[String,@core.Binding] = {}
  eval_exports["eval"] = @runtime.make_binding(@core.Value::Primitive(@core.Primitive::Eval))
  eval_exports["environment"] = @runtime.make_binding(@core.Value::Primitive(@core.Primitive::Environment))
  register_library_entry("rnrs.eval", Some([6]), eval_exports)
  let condition_exports : Map[String,@core.Binding] = {}
  let condition_names : Array[String] = [
    "condition", "condition?", "simple-conditions", "condition-predicate", "condition-accessor",
    "&condition", "&message", "make-message-condition", "message-condition?", "condition-message",
    "&warning", "make-warning", "warning?", "&serious", "make-serious-condition",
    "serious-condition?", "&error", "make-error", "error?", "&violation", "make-violation",
    "violation?", "&assertion", "make-assertion-violation", "assertion-violation?",
    "&irritants", "make-irritants-condition", "irritants-condition?", "condition-irritants",
    "&who", "make-who-condition", "who-condition?", "condition-who", "&non-continuable",
    "make-non-continuable-violation", "non-continuable-violation?", "&implementation-restriction",
    "make-implementation-restriction-violation", "implementation-restriction-violation?",
    "&lexical", "make-lexical-violation", "lexical-violation?", "&syntax", "make-syntax-violation",
    "syntax-violation?", "syntax-violation-form", "syntax-violation-subform", "&undefined",
    "make-undefined-violation", "undefined-violation?",
  ]
  for name in condition_names {
    condition_exports[name] = @runtime.env_lookup_binding(env, name)
  }
  condition_exports["define-condition-type"] =
    @runtime.make_binding(@core.Value::SyntaxKeyword("define-condition-type"))
  register_library_entry("rnrs.conditions", Some([6]), condition_exports)
  let flonum_exports : Map[String,@core.Binding] = {}
  let flonum_names : Array[String] = [
    "flonum?", "real->flonum", "fixnum->flonum", "fl=?", "fl<?", "fl>?", "fl<=?",
    "fl>=?", "flinteger?", "flzero?", "flpositive?", "flnegative?", "flodd?", "fleven?",
    "flfinite?", "flinfinite?", "flnan?", "flmax", "flmin", "fl+", "fl*", "fl-",
    "fl/", "flabs", "fldiv-and-mod", "fldiv", "flmod", "fldiv0-and-mod0", "fldiv0",
    "flmod0", "flnumerator", "fldenominator", "flfloor", "flceiling", "fltruncate",
    "flround", "flexp", "fllog", "flsin", "flcos", "fltan", "flasin", "flacos", "flatan",
    "flsqrt", "flexpt", "&no-infinities", "make-no-infinities-violation", "no-infinities-violation?",
    "&no-nans", "make-no-nans-violation", "no-nans-violation?",
  ]
  for name in flonum_names {
    flonum_exports[name] = @runtime.env_lookup_binding(env, name)
  }
  register_library_entry("rnrs.arithmetic.flonums", Some([6]), flonum_exports)
}

///|
priv struct Counter {
  cell : Ref[Int]
}

///|
fn Counter::new() -> Counter {
  { cell: Ref::new(0) }
}

///|
fn Counter::next(self : Counter) -> Int {
  let id = self.cell.val
  self.cell.val = id + 1
  id
}

///|
let closure_counter : Counter = Counter::new()

///|
fn next_closure_id() -> Int {
  closure_counter.next()
}

///|
fn make_closure(
  params : Array[String],
  rest : String?,
  body : Array[@core.Datum],
  env : @core.Env,
) -> @core.Closure {
  @core.Closure::new(next_closure_id(), params, rest, body, env)
}

///|
let parameter_counter : Counter = Counter::new()

///|
fn next_parameter_id() -> Int {
  parameter_counter.next()
}

///|
fn make_parameter(value : @core.Value) -> @core.Parameter {
  @core.Parameter::new(next_parameter_id(), value, None)
}

///|
fn value_as_parameter(value : @core.Value) -> @core.Parameter raise @core.EvalError {
  match value {
    Parameter(param) => param
    _ => raise @core.EvalError("type error: parameter expected")
  }
}

///|
fn parameter_get(param : @core.Parameter) -> @core.Value {
  param.value.val
}

///|
fn parameter_set(param : @core.Parameter, value : @core.Value) -> Unit {
  param.value.val = value
}

///|
fn parameter_with_converter(value : @core.Value, converter : @core.Value) -> @core.Parameter {
  @core.Parameter::new(next_parameter_id(), value, Some(converter))
}

///|
let promise_counter : Counter = Counter::new()

///|
fn next_promise_id() -> Int {
  promise_counter.next()
}

///|
fn make_promise(thunk : @core.Value) -> @core.Promise {
  @core.Promise::new(next_promise_id(), @core.PromiseState::Thunk(thunk))
}

///|
let eval_env_counter : Counter = Counter::new()

///|
fn next_eval_env_id() -> Int {
  eval_env_counter.next()
}

///|
fn make_eval_env(env : @core.Env) -> @core.EvalEnv {
  @core.EvalEnv::new(next_eval_env_id(), env)
}

///|
fn value_as_eval_env(value : @core.Value) -> @core.EvalEnv raise @core.EvalError {
  match value {
    EvalEnv(env) => env
    _ => raise @core.EvalError("type error: environment expected")
  }
}

///|
fn make_case_closure(clauses : Array[@core.CaseClause], env : @core.Env) -> @core.CaseClosure {
  @core.CaseClosure::new(next_closure_id(), clauses, env)
}

///|
fn make_guard_handler(
  name : String,
  clauses : Array[@core.Datum],
  env : @core.Env,
  resume_value : @core.Value,
  handlers : Array[@core.Value],
) -> @core.GuardHandler {
 @core.GuardHandler::{
    id: next_guard_handler_id(),
    name,
    clauses,
    env,
    resume_value,
    handlers,
  }
}

///|
fn capture_continuation(kont : @core.Kont, handlers : Array[@core.Value]) -> @core.Continuation {
  let winds_copy = wind_stack.val.copy()
 @core.Continuation::{
    id: next_continuation_id(),
    kont,
    handlers,
    winds: winds_copy,
  }
}

///|
priv enum MachineState {
  Eval(@core.Datum, @core.Env, @core.Kont, Array[@core.Value])
  Apply(@core.Value, @core.Kont, Array[@core.Value])
}

///|
let winder_counter : Counter = Counter::new()

///|
fn next_winder_id() -> Int {
  winder_counter.next()
}

///|
fn make_proc_winder(before : @core.Value, after : @core.Value) -> @core.Winder {
 @core.Winder::{ id: next_winder_id(), kind: Proc(before, after) }
}

///|
fn make_param_winder(bindings : Array[@core.ParamBinding]) -> @core.Winder {
 @core.Winder::{ id: next_winder_id(), kind: Params(bindings) }
}

///|
let wind_stack : Ref[Array[@core.Winder]] = Ref::new([])

///|
let continuation_counter : Counter = Counter::new()

///|
let guard_handler_counter : Counter = Counter::new()

///|
fn next_continuation_id() -> Int {
  continuation_counter.next()
}

///|
fn next_guard_handler_id() -> Int {
  guard_handler_counter.next()
}

///|
let do_counter : Counter = Counter::new()

///|
fn next_do_name() -> String {
  let id = do_counter.next()
  "__do_loop__\{id}"
}

///|
/// Evaluate a Scheme program and return the final value.
///
/// # Example
/// ```mbt check
/// test "eval program" {
///   let value = eval_program("(+ 1 2)")
///   inspect(@runtime.value_to_string(value), content="3")
/// }
/// ```
pub fn eval_program(src : String) -> @core.Value raise {
  let results = eval_program_all(src)
  match results {
    [] => @core.Value::Void
    [.., last] => last
  }
}

///|
/// Evaluate a Scheme program and return all top-level results.
///
/// # Example
/// ```mbt check
/// test "eval program all" {
///   let values = eval_program_all("(define x 1) (+ x 2)")
///   inspect(values.length(), content="2")
///   inspect(@runtime.value_to_string(values[1]), content="3")
/// }
/// ```
pub fn eval_program_all(src : String) -> Array[@core.Value] raise {
  let forms = @parser.parse_program(src)
  @runtime.reset_library_registry()
  reset_library_version_registry()
  @runtime.reset_record_type_registry()
  @runtime.set_current_output_port(@runtime.new_output_string_port())
  wind_stack.val = []
  let env = init_env()
  load_stdlib(env)
  register_builtin_libraries(env)
  let results : Array[@core.Value] = []
  for form in forms {
    let value = eval_expr(form, env)
    results.push(value)
  }
  results
}

///|
fn env_with_captures(env : @core.Env, captures : Map[String,@core.Value]) -> @core.Env {
  if captures.is_empty() {
    return env
  }
  let next_env = @runtime.env_extend(env)
  captures.iter2().each((name, value) =>
    @runtime.env_define(next_env, name, value)
  )
  next_env
}

///|
fn values_with_push(values : Array[@core.Value], extra : @core.Value) -> Array[@core.Value] {
  let next = values.copy()
  next.push(extra)
  next
}

///|
fn eval_expr_impl(
  expr : @core.Datum,
  env : @core.Env,
  unwrap_syntax : Bool,
  strip_literals : Bool,
) -> @core.Value raise @core.EvalError {
  let handlers : Array[@core.Value] = []
  let mut state = MachineState::Eval(expr, env, @core.Kont::Halt, handlers)
  while true {
    match state {
      MachineState::Eval(current_expr, current_env, kont, handlers) =>
        match current_expr {
          Label(_, cell) =>
            state = MachineState::Eval(cell.val, current_env, kont, handlers)
          Value(SyntaxObject(obj)) =>
            if unwrap_syntax {
              state = MachineState::Eval(obj.datum, current_env, kont, handlers)
            } else {
              let literal = if strip_literals {
                @runtime.strip_syntax_datum(current_expr)
              } else {
                current_expr
              }
              state = MachineState::Apply(
                value_from_datum(literal),
                kont,
                handlers,
              )
            }
          Nil
          | Bool(_)
          | Int(_)
          | BigInt(_)
          | Rat(_, _)
          | BigRat(_, _)
          | Float(_)
          | Complex(_, _)
          | Char(_)
          | String(_)
          | Vector(_)
          | ByteVector(_)
          | Record(_)
          | Condition(_)
          | Value(_) => {
            let literal = if strip_literals {
              @runtime.strip_syntax_datum(current_expr)
            } else {
              current_expr
            }
            state = MachineState::Apply(
              value_from_datum(literal),
              kont,
              handlers,
            )
          }
          Symbol(name) =>
            match @runtime.env_get_macro(current_env, name) {
              Some(transformer) => {
                let expansion = expand_macro(
                  transformer,
                  Symbol(name),
                  current_env,
                )
                let next_env = env_with_captures(
                  current_env,
                  expansion.captures,
                )
                state = MachineState::Eval(
                  expansion.datum,
                  next_env,
                  kont,
                  handlers,
                )
              }
              None => {
                let value = @runtime.env_lookup(current_env, name)
                state = MachineState::Apply(value, kont, handlers)
              }
            }
          Pair(_, _) =>
            state = eval_pair(current_expr, current_env, kont, handlers)
        }
      MachineState::Apply(value, kont, handlers) =>
        match kont {
          @core.Kont::Halt => return value
          @core.Kont::If(then_expr, else_expr, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = MachineState::Eval(else_expr, env, next, handlers)
            } else {
              state = MachineState::Eval(then_expr, env, next, handlers)
            }
          }
          @core.Kont::Begin(rest, env, next) =>
            if rest.is_empty() {
              state = MachineState::Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(
                rest[0],
                env,
                @core.Kont::Begin(tail, env, next),
                handlers,
              )
            }
          @core.Kont::Set(name, env, next) => {
            let single = expect_single(value)
            @runtime.env_set(env, name, single)
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::Define(name, env, next) => {
            let single = expect_single(value)
            @runtime.env_define(env, name, single)
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::AppFun(args, env, next) => {
            let proc_value = expect_single(value)
            if args.is_empty() {
              state = apply_proc(proc_value, [], next, handlers)
            } else {
              let rest = args.sub(start=1).to_array()
              state = MachineState::Eval(
                args[0],
                env,
                @core.Kont::AppArgs(proc_value, [], rest, env, next),
                handlers,
              )
            }
          }
          @core.Kont::AppArgs(proc, evaluated, remaining, env, next) => {
            let arg_value = expect_single(value)
            let next_values = values_with_push(evaluated, arg_value)
            if remaining.is_empty() {
              state = apply_proc(proc, next_values, next, handlers)
            } else {
              let rest = remaining.sub(start=1).to_array()
              state = MachineState::Eval(
                remaining[0],
                env,
                @core.Kont::AppArgs(proc, next_values, rest, env, next),
                handlers,
              )
            }
          }
          @core.Kont::And(rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = MachineState::Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(
                rest[0],
                env,
                @core.Kont::And(tail, env, next),
                handlers,
              )
            }
          }
          @core.Kont::Or(rest, env, next) => {
            let test_value = expect_single(value)
            if !is_false(test_value) {
              state = MachineState::Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(
                rest[0],
                env,
                @core.Kont::Or(tail, env, next),
                handlers,
              )
            }
          }
          @core.Kont::Cond(parts, rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = cond_start(rest, env, next, handlers)
            } else {
              match parts {
                [_] => state = MachineState::Apply(test_value, next, handlers)
                [_, arrow, proc] if @runtime.symbol_name(arrow) is Some("=>") =>
                  state = MachineState::Eval(
                    proc,
                    env,
                    @core.Kont::CondArrow(test_value, next),
                    handlers,
                  )
                [_, arrow, ..] if @runtime.symbol_name(arrow) is Some("=>") =>
                  raise @core.EvalError("invalid cond")
                _ => {
                  let body = parts.sub(start=1).to_array()
                  state = eval_sequence_state(body, env, next, handlers)
                }
              }
            }
          }
          @core.Kont::CondArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          @core.Kont::Let(bindings, index, values, env, body, next) => {
            let single = expect_single(value)
            let next_values = values_with_push(values, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                @core.Kont::Let(bindings, next_index, next_values, env, body, next),
                handlers,
              )
            } else {
              let new_env = @runtime.env_extend(env)
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let (name, _) = bindings[i]
                @runtime.env_define(new_env, name, next_values[i])
                continue i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          @core.Kont::LetStar(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            let new_env = @runtime.env_extend(env)
            @runtime.env_define(new_env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                @core.Kont::LetStar(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          @core.Kont::LetRec(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            @runtime.env_set(env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                @core.Kont::LetRec(bindings, next_index, env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          @core.Kont::LetRecInit(bindings, index, collected, env, body, next) => {
            let single = expect_single(value)
            let next_values = values_with_push(collected, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                @core.Kont::LetRecInit(
                  bindings, next_index, next_values, env, body, next,
                ),
                handlers,
              )
            } else {
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let (name, _) = bindings[i]
                @runtime.env_set(env, name, next_values[i])
                continue i + 1
              }
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          @core.Kont::LetValues(bindings, index, collected, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let bound = formals_bindings(formals, values)
            let next_collected : Array[Array[(String,@core.Value)]] = []
            for item in collected {
              next_collected.push(item)
            }
            next_collected.push(bound)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                @core.Kont::LetValues(
                  bindings, next_index, next_collected, env, body, next,
                ),
                handlers,
              )
            } else {
              let new_env = @runtime.env_extend(env)
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let binding_values = next_collected[i]
                for binding in binding_values {
                  let (name, bound_value) = binding
                  @runtime.env_define(new_env, name, bound_value)
                }
                continue i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          @core.Kont::LetStarValues(bindings, index, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let binding_values = formals_bindings(formals, values)
            let new_env = @runtime.env_extend(env)
            for binding in binding_values {
              let (name, bound_value) = binding
              @runtime.env_define(new_env, name, bound_value)
            }
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                @core.Kont::LetStarValues(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          @core.Kont::DefineValues(formals, env, next) => {
            let values = value_to_values(value)
            let binding_values = formals_bindings(formals, values)
            for binding in binding_values {
              let (name, bound_value) = binding
              @runtime.env_define(env, name, bound_value)
            }
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::SyntaxCase(rest, env, next) => {
            if rest.length() < 2 {
              raise @core.EvalError("invalid syntax-case")
            }
            let input = expect_single(value)
            let literals_expr = rest[0]
            let clauses = rest.sub(start=1).to_array()
            let rules = parse_syntax_case_rules(literals_expr, clauses, env)
            let call = value_to_syntax_datum(input)
            let expansion = expand_macro(
             @core.MacroTransformer::Rules(rules),
              call,
              env,
            )
            let result = match expansion.datum {
              Value(SyntaxObject(obj)) => @core.Value::SyntaxObject(obj)
              _ => @core.Value::Datum(expansion.datum)
            }
            state = MachineState::Apply(result, next, handlers)
          }
          @core.Kont::ParameterizeParam(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let param_value = expect_single(value)
            let param = value_as_parameter(param_value)
            let next_params : Array[@core.Parameter] = []
            for item in params {
              next_params.push(item)
            }
            next_params.push(param)
            let (_, expr) = bindings[index]
            state = MachineState::Eval(
              expr,
              env,
              @core.Kont::ParameterizeValue(
                bindings, index, next_params, values, env, body, next,
              ),
              handlers,
            )
          }
          @core.Kont::ParameterizeValue(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let new_value = expect_single(value)
            let param = params[index]
            match param.converter {
              Some(converter) =>
                state = apply_proc(
                  converter,
                  [new_value],
                  @core.Kont::ParameterizeConvert(
                    bindings, index, params, values, env, body, next,
                  ),
                  handlers,
                )
              None => {
                let next_values = values_with_push(values, new_value)
                let next_index = index + 1
                if next_index < bindings.length() {
                  let (param_expr, _) = bindings[next_index]
                  state = MachineState::Eval(
                    param_expr,
                    env,
                    @core.Kont::ParameterizeParam(
                      bindings, next_index, params, next_values, env, body, next,
                    ),
                    handlers,
                  )
                } else {
                  let bindings_info : Array[@core.ParamBinding] = []
                  for i = 0; i < params.length(); {
                    // invariant : i >= 0 && i <= params.length()
                    // decreases : params.length() - i
                    // assert : i <= params.length()
                    let current = params[i]
                    let old_value = parameter_get(current)
                    let new_val = next_values[i]
                    bindings_info.push(@core.ParamBinding::{
                      param: current,
                      old_value,
                      new_value: new_val,
                    })
                    parameter_set(current, new_val)
                    continue i + 1
                  }
                  let winder = make_param_winder(bindings_info)
                  wind_push(winder)
                  state = eval_sequence_state(
                    body,
                    env,
                    @core.Kont::WindExit(winder, next),
                    handlers,
                  )
                }
              }
            }
          }
          @core.Kont::ParameterizeConvert(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let converted = expect_single(value)
            let next_values = values_with_push(values, converted)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (param_expr, _) = bindings[next_index]
              state = MachineState::Eval(
                param_expr,
                env,
                @core.Kont::ParameterizeParam(
                  bindings, next_index, params, next_values, env, body, next,
                ),
                handlers,
              )
            } else {
              let bindings_info : Array[@core.ParamBinding] = []
              for i = 0; i < params.length(); {
                // invariant : i >= 0 && i <= params.length()
                // decreases : params.length() - i
                // assert : i <= params.length()
                let current = params[i]
                let old_value = parameter_get(current)
                let new_val = next_values[i]
                bindings_info.push(@core.ParamBinding::{
                  param: current,
                  old_value,
                  new_value: new_val,
                })
                parameter_set(current, new_val)
                continue i + 1
              }
              let winder = make_param_winder(bindings_info)
              wind_push(winder)
              state = eval_sequence_state(
                body,
                env,
                @core.Kont::WindExit(winder, next),
                handlers,
              )
            }
          }
          @core.Kont::MakeParameter(converter, next) => {
            let converted = expect_single(value)
            let param = parameter_with_converter(converted, converter)
            state = MachineState::Apply(@core.Value::Parameter(param), next, handlers)
          }
          @core.Kont::SetParameter(param, next) => {
            let converted = expect_single(value)
            parameter_set(param, converted)
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::GuardResult(info) => {
            let guard_value = expect_single(value)
            state = apply_proc(
              info.resume_value,
              [guard_value],
              kont,
              info.handlers,
            )
          }
          @core.Kont::GuardCond(parts, rest, env, info) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = guard_start(rest, env, info, handlers)
            } else {
              match parts {
                [_] =>
                  state = apply_proc(
                    info.resume_value,
                    [test_value],
                    kont,
                    info.handlers,
                  )
                [_, arrow, proc] if @runtime.symbol_name(arrow) is Some("=>") =>
                  state = MachineState::Eval(
                    proc,
                    env,
                    @core.Kont::GuardCondArrow(test_value, info),
                    handlers,
                  )
                [_, arrow, ..] if @runtime.symbol_name(arrow) is Some("=>") =>
                  raise @core.EvalError("invalid guard")
                _ => {
                  let body = parts.sub(start=1).to_array()
                  state = eval_sequence_state(
                    body,
                    env,
                    @core.Kont::GuardResult(info),
                    handlers,
                  )
                }
              }
            }
          }
          @core.Kont::GuardCondArrow(test_value, info) => {
            let proc_value = expect_single(value)
            state = apply_proc(
              proc_value,
              [test_value],
              @core.Kont::GuardResult(info),
              handlers,
            )
          }
          @core.Kont::Case(clauses, env, next) => {
            let key_value = expect_single(value)
            state = case_dispatch(key_value, clauses, env, next, handlers)
          }
          @core.Kont::CaseArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          @core.Kont::CallWithValues(consumer, next) => {
            let values = value_to_values(value)
            state = apply_proc(consumer, values, next, handlers)
          }
          @core.Kont::RecordConstructorDone(next) => {
            let ctor_value = expect_single(value)
            if !@runtime.is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            state = MachineState::Apply(ctor_value, next, handlers)
          }
          @core.Kont::RecordProtocolNApply(record_type, parent_args, next) => {
            let ctor_value = expect_single(value)
            state = apply_proc(
              ctor_value,
              parent_args,
              @core.Kont::RecordProtocolNResult(record_type, next),
              handlers,
            )
          }
          @core.Kont::RecordProtocolNResult(record_type, next) => {
            let record_value = expect_single(value)
            let record = value_as_record(record_value)
            let parent_type = match record_type.parent {
              Some(parent) => parent
              None =>
                raise @core.EvalError("invalid record constructor descriptor")
            }
            if !@runtime.record_type_is_a(record.record_type, parent_type) {
              raise @core.EvalError(
                "type error: record type mismatch: expected \{parent_type.name}",
              )
            }
            let parent_values : Array[@core.Value] =
              record.fields.map((field) => field.val)
            let proc = @core.Value::RecordProc(
              @runtime.make_record_proc(
               @core.RecordProcKind::ProtocolP(record_type, parent_values),
              ),
            )
            state = MachineState::Apply(proc, next, handlers)
          }
          @core.Kont::DefineRecordType(
            ctor_name,
            pred_name,
            record_type,
            bindings,
            env,
            next
          ) => {
            let ctor_value = expect_single(value)
            if !@runtime.is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            @runtime.env_define(env, ctor_name, ctor_value)
            @runtime.env_define(
              env,
              pred_name,
              @core.Value::RecordProc(
                @runtime.make_record_proc(@core.RecordProcKind::Predicate(record_type)),
              ),
            )
            for binding in bindings {
              @runtime.env_define(
                env,
                binding.accessor(),
                @core.Value::RecordProc(
                  @runtime.make_record_proc(
                   @core.RecordProcKind::Accessor(record_type, binding.index()),
                  ),
                ),
              )
              match binding.mutator() {
                Some(mut_name) =>
                  @runtime.env_define(
                    env,
                    mut_name,
                    @core.Value::RecordProc(
                      @runtime.make_record_proc(
                       @core.RecordProcKind::Mutator(record_type, binding.index()),
                      ),
                    ),
                  )
                None => ()
              }
            }
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::DefineConditionType(
            ctor_name,
            pred_name,
            record_type,
            bindings,
            env,
            next
          ) => {
            let ctor_value = expect_single(value)
            if !@runtime.is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            @runtime.env_define(env, ctor_name, ctor_value)
            @runtime.env_define(
              env,
              pred_name,
              @core.Value::ConditionProc(
                @runtime.make_condition_proc(@core.ConditionProcKind::Predicate(record_type)),
              ),
            )
            for binding in bindings {
              @runtime.env_define(
                env,
                binding.accessor(),
                @core.Value::ConditionProc(
                  @runtime.make_condition_proc(
                   @core.ConditionProcKind::Accessor(record_type, binding.index()),
                  ),
                ),
              )
            }
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::HashtableFindResult(table, key, idx, op, next) => {
            let result_value = expect_single(value)
            if is_false(result_value) {
              state = hashtable_find_state(
                table,
                key,
                idx + 1,
                op,
                next,
                handlers,
              )
            } else {
              state = hashtable_found_state(table, key, idx, op, next, handlers)
            }
          }
          @core.Kont::HashtableUpdateApply(table, key, idx, found, next) => {
            let next_value = expect_single(value)
            if !table.mutable {
              raise @core.EvalError("hashtable is immutable")
            }
            if found {
              let entries = table.entries.val
              let entry = entries[idx]
              entry.value.val = next_value
            } else {
              let entries = table.entries.val
              entries.push(@core.HashtableEntry::{ key, value: Ref::new(next_value) })
            }
            state = MachineState::Apply(@core.Value::Void, next, handlers)
          }
          @core.Kont::MapStep(proc, lists, results, for_each, next) => {
            let single = expect_single(value)
            let next_results = results
            if !for_each {
              next_results.push(value_to_datum_element(single))
            }
            state = map_step(
              proc, lists, next_results, for_each, next, handlers,
            )
          }
          @core.Kont::VectorMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let items = datum_list_to_array(result_datum)
            state = MachineState::Apply(
              @core.Value::Datum(Vector(items)),
              next,
              handlers,
            )
          }
          @core.Kont::StringMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let string_value = apply_primitive(@core.Primitive::ListToString, [
              @core.Value::Datum(result_datum),
            ])
            state = MachineState::Apply(string_value, next, handlers)
          }
          @core.Kont::WindEnter(thunk, winder, next) => {
            wind_push(winder)
            state = apply_proc(
              thunk,
              [],
              @core.Kont::WindExit(winder, next),
              handlers,
            )
          }
          @core.Kont::WindExit(winder, next) => {
            let actions = [@core.WindAction::After(winder)]
            state = run_wind_actions(actions, value, next, handlers, handlers)
          }
          @core.Kont::WindActions(actions, resume_value, final_kont, final_handlers) => {
            let _ = value
            state = run_wind_actions(
              actions, resume_value, final_kont, final_handlers, handlers,
            )
          }
          @core.Kont::WindPush(
            winder,
            actions,
            resume_value,
            final_kont,
            final_handlers
          ) => {
            let _ = value
            wind_push(winder)
            state = run_wind_actions(
              actions, resume_value, final_kont, final_handlers, handlers,
            )
          }
          @core.Kont::ForcePromise(promise, next) => {
            promise.state.val = @core.PromiseState::Value(value)
            state = MachineState::Apply(value, next, handlers)
          }
          @core.Kont::RaiseNonCont =>
            raise @core.EvalError("non-continuable exception")
          @core.Kont::RaiseCont(next) =>
            state = MachineState::Apply(value, next, handlers)
          @core.Kont::RestoreHandlers(prev_handlers, next) =>
            state = MachineState::Apply(value, next, prev_handlers)
        }
    }
  }
  @core.Value::Void
}

///|
fn eval_expr(expr : @core.Datum, env : @core.Env) -> @core.Value raise @core.EvalError {
  eval_expr_impl(expr, env, true, true)
}

///|
fn eval_expr_transformer(expr : @core.Datum, env : @core.Env) -> @core.Value raise @core.EvalError {
  eval_expr_impl(expr, env, false, false)
}

///|

///|
fn eval_quote(args : Array[@core.Datum]) -> @core.Value raise @core.EvalError {
  match args {
    [arg] => @core.Value::Datum(@runtime.strip_syntax_datum(arg))
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_syntax(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  match args {
    [arg] => {
      let scope = @runtime.next_syntax_context()
      let scopes = [scope]
      let binding_id = match @runtime.datum_unlabel(arg) {
        Symbol(name) =>
          match @runtime.env_lookup_optional(env, name) {
            Some(SyntaxObject(obj)) => obj.binding_id
            _ => @runtime.env_binding_id_optional(env, name)
          }
        Value(SyntaxObject(obj)) => obj.binding_id
        _ => None
      }
      let datum = @runtime.syntax_wrap_root(arg, scopes)
      @core.Value::SyntaxObject(@core.SyntaxObject::{ datum, scopes, binding_id })
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_quasiquote(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  match args {
    [arg] =>
      @core.Value::Datum(@runtime.strip_syntax_datum(quasiquote_expand(arg, env, 1)))
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_quasisyntax(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  match args {
    [arg] => {
      let expanded = quasisyntax_expand(arg, env, 1)
      match expanded {
        Value(SyntaxObject(obj)) => @core.Value::SyntaxObject(obj)
        _ => {
          let scope = @runtime.next_syntax_context()
          let scopes = [scope]
          let binding_id = match @runtime.datum_unlabel(expanded) {
            Symbol(name) =>
              match @runtime.env_lookup_optional(env, name) {
                Some(SyntaxObject(obj)) => obj.binding_id
                _ => @runtime.env_binding_id_optional(env, name)
              }
            _ => None
          }
          let datum = @runtime.syntax_wrap_root(expanded, scopes)
          @core.Value::SyntaxObject(@core.SyntaxObject::{ datum, scopes, binding_id })
        }
      }
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_syntax_case_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 3 {
    raise arity_mismatch(3, args.length())
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(
      args[0],
      env,
      @core.Kont::SyntaxCase(rest, env, kont),
      handlers,
    )
  }
}

///|
fn eval_delay(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  match args {
    [arg] => {
      let thunk = @core.Value::Closure(make_closure([], None, [arg], env))
      @core.Value::Promise(make_promise(thunk))
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn unwrap_form(expr : @core.Datum, name : String) -> @core.Datum? {
  match datum_unwrap_syntax(expr) {
    Pair(head, tail) =>
      match datum_unwrap_syntax(head.val) {
        Symbol(head_name) if head_name == name =>
          match tail.val {
            Pair(arg, rest) =>
              match rest.val {
                Nil => Some(arg.val)
                _ => None
              }
            _ => None
          }
        _ => None
      }
    _ => None
  }
}

///|
fn quasiquote_expand(
  expr : @core.Datum,
  env : @core.Env,
  depth : Int,
) -> @core.Datum raise @core.EvalError {
  match unwrap_form(expr, "quasiquote") {
    Some(arg) =>
      return list_from_array([
        Symbol("quasiquote"),
        quasiquote_expand(arg, env, depth + 1),
      ])
    None => ()
  }
  match unwrap_form(expr, "unquote") {
    Some(arg) =>
      if depth == 1 {
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([
          Symbol("unquote"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) =>
      if depth == 1 {
        raise @core.EvalError("unquote-splicing not in list context")
      } else {
        return list_from_array([
          Symbol("unquote-splicing"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match expr {
    Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unquote-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasiquote_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasiquote_expand(car.val, env, depth),
        quasiquote_expand(cdr.val, env, depth),
      )
    }
    Vector(items) => {
      let expanded : Array[@core.Datum] = []
      if depth == 1 {
        for item in items {
          match unwrap_form(item, "unquote-splicing") {
            Some(arg) => {
              let value = eval_expr(arg, env)
              let datum = value_as_datum(value)
              let list_items = datum_list_to_array(datum)
              for list_item in list_items {
                expanded.push(list_item)
              }
            }
            None => expanded.push(quasiquote_expand(item, env, depth))
          }
        }
      } else {
        for item in items {
          expanded.push(quasiquote_expand(item, env, depth))
        }
      }
      Vector(expanded)
    }
    _ => expr
  }
}

///|
fn quasisyntax_expand(
  expr : @core.Datum,
  env : @core.Env,
  depth : Int,
) -> @core.Datum raise @core.EvalError {
  match unwrap_form(expr, "quasisyntax") {
    Some(arg) =>
      return list_from_array([
        Symbol("quasisyntax"),
        quasisyntax_expand(arg, env, depth + 1),
      ])
    None => ()
  }
  match unwrap_form(expr, "unsyntax") {
    Some(arg) =>
      if depth == 1 {
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([
          Symbol("unsyntax"),
          quasisyntax_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match unwrap_form(expr, "unsyntax-splicing") {
    Some(arg) =>
      if depth == 1 {
        raise @core.EvalError("unsyntax-splicing not in list context")
      } else {
        return list_from_array([
          Symbol("unsyntax-splicing"),
          quasisyntax_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match expr {
    Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unsyntax-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = match value {
              SyntaxObject(obj) => obj.datum
              _ => value_as_datum(value)
            }
            let items = datum_list_to_array(datum)
            let tail = quasisyntax_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasisyntax_expand(car.val, env, depth),
        quasisyntax_expand(cdr.val, env, depth),
      )
    }
    Vector(items) => {
      let expanded : Array[@core.Datum] = []
      if depth == 1 {
        for item in items {
          match unwrap_form(item, "unsyntax-splicing") {
            Some(arg) => {
              let value = eval_expr(arg, env)
              let datum = match value {
                SyntaxObject(obj) => obj.datum
                _ => value_as_datum(value)
              }
              let list_items = datum_list_to_array(datum)
              for list_item in list_items {
                expanded.push(list_item)
              }
            }
            None => expanded.push(quasisyntax_expand(item, env, depth))
          }
        }
      } else {
        for item in items {
          expanded.push(quasisyntax_expand(item, env, depth))
        }
      }
      Vector(expanded)
    }
    _ => expr
  }
}

///|
fn list_with_tail(items : Array[@core.Datum], tail : @core.Datum) -> @core.Datum {
  for i = items.length(), result = tail; i > 0; {
    // invariant : i >= 0 && i <= items.length()
    // decreases : i
    // assert : i <= items.length()
    let idx = i - 1
    continue i - 1, pair_new(items[idx], result)
  } else {
    result
  }
}

///|
fn parse_formals(expr : @core.Datum) -> @core.Formals raise @core.EvalError {
  let names : Array[String] = []
  for cur = expr; true; {
    // invariant : names.length() >= 0
    // TODO(decreases) : list structure not explicit; possible bug
    // assert : names.length() >= 0
    match datum_unwrap_syntax(cur) {
      Nil => break@core.Formals::{ params: names, rest: None }
      Symbol(name) => break@core.Formals::{ params: names, rest: Some(name) }
      Pair(car, cdr) => {
        let name = parse_symbol(car.val)
        names.push(name)
        continue cdr.val
      }
      _ => raise @core.EvalError("type error: symbol expected")
    }
  } else {
   @core.Formals::{ params: names, rest: None }
  }
}

///|
fn arity_mismatch_at_least(expected : Int, got : Int) -> @core.EvalError {
  @core.EvalError("arity mismatch: expected at least \{expected} got \{got}")
}

///|
fn values_to_list_value(values : Array[@core.Value]) -> @core.Value {
  let items : Array[@core.Datum] =
    values.map((value) => value_to_datum_element(value))
  @core.Value::Datum(list_from_array(items))
}

///|
fn formals_bindings(
  formals : @core.Formals,
  values : Array[@core.Value],
) -> Array[(String,@core.Value)] raise @core.EvalError {
  let params = formals.params
  let min_len = params.length()
  match formals.rest {
    None =>
      if values.length() != min_len {
        raise arity_mismatch(min_len, values.length())
      }
    Some(_) =>
      if values.length() < min_len {
        raise arity_mismatch_at_least(min_len, values.length())
      }
  }
  let bindings : Array[(String,@core.Value)] = []
  for i = 0; i < params.length(); {
    // invariant : i >= 0 && i <= params.length()
    // decreases : params.length() - i
    // assert : i <= params.length()
    bindings.push((params[i], values[i]))
    continue i + 1
  }
  match formals.rest {
    Some(name) => {
      let rest_values : Array[@core.Value] = []
      for j = min_len; j < values.length(); {
        // invariant : j >= min_len && j <= values.length()
        // decreases : values.length() - j
        // assert : j <= values.length()
        rest_values.push(values[j])
        continue j + 1
      }
      bindings.push((name, values_to_list_value(rest_values)))
    }
    None => ()
  }
  bindings
}

///|
fn formals_accept_len(formals : @core.Formals, arg_len : Int) -> Bool {
  match formals.rest {
    None => arg_len == formals.params.length()
    Some(_) => arg_len >= formals.params.length()
  }
}

///|
fn formals_expected_desc(formals : @core.Formals) -> String {
  match formals.rest {
    None => formals.params.length().to_string()
    Some(_) => ">=\{formals.params.length()}"
  }
}

///|
fn eval_lambda(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let formals = parse_formals(args[0])
    let body = args.sub(start=1).to_array()
    @core.Value::Closure(make_closure(formals.params, formals.rest, body, env))
  }
}

///|
fn eval_case_lambda(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  if args.is_empty() {
    raise arity_mismatch(1, 0)
  }
  let clauses : Array[@core.CaseClause] = args.map((clause) => {
    let (formals_expr, body_exprs) = match datum_list_to_array(clause) {
      [formals_expr, first_body, ..rest] => {
        let body : Array[@core.Datum] = [first_body]
        for expr in rest {
          body.push(expr)
        }
        (formals_expr, body)
      }
      _ => raise @core.EvalError("invalid case-lambda")
    }
    let formals = parse_formals(formals_expr)
   @core.CaseClause::{
      params: formals.params,
      rest: formals.rest,
      body: body_exprs,
    }
  })
  @core.Value::CaseClosure(make_case_closure(clauses, env))
}

///|
fn eval_define_syntax(args : Array[@core.Datum], env : @core.Env) -> @core.Value raise @core.EvalError {
  match args {
    [name_expr, transformer_expr] => {
      let name = parse_symbol(name_expr)
      let transformer = parse_transformer(transformer_expr, name, env)
      @runtime.env_define(env, name, @core.Value::Macro(transformer))
      @core.Value::Void
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn parse_syntax_bindings(
  expr : @core.Datum,
  env : @core.Env,
) -> Array[(String,@core.MacroTransformer)] raise @core.EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String,@core.MacroTransformer)] = []
  for binding in bindings {
    match datum_list_to_array(binding) {
      [name_expr, transformer_expr] => {
        let name = parse_symbol(name_expr)
        let transformer = parse_transformer(transformer_expr, name, env)
        parsed.push((name, transformer))
      }
      _ => raise @core.EvalError("type error: syntax binding expected")
    }
  }
  parsed
}

///|
fn parse_bindings(expr : @core.Datum) -> Array[(String,@core.Datum)] raise @core.EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String,@core.Datum)] = []
  for binding in bindings {
    match datum_list_to_array(binding) {
      [name_expr, value_expr] => {
        let name = parse_symbol(name_expr)
        parsed.push((name, value_expr))
      }
      _ => raise @core.EvalError("type error: binding expected")
    }
  }
  parsed
}

///|
fn parse_value_bindings(
  expr : @core.Datum,
) -> Array[(@core.Formals,@core.Datum)] raise @core.EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(@core.Formals,@core.Datum)] = []
  for binding in bindings {
    match datum_list_to_array(binding) {
      [formals_expr, body_expr] => {
        let formals = parse_formals(formals_expr)
        parsed.push((formals, body_expr))
      }
      _ => raise @core.EvalError("invalid let-values")
    }
  }
  parsed
}

///|
fn parse_parameterize_bindings(
  expr : @core.Datum,
) -> Array[(@core.Datum,@core.Datum)] raise @core.EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(@core.Datum,@core.Datum)] = []
  for binding in bindings {
    match datum_list_to_array(binding) {
      [param_expr, value_expr] => parsed.push((param_expr, value_expr))
      _ => raise @core.EvalError("invalid parameterize")
    }
  }
  parsed
}

///|

///|
fn parse_symbol(expr : @core.Datum) -> String raise @core.EvalError {
  match @runtime.symbol_name(datum_unwrap_syntax(expr)) {
    Some(name) => name
    None => raise @core.EvalError("type error: symbol expected")
  }
}

///|
fn parse_bool(expr : @core.Datum) -> Bool raise @core.EvalError {
  match expr {
    Bool(value) => value
    _ => raise @core.EvalError("invalid define-record-type")
  }
}

///|
fn parse_symbol_list(
  items : ArrayView[@core.Datum],
) -> Array[String] raise @core.EvalError {
  let names : Array[String] = []
  for item in items {
    names.push(parse_symbol(item))
  }
  names
}

///|
fn expect_single(value : @core.Value) -> @core.Value raise @core.EvalError {
  match value {
    Values(values) =>
      match values {
        [single] => single
        _ => raise @core.EvalError("multiple values in single-value context")
      }
    _ => value
  }
}

///|
fn value_to_values(value : @core.Value) -> Array[@core.Value] {
  match value {
    Values(values) => values
    _ => [value]
  }
}

///|
fn raise_to_handlers(
  condition : @core.Value,
  handlers : Array[@core.Value],
  kont : @core.Kont,
  continuable : Bool,
) -> MachineState raise @core.EvalError {
  match handlers {
    [] => raise @core.EvalError("uncaught exception")
    [.., handler] =>
      if continuable {
        apply_proc(handler, [condition], @core.Kont::RaiseCont(kont), handlers)
      } else {
        apply_proc(handler, [condition], @core.Kont::RaiseNonCont, handlers)
      }
  }
}

///|
fn guard_start(
  clauses : Array[@core.Datum],
  env : @core.Env,
  info : @core.GuardInfo,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      let head = @runtime.symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, @core.Kont::GuardResult(info), handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(
          parts[0],
          env,
          @core.Kont::GuardCond(parts, rest, env, info),
          handlers,
        )
      }
    }
  }
  raise_to_handlers(
    info.condition,
    info.handlers,
    info.raise_kont,
    info.continuable,
  )
}

///|
fn handle_guard(
  handler : @core.GuardHandler,
  condition : @core.Value,
  raise_kont : @core.Kont,
  continuable : Bool,
) -> MachineState raise @core.EvalError {
  let guard_env = @runtime.env_extend(handler.env)
  @runtime.env_define(guard_env, handler.name, condition)
  let info =@core.GuardInfo::{
    condition,
    resume_value: handler.resume_value,
    handlers: handler.handlers,
    raise_kont,
    continuable,
  }
  guard_start(handler.clauses, guard_env, info, handler.handlers)
}

///|
fn wind_push(winder : @core.Winder) -> Unit {
  let next : Array[@core.Winder] = []
  for item in wind_stack.val {
    next.push(item)
  }
  next.push(winder)
  wind_stack.val = next
}

///|
fn wind_pop() -> @core.Winder {
  let current = wind_stack.val
  match current {
    [] => panic()
    [..prefix, last] => {
      wind_stack.val = prefix.to_array()
      last
    }
  }
}

///|
fn apply_winder_before(
  winder : @core.Winder,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match winder.kind {
    Proc(before, _) => apply_proc(before, [], kont, handlers)
    Params(bindings) => {
      for binding in bindings {
        parameter_set(binding.param, binding.new_value)
      }
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
  }
}

///|
fn apply_winder_after(
  winder : @core.Winder,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match winder.kind {
    Proc(_, after) => apply_proc(after, [], kont, handlers)
    Params(bindings) => {
      for binding in bindings {
        parameter_set(binding.param, binding.old_value)
      }
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
  }
}

///|
fn common_wind_prefix(current : Array[@core.Winder], target : Array[@core.Winder]) -> Int {
  for i = 0; i < current.length() && i < target.length(); {
    // invariant : i >= 0 && i <= current.length()
    // invariant : i >= 0 && i <= target.length()
    // decreases : (if current.length() < target.length() { current.length() } else { target.length() }) - i
    // assert : i <= current.length() && i <= target.length()
    if current[i].id != target[i].id {
      break i
    }
    continue i + 1
  } else {
    i
  }
}

///|

///|
fn run_wind_actions(
  actions : Array[@core.WindAction],
  resume_value : @core.Value,
  final_kont : @core.Kont,
  final_handlers : Array[@core.Value],
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if actions.is_empty() {
    MachineState::Apply(resume_value, final_kont, final_handlers)
  } else {
    let rest = actions.sub(start=1).to_array()
    match actions[0] {
      After(winder) => {
        let _ = wind_pop()
        apply_winder_after(
          winder,
          @core.Kont::WindActions(rest, resume_value, final_kont, final_handlers),
          handlers,
        )
      }
      Before(winder) =>
        apply_winder_before(
          winder,
          @core.Kont::WindPush(winder, rest, resume_value, final_kont, final_handlers),
          handlers,
        )
      SwitchHandlers(next_handlers) =>
        run_wind_actions(
          rest, resume_value, final_kont, final_handlers, next_handlers,
        )
    }
  }
}

///|
fn eval_sequence_state(
  exprs : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState {
  if exprs.is_empty() {
    MachineState::Apply(@core.Value::Void, kont, handlers)
  } else {
    let rest = exprs.sub(start=1).to_array()
    MachineState::Eval(exprs[0], env, @core.Kont::Begin(rest, env, kont), handlers)
  }
}

///|
fn eval_begin_for_syntax_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  let syntax_env = @runtime.env_clone(env)
  for form in args {
    let _ = eval_expr(form, syntax_env)

  }
  let frame = match syntax_env {
    [.., frame] => frame
    [] => panic()
  }
  frame.iter2().each((key, binding) =>
    match binding.value() {
      Macro(rules) => @runtime.env_define(env, key, @core.Value::Macro(rules))
      _ => ()
    }
  )
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|
fn eval_if_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match args {
    [test_expr, then_expr, else_expr] =>
      MachineState::Eval(
        test_expr,
        env,
        @core.Kont::If(then_expr, else_expr, env, kont),
        handlers,
      )
    _ => raise arity_mismatch(3, args.length())
  }
}

///|
fn eval_define_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match datum_unwrap_syntax(args[0]) {
      Symbol(_) => {
        let name = parse_symbol(args[0])
        match args {
          [_, value_expr] =>
            MachineState::Eval(
              value_expr,
              env,
              @core.Kont::Define(name, env, kont),
              handlers,
            )
          _ => raise arity_mismatch(2, args.length())
        }
      }
      Pair(head, tail) => {
        let name = parse_symbol(head.val)
        let formals = parse_formals(tail.val)
        let body = args.sub(start=1).to_array()
        @runtime.env_define(
          env,
          name,
          @core.Value::Closure(make_closure(formals.params, formals.rest, body, env)),
        )
        MachineState::Apply(@core.Value::Void, kont, handlers)
      }
      _ => raise @core.EvalError("type error: symbol expected")
    }
  }
}

///|
fn eval_define_values_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match args {
    [formals_expr, value_expr] => {
      let formals = parse_formals(formals_expr)
      MachineState::Eval(
        value_expr,
        env,
        @core.Kont::DefineValues(formals, env, kont),
        handlers,
      )
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn eval_guard_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let spec = datum_list_to_array(args[0])
    if spec.is_empty() {
      raise @core.EvalError("invalid guard")
    }
    let name = parse_symbol(spec[0])
    let clauses = spec.sub(start=1).to_array()
    let body = args.sub(start=1).to_array()
    let resume_value = @core.Value::Continuation(
      capture_continuation(kont, handlers),
    )
    let guard_handlers : Array[@core.Value] = []
    for item in handlers {
      guard_handlers.push(item)
    }
    let handler = @core.Value::GuardHandler(
      make_guard_handler(name, clauses, env, resume_value, guard_handlers),
    )
    let next_handlers : Array[@core.Value] = []
    for item in handlers {
      next_handlers.push(item)
    }
    next_handlers.push(handler)
    eval_sequence_state(
      body,
      env,
      @core.Kont::RestoreHandlers(handlers, kont),
      next_handlers,
    )
  }
}

///|
fn eval_parameterize_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_parameterize_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (param_expr, _) = bindings[0]
      MachineState::Eval(
        param_expr,
        env,
        @core.Kont::ParameterizeParam(bindings, 0, [], [], env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_set_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match args {
    [name_expr, value_expr] => {
      let name = parse_symbol(name_expr)
      match @runtime.env_get_macro(env, name) {
        Some(transformer) => {
          let expansion = expand_macro(
            transformer,
            list_from_array([
              Symbol("set!"),
              Symbol(name),
              value_expr,
            ]),
            env,
          )
          let next_env = env_with_captures(env, expansion.captures)
          MachineState::Eval(expansion.datum, next_env, kont, handlers)
        }
        None =>
          MachineState::Eval(
            value_expr,
            env,
            @core.Kont::Set(name, env, kont),
            handlers,
          )
      }
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn eval_let_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Symbol(name) => {
        if args.length() < 3 {
          raise arity_mismatch(3, args.length())
        }
        let bindings = parse_bindings(args[1])
        let body = args.sub(start=2).to_array()
        let expanded = expand_named_let(name, bindings, body)
        MachineState::Eval(expanded, env, kont, handlers)
      }
      _ => {
        let bindings = parse_bindings(args[0])
        let body = args.sub(start=1).to_array()
        if bindings.is_empty() {
          eval_sequence_state(body, env, kont, handlers)
        } else {
          let (_, expr) = bindings[0]
          MachineState::Eval(
            expr,
            env,
            @core.Kont::Let(bindings, 0, [], env, body, kont),
            handlers,
          )
        }
      }
    }
  }
}

///|

///|
fn eval_let_values_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        env,
        @core.Kont::LetValues(bindings, 0, [], env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_let_star_values_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        env,
        @core.Kont::LetStarValues(bindings, 0, env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_let_star_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        env,
        @core.Kont::LetStar(bindings, 0, env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_letrec_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = @runtime.env_extend(env)
    for binding in bindings {
      let (name, _) = binding
      @runtime.env_define(new_env, name, @core.Value::Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        new_env,
        @core.Kont::LetRecInit(bindings, 0, [], new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_letrec_star_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = @runtime.env_extend(env)
    for binding in bindings {
      let (name, _) = binding
      @runtime.env_define(new_env, name, @core.Value::Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        new_env,
        @core.Kont::LetRec(bindings, 0, new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn expand_named_let(
  name : String,
  bindings : Array[(String,@core.Datum)],
  body : Array[@core.Datum],
) -> @core.Datum {
  let param_items : Array[@core.Datum] = []
  let arg_items : Array[@core.Datum] = []
  for binding in bindings {
    let (param, expr) = binding
    param_items.push(Symbol(param))
    arg_items.push(expr)
  }
  let lambda_items : Array[@core.Datum] = []
  lambda_items.push(Symbol("lambda"))
  lambda_items.push(list_from_array(param_items))
  for expr in body {
    lambda_items.push(expr)
  }
  let lambda_expr = list_from_array(lambda_items)
  let binding_expr = list_from_array([Symbol(name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let call_items : Array[@core.Datum] = []
  call_items.push(Symbol(name))
  for arg in arg_items {
    call_items.push(arg)
  }
  let call_expr = list_from_array(call_items)
  list_from_array([Symbol("letrec"), bindings_expr, call_expr])
}

///|
fn eval_let_syntax_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0], env)
    let body = args.sub(start=1).to_array()
    let new_env = @runtime.env_extend(env)
    for binding in bindings {
      let (name, transformer) = binding
      @runtime.env_define(new_env, name, @core.Value::Macro(transformer))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn eval_letrec_syntax_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let new_env = @runtime.env_extend(env)
    let binding_specs = datum_list_to_array(args[0])
    for i = 0; i < binding_specs.length(); {
      // invariant : i >= 0 && i <= binding_specs.length()
      // decreases : binding_specs.length() - i
      // assert : i <= binding_specs.length()
      let name = match datum_list_to_array(binding_specs[i]) {
        [name_expr, _] => parse_symbol(name_expr)
        _ => raise @core.EvalError("type error: syntax binding expected")
      }
      let placeholder =@core.MacroTransformer::Rules(@core.SyntaxRules::{
        literals: [],
        rules: [],
        ellipsis: "...",
        kind: @core.SyntaxRulesKind::SyntaxRules,
        def_env: new_env,
      })
      @runtime.env_define(new_env, name, @core.Value::Macro(placeholder))
      continue i + 1
    }
    let bindings = parse_syntax_bindings(args[0], new_env)
    let body = args.sub(start=1).to_array()
    for binding in bindings {
      let (name, transformer) = binding
      @runtime.env_define(new_env, name, @core.Value::Macro(transformer))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn build_begin(exprs : Array[@core.Datum]) -> @core.Datum {
  let items : Array[@core.Datum] = []
  items.push(Symbol("begin"))
  for expr in exprs {
    items.push(expr)
  }
  list_from_array(items)
}

///|
fn parse_do_bindings(
  expr : @core.Datum,
) -> (Array[String], Array[@core.Datum], Array[@core.Datum]) raise @core.EvalError {
  let bindings = datum_list_to_array(expr)
  let names : Array[String] = []
  let inits : Array[@core.Datum] = []
  let steps : Array[@core.Datum] = []
  for binding in bindings {
    match datum_list_to_array(binding) {
      [name_expr, init_expr] => {
        let name = parse_symbol(name_expr)
        names.push(name)
        inits.push(init_expr)
        steps.push(Symbol(name))
      }
      [name_expr, init_expr, step_expr] => {
        let name = parse_symbol(name_expr)
        names.push(name)
        inits.push(init_expr)
        steps.push(step_expr)
      }
      _ => raise @core.EvalError("invalid do")
    }
  }
  (names, inits, steps)
}

///|
fn eval_do_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let (names, inits, steps) = parse_do_bindings(args[0])
  let (test_expr, result_exprs) = match datum_list_to_array(args[1]) {
    [test_expr, ..rest] => {
      let results : Array[@core.Datum] = []
      for expr in rest {
        results.push(expr)
      }
      (test_expr, results)
    }
    _ => raise @core.EvalError("invalid do")
  }
  let command_exprs = args.sub(start=2).to_array()
  let loop_name = next_do_name()
  let params : Array[@core.Datum] = []
  for name in names {
    params.push(Symbol(name))
  }
  let call_items : Array[@core.Datum] = []
  call_items.push(Symbol(loop_name))
  for step in steps {
    call_items.push(step)
  }
  let call_expr = list_from_array(call_items)
  let else_body : Array[@core.Datum] = []
  for expr in command_exprs {
    else_body.push(expr)
  }
  else_body.push(call_expr)
  let then_expr = build_begin(result_exprs)
  let else_expr = build_begin(else_body)
  let if_expr = list_from_array([
    Symbol("if"),
    test_expr,
    then_expr,
    else_expr,
  ])
  let lambda_expr = list_from_array([
    Symbol("lambda"),
    list_from_array(params),
    if_expr,
  ])
  let binding_expr = list_from_array([Symbol(loop_name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let init_call_items : Array[@core.Datum] = []
  init_call_items.push(Symbol(loop_name))
  for init in inits {
    init_call_items.push(init)
  }
  let init_call_expr = list_from_array(init_call_items)
  let expanded = list_from_array([
    Symbol("letrec"),
    bindings_expr,
    init_call_expr,
  ])
  MachineState::Eval(expanded, env, kont, handlers)
}

///|
fn eval_define_record_type_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 3 {
    raise @core.EvalError("invalid define-record-type")
  }
  let record_name = parse_symbol(args[0])
  let ctor_parts = datum_list_to_array(args[1])
  if ctor_parts.is_empty() {
    raise @core.EvalError("invalid define-record-type")
  }
  let ctor_name = parse_symbol(ctor_parts[0])
  let ctor_fields = parse_symbol_list(ctor_parts.sub(start=1))
  let pred_name = parse_symbol(args[2])
  let raw_specs = args.sub(start=3).to_array()
  let field_specs : Array[@core.Datum] = []
  let (parent_name, protocol_expr, is_sealed, is_opaque, uid) =
    for i = 0,
      parent_name = None,
      protocol_expr = None,
      is_sealed = false,
      is_opaque = false,
      uid = None;
      i < raw_specs.length(); {
      // invariant : i >= 0 && i <= raw_specs.length()
      // decreases : raw_specs.length() - i
      // assert : i <= raw_specs.length()
      let spec = raw_specs[i]
      let parts = datum_list_to_array(spec)
      match parts {
        [head] =>
          match @runtime.symbol_name(head) {
            Some("sealed") => {
              let is_sealed = true
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("opaque") => {
              let is_opaque = true
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("nongenerative") => {
              if uid is Some(_) {
                raise @core.EvalError("invalid define-record-type")
              }
              let uid = Some(record_name)
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            _ => ()
          }
        [head, arg] =>
          match @runtime.symbol_name(head) {
            Some("parent") => {
              if parent_name is Some(_) {
                raise @core.EvalError("invalid define-record-type")
              }
              let parent_name = Some(parse_symbol(arg))
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("protocol") => {
              if protocol_expr is Some(_) {
                raise @core.EvalError("invalid define-record-type")
              }
              let protocol_expr = Some(arg)
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("sealed") => {
              let is_sealed = parse_bool(arg)
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("opaque") => {
              let is_opaque = parse_bool(arg)
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            Some("nongenerative") => {
              if uid is Some(_) {
                raise @core.EvalError("invalid define-record-type")
              }
              let uid = Some(parse_symbol(arg))
              continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
            }
            _ => ()
          }
        _ => ()
      }
      field_specs.push(spec)
      continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
    } else {
      (parent_name, protocol_expr, is_sealed, is_opaque, uid)
    }
  let parent_desc = match parent_name {
    Some(name) =>
      match @runtime.lookup_record_type_descriptor(name) {
        Some(desc) => Some(desc)
        None => raise @core.EvalError("invalid define-record-type")
      }
    None => None
  }
  let parent_type = match parent_desc {
    Some(desc) => Some(desc.record_type)
    None => None
  }
  match parent_type {
    Some(parent) =>
      if parent.is_sealed {
        raise @core.EvalError("invalid define-record-type")
      }
    None => ()
  }
  let parent_fields = match parent_type {
    Some(record_type) => record_type.fields
    None => []
  }
  let parent_count = parent_fields.length()
  let parent_field_names : Array[String] =
    parent_fields.map((field) => field.name)
  let accessors : Map[String, String] = {}
  let mutators : Map[String, String] = {}
  let mutable : Map[String, Bool] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    let (field_name_expr, accessor_expr, mutator_expr) =
      match datum_list_to_array(spec) {
        [field_name_expr, accessor_expr] => (field_name_expr, accessor_expr, None)
        [field_name_expr, accessor_expr, mutator_expr] =>
          (field_name_expr, accessor_expr, Some(mutator_expr))
        _ => raise @core.EvalError("invalid define-record-type")
      }
    let field_name = parse_symbol(field_name_expr)
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    let accessor = parse_symbol(accessor_expr)
    accessors[field_name] = accessor
    match mutator_expr {
      Some(expr) => {
        let mutator = parse_symbol(expr)
        mutators[field_name] = mutator
        mutable[field_name] = true
      }
      None => mutable[field_name] = false
    }
    new_field_names.push(field_name)
  }
  if parent_count == 0 {
    if ctor_fields.length() != new_field_names.length() {
      raise @core.EvalError("invalid define-record-type")
    }
  } else if ctor_fields.length() != parent_count + new_field_names.length() {
    raise @core.EvalError("invalid define-record-type")
  }
  let fields : Array[@core.RecordField] = []
  let field_index : Map[String, Int] = {}
  for i = 0; i < ctor_fields.length(); {
    // invariant : i >= 0 && i <= ctor_fields.length()
    // decreases : ctor_fields.length() - i
    // assert : i <= ctor_fields.length()
    let field_name = ctor_fields[i]
    if field_index.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if i < parent_count {
      let parent_field = parent_fields[i]
      if parent_field.name != field_name {
        raise @core.EvalError("invalid define-record-type")
      }
      fields.push(parent_field)
    } else {
      let is_mutable = match mutable.get(field_name) {
        Some(flag) => flag
        None => raise @core.EvalError("invalid define-record-type")
      }
      fields.push(@core.RecordField::{ name: field_name, mutable: is_mutable })
    }
    field_index[field_name] = i
    continue i + 1
  }
  let bindings : Array[@core.RecordFieldBinding] = new_field_names.map((field_name) => {
    let idx = match field_index.get(field_name) {
      Some(value) => value
      None => raise @core.EvalError("invalid define-record-type")
    }
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-record-type")
    }
    let mutator_name = mutators.get(field_name)
    @core.RecordFieldBinding::new(accessor_name, idx, mutator_name)
  })
  let existing_desc = match uid {
    Some(uid_name) => @runtime.lookup_record_type_descriptor_by_uid(uid_name)
    None => None
  }
  let (record_type, ctor_desc) = match existing_desc {
    Some(desc) => {
      if !record_type_matches(
          desc.record_type,
          parent_type,
          is_sealed,
          is_opaque,
          fields,
        ) {
        raise @core.EvalError("invalid define-record-type")
      }
      if protocol_expr is Some(_) {
        raise @core.EvalError("invalid define-record-type")
      }
      @runtime.register_record_type_alias(record_name, desc)
      (desc.record_type, desc.constructor_desc)
    }
    None => {
      let effective_opaque = match parent_type {
        Some(parent) => parent.is_opaque || is_opaque
        None => is_opaque
      }
      let record_type = @runtime.make_record_type(
        record_name, parent_type, is_sealed, effective_opaque, uid, fields,
      )
      let protocol_value = match protocol_expr {
        Some(expr) => {
          let proc = eval_expr(expr, env)
          if !@runtime.is_procedure_value(proc) {
            raise @core.EvalError("type error: procedure expected")
          }
          Some(proc)
        }
        None => None
      }
      let parent_ctor_desc = match parent_desc {
        Some(desc) => Some(desc.constructor_desc)
        None => None
      }
      let ctor_desc = @runtime.make_record_constructor_descriptor(
        record_type, parent_ctor_desc, protocol_value,
      )
      let rtd = @runtime.make_record_type_descriptor(record_type, ctor_desc)
      let _ = @runtime.register_record_type(record_name, rtd)
      (record_type, ctor_desc)
    }
  }
  let next_kont = @core.Kont::DefineRecordType(
    ctor_name, pred_name, record_type, bindings, env, kont,
  )
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn eval_define_condition_type_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 4 {
    raise @core.EvalError("invalid define-condition-type")
  }
  let record_name = parse_symbol(args[0])
  let parent_name = parse_symbol(args[1])
  let ctor_name = parse_symbol(args[2])
  let pred_name = parse_symbol(args[3])
  let field_specs = args.sub(start=4).to_array()
  match @runtime.lookup_record_type_descriptor(record_name) {
    Some(_) => raise @core.EvalError("invalid define-condition-type")
    None => ()
  }
  let parent_desc = match @runtime.lookup_record_type_descriptor(parent_name) {
    Some(desc) => desc
    None => raise @core.EvalError("invalid define-condition-type")
  }
  let parent_type = parent_desc.record_type
  if parent_type.is_sealed || parent_type.is_opaque {
    raise @core.EvalError("invalid define-condition-type")
  }
  let base_type = condition_base_type()
  if !@runtime.record_type_is_a(parent_type, base_type) {
    raise @core.EvalError("invalid define-condition-type")
  }
  let parent_fields = parent_type.fields
  let parent_field_names : Array[String] =
    parent_fields.map((field) => field.name)
  let accessors : Map[String, String] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    let (field_name_expr, accessor_expr) = match datum_list_to_array(spec) {
      [field_name_expr, accessor_expr] => (field_name_expr, accessor_expr)
      _ => raise @core.EvalError("invalid define-condition-type")
    }
    let field_name = parse_symbol(field_name_expr)
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    let accessor = parse_symbol(accessor_expr)
    accessors[field_name] = accessor
    new_field_names.push(field_name)
  }
  let fields = parent_fields.copy()
  fields.push_iter(
    new_field_names
      .map((field_name) =>@core.RecordField::{ name: field_name, mutable: false })
      .iter(),
  )
  let bindings : Array[@core.RecordFieldBinding] = []
  let parent_count = parent_fields.length()
  for i = 0; i < new_field_names.length(); {
    // invariant : i >= 0 && i <= new_field_names.length()
    // decreases : new_field_names.length() - i
    // assert : i <= new_field_names.length()
    let field_name = new_field_names[i]
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-condition-type")
    }
    bindings.push(@core.RecordFieldBinding::new(accessor_name, parent_count + i, None))
    continue i + 1
  }
  let record_type = @runtime.make_record_type(
    record_name,
    Some(parent_type),
    false,
    false,
    None,
    fields,
  )
  let ctor_desc = @runtime.make_record_constructor_descriptor(
    record_type,
    Some(parent_desc.constructor_desc),
    None,
  )
  let rtd = @runtime.make_record_type_descriptor(record_type, ctor_desc)
  let _ = @runtime.register_record_type(record_name, rtd)
  @runtime.env_define(env, record_name, @core.Value::RecordTypeDescriptor(rtd))
  let next_kont = @core.Kont::DefineConditionType(
    ctor_name, pred_name, record_type, bindings, env, kont,
  )
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn eval_define_enumeration_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 3 {
    raise @core.EvalError("invalid define-enumeration")
  }
  let type_name = parse_symbol(args[0])
  let symbols = parse_symbol_list(datum_list_to_array(args[1]))
  let ctor_name = parse_symbol(args[2])
  let seen : Map[String, Bool] = {}
  for name in symbols {
    if seen.contains(name) {
      raise @core.EvalError("invalid define-enumeration")
    }
    seen[name] = true
  }
  let members = enum_set_members_all_true(symbols)
  let universe = @runtime.make_enum_set(symbols, members)
  @runtime.env_define(env, type_name, @core.Value::EnumSet(universe))
  @runtime.env_define(
    env,
    ctor_name,
    @core.Value::EnumSetProc(
      @runtime.make_enum_set_proc(@core.EnumSetProcKind::Constructor(universe)),
    ),
  )
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|

///|

///|
fn array_contains_name(items : Array[String], name : String) -> Bool {
  for item in items {
    if item == name {
      return true
    }
  }
  false
}

///|
fn value_as_record(value : @core.Value) -> @core.Record raise @core.EvalError {
  match value {
    Record(record) => record
    _ => raise @core.EvalError("type error: record expected")
  }
}

///|
fn record_constructor_from_descriptor(
  desc : @core.RecordConstructorDescriptor,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match desc.protocol {
    None => {
      let ctor = @core.Value::RecordProc(
        @runtime.make_record_proc(
         @core.RecordProcKind::Constructor(desc.record_type),
        ),
      )
      MachineState::Apply(ctor, kont, handlers)
    }
    Some(protocol) => {
      if !@runtime.is_procedure_value(protocol) {
        raise @core.EvalError("type error: procedure expected")
      }
      let arg = match desc.record_type.parent {
        None =>
          @core.Value::RecordProc(
            @runtime.make_record_proc(
             @core.RecordProcKind::Constructor(desc.record_type),
            ),
          )
        Some(parent) => {
          let parent_desc = match desc.parent_desc {
            Some(p) => p
            None => default_constructor_descriptor(parent)
          }
          @core.Value::RecordProc(
            @runtime.make_record_proc(
             @core.RecordProcKind::ProtocolN(desc.record_type, parent_desc),
            ),
          )
        }
      }
      apply_proc(
        protocol,
        [arg],
        @core.Kont::RecordConstructorDone(kont),
        handlers,
      )
    }
  }
}

///|
fn apply_record_proc(
  proc : @core.RecordProc,
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match proc.kind {
    Constructor(record_type) => {
      let field_count = record_type.fields.length()
      if args.length() != field_count {
        raise arity_mismatch(field_count, args.length())
      } else {
        let record = @runtime.make_record_instance(record_type, args)
        MachineState::Apply(@core.Value::Record(record), kont, handlers)
      }
    }
    Predicate(record_type) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let result = match args[0] {
          Record(record) =>
            @runtime.record_type_is_a(record.record_type, record_type)
          _ => false
        }
        MachineState::Apply(bool_value(result), kont, handlers)
      }
    Accessor(record_type, idx) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let record = value_as_record(args[0])
        if !@runtime.record_type_is_a(record.record_type, record_type) {
          raise @core.EvalError(
            "type error: record type mismatch: expected \{record_type.name}",
          )
        }
        MachineState::Apply(record.fields[idx].val, kont, handlers)
      }
    Mutator(record_type, idx) =>
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let record = value_as_record(args[0])
        if !@runtime.record_type_is_a(record.record_type, record_type) {
          raise @core.EvalError(
            "type error: record type mismatch: expected \{record_type.name}",
          )
        }
        let field = record.record_type.fields[idx]
        if !field.mutable {
          raise @core.EvalError("record field is immutable: \{field.name}")
        }
        record.fields[idx].val = args[1]
        MachineState::Apply(@core.Value::Void, kont, handlers)
      }
    ProtocolN(record_type, parent_desc) => {
      let parent_type = match record_type.parent {
        Some(parent) => parent
        None => raise @core.EvalError("invalid record constructor descriptor")
      }
      match parent_desc.protocol {
        None => {
          if args.length() != parent_type.fields.length() {
            raise arity_mismatch(parent_type.fields.length(), args.length())
          }
          let parent_values = args.copy()
          let proc = @core.Value::RecordProc(
            @runtime.make_record_proc(
             @core.RecordProcKind::ProtocolP(record_type, parent_values),
            ),
          )
          MachineState::Apply(proc, kont, handlers)
        }
        Some(_) => {
          let parent_args = args.copy()
          record_constructor_from_descriptor(
            parent_desc,
            @core.Kont::RecordProtocolNApply(record_type, parent_args, kont),
            handlers,
          )
        }
      }
    }
    ProtocolP(record_type, parent_values) => {
      let total_fields = record_type.fields.length()
      if parent_values.length() > total_fields {
        raise @core.EvalError("invalid record constructor descriptor")
      }
      let child_count = total_fields - parent_values.length()
      if args.length() != child_count {
        raise arity_mismatch(child_count, args.length())
      }
      let values = parent_values.copy()
      values.push_iter(args.iter())
      let record = @runtime.make_record_instance(record_type, values)
      MachineState::Apply(@core.Value::Record(record), kont, handlers)
    }
  }
}

///|
fn apply_condition_proc(
  proc : @core.ConditionProc,
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match proc.kind {
    Predicate(record_type) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let ok = match condition_components_opt(args[0], base) {
          Some(components) =>
            for i = 0; i < components.length(); {
              // invariant : i >= 0 && i <= components.length()
              // decreases : components.length() - i
              // assert : i <= components.length()
              if @runtime.record_type_is_a(components[i].record_type, record_type) {
                break true
              }
              continue i + 1
            } else {
              false
            }
          None => false
        }
        MachineState::Apply(bool_value(ok), kont, handlers)
      }
    Accessor(record_type, idx) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let components = condition_components(args[0], base)
        let found =
          for i = 0; i < components.length(); {
            // invariant : i >= 0 && i <= components.length()
            // decreases : components.length() - i
            // assert : i <= components.length()
            let record = components[i]
            if @runtime.record_type_is_a(record.record_type, record_type) {
              break Some(record)
            }
            continue i + 1
          } else {
            None
          }
        match found {
          Some(record) =>
            MachineState::Apply(record.fields[idx].val, kont, handlers)
          None =>
            raise @core.EvalError(
              "type error: condition type mismatch: expected \{record_type.name}",
            )
        }
      }
  }
}

///|
fn apply_enum_set_proc(
  proc : @core.EnumSetProc,
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match proc.kind {
    Indexer(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let name = parse_symbol(value_as_datum(args[0]))
      match @runtime.enum_set_index_of(set.universe, name) {
        Some(idx) =>
          MachineState::Apply(@core.Value::Datum(Int(idx)), kont, handlers)
        None =>
          MachineState::Apply(@core.Value::Datum(Bool(false)), kont, handlers)
      }
    }
    Constructor(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let names = parse_symbol_list(
        datum_list_to_array(value_as_datum(args[0])),
      )
      let subset = @runtime.enum_set_from_names(set.universe, names)
      MachineState::Apply(@core.Value::EnumSet(subset), kont, handlers)
    }
  }
}

///|
fn make_condition_value(components : Array[@core.Record]) -> @core.Value {
  if components.length() == 1 {
    @core.Value::Record(components[0])
  } else {
    @core.Value::Datum(Condition(@runtime.make_condition(components)))
  }
}

///|
fn lookup_condition_record_type(name : String) -> @core.RecordType raise @core.EvalError {
  match @runtime.lookup_record_type_descriptor(name) {
    Some(desc) => desc.record_type
    None => raise @core.EvalError("type error: condition type expected")
  }
}

///|
fn make_condition_record(
  name : String,
  values : Array[@core.Value],
) -> @core.Record raise @core.EvalError {
  let record_type = lookup_condition_record_type(name)
  if record_type.fields.length() != values.length() {
    raise @core.EvalError("type error: condition field count mismatch")
  }
  @runtime.make_record_instance(record_type, values)
}

///|
fn irritants_list_value(values : Array[@core.Value]) -> @core.Value {
  values_to_list_value(values)
}

///|
fn raise_condition(
  components : Array[@core.Record],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  let cond_value = make_condition_value(components)
  apply_proc(@core.Value::Primitive(@core.Primitive::Raise), [cond_value], kont, handlers)
}

///|
fn hashtable_key_equal(
  equiv : @core.HashtableEquiv,
  left : @core.Value,
  right : @core.Value,
) -> Bool {
  match equiv {
    @core.HashtableEquiv::Eq => value_eqv(left, right)
    @core.HashtableEquiv::Eqv => value_eqv(left, right)
    @core.HashtableEquiv::Equal => value_equal(left, right)
    @core.HashtableEquiv::Proc(_) => false
  }
}

///|
fn hashtable_find_state(
  table : @core.Hashtable,
  key : @core.Value,
  idx : Int,
  op : @core.HashtableOp,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  let entries = table.entries.val
  if idx >= entries.length() {
    return hashtable_not_found_state(table, key, op, kont, handlers)
  }
  let entry = entries[idx]
  match table.equiv {
    @core.HashtableEquiv::Proc(proc) =>
      apply_proc(
        proc,
        [key, entry.key],
        @core.Kont::HashtableFindResult(table, key, idx, op, kont),
        handlers,
      )
    _ =>
      if hashtable_key_equal(table.equiv, key, entry.key) {
        hashtable_found_state(table, key, idx, op, kont, handlers)
      } else {
        hashtable_find_state(table, key, idx + 1, op, kont, handlers)
      }
  }
}

///|
fn hashtable_found_state(
  table : @core.Hashtable,
  key : @core.Value,
  idx : Int,
  op : @core.HashtableOp,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  let entries = table.entries.val
  let entry = entries[idx]
  match op {
    @core.HashtableOp::Ref(_) => MachineState::Apply(entry.value.val, kont, handlers)
    @core.HashtableOp::Contains =>
      MachineState::Apply(bool_value(true), kont, handlers)
    @core.HashtableOp::Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      entry.value.val = value
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
    @core.HashtableOp::Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let next_entries : Array[@core.HashtableEntry] = []
      for i = 0; i < entries.length(); {
        // invariant : i >= 0 && i <= entries.length()
        // decreases : entries.length() - i
        // assert : i <= entries.length()
        if i != idx {
          next_entries.push(entries[i])
        }
        continue i + 1
      }
      table.entries.val = next_entries
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
    @core.HashtableOp::Update(proc, _) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [entry.value.val],
        @core.Kont::HashtableUpdateApply(table, key, idx, true, kont),
        handlers,
      )
    }
  }
}

///|
fn hashtable_not_found_state(
  table : @core.Hashtable,
  key : @core.Value,
  op : @core.HashtableOp,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match op {
    @core.HashtableOp::Ref(default_value) =>
      MachineState::Apply(default_value, kont, handlers)
    @core.HashtableOp::Contains =>
      MachineState::Apply(bool_value(false), kont, handlers)
    @core.HashtableOp::Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let entries = table.entries.val
      entries.push(@core.HashtableEntry::{ key, value: Ref::new(value) })
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
    @core.HashtableOp::Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      MachineState::Apply(@core.Value::Void, kont, handlers)
    }
    @core.HashtableOp::Update(proc, default_value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [default_value],
        @core.Kont::HashtableUpdateApply(table, key, -1, false, kont),
        handlers,
      )
    }
  }
}

///|
fn apply_hashtable_ref(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let default_value = args[2]
  hashtable_find_state(
    table,
    key,
    0,
    @core.HashtableOp::Ref(default_value),
    kont,
    handlers,
  )
}

///|
fn apply_hashtable_set(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let value = args[2]
  hashtable_find_state(
    table,
    key,
    0,
    @core.HashtableOp::Set(value),
    kont,
    handlers,
  )
}

///|
fn apply_hashtable_delete(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(
    table,
    key,
    0,
    @core.HashtableOp::Delete,
    kont,
    handlers,
  )
}

///|
fn apply_hashtable_contains(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(
    table,
    key,
    0,
    @core.HashtableOp::Contains,
    kont,
    handlers,
  )
}

///|
fn apply_hashtable_update(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() != 4 {
    raise arity_mismatch(4, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let proc = args[2]
  let default_value = args[3]
  if !@runtime.is_procedure_value(proc) {
    raise @core.EvalError("type error: procedure expected")
  }
  hashtable_find_state(
    table,
    key,
    0,
    @core.HashtableOp::Update(proc, default_value),
    kont,
    handlers,
  )
}

///|
priv struct LibraryNameSpec {
  base : String
  version : Array[Int]?
}

///|
priv enum VersionRef {
  VersionExact(Array[Int])
  VersionGt(Array[Int])
  VersionGe(Array[Int])
  VersionLt(Array[Int])
  VersionLe(Array[Int])
  VersionAnd(Array[VersionRef])
  VersionOr(Array[VersionRef])
  VersionNot(Ref[VersionRef])
}

///|
priv struct LibraryRef {
  base : String
  version_ref : VersionRef?
}

///|
priv struct LibraryVersionInfo {
  name : String
  version : Array[Int]
  has_version : Bool
}

///|
let library_versions_registry : Ref[Map[String, Array[LibraryVersionInfo]]] = Ref::new({},
)

///|
fn reset_library_version_registry() -> Unit {
  library_versions_registry.val = {}
}

///|
fn format_version_suffix(version : Array[Int]) -> String {
  let versions : Array[String] = []
  for part in version {
    versions.push(part.to_string())
  }
  let joined = versions.join(".")
  "v\{joined}"
}

///|
fn library_full_name(base : String, version : Array[Int]?) -> String {
  match version {
    Some(value) => "\{base}.\{format_version_suffix(value)}"
    None => base
  }
}

///|
fn register_library_entry(
  base : String,
  version : Array[Int]?,
  exports : Map[String,@core.Binding],
) -> Unit {
  let name = library_full_name(base, version)
  @runtime.register_library(name, exports)
  let entry = LibraryVersionInfo::{
    name,
    version: match version {
      Some(value) => value
      None => []
    },
    has_version: version is Some(_),
  }
  match library_versions_registry.val.get(base) {
    Some(existing) => {
      let updated = existing
      updated.push(entry)
      library_versions_registry.val[base] = updated
    }
    None => library_versions_registry.val[base] = [entry]
  }
}

///|
fn parse_version_items(
  items : ArrayView[@core.Datum],
) -> Array[Int] raise @core.EvalError {
  match items {
    [] => raise @core.EvalError("invalid library name")
    _ => {
      let versions : Array[Int] = []
      for part in items {
        match part {
          Int(n) =>
            if n < 0 {
              raise @core.EvalError("invalid library name")
            } else {
              versions.push(n)
            }
          _ => raise @core.EvalError("invalid library name")
        }
      }
      versions
    }
  }
}

///|
fn parse_version_list(expr : @core.Datum) -> Array[Int] raise @core.EvalError {
  parse_version_items(datum_list_to_array(expr))
}

///|
fn parse_version_ref_version(
  items : ArrayView[@core.Datum],
) -> Array[Int] raise @core.EvalError {
  match items {
    [] => raise @core.EvalError("invalid library name")
    [item] =>
      match item {
        Pair(_, _) | Nil => parse_version_list(item)
        _ => parse_version_items(items)
      }
    _ => parse_version_items(items)
  }
}

///|
fn parse_version_ref(expr : @core.Datum) -> VersionRef raise @core.EvalError {
  let items = datum_list_to_array(expr)
  match items {
    [] => raise @core.EvalError("invalid library name")
    [Symbol("and"), ..refs] => {
      if refs.is_empty() {
        raise @core.EvalError("invalid library name")
      }
      let parsed : Array[VersionRef] = []
      for item in refs {
        parsed.push(parse_version_ref(item))
      }
      VersionAnd(parsed)
    }
    [Symbol("or"), ..refs] => {
      if refs.is_empty() {
        raise @core.EvalError("invalid library name")
      }
      let parsed : Array[VersionRef] = []
      for item in refs {
        parsed.push(parse_version_ref(item))
      }
      VersionOr(parsed)
    }
    [Symbol("not"), ref_expr] =>
      VersionNot(Ref::new(parse_version_ref(ref_expr)))
    [Symbol(">"), ..rest] => {
      VersionGt(parse_version_ref_version(rest))
    }
    [Symbol(">="), ..rest] => {
      VersionGe(parse_version_ref_version(rest))
    }
    [Symbol("<"), ..rest] => {
      VersionLt(parse_version_ref_version(rest))
    }
    [Symbol("<="), ..rest] => {
      VersionLe(parse_version_ref_version(rest))
    }
    [Int(_), ..] => VersionExact(parse_version_items(items))
    _ => raise @core.EvalError("invalid library name")
  }
}

///|
fn parse_library_name_spec(expr : @core.Datum) -> LibraryNameSpec raise @core.EvalError {
  match datum_list_to_array(expr) {
    [] => raise @core.EvalError("invalid library name")
    [..prefix, last] => {
      let names : Array[String] = []
      for part in prefix {
        match part {
          Symbol(name) => names.push(name)
          _ => raise @core.EvalError("invalid library name")
        }
      }
      let version = match last {
        Symbol(name) => {
          names.push(name)
          None
        }
        Pair(_, _) | Nil => Some(parse_version_list(last))
        _ => raise @core.EvalError("invalid library name")
      }
      if names.is_empty() {
        raise @core.EvalError("invalid library name")
      }
      { base: names.join("."), version }
    }
  }
}

///|
fn parse_library_ref(expr : @core.Datum) -> LibraryRef raise @core.EvalError {
  match datum_list_to_array(expr) {
    [] => raise @core.EvalError("invalid library name")
    [..prefix, last] => {
      let names : Array[String] = []
      for part in prefix {
        match part {
          Symbol(name) => names.push(name)
          _ => raise @core.EvalError("invalid library name")
        }
      }
      let version_ref = match last {
        Symbol(name) => {
          names.push(name)
          None
        }
        Pair(_, _) | Nil => Some(parse_version_ref(last))
        _ => raise @core.EvalError("invalid library name")
      }
      if names.is_empty() {
        raise @core.EvalError("invalid library name")
      }
      { base: names.join("."), version_ref }
    }
  }
}

///|
fn compare_versions(left : Array[Int], right : Array[Int]) -> Int {
  let min_len = if left.length() < right.length() {
    left.length()
  } else {
    right.length()
  }
  let prefix_cmp = for i = 0; i < min_len; {
    // invariant : i >= 0 && i <= min_len
    // decreases : min_len - i
    // assert : i <= min_len
    if left[i] < right[i] {
      break -1
    }
    if left[i] > right[i] {
      break 1
    }
    continue i + 1
  } else {
    0
  }
  if prefix_cmp != 0 {
    prefix_cmp
  } else if left.length() < right.length() {
    -1
  } else if left.length() > right.length() {
    1
  } else {
    0
  }
}

///|
fn version_ref_matches(version : Array[Int], reference : VersionRef) -> Bool {
  match reference {
    VersionExact(expected) => compare_versions(version, expected) == 0
    VersionGt(expected) => compare_versions(version, expected) > 0
    VersionGe(expected) => compare_versions(version, expected) >= 0
    VersionLt(expected) => compare_versions(version, expected) < 0
    VersionLe(expected) => compare_versions(version, expected) <= 0
    VersionAnd(items) => {
      for i = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        if !version_ref_matches(version, items[i]) {
          break false
        }
        continue i + 1
      } else {
        true
      }
    }
    VersionOr(items) => {
      for i = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        if version_ref_matches(version, items[i]) {
          break true
        }
        continue i + 1
      } else {
        false
      }
    }
    VersionNot(item) => !version_ref_matches(version, item.val)
  }
}

///|
fn select_library_for_ref(reference : LibraryRef) -> LibraryVersionInfo? {
  match library_versions_registry.val.get(reference.base) {
    None => None
    Some(available) => {
      let version_ref = reference.version_ref
      let best = for i = 0, best = (None : LibraryVersionInfo?);
        i < available.length(); {
        // invariant : i >= 0 && i <= available.length()
        // decreases : available.length() - i
        // assert : i <= available.length()
        let entry = available[i]
        match version_ref {
          Some(ref_value) => {
            if !entry.has_version {
              continue i + 1, best
            }
            if !version_ref_matches(entry.version, ref_value) {
              continue i + 1, best
            }
          }
          None => ()
        }
        let next_best = match best {
          None => Some(entry)
          Some(current) =>
            if version_ref is None {
              if entry.has_version && !current.has_version {
                Some(entry)
              } else if entry.has_version && current.has_version {
                if compare_versions(entry.version, current.version) > 0 {
                  Some(entry)
                } else {
                  Some(current)
                }
              } else {
                Some(current)
              }
            } else if compare_versions(entry.version, current.version) > 0 {
              Some(entry)
            } else {
              Some(current)
            }
        }
        continue i + 1, next_best
      } else {
        best
      }
      best
    }
  }
}

///|
fn library_exports_from_ref(
  reference : LibraryRef,
) -> Map[String,@core.Binding] raise @core.EvalError {
  match select_library_for_ref(reference) {
    Some(entry) => library_exports(entry.name)
    None => raise @core.EvalError("unknown library: \{reference.base}")
  }
}

///|
fn parse_export_clause(expr : @core.Datum) -> Array[(String, String)] raise @core.EvalError {
  match datum_list_to_array(expr) {
    [Symbol("export"), ..specs] => {
      let exports : Array[(String, String)] = []
      for spec in specs {
        match spec {
          Symbol(name) => exports.push((name, name))
          Pair(_, _) =>
            match datum_list_to_array(spec) {
              [Symbol("rename"), ..rename_specs] =>
                for rename_spec in rename_specs {
                  match datum_list_to_array(rename_spec) {
                    [internal_expr, external_expr] => {
                      let internal = parse_symbol(internal_expr)
                      let external = parse_symbol(external_expr)
                      exports.push((external, internal))
                    }
                    _ => raise @core.EvalError("invalid library")
                  }
                }
              _ => raise @core.EvalError("invalid library")
            }
          _ => raise @core.EvalError("invalid library")
        }
      }
      exports
    }
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn base_syntax_keywords() -> Array[String] {
  [
    "...", "_", "=>", "and", "begin", "case", "cond", "define", "define-syntax",
    "else", "identifier-syntax", "if", "lambda", "let", "let*", "let*-values", "let-syntax",
    "let-values", "letrec", "letrec*", "letrec-syntax", "or", "quasiquote", "quote",
    "set!", "syntax-rules", "unquote", "unquote-splicing",
  ]
}

///|
fn syntax_case_keywords() -> Array[String] {
  [
    "...", "_", "with-syntax", "syntax", "quasisyntax", "unsyntax", "unsyntax-splicing",
    "syntax-case",
  ]
}

///|
fn control_syntax_keywords() -> Array[String] {
  ["case-lambda", "do"]
}

///|
fn exceptions_syntax_keywords() -> Array[String] {
  ["guard"]
}

///|
fn enums_syntax_keywords() -> Array[String] {
  ["define-enumeration"]
}

///|
fn records_syntax_keywords() -> Array[String] {
  [
    "define-record-type", "fields", "immutable", "mutable", "nongenerative", "opaque",
    "parent", "parent-rtd", "protocol", "record-constructor-descriptor", "record-type-descriptor",
    "sealed",
  ]
}

///|
fn conditions_syntax_keywords() -> Array[String] {
  ["define-condition-type"]
}

///|
fn is_syntax_keyword(name : String) -> Bool {
  let lists : Array[Array[String]] = [
    base_syntax_keywords(),
    syntax_case_keywords(),
    control_syntax_keywords(),
    exceptions_syntax_keywords(),
    enums_syntax_keywords(),
    records_syntax_keywords(),
    conditions_syntax_keywords(),
  ]
  for keywords in lists {
    for kw in keywords {
      if kw == name {
        return true
      }
    }
  }
  false
}

///|
fn parse_import_clause(expr : @core.Datum) -> Array[@core.Datum] raise @core.EvalError {
  match datum_list_to_array(expr) {
    [Symbol("import"), ..rest] => rest.to_array()
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn library_exports(name : String) -> Map[String,@core.Binding] raise @core.EvalError {
  match @runtime.lookup_library(name) {
    Some(exports_map) => {
      let exports : Map[String,@core.Binding] = {}
      exports_map.iter2().each((key, binding) => exports[key] = binding)
      exports
    }
    None => raise @core.EvalError("unknown library: \{name}")
  }
}

///|
fn import_set_only(
  exports : Map[String,@core.Binding],
  names : Array[String],
) -> Map[String,@core.Binding] raise @core.EvalError {
  let result : Map[String,@core.Binding] = {}
  for name in names {
    match exports.get(name) {
      Some(binding) => result[name] = binding
      None => raise @core.EvalError("unknown import: \{name}")
    }
  }
  result
}

///|
fn import_set_except(
  exports : Map[String,@core.Binding],
  names : Array[String],
) -> Map[String,@core.Binding] raise @core.EvalError {
  let excluded : Map[String, Bool] = {}
  for name in names {
    if !exports.contains(name) {
      raise @core.EvalError("unknown import: \{name}")
    }
    excluded[name] = true
  }
  let result : Map[String,@core.Binding] = {}
  exports.iter2().each((key, binding) =>
    if !excluded.contains(key) {
      result[key] = binding
    }
  )
  result
}

///|
fn import_set_rename(
  exports : Map[String,@core.Binding],
  renames : Array[(String, String)],
) -> Map[String,@core.Binding] raise @core.EvalError {
  let rename_map : Map[String, String] = {}
  for rename in renames {
    let (old_name, new_name) = rename
    if !exports.contains(old_name) {
      raise @core.EvalError("unknown import: \{old_name}")
    }
    if rename_map.contains(old_name) {
      raise @core.EvalError("invalid import set")
    }
    rename_map[old_name] = new_name
  }
  let result : Map[String,@core.Binding] = {}
  for key in exports.keys() {
    let target = match rename_map.get(key) {
      Some(name) => name
      None => key
    }
    if result.contains(target) {
      raise @core.EvalError("invalid import set")
    }
    match exports.get(key) {
      Some(binding) => result[target] = binding
      None => ()
    }
  }
  result
}

///|
fn import_set_prefix(
  exports : Map[String,@core.Binding],
  prefix : String,
) -> Map[String,@core.Binding] {
  let result : Map[String,@core.Binding] = {}
  exports.iter2().each((key, binding) =>
    result["\{prefix}\{key}"] = binding
  )
  result
}

///|
fn parse_rename_specs(
  items : Array[@core.Datum],
) -> Array[(String, String)] raise @core.EvalError {
  items.map(fn (item) raise @core.EvalError {
    match datum_list_to_array(item) {
      [old_expr, new_expr] => {
        let old_name = parse_symbol(old_expr)
        let new_name = parse_symbol(new_expr)
        (old_name, new_name)
      }
      _ => raise @core.EvalError("invalid import set")
    }
  })
}

///|
priv struct ImportPhases {
  run : Bool
  expand : Bool
}

///|
fn parse_for_phases(items : Array[@core.Datum]) -> ImportPhases raise @core.EvalError {
  match items {
    [] => raise @core.EvalError("invalid import set")
    _ => {
      let (run, expand) = for i = 0, run = false, expand = false;
        i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        let item = items[i]
        match item {
          Symbol("run") => continue i + 1, true, expand
          Symbol("expand") => continue i + 1, run, true
          Pair(_, _) | Nil => {
            match datum_list_to_array(item) {
              [Symbol("meta"), Int(n)] =>
                if n == 0 {
                  continue i + 1, true, expand
                } else if n > 0 {
                  continue i + 1, run, true
                } else {
                  raise @core.EvalError("invalid import set")
                }
              _ => raise @core.EvalError("invalid import set")
            }
          }
          _ => raise @core.EvalError("invalid import set")
        }
      } else {
        (run, expand)
      }
      if !run && !expand {
        raise @core.EvalError("invalid import set")
      }
      { run, expand }
    }
  }
}

///|
fn filter_exports_by_phase(
  exports : Map[String,@core.Binding],
  phases : ImportPhases,
) -> Map[String,@core.Binding] {
  if phases.run && phases.expand {
    return exports
  }
  let result : Map[String,@core.Binding] = {}
  exports.iter2().each((key, binding) =>
    match binding.value() {
      Macro(_) => if phases.expand { result[key] = binding }
      SyntaxKeyword(_) => if phases.expand { result[key] = binding }
      _ => if phases.run { result[key] = binding }
    }
  )
  result
}

///|
fn import_set_exports(expr : @core.Datum) -> Map[String,@core.Binding] raise @core.EvalError {
  match datum_list_to_array(expr) {
    [Symbol("only"), base_expr, name_expr, ..rest] => {
      let names : Array[@core.Datum] = [name_expr]
      for item in rest {
        names.push(item)
      }
      let base = import_set_exports(base_expr)
      let names = parse_symbol_list(names)
      import_set_only(base, names)
    }
    [Symbol("only"), ..] => raise @core.EvalError("invalid import set")
    [Symbol("except"), base_expr, name_expr, ..rest] => {
      let names : Array[@core.Datum] = [name_expr]
      for item in rest {
        names.push(item)
      }
      let base = import_set_exports(base_expr)
      let names = parse_symbol_list(names)
      import_set_except(base, names)
    }
    [Symbol("except"), ..] => raise @core.EvalError("invalid import set")
    [Symbol("rename"), base_expr, spec_expr, ..rest] => {
      let specs : Array[@core.Datum] = [spec_expr]
      for item in rest {
        specs.push(item)
      }
      let base = import_set_exports(base_expr)
      let renames = parse_rename_specs(specs)
      import_set_rename(base, renames)
    }
    [Symbol("rename"), ..] => raise @core.EvalError("invalid import set")
    [Symbol("prefix"), base_expr, prefix_expr] => {
      let base = import_set_exports(base_expr)
      let prefix = parse_symbol(prefix_expr)
      import_set_prefix(base, prefix)
    }
    [Symbol("prefix"), ..] => raise @core.EvalError("invalid import set")
    [Symbol("for"), base_expr, phase_expr, ..rest] => {
      let phases : Array[@core.Datum] = [phase_expr]
      for item in rest {
        phases.push(item)
      }
      let base = import_set_exports(base_expr)
      let phases = parse_for_phases(phases)
      filter_exports_by_phase(base, phases)
    }
    [Symbol("for"), ..] => raise @core.EvalError("invalid import set")
    _ => {
      let reference = parse_library_ref(expr)
      library_exports_from_ref(reference)
    }
  }
}

///|
fn import_set_into_env(expr : @core.Datum, env : @core.Env) -> Unit raise @core.EvalError {
  let exports = import_set_exports(expr)
  exports.iter2().each((key, binding) =>
    @runtime.env_define_binding(env, key, binding)
  )
}

///|
fn eval_library_state(
  args : Array[@core.Datum],
  _env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 3 {
    raise @core.EvalError("invalid library")
  }
  let name_spec = parse_library_name_spec(args[0])
  let exports = parse_export_clause(args[1])
  let imports = parse_import_clause(args[2])
  let body = args.sub(start=3).to_array()
  let lib_env = init_env()
  for import_set in imports {
    import_set_into_env(import_set, lib_env)
  }
  for form in body {
    let _ = eval_expr(form, lib_env)

  }
  let export_map : Map[String,@core.Binding] = {}
  for export_spec in exports {
    let (external, internal) = export_spec
    if is_syntax_keyword(internal) {
      export_map[external] = @runtime.make_binding(@core.Value::SyntaxKeyword(external))
    } else {
      export_map[external] = @runtime.env_lookup_binding(lib_env, internal)
    }
  }
  register_library_entry(name_spec.base, name_spec.version, export_map)
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|
fn eval_import_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  for spec in args {
    import_set_into_env(spec, env)
  }
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|
fn eval_and_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState {
  match args {
    [] => MachineState::Apply(bool_value(true), kont, handlers)
    [head, ..rest] => {
      let tail = rest.to_array()
      MachineState::Eval(head, env, @core.Kont::And(tail, env, kont), handlers)
    }
  }
}

///|
fn eval_or_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState {
  match args {
    [] => MachineState::Apply(bool_value(false), kont, handlers)
    [head, ..rest] => {
      let tail = rest.to_array()
      MachineState::Eval(head, env, @core.Kont::Or(tail, env, kont), handlers)
    }
  }
}

///|
fn eval_case_state(
  args : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let key_expr = args[0]
  let clauses = args.sub(start=1).to_array()
  MachineState::Eval(key_expr, env, @core.Kont::Case(clauses, env, kont), handlers)
}

///|
fn case_dispatch(
  key : @core.Value,
  clauses : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      match @runtime.symbol_name(parts[0]) {
        Some("else") => {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
        _ => ()
      }
      let datums = datum_list_to_array(parts[0])
      let matched = for j = 0; j < datums.length(); {
        // invariant : j >= 0 && j <= datums.length()
        // decreases : datums.length() - j
        // assert : j <= datums.length()
        if value_eqv(key, @core.Value::Datum(datums[j])) {
          break true
        }
        continue j + 1
      } else {
        false
      }
      if matched {
        match parts {
          [_, arrow, proc] if @runtime.symbol_name(arrow) is Some("=>") =>
            return MachineState::Eval(
              proc,
              env,
              @core.Kont::CaseArrow(key, kont),
              handlers,
            )
          [_, arrow, ..] if @runtime.symbol_name(arrow) is Some("=>") =>
            raise @core.EvalError("invalid case")
          _ => {
            let body = parts.sub(start=1).to_array()
            return eval_sequence_state(body, env, kont, handlers)
          }
        }
      }
      continue i + 1
    }
  }
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|
fn cond_start(
  clauses : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      let head = @runtime.symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(
          parts[0],
          env,
          @core.Kont::Cond(parts, rest, env, kont),
          handlers,
        )
      }
    }
  }
  MachineState::Apply(@core.Value::Void, kont, handlers)
}

///|
fn feature_enabled(name : String) -> Bool {
  match name {
    "r6rs" => true
    "ratios" => true
    "ieee-float" => true
    "full-unicode" => true
    "exact-closed" => false
    "exact-complex" => false
    _ => false
  }
}

///|
fn cond_expand_feature(expr : @core.Datum) -> Bool raise @core.EvalError {
  match expr {
    Symbol(name) => feature_enabled(name)
    Pair(_, _) => {
      match datum_list_to_array(expr) {
        [Symbol("and")] => true
        [Symbol("and"), ..rest] =>
          for item in rest {
            if !cond_expand_feature(item) {
              break false
            }
          } else {
            true
          }
        [Symbol("or"), ..rest] =>
          for item in rest {
            if cond_expand_feature(item) {
              break true
            }
          } else {
            false
          }
        [Symbol("not"), item] => !cond_expand_feature(item)
        [Symbol("not"), ..] => raise @core.EvalError("invalid cond-expand")
        [Symbol("library"), item] => {
          let reference = parse_library_ref(item)
          select_library_for_ref(reference) is Some(_)
        }
        [Symbol("library"), ..] => raise @core.EvalError("invalid cond-expand")
        _ => raise @core.EvalError("invalid cond-expand")
      }
    }
    _ => raise @core.EvalError("invalid cond-expand")
  }
}

///|
fn eval_cond_expand_state(
  clauses : Array[@core.Datum],
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match clauses {
    [] => MachineState::Apply(@core.Value::Void, kont, handlers)
    [..prefix, last] => {
      for clause in prefix {
        let parts = datum_list_to_array(clause)
        match parts {
          [] => raise @core.EvalError("invalid cond-expand")
          [head, ..rest] =>
            match head {
              Symbol("else") => raise @core.EvalError("invalid cond-expand")
              _ =>
                if cond_expand_feature(head) {
                  return eval_sequence_state(rest.to_array(), env, kont, handlers)
                }
            }
        }
      }
      let parts = datum_list_to_array(last)
      match parts {
        [] => raise @core.EvalError("invalid cond-expand")
        [head, ..rest] =>
          match head {
            Symbol("else") =>
              eval_sequence_state(rest.to_array(), env, kont, handlers)
            _ =>
              if cond_expand_feature(head) {
                eval_sequence_state(rest.to_array(), env, kont, handlers)
              } else {
                MachineState::Apply(@core.Value::Void, kont, handlers)
              }
          }
      }
    }
  }
}

///|
fn eval_pair(
  expr : @core.Datum,
  env : @core.Env,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  let expr_unwrapped = datum_unwrap_syntax(expr)
  match datum_list_to_array(expr_unwrapped) {
    [] => MachineState::Apply(@core.Value::Datum(Nil), kont, handlers)
    [head, ..rest] => {
      let args = rest.to_array()
      match @runtime.symbol_name(datum_unwrap_syntax(head)) {
        Some("define-syntax") =>
          MachineState::Apply(eval_define_syntax(args, env), kont, handlers)
        Some("library") => eval_library_state(args, env, kont, handlers)
        Some("import") => eval_import_state(args, env, kont, handlers)
        Some("include") => eval_include_state(args, env, kont, handlers, false)
        Some("include-ci") => eval_include_state(args, env, kont, handlers, true)
        Some("include-library-declarations") =>
          eval_include_state(args, env, kont, handlers, false)
        Some("define-record-type") =>
          eval_define_record_type_state(args, env, kont, handlers)
        Some("define-condition-type") =>
          eval_define_condition_type_state(args, env, kont, handlers)
        Some("define-enumeration") =>
          eval_define_enumeration_state(args, env, kont, handlers)
        Some("quote") => MachineState::Apply(eval_quote(args), kont, handlers)
        Some("quasiquote") =>
          MachineState::Apply(eval_quasiquote(args, env), kont, handlers)
        Some("syntax") =>
          MachineState::Apply(eval_syntax(args, env), kont, handlers)
        Some("quasisyntax") =>
          MachineState::Apply(eval_quasisyntax(args, env), kont, handlers)
        Some("syntax-case") => eval_syntax_case_state(args, env, kont, handlers)
        Some("delay") =>
          MachineState::Apply(eval_delay(args, env), kont, handlers)
        Some("if") => eval_if_state(args, env, kont, handlers)
        Some("begin") => eval_sequence_state(args, env, kont, handlers)
        Some("begin-for-syntax") =>
          eval_begin_for_syntax_state(args, env, kont, handlers)
        Some("lambda") =>
          MachineState::Apply(eval_lambda(args, env), kont, handlers)
        Some("case-lambda") =>
          MachineState::Apply(eval_case_lambda(args, env), kont, handlers)
        Some("define") => eval_define_state(args, env, kont, handlers)
        Some("define-values") =>
          eval_define_values_state(args, env, kont, handlers)
        Some("set!") => eval_set_state(args, env, kont, handlers)
        Some("let") => eval_let_state(args, env, kont, handlers)
        Some("let-values") => eval_let_values_state(args, env, kont, handlers)
        Some("let*-values") =>
          eval_let_star_values_state(args, env, kont, handlers)
        Some("let*") => eval_let_star_state(args, env, kont, handlers)
        Some("letrec") => eval_letrec_state(args, env, kont, handlers)
        Some("letrec*") => eval_letrec_star_state(args, env, kont, handlers)
        Some("parameterize") => eval_parameterize_state(args, env, kont, handlers)
        Some("let-syntax") => eval_let_syntax_state(args, env, kont, handlers)
        Some("letrec-syntax") =>
          eval_letrec_syntax_state(args, env, kont, handlers)
        Some("and") => eval_and_state(args, env, kont, handlers)
        Some("or") => eval_or_state(args, env, kont, handlers)
        Some("cond") => cond_start(args, env, kont, handlers)
        Some("cond-expand") => eval_cond_expand_state(args, env, kont, handlers)
        Some("guard") => eval_guard_state(args, env, kont, handlers)
        Some("do") => eval_do_state(args, env, kont, handlers)
        Some("case") => eval_case_state(args, env, kont, handlers)
        Some(name) =>
          match @runtime.env_get_macro(env, name) {
            Some(transformer) => {
              let expansion = expand_macro(transformer, expr_unwrapped, env)
              let next_env = env_with_captures(env, expansion.captures)
              MachineState::Eval(expansion.datum, next_env, kont, handlers)
            }
            None =>
              MachineState::Eval(
                head,
                env,
                @core.Kont::AppFun(args, env, kont),
                handlers,
              )
          }
        None =>
          MachineState::Eval(
            head,
            env,
            @core.Kont::AppFun(args, env, kont),
            handlers,
          )
      }
    }
  }
}

///|
fn map_step(
  proc : @core.Value,
  lists : Array[@core.Datum],
  results : Array[@core.Datum],
  for_each : Bool,
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  if lists.is_empty() {
    raise @core.EvalError("arity mismatch: expected at least 1 list")
  }
  let args : Array[@core.Value] = []
  let next_lists : Array[@core.Datum] = []
  for list in lists {
    match list {
      Nil => {
        if for_each {
          return MachineState::Apply(@core.Value::Void, kont, handlers)
        }
        return MachineState::Apply(
          @core.Value::Datum(list_from_array(results)),
          kont,
          handlers,
        )
      }
      Pair(car, cdr) => {
        args.push(value_from_datum(car.val))
        next_lists.push(cdr.val)
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  }
  apply_proc(
    proc,
    args,
    @core.Kont::MapStep(proc, next_lists, results, for_each, kont),
    handlers,
  )
}

///|
fn string_to_char_list(s : String) -> @core.Datum raise @core.EvalError {
  let items : Array[@core.Datum] = []
  for i = 0; i < s.length(); {
    // invariant : i >= 0 && i <= s.length()
    // decreases : s.length() - i
    // assert : i <= s.length()
    match s.get_char(i) {
      Some(ch) => items.push(Char(ch))
      None => raise @core.EvalError("index out of range")
    }
    continue i + 1
  }
  list_from_array(items)
}

///|
fn apply_proc(
  proc : @core.Value,
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match proc {
    Primitive(@core.Primitive::Apply) => apply_apply(args, kont, handlers)
    Primitive(@core.Primitive::CallWithValues) =>
      match args {
        [producer, consumer] =>
          apply_proc(
            producer,
            [],
            @core.Kont::CallWithValues(consumer, kont),
            handlers,
          )
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::RecordConstructor) =>
      match args {
        [desc_value] => {
          let desc = value_as_record_constructor_descriptor(desc_value)
          record_constructor_from_descriptor(desc, kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::HashtableRef) =>
      apply_hashtable_ref(args, kont, handlers)
    Primitive(@core.Primitive::HashtableSet) =>
      apply_hashtable_set(args, kont, handlers)
    Primitive(@core.Primitive::HashtableDelete) =>
      apply_hashtable_delete(args, kont, handlers)
    Primitive(@core.Primitive::HashtableContainsP) =>
      apply_hashtable_contains(args, kont, handlers)
    Primitive(@core.Primitive::HashtableUpdate) =>
      apply_hashtable_update(args, kont, handlers)
    Primitive(@core.Primitive::MakeParameter) =>
      match args {
        [value] =>
          MachineState::Apply(
            @core.Value::Parameter(make_parameter(value)),
            kont,
            handlers,
          )
        [value, converter] =>
          apply_proc(
            converter,
            [value],
            @core.Kont::MakeParameter(converter, kont),
            handlers,
          )
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Primitive(@core.Primitive::DynamicWind) =>
      match args {
        [before, thunk, after] => {
          let winder = make_proc_winder(before, after)
          apply_winder_before(
            winder,
            @core.Kont::WindEnter(thunk, winder, kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive(@core.Primitive::Eval) =>
      match args {
        [expr_value, env_value] => {
          let expr = value_as_datum(expr_value)
          let eval_env = value_as_eval_env(env_value)
          MachineState::Eval(expr, eval_env.env, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::Environment) => {
      match args {
        [] => raise arity_mismatch(1, 0)
        _ => {
          let env = @runtime.env_new()
          for arg in args {
            let spec = value_as_datum(arg)
            import_set_into_env(spec, env)
          }
          MachineState::Apply(@core.Value::EvalEnv(make_eval_env(env)), kont, handlers)
        }
      }
    }
    Primitive(@core.Primitive::MakePromise) =>
      match args {
        [thunk] => {
          if !@runtime.is_procedure_value(thunk) {
            raise @core.EvalError("type error: procedure expected")
          }
          MachineState::Apply(@core.Value::Promise(make_promise(thunk)), kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::Force) =>
      match args {
        [promise_value] =>
          match promise_value {
            Promise(promise) =>
              match promise.state.val {
                @core.PromiseState::Value(result) =>
                  MachineState::Apply(result, kont, handlers)
                @core.PromiseState::Thunk(thunk) =>
                  apply_proc(
                    thunk,
                    [],
                    @core.Kont::ForcePromise(promise, kont),
                    handlers,
                  )
              }
            _ => raise @core.EvalError("type error: promise expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::WithExceptionHandler) =>
      match args {
        [handler, thunk] => {
          let next_handlers : Array[@core.Value] = []
          for item in handlers {
            next_handlers.push(item)
          }
          next_handlers.push(handler)
          let restore = @core.Kont::RestoreHandlers(handlers, kont)
          apply_proc(thunk, [], restore, next_handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::Error) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[@core.Record] = []
        components.push(make_condition_record("&error", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(@core.Primitive::AssertionViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[@core.Record] = []
        components.push(make_condition_record("&assertion", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(@core.Primitive::ImplementationRestrictionViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[@core.Record] = []
        components.push(
          make_condition_record("&implementation-restriction", []),
        )
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(@core.Primitive::UndefinedViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[@core.Record] = []
        components.push(make_condition_record("&undefined", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(@core.Primitive::SyntaxViolation) =>
      if args.length() < 3 {
        raise arity_mismatch(3, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let form = args[2]
        let subform = if args.length() >= 4 {
          args[3]
        } else {
          @core.Value::Datum(Bool(false))
        }
        let irritants = if args.length() >= 4 {
          args.sub(start=4).to_array()
        } else {
          []
        }
        let components : Array[@core.Record] = []
        components.push(make_condition_record("&syntax", [form, subform]))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(@core.Primitive::Raise) =>
      match args {
        [condition] =>
          match handlers {
            [] => raise @core.EvalError("uncaught exception")
            [.., handler] =>
              match handler {
                GuardHandler(guard_handler) =>
                  handle_guard(guard_handler, condition, kont, false)
                _ =>
                  apply_proc(handler, [condition], @core.Kont::RaiseNonCont, handlers)
              }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::RaiseContinuable) =>
      match args {
        [condition] =>
          match handlers {
            [] => raise @core.EvalError("uncaught exception")
            [.., handler] =>
              match handler {
                GuardHandler(guard_handler) =>
                  handle_guard(guard_handler, condition, kont, true)
                _ =>
                  apply_proc(handler, [condition], @core.Kont::RaiseCont(kont), handlers)
              }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::CallCC) =>
      match args {
        [proc_value] => {
          let cont = capture_continuation(kont, handlers)
          apply_proc(proc_value, [@core.Value::Continuation(cont)], kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(@core.Primitive::Map) =>
      match args {
        [proc_value, first_list, ..rest] => {
          let lists : Array[@core.Datum] = []
          lists.push(value_as_datum(first_list))
          for item in rest {
            lists.push(value_as_datum(item))
          }
          map_step(proc_value, lists, [], false, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::ForEach) =>
      match args {
        [proc_value, first_list, ..rest] => {
          let lists : Array[@core.Datum] = []
          lists.push(value_as_datum(first_list))
          for item in rest {
            lists.push(value_as_datum(item))
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::VectorMap) =>
      match args {
        [proc_value, first_vector, ..rest] => {
          let vectors : Array[Array[@core.Datum]] = []
          vectors.push(value_as_vector(first_vector))
          for item in rest {
            vectors.push(value_as_vector(item))
          }
          let len = vectors[0].length()
          for j = 0; j < vectors.length(); {
            // invariant : j >= 0 && j <= vectors.length()
            // decreases : vectors.length() - j
            // assert : j <= vectors.length()
            if vectors[j].length() != len {
              raise @core.EvalError("vector length mismatch")
            }
            continue j + 1
          }
          let lists : Array[@core.Datum] = []
          for k = 0; k < vectors.length(); {
            // invariant : k >= 0 && k <= vectors.length()
            // decreases : vectors.length() - k
            // assert : k <= vectors.length()
            lists.push(list_from_array(vectors[k]))
            continue k + 1
          }
          map_step(
            proc_value,
            lists,
            [],
            false,
            @core.Kont::VectorMapFinalize(kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::VectorForEach) =>
      match args {
        [proc_value, first_vector, ..rest] => {
          let vectors : Array[Array[@core.Datum]] = []
          vectors.push(value_as_vector(first_vector))
          for item in rest {
            vectors.push(value_as_vector(item))
          }
          let len = vectors[0].length()
          for j = 0; j < vectors.length(); {
            // invariant : j >= 0 && j <= vectors.length()
            // decreases : vectors.length() - j
            // assert : j <= vectors.length()
            if vectors[j].length() != len {
              raise @core.EvalError("vector length mismatch")
            }
            continue j + 1
          }
          let lists : Array[@core.Datum] = []
          for k = 0; k < vectors.length(); {
            // invariant : k >= 0 && k <= vectors.length()
            // decreases : vectors.length() - k
            // assert : k <= vectors.length()
            lists.push(list_from_array(vectors[k]))
            continue k + 1
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::StringMap) =>
      match args {
        [proc_value, first_value, ..rest] => {
          let first = value_as_string(first_value)
          let len = first.length()
          let lists : Array[@core.Datum] = []
          lists.push(string_to_char_list(first))
          for item in rest {
            let s = value_as_string(item)
            if s.length() != len {
              raise @core.EvalError("string length mismatch")
            }
            lists.push(string_to_char_list(s))
          }
          map_step(
            proc_value,
            lists,
            [],
            false,
            @core.Kont::StringMapFinalize(kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(@core.Primitive::StringForEach) =>
      match args {
        [proc_value, first_value, ..rest] => {
          let first = value_as_string(first_value)
          let len = first.length()
          let lists : Array[@core.Datum] = []
          lists.push(string_to_char_list(first))
          for item in rest {
            let s = value_as_string(item)
            if s.length() != len {
              raise @core.EvalError("string length mismatch")
            }
            lists.push(string_to_char_list(s))
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(prim) =>
      MachineState::Apply(apply_primitive(prim, args), kont, handlers)
    Closure(cl) => {
      let formals =@core.Formals::{ params: cl.params, rest: cl.rest }
      let bindings = formals_bindings(formals, args)
      let new_env = @runtime.env_extend(cl.env)
      for binding in bindings {
        let (name, bound_value) = binding
        @runtime.env_define(new_env, name, bound_value)
      }
      eval_sequence_state(cl.body, new_env, kont, handlers)
    }
    CaseClosure(cl) => {
      let matched =
        for i = 0; i < cl.clauses.length(); {
          // invariant : i >= 0 && i <= cl.clauses.length()
          // decreases : cl.clauses.length() - i
          // assert : i <= cl.clauses.length()
          let clause = cl.clauses[i]
          let clause_formals =@core.Formals::{
            params: clause.params,
            rest: clause.rest,
          }
          if formals_accept_len(clause_formals, args.length()) {
            break Some((clause_formals, clause.body))
          }
          continue i + 1
        } else {
          None
        }
      match matched {
        None => {
          if cl.clauses.length() == 1 {
            let clause = cl.clauses[0]
            match clause.rest {
              None =>
                raise arity_mismatch(clause.params.length(), args.length())
              Some(_) =>
                raise arity_mismatch_at_least(
                  clause.params.length(),
                  args.length(),
                )
            }
          }
          let expected_parts : Array[String] = []
          for clause in cl.clauses {
            let clause_formals =@core.Formals::{
              params: clause.params,
              rest: clause.rest,
            }
            expected_parts.push(formals_expected_desc(clause_formals))
          }
          let expected = expected_parts.join(", ")
          raise @core.EvalError(
            "arity mismatch: expected one of \{expected} got \{args.length()}",
          )
        }
        Some((matched_formals, matched_body)) => {
          let new_env = @runtime.env_extend(cl.env)
          let bindings = formals_bindings(matched_formals, args)
          for binding in bindings {
            let (name, bound_value) = binding
            @runtime.env_define(new_env, name, bound_value)
          }
          eval_sequence_state(matched_body, new_env, kont, handlers)
        }
      }
    }
    Parameter(param) =>
      match args {
        [] => MachineState::Apply(parameter_get(param), kont, handlers)
        [value] =>
          match param.converter {
            Some(converter) =>
              apply_proc(
                converter,
                [value],
                @core.Kont::SetParameter(param, kont),
                handlers,
              )
            None => {
              parameter_set(param, value)
              MachineState::Apply(@core.Value::Void, kont, handlers)
            }
          }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 0 or 1 got \{args.length()}",
          )
      }
    Continuation(cont) => {
      let resume_value = if args.length() == 1 {
        args[0]
      } else {
        @core.Value::Values(args)
      }
      let current_winds = wind_stack.val
      let target_winds = cont.winds
      let actions : Array[@core.WindAction] = []
      let prefix = common_wind_prefix(current_winds, target_winds)
      for i = current_winds.length(); i > prefix; {
        // invariant : i >= prefix && i <= current_winds.length()
        // decreases : i - prefix
        // assert : i <= current_winds.length()
        let idx = i - 1
        actions.push(After(current_winds[idx]))
        continue i - 1
      }
      actions.push(SwitchHandlers(cont.handlers))
      for i = prefix; i < target_winds.length(); {
        // invariant : i >= prefix && i <= target_winds.length()
        // decreases : target_winds.length() - i
        // assert : i <= target_winds.length()
        actions.push(Before(target_winds[i]))
        continue i + 1
      }
      run_wind_actions(
        actions,
        resume_value,
        cont.kont,
        cont.handlers,
        handlers,
      )
    }
    RecordProc(proc) => apply_record_proc(proc, args, kont, handlers)
    ConditionProc(proc) =>
      apply_condition_proc(proc, args, kont, handlers)
    EnumSetProc(proc) => apply_enum_set_proc(proc, args, kont, handlers)
    _ => raise @core.EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(
  args : Array[@core.Value],
  kont : @core.Kont,
  handlers : Array[@core.Value],
) -> MachineState raise @core.EvalError {
  match args {
    [] | [_] => raise arity_mismatch(2, args.length())
    [proc, ..rest] => {
      match rest {
        [] => raise arity_mismatch(2, args.length())
        [..prefix, last] => {
          let flat : Array[@core.Value] = []
          for item in prefix {
            flat.push(item)
          }
          let list_value = value_as_datum(last)
          let tail = datum_list_to_value_array(list_value)
          flat.append(tail[:])
          apply_proc(proc, flat, kont, handlers)
        }
      }
    }
  }
}

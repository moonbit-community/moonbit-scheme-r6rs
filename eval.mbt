///|
fn init_env() -> Env {
  let env = env_new()
  env_define(env, "+", Value::Primitive(Primitive::Add))
  env_define(env, "-", Value::Primitive(Primitive::Sub))
  env_define(env, "*", Value::Primitive(Primitive::Mul))
  env_define(env, "/", Value::Primitive(Primitive::Div))
  env_define(env, "=", Value::Primitive(Primitive::NumEq))
  env_define(env, "<", Value::Primitive(Primitive::Less))
  env_define(env, ">", Value::Primitive(Primitive::Greater))
  env_define(env, "<=", Value::Primitive(Primitive::LessEq))
  env_define(env, ">=", Value::Primitive(Primitive::GreaterEq))
  env_define(env, "cons", Value::Primitive(Primitive::Cons))
  env_define(env, "car", Value::Primitive(Primitive::Car))
  env_define(env, "cdr", Value::Primitive(Primitive::Cdr))
  env_define(env, "list", Value::Primitive(Primitive::List))
  env_define(env, "null?", Value::Primitive(Primitive::NullP))
  env_define(env, "pair?", Value::Primitive(Primitive::PairP))
  env_define(env, "symbol?", Value::Primitive(Primitive::SymbolP))
  env_define(env, "boolean?", Value::Primitive(Primitive::BooleanP))
  env_define(env, "number?", Value::Primitive(Primitive::NumberP))
  env_define(env, "integer?", Value::Primitive(Primitive::IntegerP))
  env_define(env, "rational?", Value::Primitive(Primitive::RationalP))
  env_define(env, "real?", Value::Primitive(Primitive::RealP))
  env_define(env, "exact?", Value::Primitive(Primitive::ExactP))
  env_define(env, "inexact?", Value::Primitive(Primitive::InexactP))
  env_define(env, "procedure?", Value::Primitive(Primitive::ProcedureP))
  env_define(env, "not", Value::Primitive(Primitive::Not))
  env_define(env, "apply", Value::Primitive(Primitive::Apply))
  env_define(env, "exact->inexact", Value::Primitive(Primitive::ExactToInexact))
  env_define(env, "inexact->exact", Value::Primitive(Primitive::InexactToExact))
  env_define(env, "numerator", Value::Primitive(Primitive::Numerator))
  env_define(env, "denominator", Value::Primitive(Primitive::Denominator))
  env_define(env, "eq?", Value::Primitive(Primitive::Eq))
  env_define(env, "eqv?", Value::Primitive(Primitive::Eqv))
  env_define(env, "equal?", Value::Primitive(Primitive::Equal))
  env_define(env, "list?", Value::Primitive(Primitive::ListP))
  env_define(env, "length", Value::Primitive(Primitive::Length))
  env_define(env, "append", Value::Primitive(Primitive::Append))
  env_define(env, "reverse", Value::Primitive(Primitive::Reverse))
  env_define(env, "char?", Value::Primitive(Primitive::CharP))
  env_define(env, "string?", Value::Primitive(Primitive::StringP))
  env_define(env, "string-length", Value::Primitive(Primitive::StringLength))
  env_define(env, "string-append", Value::Primitive(Primitive::StringAppend))
  env_define(env, "string-ref", Value::Primitive(Primitive::StringRef))
  env_define(env, "vector", Value::Primitive(Primitive::Vector))
  env_define(env, "vector?", Value::Primitive(Primitive::VectorP))
  env_define(env, "vector-length", Value::Primitive(Primitive::VectorLength))
  env_define(env, "vector-ref", Value::Primitive(Primitive::VectorRef))
  env_define(env, "vector-set!", Value::Primitive(Primitive::VectorSet))
  env_define(env, "bytevector", Value::Primitive(Primitive::ByteVector))
  env_define(env, "bytevector?", Value::Primitive(Primitive::ByteVectorP))
  env_define(env, "bytevector-length", Value::Primitive(Primitive::ByteVectorLength))
  env_define(env, "bytevector-u8-ref", Value::Primitive(Primitive::ByteVectorU8Ref))
  env_define(env, "bytevector-u8-set!", Value::Primitive(Primitive::ByteVectorU8Set))
  env_define(env, "display", Value::Primitive(Primitive::Display))
  env_define(env, "write", Value::Primitive(Primitive::Write))
  env_define(env, "newline", Value::Primitive(Primitive::Newline))
  env_define(env, "open-output-string", Value::Primitive(Primitive::OpenOutputString))
  env_define(env, "get-output-string", Value::Primitive(Primitive::GetOutputString))
  env_define(env, "current-output-port", Value::Primitive(Primitive::CurrentOutputPort))
  env_define(
    env,
    "with-exception-handler",
    Value::Primitive(Primitive::WithExceptionHandler),
  )
  env_define(env, "raise", Value::Primitive(Primitive::Raise))
  env_define(env, "raise-continuable", Value::Primitive(Primitive::RaiseContinuable))
  env_define(env, "call/cc", Value::Primitive(Primitive::CallCC))
  env_define(
    env,
    "call-with-current-continuation",
    Value::Primitive(Primitive::CallCC),
  )
  env
}

///|
let closure_counter : Ref[Int] = Ref::new(0)

///|
fn next_closure_id() -> Int {
  let id = closure_counter.val
  closure_counter.val = id + 1
  id
}

///|
fn make_closure(params : Array[String], body : Array[Datum], env : Env) -> Closure {
  { id: next_closure_id(), params, body, env }
}

///|
fn make_case_closure(clauses : Array[CaseClause], env : Env) -> CaseClosure {
  { id: next_closure_id(), clauses, env }
}

///|
priv enum Kont {
  Halt
  If(Datum, Datum, Env, Kont)
  Begin(Array[Datum], Env, Kont)
  Set(String, Env, Kont)
  Define(String, Env, Kont)
  AppFun(Array[Datum], Env, Kont)
  AppArgs(Value, Array[Value], Array[Datum], Env, Kont)
  And(Array[Datum], Env, Kont)
  Or(Array[Datum], Env, Kont)
  Cond(Array[Datum], Array[Datum], Env, Kont)
  CondArrow(Value, Kont)
  Let(Array[(String, Datum)], Int, Array[Value], Env, Array[Datum], Kont)
  LetStar(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
  LetRec(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
  Case(Array[Datum], Env, Kont)
  CaseArrow(Value, Kont)
  RaiseNonCont
  RaiseCont(Kont)
  RestoreHandlers(Array[Value], Kont)
}

///|
struct Continuation {
  id : Int
  kont : Kont
  handlers : Array[Value]
}

///|
priv enum MachineState {
  Eval(Datum, Env, Kont, Array[Value])
  Apply(Value, Kont, Array[Value])
}

///|
let continuation_counter : Ref[Int] = Ref::new(0)

///|
fn next_continuation_id() -> Int {
  let id = continuation_counter.val
  continuation_counter.val = id + 1
  id
}

///|
let do_counter : Ref[Int] = Ref::new(0)

///|
fn next_do_name() -> String {
  let id = do_counter.val
  do_counter.val = id + 1
  "__do_loop__\{id}"
}

///|
pub fn eval_program(src : String) -> Value raise {
  let results = eval_program_all(src)
  if results.is_empty() {
    Value::Void
  } else {
    results[results.length() - 1]
  }
}

///|
pub fn eval_program_all(src : String) -> Array[Value] raise {
  let forms = parse_program(src)
  reset_library_registry()
  set_current_output_port(new_output_string_port())
  let env = init_env()
  let results : Array[Value] = []
  for form in forms {
    let value = eval_expr(form, env)
    results.push(value)
  }
  results
}

///|
fn eval_expr(expr : Datum, env : Env) -> Value raise EvalError {
  let handlers : Array[Value] = []
  let mut state = MachineState::Eval(expr, env, Kont::Halt, handlers)
  while true {
    match state {
      MachineState::Eval(current_expr, current_env, kont, handlers) => {
        match current_expr {
          Datum::Nil
          | Datum::Bool(_)
          | Datum::Int(_)
          | Datum::Rat(_, _)
          | Datum::Float(_)
          | Datum::Char(_)
          | Datum::String(_)
          | Datum::Vector(_)
          | Datum::ByteVector(_) =>
            state = MachineState::Apply(Value::Datum(current_expr), kont, handlers)
          Datum::Symbol(name) => {
            match env_lookup(current_env, name) {
              Value::Macro(_) => raise EvalError("macro used as value")
              value => state = MachineState::Apply(value, kont, handlers)
            }
          }
          Datum::Pair(_, _) => state = eval_pair(current_expr, current_env, kont, handlers)
        }
      }
      MachineState::Apply(value, kont, handlers) => {
        match kont {
          Kont::Halt => return value
          Kont::If(then_expr, else_expr, env, next) => {
            if is_false(value) {
              state = MachineState::Eval(else_expr, env, next, handlers)
            } else {
              state = MachineState::Eval(then_expr, env, next, handlers)
            }
          }
          Kont::Begin(rest, env, next) => {
            if rest.is_empty() {
              state = MachineState::Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Begin(tail, env, next), handlers)
            }
          }
          Kont::Set(name, env, next) => {
            env_set(env, name, value)
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::Define(name, env, next) => {
            env_define(env, name, value)
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::AppFun(args, env, next) => {
            if args.is_empty() {
              state = apply_proc(value, [], next, handlers)
            } else {
              let rest = args.sub(start=1).to_array()
              state = MachineState::Eval(
                args[0],
                env,
                Kont::AppArgs(value, [], rest, env, next),
                handlers,
              )
            }
          }
          Kont::AppArgs(proc, evaluated, remaining, env, next) => {
            let next_values : Array[Value] = []
            for item in evaluated {
              next_values.push(item)
            }
            next_values.push(value)
            if remaining.is_empty() {
              state = apply_proc(proc, next_values, next, handlers)
            } else {
              let rest = remaining.sub(start=1).to_array()
              state = MachineState::Eval(
                remaining[0],
                env,
                Kont::AppArgs(proc, next_values, rest, env, next),
                handlers,
              )
            }
          }
          Kont::And(rest, env, next) => {
            if is_false(value) {
              state = MachineState::Apply(value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::And(tail, env, next), handlers)
            }
          }
          Kont::Or(rest, env, next) => {
            if !is_false(value) {
              state = MachineState::Apply(value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Or(tail, env, next), handlers)
            }
          }
          Kont::Cond(parts, rest, env, next) => {
            if is_false(value) {
              state = cond_start(rest, env, next, handlers)
            } else if parts.length() == 1 {
              state = MachineState::Apply(value, next, handlers)
            } else if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
              if parts.length() != 3 {
                raise EvalError("invalid cond")
              }
              state = MachineState::Eval(parts[2], env, Kont::CondArrow(value, next), handlers)
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          Kont::CondArrow(test_value, next) => {
            state = apply_proc(value, [test_value], next, handlers)
          }
          Kont::Let(bindings, index, values, env, body, next) => {
            let next_values : Array[Value] = []
            for item in values {
              next_values.push(item)
            }
            next_values.push(value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::Let(bindings, next_index, next_values, env, body, next),
                handlers,
              )
            } else {
              let new_env = env_extend(env)
              let mut i = 0
              while i < bindings.length() {
                let (name, _) = bindings[i]
                env_define(new_env, name, next_values[i])
                i = i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetStar(bindings, index, env, body, next) => {
            let (name, _) = bindings[index]
            let new_env = env_extend(env)
            env_define(new_env, name, value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                Kont::LetStar(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetRec(bindings, index, env, body, next) => {
            let (name, _) = bindings[index]
            env_set(env, name, value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::LetRec(bindings, next_index, env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          Kont::Case(clauses, env, next) => {
            state = case_dispatch(value, clauses, env, next, handlers)
          }
          Kont::CaseArrow(test_value, next) => {
            state = apply_proc(value, [test_value], next, handlers)
          }
          Kont::RaiseNonCont => raise EvalError("non-continuable exception")
          Kont::RaiseCont(next) => state = MachineState::Apply(value, next, handlers)
          Kont::RestoreHandlers(prev_handlers, next) =>
            state = MachineState::Apply(value, next, prev_handlers)
        }
      }
    }
  }
  Value::Void
}

///|
///|
fn eval_quote(args : Array[Datum]) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(args[0])
    n => raise arity_mismatch(1, n)
  }
}

///|
fn eval_quasiquote(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(quasiquote_expand(args[0], env, 1))
    n => raise arity_mismatch(1, n)
  }
}

///|
fn unwrap_form(expr : Datum, name : String) -> Datum? {
  match expr {
    Datum::Pair(Datum::Symbol(head), Datum::Pair(arg, Datum::Nil)) if head == name => Some(arg)
    _ => None
  }
}

///|
fn quasiquote_expand(expr : Datum, env : Env, depth : Int) -> Datum raise EvalError {
  match unwrap_form(expr, "quasiquote") {
    Some(arg) =>
      return list_from_array([Datum::Symbol("quasiquote"), quasiquote_expand(arg, env, depth + 1)])
    None => ()
  }
  match unwrap_form(expr, "unquote") {
    Some(arg) => {
      if depth == 1 {
        return value_as_datum(eval_expr(arg, env))
      } else {
        return list_from_array([Datum::Symbol("unquote"), quasiquote_expand(arg, env, depth - 1)])
      }
    }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) => {
      if depth == 1 {
        raise EvalError("unquote-splicing not in list context")
      } else {
        return list_from_array([
          Datum::Symbol("unquote-splicing"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    }
    None => ()
  }
  match expr {
    Datum::Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car, "unquote-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasiquote_expand(cdr, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      Datum::Pair(
        quasiquote_expand(car, env, depth),
        quasiquote_expand(cdr, env, depth),
      )
    }
    _ => expr
  }
}

///|
fn list_with_tail(items : Array[Datum], tail : Datum) -> Datum {
  let mut result = tail
  for item in items.rev_iter() {
    result = Datum::Pair(item, result)
  }
  result
}
///|
fn parse_params(expr : Datum) -> Array[String] raise EvalError {
  let params = datum_list_to_array(expr)
  let names : Array[String] = []
  for param in params {
    match param {
      Datum::Symbol(name) => names.push(name)
      _ => raise EvalError("type error: symbol expected")
    }
  }
  names
}

///|
fn eval_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let params = parse_params(args[0])
    let body = args.sub(start=1).to_array()
    Value::Closure(make_closure(params, body, env))
  }
}

///|
fn eval_case_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.is_empty() {
    raise arity_mismatch(1, 0)
  }
  let clauses : Array[CaseClause] = []
  for clause in args {
    let parts = datum_list_to_array(clause)
    if parts.length() < 2 {
      raise EvalError("invalid case-lambda")
    }
    let params = parse_params(parts[0])
    let body = parts.sub(start=1).to_array()
    clauses.push(CaseClause::{ params, body })
  }
  Value::CaseClosure(make_case_closure(clauses, env))
}

///|
fn eval_define_syntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => {
          let rules = parse_syntax_rules(args[1])
          env_define(env, name, Value::Macro(rules))
          Value::Void
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn parse_syntax_bindings(expr : Datum) -> Array[(String, SyntaxRules)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, SyntaxRules)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: syntax binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => {
          let rules = parse_syntax_rules(parts[1])
          parsed.push((name, rules))
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_bindings(expr : Datum) -> Array[(String, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => parsed.push((name, parts[1]))
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_symbol(expr : Datum) -> String raise EvalError {
  match expr {
    Datum::Symbol(name) => name
    _ => raise EvalError("type error: symbol expected")
  }
}

///|
fn parse_symbol_list(items : Array[Datum]) -> Array[String] raise EvalError {
  let names : Array[String] = []
  for item in items {
    names.push(parse_symbol(item))
  }
  names
}

///|
fn eval_sequence_state(
  exprs : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if exprs.is_empty() {
    MachineState::Apply(Value::Void, kont, handlers)
  } else {
    let rest = exprs.sub(start=1).to_array()
    MachineState::Eval(exprs[0], env, Kont::Begin(rest, env, kont), handlers)
  }
}

///|
fn eval_if_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  } else {
    MachineState::Eval(args[0], env, Kont::If(args[1], args[2], env, kont), handlers)
  }
}

///|
fn eval_define_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
        match args.length() {
          2 => MachineState::Eval(args[1], env, Kont::Define(name, env, kont), handlers)
          n => raise arity_mismatch(2, n)
        }
      }
      Datum::Pair(_, _) => {
        let sig = datum_list_to_array(args[0])
        if sig.is_empty() {
          raise EvalError("type error: symbol expected")
        } else {
          match sig[0] {
            Datum::Symbol(name) => {
              let params = parse_params(list_from_array(sig.sub(start=1).to_array()))
              let body = args.sub(start=1).to_array()
              env_define(env, name, Value::Closure(make_closure(params, body, env)))
              MachineState::Apply(Value::Void, kont, handlers)
            }
            _ => raise EvalError("type error: symbol expected")
          }
        }
      }
      _ => raise EvalError("type error: symbol expected")
    }
  }
}

///|
fn eval_set_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) =>
          MachineState::Eval(args[1], env, Kont::Set(name, env, kont), handlers)
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn eval_let_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
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
          MachineState::Eval(expr, env, Kont::Let(bindings, 0, [], env, body, kont), handlers)
        }
      }
    }
  }
}

///|
fn eval_let_star_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::LetStar(bindings, 0, env, body, kont), handlers)
    }
  }
}

///|
fn eval_letrec_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, _) = binding
      env_define(new_env, name, Value::Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        new_env,
        Kont::LetRec(bindings, 0, new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn expand_named_let(name : String, bindings : Array[(String, Datum)], body : Array[Datum]) -> Datum {
  let param_items : Array[Datum] = []
  let arg_items : Array[Datum] = []
  for binding in bindings {
    let (param, expr) = binding
    param_items.push(Datum::Symbol(param))
    arg_items.push(expr)
  }
  let lambda_items : Array[Datum] = []
  lambda_items.push(Datum::Symbol("lambda"))
  lambda_items.push(list_from_array(param_items))
  for expr in body {
    lambda_items.push(expr)
  }
  let lambda_expr = list_from_array(lambda_items)
  let binding_expr = list_from_array([Datum::Symbol(name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let call_items : Array[Datum] = []
  call_items.push(Datum::Symbol(name))
  for arg in arg_items {
    call_items.push(arg)
  }
  let call_expr = list_from_array(call_items)
  list_from_array([Datum::Symbol("letrec"), bindings_expr, call_expr])
}

///|
fn eval_let_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn eval_letrec_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn build_begin(exprs : Array[Datum]) -> Datum {
  let items : Array[Datum] = []
  items.push(Datum::Symbol("begin"))
  for expr in exprs {
    items.push(expr)
  }
  list_from_array(items)
}

///|
fn parse_do_bindings(expr : Datum) -> (Array[String], Array[Datum], Array[Datum]) raise EvalError {
  let bindings = datum_list_to_array(expr)
  let names : Array[String] = []
  let inits : Array[Datum] = []
  let steps : Array[Datum] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() < 2 || parts.length() > 3 {
      raise EvalError("invalid do")
    }
    let name = parse_symbol(parts[0])
    names.push(name)
    inits.push(parts[1])
    if parts.length() == 3 {
      steps.push(parts[2])
    } else {
      steps.push(Datum::Symbol(name))
    }
  }
  (names, inits, steps)
}

///|
fn eval_do_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let (names, inits, steps) = parse_do_bindings(args[0])
  let test_parts = datum_list_to_array(args[1])
  if test_parts.is_empty() {
    raise EvalError("invalid do")
  }
  let test_expr = test_parts[0]
  let result_exprs = test_parts.sub(start=1).to_array()
  let command_exprs = args.sub(start=2).to_array()
  let loop_name = next_do_name()
  let params : Array[Datum] = []
  for name in names {
    params.push(Datum::Symbol(name))
  }
  let call_items : Array[Datum] = []
  call_items.push(Datum::Symbol(loop_name))
  for step in steps {
    call_items.push(step)
  }
  let call_expr = list_from_array(call_items)
  let else_body : Array[Datum] = []
  for expr in command_exprs {
    else_body.push(expr)
  }
  else_body.push(call_expr)
  let then_expr = build_begin(result_exprs)
  let else_expr = build_begin(else_body)
  let if_expr = list_from_array([
    Datum::Symbol("if"),
    test_expr,
    then_expr,
    else_expr,
  ])
  let lambda_expr = list_from_array([
    Datum::Symbol("lambda"),
    list_from_array(params),
    if_expr,
  ])
  let binding_expr = list_from_array([Datum::Symbol(loop_name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let init_call_items : Array[Datum] = []
  init_call_items.push(Datum::Symbol(loop_name))
  for init in inits {
    init_call_items.push(init)
  }
  let init_call_expr = list_from_array(init_call_items)
  let expanded = list_from_array([
    Datum::Symbol("letrec"),
    bindings_expr,
    init_call_expr,
  ])
  MachineState::Eval(expanded, env, kont, handlers)
}

///|
fn eval_define_record_type_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise EvalError("invalid define-record-type")
  }
  let record_name = parse_symbol(args[0])
  let ctor_parts = datum_list_to_array(args[1])
  if ctor_parts.is_empty() {
    raise EvalError("invalid define-record-type")
  }
  let ctor_name = parse_symbol(ctor_parts[0])
  let ctor_fields = parse_symbol_list(ctor_parts.sub(start=1).to_array())
  let pred_name = parse_symbol(args[2])
  let field_specs = args.sub(start=3).to_array()
  if field_specs.length() != ctor_fields.length() {
    raise EvalError("invalid define-record-type")
  }
  let accessors : Map[String, String] = {}
  let mutators : Map[String, String] = {}
  let mutable : Map[String, Bool] = {}
  for spec in field_specs {
    let parts = datum_list_to_array(spec)
    if parts.length() < 2 || parts.length() > 3 {
      raise EvalError("invalid define-record-type")
    }
    let field_name = parse_symbol(parts[0])
    if accessors.contains(field_name) {
      raise EvalError("invalid define-record-type")
    }
    let accessor = parse_symbol(parts[1])
    accessors[field_name] = accessor
    if parts.length() == 3 {
      let mutator = parse_symbol(parts[2])
      mutators[field_name] = mutator
      mutable[field_name] = true
    } else {
      mutable[field_name] = false
    }
  }
  let fields : Array[RecordField] = []
  let field_index : Map[String, Int] = {}
  let mut i = 0
  while i < ctor_fields.length() {
    let field_name = ctor_fields[i]
    match accessors.get(field_name) {
      Some(_) => ()
      None => raise EvalError("invalid define-record-type")
    }
    let is_mutable = match mutable.get(field_name) {
      Some(flag) => flag
      None => false
    }
    fields.push(RecordField::{ name: field_name, mutable: is_mutable })
    field_index[field_name] = i
    i = i + 1
  }
  let record_type = make_record_type(record_name, fields)
  env_define(
    env,
    ctor_name,
    Value::RecordProc(make_record_proc(RecordProcKind::Constructor(record_type))),
  )
  env_define(
    env,
    pred_name,
    Value::RecordProc(make_record_proc(RecordProcKind::Predicate(record_type))),
  )
  for field_name in ctor_fields {
    let idx = match field_index.get(field_name) {
      Some(value) => value
      None => raise EvalError("invalid define-record-type")
    }
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise EvalError("invalid define-record-type")
    }
    env_define(
      env,
      accessor_name,
      Value::RecordProc(make_record_proc(RecordProcKind::Accessor(record_type, idx))),
    )
    match mutators.get(field_name) {
      Some(mut_name) => {
        env_define(
          env,
          mut_name,
          Value::RecordProc(make_record_proc(RecordProcKind::Mutator(record_type, idx))),
        )
      }
      None => ()
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn value_as_record(value : Value) -> Record raise EvalError {
  match value {
    Value::Record(record) => record
    _ => raise EvalError("type error: record expected")
  }
}

///|
fn apply_record_proc(
  proc : RecordProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    RecordProcKind::Constructor(record_type) => {
      let field_count = record_type.fields.length()
      if args.length() != field_count {
        raise arity_mismatch(field_count, args.length())
      } else {
        let record = make_record_instance(record_type, args)
        MachineState::Apply(Value::Record(record), kont, handlers)
      }
    }
    RecordProcKind::Predicate(record_type) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let result = match args[0] {
          Value::Record(record) => record.record_type.id == record_type.id
          _ => false
        }
        MachineState::Apply(bool_value(result), kont, handlers)
      }
    }
    RecordProcKind::Accessor(record_type, idx) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let record = value_as_record(args[0])
        if record.record_type.id != record_type.id {
          raise EvalError("type error: record type mismatch: expected \{record_type.name}")
        }
        MachineState::Apply(record.fields[idx].val, kont, handlers)
      }
    }
    RecordProcKind::Mutator(record_type, idx) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let record = value_as_record(args[0])
        if record.record_type.id != record_type.id {
          raise EvalError("type error: record type mismatch: expected \{record_type.name}")
        }
        let field = record.record_type.fields[idx]
        if !field.mutable {
          raise EvalError("record field is immutable: \{field.name}")
        }
        record.fields[idx].val = args[1]
        MachineState::Apply(Value::Void, kont, handlers)
      }
    }
  }
}

///|
fn parse_library_name(expr : Datum) -> String raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise EvalError("invalid library name")
  }
  let names = parse_symbol_list(parts)
  names.join(".")
}

///|
fn parse_export_clause(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("export") => parse_symbol_list(items.sub(start=1).to_array())
    _ => raise EvalError("invalid library")
  }
}

///|
fn parse_import_clause(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("import") => {
      let names : Array[String] = []
      let mut i = 1
      while i < items.length() {
        names.push(parse_library_name(items[i]))
        i = i + 1
      }
      names
    }
    _ => raise EvalError("invalid library")
  }
}

///|
fn import_library_into_env(name : String, env : Env) -> Unit raise EvalError {
  match lookup_library(name) {
    Some(lib) => {
      for key in lib.exports.keys() {
        match lib.exports.get(key) {
          Some(value) => env_define(env, key, value)
          None => ()
        }
      }
    }
    None => raise EvalError("unknown library: \{name}")
  }
}

///|
fn eval_library_state(
  args : Array[Datum],
  _env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise EvalError("invalid library")
  }
  let name = parse_library_name(args[0])
  let exports = parse_export_clause(args[1])
  let imports = parse_import_clause(args[2])
  let body = args.sub(start=3).to_array()
  let lib_env = init_env()
  for lib_name in imports {
    import_library_into_env(lib_name, lib_env)
  }
  for form in body {
    let _ = eval_expr(form, lib_env)
  }
  let export_map : Map[String, Value] = {}
  for export_name in exports {
    export_map[export_name] = env_lookup(lib_env, export_name)
  }
  register_library(name, export_map)
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_import_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for spec in args {
    let name = parse_library_name(spec)
    import_library_into_env(name, env)
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_and_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(true), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::And(rest, env, kont), handlers)
  }
}

///|
fn eval_or_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(false), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::Or(rest, env, kont), handlers)
  }
}

///|
fn eval_case_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let key_expr = args[0]
  let clauses = args.sub(start=1).to_array()
  MachineState::Eval(key_expr, env, Kont::Case(clauses, env, kont), handlers)
}

///|
fn case_dispatch(
  key : Value,
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      match symbol_name(parts[0]) {
        Some("else") => {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
        _ => ()
      }
      let datums = datum_list_to_array(parts[0])
      let mut matched = false
      for datum in datums {
        if value_eqv(key, Value::Datum(datum)) {
          matched = true
        }
      }
      if matched {
        if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
          if parts.length() != 3 {
            raise EvalError("invalid case")
          }
          return MachineState::Eval(parts[2], env, Kont::CaseArrow(key, kont), handlers)
        }
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      }
      i = i + 1
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn cond_start(
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(parts[0], env, Kont::Cond(parts, rest, env, kont), handlers)
      }
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_pair(
  expr : Datum,
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    MachineState::Apply(Value::Datum(Datum::Nil), kont, handlers)
  } else {
    let head = items[0]
    let args = items.sub(start=1).to_array()
    match symbol_name(head) {
      Some("define-syntax") => MachineState::Apply(eval_define_syntax(args, env), kont, handlers)
      Some("library") => eval_library_state(args, env, kont, handlers)
      Some("import") => eval_import_state(args, env, kont, handlers)
      Some("define-record-type") => eval_define_record_type_state(args, env, kont, handlers)
      Some("quote") => MachineState::Apply(eval_quote(args), kont, handlers)
      Some("quasiquote") => MachineState::Apply(eval_quasiquote(args, env), kont, handlers)
      Some("if") => eval_if_state(args, env, kont, handlers)
      Some("begin") => eval_sequence_state(args, env, kont, handlers)
      Some("lambda") => MachineState::Apply(eval_lambda(args, env), kont, handlers)
      Some("case-lambda") => MachineState::Apply(eval_case_lambda(args, env), kont, handlers)
      Some("define") => eval_define_state(args, env, kont, handlers)
      Some("set!") => eval_set_state(args, env, kont, handlers)
      Some("let") => eval_let_state(args, env, kont, handlers)
      Some("let*") => eval_let_star_state(args, env, kont, handlers)
      Some("letrec") => eval_letrec_state(args, env, kont, handlers)
      Some("letrec*") => eval_letrec_state(args, env, kont, handlers)
      Some("let-syntax") => eval_let_syntax_state(args, env, kont, handlers)
      Some("letrec-syntax") => eval_letrec_syntax_state(args, env, kont, handlers)
      Some("and") => eval_and_state(args, env, kont, handlers)
      Some("or") => eval_or_state(args, env, kont, handlers)
      Some("cond") => cond_start(args, env, kont, handlers)
      Some("do") => eval_do_state(args, env, kont, handlers)
      Some("case") => eval_case_state(args, env, kont, handlers)
      Some(name) => {
        match env_get_macro(env, name) {
          Some(rules) => {
            let expanded = expand_macro(rules, expr, env)
            MachineState::Eval(expanded, env, kont, handlers)
          }
          None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont), handlers)
        }
      }
      None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont), handlers)
    }
  }
}

///|
fn apply_proc(
  proc : Value,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc {
    Value::Primitive(Primitive::Apply) => apply_apply(args, kont, handlers)
    Value::Primitive(Primitive::WithExceptionHandler) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let handler = args[0]
        let thunk = args[1]
        let next_handlers : Array[Value] = []
        for item in handlers {
          next_handlers.push(item)
        }
        next_handlers.push(handler)
        let restore = Kont::RestoreHandlers(handlers, kont)
        apply_proc(thunk, [], restore, next_handlers)
      }
    }
    Value::Primitive(Primitive::Raise) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else if handlers.is_empty() {
        raise EvalError("uncaught exception")
      } else {
        let handler = handlers[handlers.length() - 1]
        apply_proc(handler, [args[0]], Kont::RaiseNonCont, handlers)
      }
    }
    Value::Primitive(Primitive::RaiseContinuable) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else if handlers.is_empty() {
        raise EvalError("uncaught exception")
      } else {
        let handler = handlers[handlers.length() - 1]
        apply_proc(handler, [args[0]], Kont::RaiseCont(kont), handlers)
      }
    }
    Value::Primitive(Primitive::CallCC) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let cont = Continuation::{ id: next_continuation_id(), kont, handlers }
        apply_proc(args[0], [Value::Continuation(cont)], kont, handlers)
      }
    }
    Value::Primitive(prim) => MachineState::Apply(apply_primitive(prim, args), kont, handlers)
    Value::Closure(cl) => {
      if args.length() != cl.params.length() {
        raise arity_mismatch(cl.params.length(), args.length())
      } else {
        let new_env = env_extend(cl.env)
        let mut i = 0
        while i < cl.params.length() {
          env_define(new_env, cl.params[i], args[i])
          i = i + 1
        }
        eval_sequence_state(cl.body, new_env, kont, handlers)
      }
    }
    Value::CaseClosure(cl) => {
      let mut matched = false
      let mut matched_params : Array[String] = []
      let mut matched_body : Array[Datum] = []
      for clause in cl.clauses {
        if clause.params.length() == args.length() {
          matched = true
          matched_params = clause.params
          matched_body = clause.body
          break
        }
      }
      if !matched {
        let lengths : Array[Int] = []
        for clause in cl.clauses {
          lengths.push(clause.params.length())
        }
        if lengths.length() == 1 {
          raise arity_mismatch(lengths[0], args.length())
        } else {
          let expected_parts : Array[String] = []
          for len in lengths {
            expected_parts.push(len.to_string())
          }
          let expected = expected_parts.join(", ")
          raise EvalError("arity mismatch: expected one of \{expected} got \{args.length()}")
        }
      }
      let new_env = env_extend(cl.env)
      let mut i = 0
      while i < matched_params.length() {
        env_define(new_env, matched_params[i], args[i])
        i = i + 1
      }
      eval_sequence_state(matched_body, new_env, kont, handlers)
    }
    Value::Continuation(cont) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        MachineState::Apply(args[0], cont.kont, cont.handlers)
      }
    }
    Value::RecordProc(proc) => apply_record_proc(proc, args, kont, handlers)
    _ => raise EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let proc = args[0]
    let flat : Array[Value] = []
    let mut i = 1
    while i < args.length() - 1 {
      flat.push(args[i])
      i = i + 1
    }
    let list_value = value_as_datum(args[args.length() - 1])
    let tail = datum_list_to_value_array(list_value)
    flat.append(tail[:])
    apply_proc(proc, flat, kont, handlers)
  }
}

///|
fn init_env() -> Env {
  let env = env_new()
  env_define(env, "+", Value::Primitive(Primitive::Add))
  env_define(env, "-", Value::Primitive(Primitive::Sub))
  env_define(env, "*", Value::Primitive(Primitive::Mul))
  env_define(env, "=", Value::Primitive(Primitive::NumEq))
  env_define(env, "<", Value::Primitive(Primitive::Less))
  env_define(env, "cons", Value::Primitive(Primitive::Cons))
  env_define(env, "car", Value::Primitive(Primitive::Car))
  env_define(env, "cdr", Value::Primitive(Primitive::Cdr))
  env_define(env, "list", Value::Primitive(Primitive::List))
  env_define(env, "null?", Value::Primitive(Primitive::NullP))
  env_define(env, "pair?", Value::Primitive(Primitive::PairP))
  env_define(env, "symbol?", Value::Primitive(Primitive::SymbolP))
  env_define(env, "boolean?", Value::Primitive(Primitive::BooleanP))
  env_define(env, "number?", Value::Primitive(Primitive::NumberP))
  env_define(env, "procedure?", Value::Primitive(Primitive::ProcedureP))
  env_define(env, "not", Value::Primitive(Primitive::Not))
  env_define(env, "apply", Value::Primitive(Primitive::Apply))
  env
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
  match expr {
    Datum::Nil
    | Datum::Bool(_)
    | Datum::Int(_)
    | Datum::String(_) => Value::Datum(expr)
    Datum::Symbol(name) => {
      match env_lookup(env, name) {
        Value::Macro(_) => raise EvalError("macro used as value")
        value => value
      }
    }
    Datum::Pair(_, _) => eval_list(expr, env)
  }
}

///|
fn eval_list(expr : Datum, env : Env) -> Value raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    Value::Datum(Datum::Nil)
  } else {
    let head = items[0]
    let args = items.sub(start=1).to_array()
    match symbol_name(head) {
      Some("define-syntax") => eval_define_syntax(args, env)
      Some("quote") => eval_quote(args)
      Some("quasiquote") => eval_quasiquote(args, env)
      Some("if") => eval_if(args, env)
      Some("begin") => eval_sequence(args, env)
      Some("lambda") => eval_lambda(args, env)
      Some("define") => eval_define(args, env)
      Some("set!") => eval_set(args, env)
      Some("let") => eval_let(args, env)
      Some("let*") => eval_let_star(args, env)
      Some("letrec") => eval_letrec(args, env)
      Some("and") => eval_and(args, env)
      Some("or") => eval_or(args, env)
      Some("cond") => eval_cond(args, env)
      Some(name) => {
        match env_get_macro(env, name) {
          Some(rules) => {
            let expanded = expand_macro(rules, expr)
            eval_expr(expanded, env)
          }
          None => eval_application(items, env)
        }
      }
      None => eval_application(items, env)
    }
  }
}

///|
fn eval_sequence(exprs : Array[Datum], env : Env) -> Value raise EvalError {
  let mut last = Value::Void
  for expr in exprs {
    last = eval_expr(expr, env)
  }
  last
}

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
fn eval_if(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  } else {
    let cond_value = eval_expr(args[0], env)
    if is_false(cond_value) {
      eval_expr(args[2], env)
    } else {
      eval_expr(args[1], env)
    }
  }
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
    Value::Closure({ params, body, env })
  }
}

///|
fn eval_define(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
        match args.length() {
          2 => {
            let value = eval_expr(args[1], env)
            env_define(env, name, value)
            Value::Void
          }
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
              env_define(env, name, Value::Closure({ params, body, env }))
              Value::Void
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
fn eval_set(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => {
          let value = eval_expr(args[1], env)
          env_set(env, name, value)
          Value::Void
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
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
fn eval_let(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let values : Array[(String, Value)] = []
    for binding in bindings {
      let (name, expr) = binding
      let value = eval_expr(expr, env)
      values.push((name, value))
    }
    let new_env = env_extend(env)
    for binding in values {
      let (name, value) = binding
      env_define(new_env, name, value)
    }
    eval_sequence(body, new_env)
  }
}

///|
fn eval_let_star(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let mut current = env
    for binding in bindings {
      let (name, expr) = binding
      let value = eval_expr(expr, current)
      let next_env = env_extend(current)
      env_define(next_env, name, value)
      current = next_env
    }
    eval_sequence(body, current)
  }
}

///|
fn eval_letrec(args : Array[Datum], env : Env) -> Value raise EvalError {
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
    for binding in bindings {
      let (name, expr) = binding
      let value = eval_expr(expr, new_env)
      env_set(new_env, name, value)
    }
    eval_sequence(body, new_env)
  }
}

///|
fn eval_and(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.is_empty() {
    bool_value(true)
  } else {
    let mut last = bool_value(true)
    let mut i = 0
    while i < args.length() {
      let value = eval_expr(args[i], env)
      last = value
      if is_false(value) {
        break
      } else {
        i = i + 1
      }
    }
    last
  }
}

///|
fn eval_or(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.is_empty() {
    bool_value(false)
  } else {
    let mut result = bool_value(false)
    let mut i = 0
    while i < args.length() {
      let value = eval_expr(args[i], env)
      if !is_false(value) {
        result = value
        break
      } else {
        i = i + 1
      }
    }
    result
  }
}

///|
fn eval_cond(args : Array[Datum], env : Env) -> Value raise EvalError {
  let mut i = 0
  let mut done = false
  let mut result = Value::Void
  while i < args.length() && !done {
    let parts = datum_list_to_array(args[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        result = eval_sequence(body, env)
        done = true
      } else {
        let cond_value = eval_expr(parts[0], env)
        if !is_false(cond_value) {
          if parts.length() == 1 {
            result = cond_value
          } else {
            let body = parts.sub(start=1).to_array()
            result = eval_sequence(body, env)
          }
          done = true
        } else {
          ()
        }
      }
      i = i + 1
    }
  }
  result
}

///|
fn eval_application(items : Array[Datum], env : Env) -> Value raise EvalError {
  let proc = eval_expr(items[0], env)
  let args : Array[Value] = []
  let mut i = 1
  while i < items.length() {
    args.push(eval_expr(items[i], env))
    i = i + 1
  }
  apply_proc(proc, args)
}

///|
fn apply_proc(proc : Value, args : Array[Value]) -> Value raise EvalError {
  match proc {
    Value::Primitive(Primitive::Apply) => apply_apply(args)
    Value::Primitive(prim) => apply_primitive(prim, args)
    Value::Closure(cl) => apply_closure(cl, args)
    _ => raise EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(args : Array[Value]) -> Value raise EvalError {
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
    apply_proc(proc, flat)
  }
}

///|
fn apply_closure(cl : Closure, args : Array[Value]) -> Value raise EvalError {
  if args.length() != cl.params.length() {
    raise arity_mismatch(cl.params.length(), args.length())
  } else {
    let new_env = env_extend(cl.env)
    let mut i = 0
    while i < cl.params.length() {
      env_define(new_env, cl.params[i], args[i])
      i = i + 1
    }
    eval_sequence(cl.body, new_env)
  }
}

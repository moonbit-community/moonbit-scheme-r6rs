///|
fn is_core_identifier(name : String) -> Bool {
  let core : Array[String] = [
    "quote", "quasiquote", "unquote", "unquote-splicing", "quasisyntax", "unsyntax",
    "unsyntax-splicing", "if", "begin", "lambda", "case-lambda", "define", "define-syntax",
    "library", "import", "export", "begin-for-syntax", "syntax-case", "with-syntax",
    "syntax", "syntax?", "identifier-syntax", "syntax-rules", "set!", "let", "let*",
    "letrec", "letrec*", "let-values", "let*-values", "define-values", "let-syntax",
    "letrec-syntax", "define-record-type", "define-condition-type", "define-enumeration",
    "and", "or", "cond", "cond-expand", "do", "case", "else", "=>", "guard", "+",
    "-", "*", "/", "=", "<", ">", "<=", ">=", "eq?", "eqv?", "equal?", "cons", "car",
    "cdr", "list", "make-list", "null?", "pair?", "list?", "symbol?", "identifier?",
    "free-identifier=?", "bound-identifier=?", "symbol->string", "string->symbol",
    "syntax->datum", "datum->syntax", "boolean?", "number?", "integer?", "rational?",
    "real?", "complex?", "exact?", "inexact?", "zero?", "positive?", "negative?",
    "odd?", "even?", "finite?", "infinite?", "nan?", "procedure?", "record?", "record-rtd",
    "record-type-descriptor?", "record-constructor-descriptor?", "record-type-name",
    "record-type-parent", "record-type-uid", "record-type-generative?", "record-type-sealed?",
    "record-type-opaque?", "record-type-field-names", "record-type-field-mutable?",
    "record-constructor-descriptor", "record-constructor", "record-predicate", "record-accessor",
    "record-mutator", "make-record-type-descriptor", "make-record-constructor-descriptor",
    "&condition", "condition", "condition?", "simple-conditions", "condition-predicate",
    "condition-accessor", "make-eq-hashtable", "make-eqv-hashtable", "make-hashtable",
    "hashtable?", "hashtable-size", "hashtable-ref", "hashtable-set!", "hashtable-delete!",
    "hashtable-contains?", "hashtable-update!", "hashtable-copy", "hashtable-clear!",
    "hashtable-keys", "hashtable-entries", "hashtable-equivalence-function", "hashtable-hash-function",
    "hashtable-mutable?", "make-enumeration", "enum-set-universe", "enum-set-indexer",
    "enum-set-constructor", "enum-set?", "enum-set-member?", "enum-set-subset?",
    "enum-set=?", "enum-set-union", "enum-set-intersection", "enum-set-difference",
    "enum-set-complement", "enum-set-projection", "enum-set->list", "not", "apply",
    "call/cc", "call-with-current-continuation", "values", "call-with-values", "make-parameter",
    "dynamic-wind", "eval", "environment", "promise?", "make-promise", "force", "delay",
    "parameterize", "list-ref", "list-tail", "member", "memq", "memv", "assoc", "assq",
    "assv", "map", "for-each", "set-car!", "set-cdr!", "list-copy", "caar", "cadr",
    "cdar", "cddr", "caaar", "caadr", "cadar", "caddr", "cdaar", "cdadr", "cddar",
    "cdddr", "caaaar", "caaadr", "caadar", "caaddr", "cadaar", "cadadr", "caddar",
    "cadddr", "cdaaar", "cdaadr", "cdadar", "cdaddr", "cddaar", "cddadr", "cdddar",
    "cddddr", "length", "append", "reverse", "char=?", "char<?", "char>?", "char<=?",
    "char>=?", "char-ci=?", "char-ci<?", "char-ci>?", "char-ci<=?", "char-ci>=?",
    "char?", "char->integer", "integer->char", "char-alphabetic?", "char-numeric?",
    "char-whitespace?", "char-upper-case?", "char-lower-case?", "char-upcase", "char-downcase",
    "char-foldcase", "string=?", "string<?", "string>?", "string<=?", "string>=?",
    "string-ci=?", "string-ci<?", "string-ci>?", "string-ci<=?", "string-ci>=?",
    "string", "make-string", "string?", "string-length", "string-append", "string-ref",
    "string-set!", "string-copy", "substring", "string-copy!", "string-fill!", "string->list",
    "list->string", "string-map", "string-for-each", "string-upcase", "string-downcase",
    "string-foldcase", "vector", "make-vector", "vector?", "vector-length", "vector-ref",
    "vector-set!", "vector-fill!", "vector-copy", "vector-copy!", "vector-append",
    "vector-map", "vector-for-each", "vector->list", "list->vector", "bytevector",
    "make-bytevector", "bytevector?", "bytevector-length", "bytevector=?", "bytevector-u8-ref",
    "bytevector-u8-set!", "bytevector-copy", "bytevector-copy!", "bytevector-append",
    "bytevector-fill!", "bytevector->u8-list", "u8-list->bytevector", "string->utf8",
    "utf8->string", "exact->inexact", "inexact->exact", "exact-integer-sqrt", "rationalize",
    "number->string", "string->number", "make-rectangular", "make-polar", "real-part",
    "imag-part", "magnitude", "angle", "sqrt", "exp", "log", "expt", "sin", "cos",
    "tan", "asin", "acos", "atan", "numerator", "denominator", "abs", "quotient",
    "remainder", "modulo", "gcd", "lcm", "max", "min", "floor", "ceiling", "truncate",
    "round", "display", "write", "newline", "open-output-string", "get-output-string",
    "current-output-port", "with-exception-handler", "raise", "raise-continuable",
    "error", "assertion-violation", "implementation-restriction-violation", "undefined-violation",
    "syntax-violation",
  ]
  core.contains(name)
}

///|
fn parse_syntax_rules(
  expr : @core.Datum,
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  let (ellipsis, literals_expr, rule_exprs) = match datum_list_to_array(expr) {
    [Symbol("syntax-rules"), Symbol(name), literals_expr, .. rule_exprs] =>
      (name, literals_expr, rule_exprs)
    [Symbol("syntax-rules"), literals_expr, .. rule_exprs] =>
      ("...", literals_expr, rule_exprs)
    _ => raise EvalError("invalid syntax-rules")
  }
  if rule_exprs.is_empty() {
    raise EvalError("invalid syntax-rules")
  }
  let literals = parse_literal_list(literals_expr)
  let ellipsis_active = !is_literal(literals, ellipsis)
  let rules : Array[@core.SyntaxRule] = []
  for rule_expr in rule_exprs {
    match datum_list_to_array(rule_expr) {
      [pattern, template] => {
        validate_pattern(
          pattern, literals, ellipsis, ellipsis_active, "invalid syntax-rules",
        )
        rules.push(@core.SyntaxRule::new(pattern, template, None))
      }
      _ => raise EvalError("invalid syntax-rules")
    }
  } else {
    ()
  }
  @core.SyntaxRules::new(literals, rules, ellipsis, SyntaxRules, env)
}

///|
fn parse_identifier_syntax(
  expr : @core.Datum,
  name : String,
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  let rules : Array[@core.SyntaxRule] = []
  match datum_list_to_array(expr) {
    [Symbol("identifier-syntax"), rule_expr] =>
      rules.push(@core.SyntaxRule::new(Symbol(name), rule_expr, None))
    [Symbol("identifier-syntax"), .. rule_exprs] => {
      if rule_exprs.is_empty() {
        raise EvalError("invalid identifier-syntax")
      }
      for rule_expr in rule_exprs {
        match datum_list_to_array(rule_expr) {
          [pattern, template] =>
            rules.push(@core.SyntaxRule::new(pattern, template, None))
          _ => raise EvalError("invalid identifier-syntax")
        }
      }
    }
    _ => raise EvalError("invalid identifier-syntax")
  }
  let pattern_vars : Map[String, Bool] = {}
  let pattern_literals = ["set!"]
  for rule in rules {
    collect_pattern_vars(rule.pattern, pattern_literals, "...", pattern_vars)
  }
  let literal_set : Map[String, Bool] = {}
  for rule in rules {
    collect_template_symbols(rule.template, literal_set)
  }
  let literals : Array[String] = literal_set
    .keys()
    .filter(key => !pattern_vars.contains(key))
    .to_array()
  @core.SyntaxRules::new(literals, rules, "...", SyntaxRules, env)
}

///|
fn parse_syntax_case_rules(
  literals_expr : @core.Datum,
  clauses : Array[@core.Datum],
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  let literals = parse_literal_list(literals_expr)
  let ellipsis_active = !is_literal(literals, "...")
  let rules : Array[@core.SyntaxRule] = []
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i < clauses.length()
    match datum_list_to_array(clauses[i]) {
      [pattern, template] => {
        validate_pattern(
          pattern, literals, "...", ellipsis_active, "invalid syntax-case",
        )
        rules.push(@core.SyntaxRule::new(pattern, template, None))
      }
      [pattern, fender, template] => {
        validate_pattern(
          pattern, literals, "...", ellipsis_active, "invalid syntax-case",
        )
        rules.push(@core.SyntaxRule::new(pattern, template, Some(fender)))
      }
      _ => raise EvalError("invalid syntax-case clause")
    }
    continue i + 1
  } else {
    ()
  }
  @core.SyntaxRules::new(literals, rules, "...", SyntaxCase, env)
}

///|
fn parse_syntax_case_transformer(
  expr : @core.Datum,
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  match datum_list_to_array(expr) {
    [Symbol("lambda"), params_expr, body_expr] => {
      match datum_list_to_array(params_expr) {
        [param_expr] => {
          let _ = parse_symbol_name(param_expr)
        }
        _ => raise EvalError("invalid syntax-case")
      }
      match datum_list_to_array(body_expr) {
        [Symbol("syntax-case"), _, literals_expr, .. clauses] => {
          if clauses.is_empty() {
            raise EvalError("invalid syntax-case")
          }
          parse_syntax_case_rules(literals_expr, clauses.to_array(), env)
        }
        _ => raise EvalError("invalid syntax-case")
      }
    }
    _ => raise EvalError("invalid syntax-case")
  }
}

///|
fn is_syntax_case_transformer_expr(expr : @core.Datum) -> Bool {
  match datum_list_to_array_opt(expr) {
    Some([Symbol("lambda"), params_expr, body_expr]) =>
      match datum_list_to_array_opt(params_expr) {
        Some([_]) =>
          match datum_list_to_array_opt(body_expr) {
            Some([Symbol("syntax-case"), _, _, _, ..]) => true
            _ => false
          }
        _ => false
      }
    _ => false
  }
}

///|
fn eval_transformer_expr(
  expr : @core.Datum,
  env : @core.Env,
) -> @core.Value raise @core.EvalError {
  let eval_env = @runtime.env_clone(env)
  eval_expr_transformer(expr, eval_env)
}

///|
fn parse_transformer(
  expr : @core.Datum,
  name : String,
  env : @core.Env,
) -> @core.MacroTransformer raise @core.EvalError {
  match datum_list_to_array(expr) {
    [] => raise EvalError("invalid syntax transformer")
    [Symbol("syntax-rules"), ..] => Rules(parse_syntax_rules(expr, env))
    [Symbol("identifier-syntax"), ..] =>
      Rules(parse_identifier_syntax(expr, name, env))
    [Symbol("make-variable-transformer"), transformer_expr] =>
      parse_transformer(transformer_expr, name, env)
    [Symbol("make-variable-transformer"), ..] =>
      raise EvalError("invalid syntax transformer")
    [Symbol("lambda"), ..] if is_syntax_case_transformer_expr(expr) =>
      Rules(parse_syntax_case_transformer(expr, env))
    _ => {
      let value = eval_transformer_expr(expr, env)
      if !@runtime.is_procedure_value(value) {
        raise EvalError("type error: procedure expected")
      }
      Procedure(value, env)
    }
  }
}

///|
fn parse_literal_list(
  expr : @core.Datum,
) -> Array[String] raise @core.EvalError {
  let items = datum_list_to_array(expr)
  let literals : Array[String] = []
  for item in items {
    match item {
      Symbol(name) => literals.push(name)
      _ => raise EvalError("invalid syntax-rules")
    }
  }
  literals
}

///|
fn parse_symbol_name(expr : @core.Datum) -> String raise @core.EvalError {
  match expr {
    Symbol(name) => name
    _ => raise EvalError("invalid syntax-case")
  }
}

///|
fn is_literal(literals : Array[String], name : String) -> Bool {
  literals.contains(name)
}

///|
fn validate_pattern(
  pattern : @core.Datum,
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
) -> Unit raise @core.EvalError {
  let pat = @runtime.datum_unlabel(datum_unwrap_syntax(pattern))
  match pat {
    Symbol(name) =>
      if ellipsis_active && name == ellipsis {
        raise EvalError(error_message)
      }
    Pair(_, _) =>
      match datum_list_to_array_opt(pat) {
        Some(items) =>
          validate_pattern_list(
            items, literals, ellipsis, ellipsis_active, error_message,
          )
        None =>
          match pat {
            Pair(a, b) => {
              validate_pattern(
                a.val,
                literals,
                ellipsis,
                ellipsis_active,
                error_message,
              )
              validate_pattern(
                b.val,
                literals,
                ellipsis,
                ellipsis_active,
                error_message,
              )
            }
            _ => ()
          }
      }
    Vector(items) =>
      validate_pattern_list(
        items, literals, ellipsis, ellipsis_active, error_message,
      )
    Label(_, value) =>
      validate_pattern(
        value.val,
        literals,
        ellipsis,
        ellipsis_active,
        error_message,
      )
    _ => ()
  }
}

///|
fn validate_pattern_list(
  items : Array[@core.Datum],
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
) -> Unit raise @core.EvalError {
  for i = 0; i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      validate_pattern(
        items[i],
        literals,
        ellipsis,
        ellipsis_active,
        error_message,
      )
      continue i + 2
    } else {
      validate_pattern(
        items[i],
        literals,
        ellipsis,
        ellipsis_active,
        error_message,
      )
      continue i + 1
    }
  } else {
    ()
  }
}

///|
priv enum BindingVal {
  Leaf(@core.Datum)
  Seq(Array[BindingVal])
}

///|
fn binding_equal(left : BindingVal, right : BindingVal) -> Bool {
  match (left, right) {
    (Leaf(a), Leaf(b)) => datum_equal(a, b)
    (Seq(a), Seq(b)) =>
      if a.length() != b.length() {
        false
      } else {
        for i = 0; i < a.length(); {
          // invariant : i >= 0 && i <= a.length()
          // decreases : a.length() - i
          // assert : i <= a.length()
          if !binding_equal(a[i], b[i]) {
            break false
          }
          continue i + 1
        } else {
          true
        }
      }
    _ => false
  }
}

///|
fn binding_clone(value : BindingVal) -> BindingVal {
  match value {
    Leaf(datum) => Leaf(datum)
    Seq(values) => {
      let cloned : Array[BindingVal] = values.map(item => binding_clone(item))
      Seq(cloned)
    }
  }
}

///|
fn bindings_clone(
  bindings : Map[String, BindingVal],
) -> Map[String, BindingVal] {
  bindings.map((_, value) => binding_clone(value))
}

///|
fn bindings_overwrite(
  dest : Map[String, BindingVal],
  src : Map[String, BindingVal],
) -> Unit {
  dest.clear()
  src.iter2().each((name, value) => dest[name] = value)
}

///|
fn merge_bindings(
  dest : Map[String, BindingVal],
  src : Map[String, BindingVal],
) -> Bool {
  for name in src.keys() {
    match src.get(name) {
      Some(value) =>
        match dest.get(name) {
          Some(existing) => if !binding_equal(existing, value) { return false }
          None => dest[name] = value
        }
      None => ()
    }
  }
  true
}

///|
fn binding_select(
  value : BindingVal,
  indices : Array[Int],
  depth : Int,
) -> @core.Datum? {
  match value {
    Leaf(datum) => if depth < indices.length() { None } else { Some(datum) }
    Seq(values) =>
      if depth >= indices.length() || values.is_empty() {
        None
      } else {
        let idx = indices[depth]
        let last = match values {
          [.., last] => last
          [] => return None
        }
        let actual = if idx < values.length() { values[idx] } else { last }
        binding_select(actual, indices, depth + 1)
      }
  }
}

///|
fn binding_len_at(value : BindingVal, indices : Array[Int], depth : Int) -> Int {
  match value {
    Leaf(_) => 0
    Seq(values) =>
      if depth < indices.length() {
        if values.is_empty() {
          0
        } else {
          let idx = indices[depth]
          let last = match values {
            [.., last] => last
            [] => return 0
          }
          let actual = if idx < values.length() { values[idx] } else { last }
          binding_len_at(actual, indices, depth + 1)
        }
      } else {
        values.length()
      }
  }
}

///|
fn binding_has_repeat_at(
  value : BindingVal,
  indices : Array[Int],
  depth : Int,
) -> Bool {
  match value {
    Leaf(_) => false
    Seq(values) =>
      if depth < indices.length() {
        if values.is_empty() {
          false
        } else {
          let idx = indices[depth]
          let last = match values {
            [.., last] => last
            [] => return false
          }
          let actual = if idx < values.length() { values[idx] } else { last }
          binding_has_repeat_at(actual, indices, depth + 1)
        }
      } else {
        true
      }
  }
}

///|
fn binding_to_datum(value : BindingVal) -> @core.Datum {
  match value {
    Leaf(datum) => datum
    Seq(values) => {
      let items : Array[@core.Datum] = values.map(item => binding_to_datum(item))
      list_from_array(items)
    }
  }
}

///|
fn fender_allows(
  expr : @core.Datum,
  bindings : Map[String, BindingVal],
  env : @core.Env,
) -> Bool raise @core.EvalError {
  let fenv = @runtime.env_extend(env)
  bindings
  .iter2()
  .each((name, value) => @runtime.env_define(
    fenv,
    name,
    value_from_datum(binding_to_datum(value)),
  ))
  let result = eval_expr(expr, fenv)
  !is_false(result)
}

///|
fn template_env_with_bindings(
  env : @core.Env,
  bindings : Map[String, BindingVal],
) -> @core.Env {
  let fenv = @runtime.env_extend(env)
  bindings
  .iter2()
  .each((name, value) => @runtime.env_define(
    fenv,
    name,
    value_from_datum(binding_to_datum(value)),
  ))
  fenv
}

///|
fn value_to_syntax_datum(
  value : @core.Value,
) -> @core.Datum raise @core.EvalError {
  match value {
    SyntaxObject(_) | Record(_) => value_to_datum_element(value)
    Datum(d) => d
    _ => raise EvalError("type error: datum expected")
  }
}

///|
priv struct MacroExpansion {
  datum : @core.Datum
  captures : Map[String, @core.Value]
}

///|

///|
fn scopes_equal_macro(left : Array[Int], right : Array[Int]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    for i, item in left {
      if right[i] != item {
        return false
      }
    }
    true
  }
}

///|
fn rename_proc_symbol(
  name : String,
  def_env : @core.Env,
  renames : Map[String, String],
  captures : Map[String, @core.Value],
) -> @core.Datum {
  match renames.get(name) {
    Some(rename) => Symbol(rename)
    None =>
      match captured_value(def_env, name) {
        Some(value) => {
          let renamed = @runtime.gensym(name)
          renames[name] = renamed
          captures[renamed] = value
          Symbol(renamed)
        }
        None => Symbol(name)
      }
  }
}

///|
fn rename_proc_datum(
  datum : @core.Datum,
  def_env : @core.Env,
  call_ctx : Int,
  renames : Map[String, String],
  captures : Map[String, @core.Value],
) -> @core.Datum {
  match datum {
    Symbol(name) => rename_proc_symbol(name, def_env, renames, captures)
    Pair(a, b) =>
      pair_new(
        rename_proc_datum(a.val, def_env, call_ctx, renames, captures),
        rename_proc_datum(b.val, def_env, call_ctx, renames, captures),
      )
    Vector(items) =>
      Vector(
        items.map(item => rename_proc_datum(
          item, def_env, call_ctx, renames, captures,
        )),
      )
    Label(tag, value) =>
      Label(
        tag,
        Ref::new(
          rename_proc_datum(value.val, def_env, call_ctx, renames, captures),
        ),
      )
    Complex(real, imag) =>
      Complex(
        Ref::new(
          rename_proc_datum(real.val, def_env, call_ctx, renames, captures),
        ),
        Ref::new(
          rename_proc_datum(imag.val, def_env, call_ctx, renames, captures),
        ),
      )
    Value(SyntaxObject(obj)) =>
      if obj.scopes.contains(call_ctx) {
        datum
      } else {
        let renamed_inner = rename_proc_datum(
          obj.datum,
          def_env,
          call_ctx,
          renames,
          captures,
        )
        Value(
          SyntaxObject(
            @core.SyntaxObject::new(renamed_inner, obj.scopes, obj.binding_id),
          ),
        )
      }
    _ => datum
  }
}

///|
fn rename_proc_expansion(
  datum : @core.Datum,
  def_env : @core.Env,
  call_ctx : Int,
) -> MacroExpansion {
  let renames : Map[String, String] = {}
  let captures : Map[String, @core.Value] = {}
  let renamed = rename_proc_datum(datum, def_env, call_ctx, renames, captures)
  { datum: renamed, captures }
}

///|
fn expand_macro_rules(
  rules : @core.SyntaxRules,
  call : @core.Datum,
  env : @core.Env,
) -> MacroExpansion raise @core.EvalError {
  let call_scope = match rules.kind {
    SyntaxRules => Some(@runtime.next_syntax_context())
    SyntaxCase => None
  }
  let ellipsis_active = !is_literal(rules.literals, rules.ellipsis)
  for i = 0; i < rules.rules.length(); {
    // invariant : i >= 0 && i <= rules.rules.length()
    // decreases : rules.rules.length() - i
    // assert : i <= rules.rules.length()
    let rule = rules.rules[i]
    let bindings : Map[String, BindingVal] = {}
    match (rule.pattern, call) {
      (Symbol(_), Pair(_, _)) => ()
      (Pair(_, _), Symbol(_)) => ()
      _ =>
        if match_pattern(
            rule.pattern,
            call,
            rules.literals,
            rules.ellipsis,
            bindings,
            call_scope,
            rules.def_env,
            env,
          ) {
          match rule.fender {
            Some(fender) => {
              let fender_env = match rules.kind {
                SyntaxCase => rules.def_env
                SyntaxRules => env
              }
              if !fender_allows(fender, bindings, fender_env) {
                continue i + 1
              }
            }
            None => ()
          }
          let (renames, captures) = build_rename_map(
            rule.template,
            bindings,
            rules.literals,
            rules.ellipsis,
            rules.def_env,
            env,
          )
          let expanded = match rules.kind {
            SyntaxRules =>
              expand_template(
                rule.template,
                bindings,
                renames,
                rules.ellipsis,
                ellipsis_active,
                "invalid syntax-rules",
                env,
                false,
              )
            SyntaxCase => {
              let fenv = template_env_with_bindings(rules.def_env, bindings)
              let rewritten = rewrite_template_expr(
                rule.template,
                bindings,
                renames,
                rules.ellipsis,
                ellipsis_active,
                "invalid syntax-case",
                rules.def_env,
              )
              let value = eval_expr_transformer(rewritten, fenv)
              value_to_syntax_datum(value)
            }
          }
          return { datum: expanded, captures }
        }
    }
    continue i + 1
  }
  raise EvalError("no matching syntax-rules pattern")
}

///|
fn expand_macro_proc(
  proc : @core.Value,
  def_env : @core.Env,
  call : @core.Datum,
  env : @core.Env,
) -> MacroExpansion raise @core.EvalError {
  if !@runtime.is_procedure_value(proc) {
    raise EvalError("type error: procedure expected")
  }
  let temp_name = @runtime.gensym("macro_proc")
  let fenv = @runtime.env_extend(env)
  @runtime.env_define(fenv, temp_name, proc)
  let call_ctx = @runtime.next_syntax_context()
  let marked_call = @runtime.syntax_add_scope(call, call_ctx)
  let stx = match marked_call {
    Value(SyntaxObject(obj)) => Value::SyntaxObject(obj)
    _ =>
      SyntaxObject(
        @core.SyntaxObject::new(marked_call, [call_ctx], None),
      )
  }
  let call_expr = list_from_array([Symbol(temp_name), Value(stx)])
  let value = eval_expr_transformer(call_expr, fenv)
  let datum = value_to_syntax_datum(value)
  rename_proc_expansion(datum, def_env, call_ctx)
}

///|
fn expand_macro(
  transformer : @core.MacroTransformer,
  call : @core.Datum,
  env : @core.Env,
) -> MacroExpansion raise @core.EvalError {
  match transformer {
    Rules(rules) => expand_macro_rules(rules, call, env)
    Procedure(proc, def_env) => expand_macro_proc(proc, def_env, call, env)
  }
}

///|
fn scopes_with_added(scopes : Array[Int], scope : Int) -> Array[Int] {
  if scopes.contains(scope) {
    scopes.copy()
  } else {
    let next = scopes.copy()
    next.push(scope)
    next
  }
}

///|
fn syntax_add_scope_with_env(
  datum : @core.Datum,
  scope : Int,
  env : @core.Env,
) -> @core.Datum {
  match datum {
    Symbol(name) =>
      Value(
        SyntaxObject(
          @core.SyntaxObject::new(
            Symbol(name),
            [scope],
            @runtime.env_binding_id_optional(env, name),
          ),
        ),
      )
    Value(SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        Symbol(_) => obj.datum
        _ => syntax_add_scope_with_env(obj.datum, scope, env)
      }
      let binding_id = match obj.binding_id {
        Some(_) => obj.binding_id
        None =>
          match obj.datum {
            Symbol(name) => @runtime.env_binding_id_optional(env, name)
            _ => None
          }
      }
      Value(
        SyntaxObject(
          @core.SyntaxObject::new(inner, next_scopes, binding_id),
        ),
      )
    }
    Pair(a, b) =>
      pair_new(
        syntax_add_scope_with_env(a.val, scope, env),
        syntax_add_scope_with_env(b.val, scope, env),
      )
    Vector(items) =>
      Vector(items.map(item => syntax_add_scope_with_env(item, scope, env)))
    Complex(real, imag) =>
      Complex(
        Ref::new(syntax_add_scope_with_env(real.val, scope, env)),
        Ref::new(syntax_add_scope_with_env(imag.val, scope, env)),
      )
    _ => datum
  }
}

///|
fn wrap_pattern_datum(
  value : @core.Datum,
  scope : Int?,
  env : @core.Env,
) -> @core.Datum {
  match scope {
    Some(call_scope) => {
      let marked = syntax_add_scope_with_env(value, call_scope, env)
      match marked {
        Value(SyntaxObject(_)) => marked
        _ =>
          Value(
            SyntaxObject(
              @core.SyntaxObject::new(marked, [call_scope], None),
            ),
          )
      }
    }
    None =>
      match value {
        Value(SyntaxObject(_)) => value
        _ => {
          let binding_id = match @runtime.datum_unlabel(value) {
            Symbol(name) => @runtime.env_binding_id_optional(env, name)
            _ => None
          }
          Value(
            SyntaxObject(
              @core.SyntaxObject::new(value, [], binding_id),
            ),
          )
        }
      }
  }
}

///|
fn rewrite_template_expr(
  expr : @core.Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  env : @core.Env,
) -> @core.Datum raise @core.EvalError {
  match expr {
    Pair(_, _) =>
      match datum_list_to_array_opt(expr) {
        Some(items) => {
          if !items.is_empty() {
            let head = items[0]
            let is_template_form = datum_is_symbol(head, "syntax") ||
              datum_is_symbol(head, "quasisyntax") ||
              datum_is_symbol(head, "with-syntax")
            if is_template_form {
              let indices : Array[Int] = []
              let expanded = expand_template_indexed(
                expr, bindings, renames, ellipsis, ellipsis_active, error_message,
                indices, env, true,
              )
              match expanded {
                Value(SyntaxObject(obj)) =>
                  return Value(SyntaxObject(obj))
                _ => {
                  let scope = @runtime.next_syntax_context()
                  let scopes = [scope]
                  let datum = @runtime.syntax_wrap_root(expanded, scopes)
                  let obj = @core.SyntaxObject::new(datum, scopes, None)
                  return Value(SyntaxObject(obj))
                }
              }
            }
          }
          let rewritten : Array[@core.Datum] = []
          for item in items {
            rewritten.push(
              rewrite_template_expr(
                item, bindings, renames, ellipsis, ellipsis_active, error_message,
                env,
              ),
            )
          }
          list_from_array(rewritten)
        }
        None =>
          match expr {
            Pair(a, b) =>
              pair_new(
                rewrite_template_expr(
                  a.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  env,
                ),
                rewrite_template_expr(
                  b.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  env,
                ),
              )
            _ => expr
          }
      }
    Vector(items) =>
      Vector(
        items.map(item => rewrite_template_expr(
          item, bindings, renames, ellipsis, ellipsis_active, error_message, env,
        )),
      )
    _ => expr
  }
}

///|
fn match_pattern(
  pattern : @core.Datum,
  input : @core.Datum,
  literals : Array[String],
  ellipsis : String,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : @core.Env,
  use_env : @core.Env,
) -> Bool {
  let pat = @runtime.datum_unlabel(datum_unwrap_syntax(pattern))
  let inp = @runtime.datum_unlabel(datum_unwrap_syntax(input))
  match (pat, inp) {
    (Label(_, _), _) => false
    (Symbol(name), _) =>
      if name == "_" {
        if is_literal(literals, name) {
          literal_matches(name, input, def_env, use_env)
        } else {
          true
        }
      } else if is_literal(literals, name) {
        literal_matches(name, input, def_env, use_env)
      } else {
        bind_var(name, wrap_pattern_datum(input, scope, use_env), bindings)
      }
    (Nil, Nil) => true
    (Nil, _) => false
    (Bool(b), Bool(other)) => other == b
    (Bool(_), _) => false
    (Int(n), Int(other)) => other == n
    (Int(_), _) => false
    (BigInt(n), BigInt(other)) => other == n
    (BigInt(_), _) => false
    (Rat(n, d), Rat(on, od)) => on == n && od == d
    (Rat(_, _), _) => false
    (BigRat(n, d), BigRat(on, od)) => on == n && od == d
    (BigRat(_, _), _) => false
    (Complex(real, imag), Complex(other_real, other_imag)) =>
      datum_equal(real.val, other_real.val) &&
      datum_equal(imag.val, other_imag.val)
    (Complex(_, _), _) => false
    (Float(f), Float(of)) => of == f
    (Float(_), _) => false
    (Char(ch), Char(other)) => other == ch
    (Char(_), _) => false
    (String(s), String(other)) => other.val == s.val
    (String(_), _) => false
    (Vector(items), Vector(other)) =>
      match_pattern_list(
        items, other, literals, ellipsis, bindings, scope, def_env, use_env,
      )
    (Vector(_), _) => false
    (ByteVector(items), ByteVector(other)) => array_int_equal(items, other)
    (ByteVector(_), _) => false
    (Record(rec), Record(other)) => rec.id == other.id
    (Record(_), _) => false
    (Condition(cond), Condition(other)) => cond.id == other.id
    (Condition(_), _) => false
    (Value(value), _) => value_eqv(value, value_from_datum(inp))
    (Pair(pa, pb), _) =>
      match (datum_list_to_array_opt(pat), datum_list_to_array_opt(inp)) {
        (Some(pats), Some(inputs)) =>
          match_pattern_list(
            pats, inputs, literals, ellipsis, bindings, scope, def_env, use_env,
          )
        _ =>
          match inp {
            Pair(ia, ib) =>
              if match_pattern(
                  pa.val,
                  ia.val,
                  literals,
                  ellipsis,
                  bindings,
                  scope,
                  def_env,
                  use_env,
                ) {
                match_pattern(
                  pb.val,
                  ib.val,
                  literals,
                  ellipsis,
                  bindings,
                  scope,
                  def_env,
                  use_env,
                )
              } else {
                false
              }
            _ => false
          }
      }
  }
}

///|
fn match_pattern_list(
  pats : Array[@core.Datum],
  inputs : Array[@core.Datum],
  literals : Array[String],
  ellipsis : String,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : @core.Env,
  use_env : @core.Env,
) -> Bool {
  let ellipsis_active = !is_literal(literals, ellipsis)
  match_pattern_list_from(
    pats, inputs, 0, 0, literals, ellipsis, ellipsis_active, bindings, scope, def_env,
    use_env,
  )
}

///|
fn collect_repeat_bindings(
  repeat_pat : @core.Datum,
  inputs : Array[@core.Datum],
  start : Int,
  count : Int,
  literals : Array[String],
  ellipsis : String,
  scope : Int?,
  def_env : @core.Env,
  use_env : @core.Env,
) -> Map[String, BindingVal]? {
  let result : Map[String, BindingVal] = {}
  if count == 0 {
    let names : Map[String, Bool] = {}
    collect_pattern_vars(repeat_pat, literals, ellipsis, names)
    names.keys().each(name => result[name] = Seq([]))
    return Some(result)
  }
  let repeated : Map[String, Array[BindingVal]] = {}
  for i = 0; i < count; {
    // invariant : i >= 0 && i <= count
    // decreases : count - i
    // assert : i <= count
    let iter_bindings : Map[String, BindingVal] = {}
    if !match_pattern(
        repeat_pat,
        inputs[start + i],
        literals,
        ellipsis,
        iter_bindings,
        scope,
        def_env,
        use_env,
      ) {
      return None
    }
    iter_bindings
    .iter2()
    .each((name, value) => match repeated.get(name) {
      Some(values) => values.push(value)
      None => {
        let values : Array[BindingVal] = []
        values.push(value)
        repeated[name] = values
      }
    })
    continue i + 1
  }
  repeated.iter2().each((name, values) => result[name] = Seq(values))
  Some(result)
}

///|
fn match_pattern_list_from(
  pats : Array[@core.Datum],
  inputs : Array[@core.Datum],
  pat_idx : Int,
  inp_idx : Int,
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : @core.Env,
  use_env : @core.Env,
) -> Bool {
  if pat_idx >= pats.length() {
    return inp_idx == inputs.length()
  }
  if ellipsis_active &&
    pat_idx + 1 < pats.length() &&
    datum_is_symbol(pats[pat_idx + 1], ellipsis) {
    let max_count = inputs.length() - inp_idx
    for count = max_count; count >= 0; {
      // invariant : count >= 0 && count <= max_count
      // decreases : count
      // assert : count <= max_count
      let base = bindings_clone(bindings)
      match
        collect_repeat_bindings(
          pats[pat_idx],
          inputs,
          inp_idx,
          count,
          literals,
          ellipsis,
          scope,
          def_env,
          use_env,
        ) {
        Some(repeat_bindings) =>
          if merge_bindings(base, repeat_bindings) {
            if match_pattern_list_from(
                pats,
                inputs,
                pat_idx + 2,
                inp_idx + count,
                literals,
                ellipsis,
                ellipsis_active,
                base,
                scope,
                def_env,
                use_env,
              ) {
              bindings_overwrite(bindings, base)
              return true
            }
          }
        None => ()
      }
      continue count - 1
    }
    false
  } else {
    if inp_idx >= inputs.length() {
      return false
    }
    if !match_pattern(
        pats[pat_idx],
        inputs[inp_idx],
        literals,
        ellipsis,
        bindings,
        scope,
        def_env,
        use_env,
      ) {
      return false
    }
    match_pattern_list_from(
      pats,
      inputs,
      pat_idx + 1,
      inp_idx + 1,
      literals,
      ellipsis,
      ellipsis_active,
      bindings,
      scope,
      def_env,
      use_env,
    )
  }
}

///|
fn bind_var(
  name : String,
  value : @core.Datum,
  bindings : Map[String, BindingVal],
) -> Bool {
  let leaf = Leaf(value)
  match bindings.get(name) {
    Some(existing) => binding_equal(existing, leaf)
    None => {
      bindings[name] = leaf
      true
    }
  }
}

///|
fn collect_pattern_vars(
  pattern : @core.Datum,
  literals : Array[String],
  ellipsis : String,
  names : Map[String, Bool],
) -> Unit {
  match pattern {
    Symbol(name) =>
      if name != "_" && name != ellipsis && !is_literal(literals, name) {
        names[name] = true
      }
    Pair(_, _) =>
      match datum_list_to_array_opt(pattern) {
        Some(items) =>
          for item in items {
            collect_pattern_vars(item, literals, ellipsis, names)
          }
        None =>
          match pattern {
            Pair(a, b) => {
              collect_pattern_vars(a.val, literals, ellipsis, names)
              collect_pattern_vars(b.val, literals, ellipsis, names)
            }
            _ => ()
          }
      }
    Vector(items) =>
      for item in items {
        collect_pattern_vars(item, literals, ellipsis, names)
      }
    _ => ()
  }
}

///|
fn datum_list_to_array_opt(list : @core.Datum) -> Array[@core.Datum]? {
  let items : Array[@core.Datum] = []
  let ok = for cur = list; true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert :
    //   match @runtime.datum_unlabel(cur) {
    //     Pair(_, _) | Nil => true
    //     _ => false
    //   }
    match @runtime.datum_unlabel(cur) {
      Nil => break true
      Pair(a, b) => {
        items.push(a.val)
        continue b.val
      }
      _ => break false
    }
  } else {
    false
  }
  if ok {
    Some(items)
  } else {
    None
  }
}

///|
fn datum_unwrap_syntax(value : @core.Datum) -> @core.Datum {
  match value {
    Value(SyntaxObject(obj)) => obj.datum
    _ => value
  }
}

///|
fn datum_is_symbol(value : @core.Datum, name : String) -> Bool {
  match @runtime.datum_unlabel(datum_unwrap_syntax(value)) {
    Symbol(sym) => sym == name
    _ => false
  }
}

///|
fn literal_matches(
  name : String,
  input : @core.Datum,
  def_env : @core.Env,
  use_env : @core.Env,
) -> Bool {
  let unlabel = @runtime.datum_unlabel(input)
  match unlabel {
    Value(SyntaxObject(obj)) =>
      match @runtime.datum_unlabel(obj.datum) {
        Symbol(other) => {
          if other != name {
            return false
          }
          let def_id = @runtime.env_binding_id_optional(def_env, name)
          match (def_id, obj.binding_id) {
            (Some(left), Some(right)) => left == right
            (None, None) => scopes_equal_macro(obj.scopes, [])
            _ => false
          }
        }
        _ => false
      }
    _ =>
      match @runtime.datum_unlabel(datum_unwrap_syntax(unlabel)) {
        Symbol(other) => {
          if other != name {
            return false
          }
          let def_id = @runtime.env_binding_id_optional(def_env, name)
          let use_id = @runtime.env_binding_id_optional(use_env, name)
          match (def_id, use_id) {
            (Some(left), Some(right)) => left == right
            (None, None) => true
            _ => false
          }
        }
        _ => false
      }
  }
}

///|
fn array_datum_equal(
  left : Array[@core.Datum],
  right : Array[@core.Datum],
) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    for i = 0; i < left.length(); {
      // invariant : i >= 0 && i <= left.length()
      // decreases : left.length() - i
      // assert : i <= left.length()
      if !datum_equal(left[i], right[i]) {
        break false
      }
      continue i + 1
    } else {
      true
    }
  }
}

///|
fn array_int_equal(left : Array[Int], right : Array[Int]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    for i = 0; i < left.length(); {
      // invariant : i >= 0 && i <= left.length()
      // decreases : left.length() - i
      // assert : i <= left.length()
      if left[i] != right[i] {
        break false
      }
      continue i + 1
    } else {
      true
    }
  }
}

///|
fn datum_equal(a : @core.Datum, b : @core.Datum) -> Bool {
  match (a, b) {
    (Label(_, x), _) => datum_equal(x.val, b)
    (_, Label(_, y)) => datum_equal(a, y.val)
    (Nil, Nil) => true
    (Bool(x), Bool(y)) => x == y
    (Int(x), Int(y)) => x == y
    (BigInt(x), BigInt(y)) => x == y
    (Rat(ax, ay), Rat(bx, by)) => ax == bx && ay == by
    (BigRat(ax, ay), BigRat(bx, by)) => ax == bx && ay == by
    (Float(x), Float(y)) => x == y
    (Complex(ar, ai), Complex(br, bi)) =>
      datum_equal(ar.val, br.val) && datum_equal(ai.val, bi.val)
    (Char(x), Char(y)) => x == y
    (String(x), String(y)) => x.val == y.val
    (Symbol(x), Symbol(y)) => x == y
    (Pair(ax, ay), Pair(bx, by)) =>
      datum_equal(ax.val, bx.val) && datum_equal(ay.val, by.val)
    (Vector(ax), Vector(bx)) => array_datum_equal(ax, bx)
    (ByteVector(ax), ByteVector(bx)) => array_int_equal(ax, bx)
    (Record(ax), Record(bx)) => ax.id == bx.id
    (Condition(ax), Condition(bx)) => ax.id == bx.id
    (Value(ax), _) => value_eqv(ax, value_from_datum(b))
    (_, Value(bx)) => value_eqv(value_from_datum(a), bx)
    _ => false
  }
}

///|
fn is_quote_form(items : Array[@core.Datum]) -> Bool {
  match items {
    [Symbol("quote"), _] => true
    _ => false
  }
}

///|
fn should_rename(
  name : String,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  env : @core.Env,
) -> Bool {
  if name == "_" || name == ellipsis {
    false
  } else if is_literal(literals, name) {
    false
  } else if is_core_identifier(name) {
    false
  } else if @runtime.env_get_macro(env, name) is Some(_) {
    false
  } else if bindings.contains(name) {
    false
  } else {
    true
  }
}

///|
fn captured_value(def_env : @core.Env, name : String) -> @core.Value? {
  match @runtime.env_lookup_optional(def_env, name) {
    Some(Macro(_)) => None
    Some(SyntaxKeyword(_)) => None
    Some(value) => Some(value)
    None => None
  }
}

///|
fn build_rename_map(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  def_env : @core.Env,
  env : @core.Env,
) -> (Map[String, String], Map[String, @core.Value]) {
  let renames : Map[String, String] = {}
  let captures : Map[String, @core.Value] = {}
  collect_renames(
    template, bindings, literals, ellipsis, def_env, env, renames, captures,
  )
  (renames, captures)
}

///|
fn collect_renames(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  def_env : @core.Env,
  env : @core.Env,
  renames : Map[String, String],
  captures : Map[String, @core.Value],
) -> Unit {
  match template {
    Symbol(name) =>
      if should_rename(name, bindings, literals, ellipsis, env) &&
        !renames.contains(name) {
        let renamed = @runtime.gensym(name)
        renames[name] = renamed
        match captured_value(def_env, name) {
          Some(value) => captures[renamed] = value
          None => ()
        }
      }
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if !is_quote_form(items) {
            for item in items {
              collect_renames(
                item, bindings, literals, ellipsis, def_env, env, renames, captures,
              )
            }
          }
        None =>
          match template {
            Pair(a, b) => {
              collect_renames(
                a.val,
                bindings,
                literals,
                ellipsis,
                def_env,
                env,
                renames,
                captures,
              )
              collect_renames(
                b.val,
                bindings,
                literals,
                ellipsis,
                def_env,
                env,
                renames,
                captures,
              )
            }
            _ => ()
          }
      }
    Vector(items) =>
      for item in items {
        collect_renames(
          item, bindings, literals, ellipsis, def_env, env, renames, captures,
        )
      }
    _ => ()
  }
}

///|
fn collect_template_symbols(
  template : @core.Datum,
  symbols : Map[String, Bool],
) -> Unit {
  match template {
    Symbol(name) => symbols[name] = true
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if !is_quote_form(items) {
            for item in items {
              collect_template_symbols(item, symbols)
            }
          }
        None =>
          match template {
            Pair(a, b) => {
              collect_template_symbols(a.val, symbols)
              collect_template_symbols(b.val, symbols)
            }
            _ => ()
          }
      }
    Vector(items) =>
      for item in items {
        collect_template_symbols(item, symbols)
      }
    _ => ()
  }
}

///|
fn expand_with_syntax(
  items : Array[@core.Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
  template_forms : Bool,
) -> @core.Datum raise @core.EvalError {
  let (binding_expr, body_exprs) = match items {
    [_, binding_expr, body_expr] => (binding_expr, [body_expr])
    [_, binding_expr, body_expr, .. rest] => {
      let bodies : Array[@core.Datum] = [body_expr]
      for expr in rest {
        bodies.push(expr)
      }
      (binding_expr, bodies)
    }
    _ => raise EvalError("invalid with-syntax")
  }
  let binding_specs = datum_list_to_array(binding_expr)
  let expanded = bindings_clone(bindings)
  let fenv = template_env_with_bindings(env, bindings)
  for spec in binding_specs {
    let (pattern, expr) = match datum_list_to_array(spec) {
      [pattern, expr] => (pattern, expr)
      _ => raise EvalError("invalid with-syntax")
    }
    let value = eval_expr(expr, fenv)
    let datum = value_to_syntax_datum(value)
    let local_bindings : Map[String, BindingVal] = {}
    if !match_pattern(
        pattern,
        datum,
        [],
        ellipsis,
        local_bindings,
        None,
        env,
        env,
      ) {
      raise EvalError("invalid with-syntax")
    }
    if !merge_bindings(expanded, local_bindings) {
      raise EvalError("invalid with-syntax")
    }
  }
  match body_exprs {
    [body_expr] =>
      expand_template_indexed(
        body_expr, expanded, renames, ellipsis, ellipsis_active, error_message, indices,
        env, template_forms,
      )
    _ => {
      let forms : Array[@core.Datum] = [Symbol("begin")]
      for expr in body_exprs {
        forms.push(
          expand_template_indexed(
            expr, expanded, renames, ellipsis, ellipsis_active, error_message, indices,
            env, template_forms,
          ),
        )
      }
      list_from_array(forms)
    }
  }
}

///|
fn rename_datum(
  datum : @core.Datum,
  renames : Map[String, String],
) -> @core.Datum {
  match datum {
    Symbol(name) =>
      match renames.get(name) {
        Some(rename) => Symbol(rename)
        None => datum
      }
    Pair(a, b) =>
      pair_new(rename_datum(a.val, renames), rename_datum(b.val, renames))
    Vector(items) => Vector(items.map(item => rename_datum(item, renames)))
    Label(tag, value) => Label(tag, Ref::new(rename_datum(value.val, renames)))
    _ => datum
  }
}

///|
fn expand_template(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  env : @core.Env,
  template_forms : Bool,
) -> @core.Datum raise @core.EvalError {
  let indices : Array[Int] = []
  expand_template_indexed(
    template, bindings, renames, ellipsis, ellipsis_active, error_message, indices,
    env, template_forms,
  )
}

///|
fn expand_template_indexed(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
  template_forms : Bool,
) -> @core.Datum raise @core.EvalError {
  match template {
    Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            template
          } else if template_forms {
            match items {
              [head, expr] if datum_is_symbol(head, "syntax") =>
                expand_template_indexed(
                  expr, bindings, renames, ellipsis, ellipsis_active, error_message,
                  indices, env, template_forms,
                )
              [head, expr] if datum_is_symbol(head, "quasisyntax") => {
                let fenv = template_env_with_bindings(env, bindings)
                let expanded_template = expand_template_quasisyntax(
                  expr, bindings, renames, ellipsis, ellipsis_active, error_message,
                  indices, env,
                )
                let expanded = quasisyntax_expand(expanded_template, fenv, 1)
                rename_datum(expanded, renames)
              }
              [head, _, _, ..] if datum_is_symbol(head, "with-syntax") =>
                expand_with_syntax(
                  items, bindings, renames, ellipsis, ellipsis_active, error_message,
                  indices, env, template_forms,
                )
              _ =>
                expand_template_list(
                  items, bindings, renames, ellipsis, ellipsis_active, error_message,
                  indices, env, template_forms,
                )
            }
          } else {
            expand_template_list(
              items, bindings, renames, ellipsis, ellipsis_active, error_message,
              indices, env, template_forms,
            )
          }
        None =>
          match template {
            Pair(a, b) =>
              pair_new(
                expand_template_indexed(
                  a.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  indices,
                  env,
                  template_forms,
                ),
                expand_template_indexed(
                  b.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  indices,
                  env,
                  template_forms,
                ),
              )
            _ => template
          }
      }
    Vector(items) =>
      expand_template_vector(
        items, bindings, renames, ellipsis, ellipsis_active, error_message, indices,
        env, template_forms,
      )
    _ => template
  }
}

///|
fn expand_template_quasisyntax(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
) -> @core.Datum raise @core.EvalError {
  match template {
    Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            template
          } else {
            match items {
              [head, _] if datum_is_symbol(head, "unsyntax") => template
              [head, _] if datum_is_symbol(head, "unsyntax-splicing") =>
                template
              _ =>
                expand_template_list_quasisyntax(
                  items, bindings, renames, ellipsis, ellipsis_active, error_message,
                  indices, env,
                )
            }
          }
        None =>
          match template {
            Pair(a, b) =>
              pair_new(
                expand_template_quasisyntax(
                  a.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  indices,
                  env,
                ),
                expand_template_quasisyntax(
                  b.val,
                  bindings,
                  renames,
                  ellipsis,
                  ellipsis_active,
                  error_message,
                  indices,
                  env,
                ),
              )
            _ => template
          }
      }
    Vector(items) =>
      expand_template_vector_quasisyntax(
        items, bindings, renames, ellipsis, ellipsis_active, error_message, indices,
        env,
      )
    _ => template
  }
}

///|
fn expand_symbol(
  name : String,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  indices : Array[Int],
  error_message : String,
) -> @core.Datum raise @core.EvalError {
  match bindings.get(name) {
    Some(value) =>
      match binding_select(value, indices, 0) {
        Some(datum) => datum
        None => raise EvalError(error_message)
      }
    None =>
      match renames.get(name) {
        Some(rename) => Symbol(rename)
        None => Symbol(name)
      }
  }
}

///|
fn expand_template_list(
  items : Array[@core.Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
  template_forms : Bool,
) -> @core.Datum raise @core.EvalError {
  let expanded : Array[@core.Datum] = []
  for i = 0; i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      for j = 0; j < count; {
        // invariant : j >= 0 && j <= count
        // decreases : count - j
        // assert : j <= count
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_indexed(
            repeat_template, bindings, renames, ellipsis, ellipsis_active, error_message,
            next_indices, env, template_forms,
          ),
        )
        continue j + 1
      }
      continue i + 2
    } else {
      expanded.push(
        expand_template_indexed(
          items[i],
          bindings,
          renames,
          ellipsis,
          ellipsis_active,
          error_message,
          indices,
          env,
          template_forms,
        ),
      )
      continue i + 1
    }
  }
  list_from_array(expanded)
}

///|
fn expand_template_list_quasisyntax(
  items : Array[@core.Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
) -> @core.Datum raise @core.EvalError {
  let expanded : Array[@core.Datum] = []
  for i = 0; i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      for j = 0; j < count; {
        // invariant : j >= 0 && j <= count
        // decreases : count - j
        // assert : j <= count
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_quasisyntax(
            repeat_template, bindings, renames, ellipsis, ellipsis_active, error_message,
            next_indices, env,
          ),
        )
        continue j + 1
      }
      continue i + 2
    } else {
      expanded.push(
        expand_template_quasisyntax(
          items[i],
          bindings,
          renames,
          ellipsis,
          ellipsis_active,
          error_message,
          indices,
          env,
        ),
      )
      continue i + 1
    }
  }
  list_from_array(expanded)
}

///|
fn expand_template_vector(
  items : Array[@core.Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
  template_forms : Bool,
) -> @core.Datum raise @core.EvalError {
  let expanded : Array[@core.Datum] = []
  for i = 0; i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      for j = 0; j < count; {
        // invariant : j >= 0 && j <= count
        // decreases : count - j
        // assert : j <= count
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_indexed(
            repeat_template, bindings, renames, ellipsis, ellipsis_active, error_message,
            next_indices, env, template_forms,
          ),
        )
        continue j + 1
      }
      continue i + 2
    } else {
      expanded.push(
        expand_template_indexed(
          items[i],
          bindings,
          renames,
          ellipsis,
          ellipsis_active,
          error_message,
          indices,
          env,
          template_forms,
        ),
      )
      continue i + 1
    }
  }
  Vector(expanded)
}

///|
fn expand_template_vector_quasisyntax(
  items : Array[@core.Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : @core.Env,
) -> @core.Datum raise @core.EvalError {
  let expanded : Array[@core.Datum] = []
  for i = 0; i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      for j = 0; j < count; {
        // invariant : j >= 0 && j <= count
        // decreases : count - j
        // assert : j <= count
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_quasisyntax(
            repeat_template, bindings, renames, ellipsis, ellipsis_active, error_message,
            next_indices, env,
          ),
        )
        continue j + 1
      }
      continue i + 2
    } else {
      expanded.push(
        expand_template_quasisyntax(
          items[i],
          bindings,
          renames,
          ellipsis,
          ellipsis_active,
          error_message,
          indices,
          env,
        ),
      )
      continue i + 1
    }
  }
  Vector(expanded)
}

///|
fn max_binding_len(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  indices : Array[Int],
) -> Int {
  match template {
    Symbol(name) =>
      match bindings.get(name) {
        Some(value) => binding_len_at(value, indices, 0)
        None => 0
      }
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            0
          } else {
            let max_len = for i = 0, max_len = 0; i < items.length(); {
              // invariant : i >= 0 && i <= items.length()
              // decreases : items.length() - i
              // assert : max_len >= 0
              let len = max_binding_len(items[i], bindings, indices)
              let next = if len > max_len { len } else { max_len }
              continue i + 1, next
            } else {
              max_len
            }
            max_len
          }
        None =>
          match template {
            Pair(a, b) => {
              let left = max_binding_len(a.val, bindings, indices)
              let right = max_binding_len(b.val, bindings, indices)
              if left > right {
                left
              } else {
                right
              }
            }
            _ => 0
          }
      }
    Vector(items) => {
      let max_len = for i = 0, max_len = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : max_len >= 0
        let len = max_binding_len(items[i], bindings, indices)
        let next = if len > max_len { len } else { max_len }
        continue i + 1, next
      } else {
        max_len
      }
      max_len
    }
    _ => 0
  }
}

///|
fn template_has_repeat_binding(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  ellipsis : String,
  indices : Array[Int],
) -> Bool {
  match template {
    Symbol(name) =>
      if name == ellipsis {
        false
      } else {
        match bindings.get(name) {
          Some(value) => binding_has_repeat_at(value, indices, 0)
          None => false
        }
      }
    Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            false
          } else {
            let found = for i = 0; i < items.length(); {
              // invariant : i >= 0 && i <= items.length()
              // decreases : items.length() - i
              // assert : i <= items.length()
              if template_has_repeat_binding(
                  items[i],
                  bindings,
                  ellipsis,
                  indices,
                ) {
                break true
              }
              continue i + 1
            } else {
              false
            }
            found
          }
        None =>
          match template {
            Pair(a, b) =>
              template_has_repeat_binding(a.val, bindings, ellipsis, indices) ||
              template_has_repeat_binding(b.val, bindings, ellipsis, indices)
            _ => false
          }
      }
    Vector(items) => {
      let found = for i = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        if template_has_repeat_binding(items[i], bindings, ellipsis, indices) {
          break true
        }
        continue i + 1
      } else {
        false
      }
      found
    }
    _ => false
  }
}

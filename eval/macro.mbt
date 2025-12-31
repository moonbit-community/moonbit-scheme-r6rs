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
fn parse_syntax_rules(expr : Datum, env : Env) -> SyntaxRules raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 3 {
    raise @core.EvalError("invalid syntax-rules")
  }
  match parts[0] {
    Datum::Symbol("syntax-rules") => ()
    _ => raise @core.EvalError("invalid syntax-rules")
  }
  let (ellipsis, literal_idx) = match parts[1] {
    Datum::Symbol(name) => {
      if parts.length() < 4 {
        raise @core.EvalError("invalid syntax-rules")
      }
      (name, 2)
    }
    _ => ("...", 1)
  }
  let literals = parse_literal_list(parts[literal_idx])
  let ellipsis_active = !is_literal(literals, ellipsis)
  let rules : Array[SyntaxRule] = []
  for i = literal_idx + 1; i < parts.length(); {
    let rule_parts = datum_list_to_array(parts[i])
    if rule_parts.length() != 2 {
      raise @core.EvalError("invalid syntax-rules")
    }
    validate_pattern(
      rule_parts[0],
      literals,
      ellipsis,
      ellipsis_active,
      "invalid syntax-rules",
    )
    rules.push({ pattern: rule_parts[0], template: rule_parts[1], fender: None })
    continue i + 1
  } else {
    ()
  }
  {
    literals,
    rules,
    ellipsis,
    kind: SyntaxRulesKind::SyntaxRules,
    def_env: env,
  }
}

///|
fn parse_identifier_syntax(
  expr : Datum,
  name : String,
  env : Env,
) -> SyntaxRules raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 2 {
    raise @core.EvalError("invalid identifier-syntax")
  }
  match parts[0] {
    Datum::Symbol("identifier-syntax") => ()
    _ => raise @core.EvalError("invalid identifier-syntax")
  }
  let rules : Array[SyntaxRule] = []
  if parts.length() == 2 {
    rules.push({
      pattern: Datum::Symbol(name),
      template: parts[1],
      fender: None,
    })
  } else {
    for i = 1; i < parts.length(); {
      let rule_parts = datum_list_to_array(parts[i])
      if rule_parts.length() != 2 {
        raise @core.EvalError("invalid identifier-syntax")
      }
      rules.push({
        pattern: rule_parts[0],
        template: rule_parts[1],
        fender: None,
      })
      continue i + 1
    } else {
      ()
    }
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
  let literals : Array[String] = []
  for key in literal_set.keys() {
    if !pattern_vars.contains(key) {
      literals.push(key)
    }
  }
  {
    literals,
    rules,
    ellipsis: "...",
    kind: SyntaxRulesKind::SyntaxRules,
    def_env: env,
  }
}

///|
fn parse_syntax_case_rules(
  literals_expr : Datum,
  clauses : Array[Datum],
  env : Env,
) -> SyntaxRules raise EvalError {
  let literals = parse_literal_list(literals_expr)
  let ellipsis_active = !is_literal(literals, "...")
  let rules : Array[SyntaxRule] = []
  for i = 0; i < clauses.length(); {
    let rule_parts = datum_list_to_array(clauses[i])
    if rule_parts.length() == 2 {
      validate_pattern(
        rule_parts[0],
        literals,
        "...",
        ellipsis_active,
        "invalid syntax-case",
      )
      rules.push({
        pattern: rule_parts[0],
        template: rule_parts[1],
        fender: None,
      })
    } else if rule_parts.length() == 3 {
      validate_pattern(
        rule_parts[0],
        literals,
        "...",
        ellipsis_active,
        "invalid syntax-case",
      )
      rules.push({
        pattern: rule_parts[0],
        template: rule_parts[2],
        fender: Some(rule_parts[1]),
      })
    } else {
      raise @core.EvalError("invalid syntax-case clause")
    }
    continue i + 1
  } else {
    ()
  }
  {
    literals,
    rules,
    ellipsis: "...",
    kind: SyntaxRulesKind::SyntaxCase,
    def_env: env,
  }
}

///|
fn parse_syntax_case_transformer(
  expr : Datum,
  env : Env,
) -> SyntaxRules raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 3 {
    raise @core.EvalError("invalid syntax-case")
  }
  match parts[0] {
    Datum::Symbol("lambda") => ()
    _ => raise @core.EvalError("invalid syntax-case")
  }
  let params = datum_list_to_array(parts[1])
  if params.length() != 1 {
    raise @core.EvalError("invalid syntax-case")
  }
  let _ = parse_symbol_name(params[0])
  let body = parts.sub(start=2).to_array()
  if body.length() != 1 {
    raise @core.EvalError("invalid syntax-case")
  }
  let sc_parts = datum_list_to_array(body[0])
  if sc_parts.length() < 4 {
    raise @core.EvalError("invalid syntax-case")
  }
  match sc_parts[0] {
    Datum::Symbol("syntax-case") => ()
    _ => raise @core.EvalError("invalid syntax-case")
  }
  let literals_expr = sc_parts[2]
  let clauses = sc_parts.sub(start=3).to_array()
  parse_syntax_case_rules(literals_expr, clauses, env)
}

///|
fn is_syntax_case_transformer_expr(expr : Datum) -> Bool {
  match datum_list_to_array_opt(expr) {
    Some(parts) => {
      if parts.length() != 3 {
        return false
      }
      if !datum_is_symbol(parts[0], "lambda") {
        return false
      }
      let params = match datum_list_to_array_opt(parts[1]) {
        Some(items) => items
        None => return false
      }
      if params.length() != 1 {
        return false
      }
      match datum_list_to_array_opt(parts[2]) {
        Some(body_parts) =>
          body_parts.length() >= 4 &&
          datum_is_symbol(body_parts[0], "syntax-case")
        None => false
      }
    }
    None => false
  }
}

///|
fn eval_transformer_expr(expr : Datum, env : Env) -> Value raise EvalError {
  let eval_env = env_clone(env)
  eval_expr_transformer(expr, eval_env)
}

///|
fn parse_transformer(
  expr : Datum,
  name : String,
  env : Env,
) -> MacroTransformer raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid syntax transformer")
  }
  match parts[0] {
    Datum::Symbol("syntax-rules") =>
      MacroTransformer::Rules(parse_syntax_rules(expr, env))
    Datum::Symbol("identifier-syntax") =>
      MacroTransformer::Rules(parse_identifier_syntax(expr, name, env))
    Datum::Symbol("make-variable-transformer") => {
      if parts.length() != 2 {
        raise @core.EvalError("invalid syntax transformer")
      }
      parse_transformer(parts[1], name, env)
    }
    Datum::Symbol("lambda") if is_syntax_case_transformer_expr(expr) =>
      MacroTransformer::Rules(parse_syntax_case_transformer(expr, env))
    _ => {
      let value = eval_transformer_expr(expr, env)
      if !is_procedure_value(value) {
        raise @core.EvalError("type error: procedure expected")
      }
      MacroTransformer::Procedure(value, env)
    }
  }
}

///|
fn parse_literal_list(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  let literals : Array[String] = []
  for item in items {
    match item {
      Datum::Symbol(name) => literals.push(name)
      _ => raise @core.EvalError("invalid syntax-rules")
    }
  }
  literals
}

///|
fn parse_symbol_name(expr : Datum) -> String raise EvalError {
  match expr {
    Datum::Symbol(name) => name
    _ => raise @core.EvalError("invalid syntax-case")
  }
}

///|
fn is_literal(literals : Array[String], name : String) -> Bool {
  literals.contains(name)
}

///|
fn validate_pattern(
  pattern : Datum,
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
) -> Unit raise EvalError {
  let pat = datum_unlabel(datum_unwrap_syntax(pattern))
  match pat {
    Datum::Symbol(name) =>
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(pat) {
        Some(items) =>
          validate_pattern_list(
            items, literals, ellipsis, ellipsis_active, error_message,
          )
        None =>
          match pat {
            Datum::Pair(a, b) => {
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
    Datum::Vector(items) =>
      validate_pattern_list(
        items, literals, ellipsis, ellipsis_active, error_message,
      )
    Datum::Label(_, value) =>
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
  items : Array[Datum],
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
) -> Unit raise EvalError {
  for i = 0; i < items.length(); {
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise @core.EvalError(error_message)
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
  Leaf(Datum)
  Seq(Array[BindingVal])
}

///|
fn binding_equal(left : BindingVal, right : BindingVal) -> Bool {
  match (left, right) {
    (BindingVal::Leaf(a), BindingVal::Leaf(b)) => datum_equal(a, b)
    (BindingVal::Seq(a), BindingVal::Seq(b)) =>
      if a.length() != b.length() {
        false
      } else {
        for i = 0; i < a.length(); {
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
    BindingVal::Leaf(datum) => BindingVal::Leaf(datum)
    BindingVal::Seq(values) => {
      let cloned : Array[BindingVal] = []
      for item in values {
        cloned.push(binding_clone(item))
      }
      BindingVal::Seq(cloned)
    }
  }
}

///|
fn bindings_clone(
  bindings : Map[String, BindingVal],
) -> Map[String, BindingVal] {
  let result : Map[String, BindingVal] = {}
  for name in bindings.keys() {
    match bindings.get(name) {
      Some(value) => result[name] = binding_clone(value)
      None => ()
    }
  }
  result
}

///|
fn bindings_overwrite(
  dest : Map[String, BindingVal],
  src : Map[String, BindingVal],
) -> Unit {
  dest.clear()
  for name in src.keys() {
    match src.get(name) {
      Some(value) => dest[name] = value
      None => ()
    }
  }
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
) -> Datum? {
  match value {
    BindingVal::Leaf(datum) =>
      if depth < indices.length() {
        None
      } else {
        Some(datum)
      }
    BindingVal::Seq(values) =>
      if depth >= indices.length() || values.is_empty() {
        None
      } else {
        let idx = indices[depth]
        let actual = if idx < values.length() {
          idx
        } else {
          values.length() - 1
        }
        binding_select(values[actual], indices, depth + 1)
      }
  }
}

///|
fn binding_len_at(value : BindingVal, indices : Array[Int], depth : Int) -> Int {
  match value {
    BindingVal::Leaf(_) => 0
    BindingVal::Seq(values) =>
      if depth < indices.length() {
        if values.is_empty() {
          0
        } else {
          let idx = indices[depth]
          let actual = if idx < values.length() {
            idx
          } else {
            values.length() - 1
          }
          binding_len_at(values[actual], indices, depth + 1)
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
    BindingVal::Leaf(_) => false
    BindingVal::Seq(values) =>
      if depth < indices.length() {
        if values.is_empty() {
          false
        } else {
          let idx = indices[depth]
          let actual = if idx < values.length() {
            idx
          } else {
            values.length() - 1
          }
          binding_has_repeat_at(values[actual], indices, depth + 1)
        }
      } else {
        true
      }
  }
}

///|
fn binding_to_datum(value : BindingVal) -> Datum {
  match value {
    BindingVal::Leaf(datum) => datum
    BindingVal::Seq(values) => {
      let items : Array[Datum] = []
      for item in values {
        items.push(binding_to_datum(item))
      }
      list_from_array(items)
    }
  }
}

///|
fn fender_allows(
  expr : Datum,
  bindings : Map[String, BindingVal],
  env : Env,
) -> Bool raise EvalError {
  let fenv = env_extend(env)
  for name in bindings.keys() {
    match bindings.get(name) {
      Some(value) =>
        env_define(fenv, name, value_from_datum(binding_to_datum(value)))
      None => ()
    }
  }
  let result = eval_expr(expr, fenv)
  !is_false(result)
}

///|
fn template_env_with_bindings(
  env : Env,
  bindings : Map[String, BindingVal],
) -> Env {
  let fenv = env_extend(env)
  for name in bindings.keys() {
    match bindings.get(name) {
      Some(value) =>
        env_define(fenv, name, value_from_datum(binding_to_datum(value)))
      None => ()
    }
  }
  fenv
}

///|
fn value_to_syntax_datum(value : Value) -> Datum raise EvalError {
  match value {
    Value::SyntaxObject(_) | Value::Record(_) => value_to_datum_element(value)
    Value::Datum(d) => d
    _ => raise @core.EvalError("type error: datum expected")
  }
}

///|
priv struct MacroExpansion {
  datum : Datum
  captures : Map[String, Value]
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
  def_env : Env,
  renames : Map[String, String],
  captures : Map[String, Value],
) -> Datum {
  match renames.get(name) {
    Some(rename) => Datum::Symbol(rename)
    None =>
      match captured_value(def_env, name) {
        Some(value) => {
          let renamed = gensym(name)
          renames[name] = renamed
          captures[renamed] = value
          Datum::Symbol(renamed)
        }
        None => Datum::Symbol(name)
      }
  }
}

///|
fn rename_proc_datum(
  datum : Datum,
  def_env : Env,
  call_ctx : Int,
  renames : Map[String, String],
  captures : Map[String, Value],
) -> Datum {
  match datum {
    Datum::Symbol(name) => rename_proc_symbol(name, def_env, renames, captures)
    Datum::Pair(a, b) =>
      pair_new(
        rename_proc_datum(a.val, def_env, call_ctx, renames, captures),
        rename_proc_datum(b.val, def_env, call_ctx, renames, captures),
      )
    Datum::Vector(items) =>
      Datum::Vector(
        items.map((item) =>
          rename_proc_datum(item, def_env, call_ctx, renames, captures)
        ),
      )
    Datum::Label(tag, value) =>
      Datum::Label(
        tag,
        Ref::new(
          rename_proc_datum(value.val, def_env, call_ctx, renames, captures),
        ),
      )
    Datum::Complex(real, imag) =>
      Datum::Complex(
        Ref::new(
          rename_proc_datum(real.val, def_env, call_ctx, renames, captures),
        ),
        Ref::new(
          rename_proc_datum(imag.val, def_env, call_ctx, renames, captures),
        ),
      )
    Datum::Value(Value::SyntaxObject(obj)) =>
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
        Datum::Value(
          Value::SyntaxObject(SyntaxObject::{
            datum: renamed_inner,
            scopes: obj.scopes,
            binding_id: obj.binding_id,
          }),
        )
      }
    _ => datum
  }
}

///|
fn rename_proc_expansion(
  datum : Datum,
  def_env : Env,
  call_ctx : Int,
) -> MacroExpansion {
  let renames : Map[String, String] = {}
  let captures : Map[String, Value] = {}
  let renamed = rename_proc_datum(datum, def_env, call_ctx, renames, captures)
  MacroExpansion::{ datum: renamed, captures }
}

///|
fn expand_macro_rules(
  rules : SyntaxRules,
  call : Datum,
  env : Env,
) -> MacroExpansion raise EvalError {
  let call_scope = match rules.kind {
    SyntaxRulesKind::SyntaxRules => Some(next_syntax_context())
    SyntaxRulesKind::SyntaxCase => None
  }
  let ellipsis_active = !is_literal(rules.literals, rules.ellipsis)
  for i = 0; i < rules.rules.length(); {
    let rule = rules.rules[i]
    let bindings : Map[String, BindingVal] = {}
    match (rule.pattern, call) {
      (Datum::Symbol(_), Datum::Pair(_, _)) => ()
      (Datum::Pair(_, _), Datum::Symbol(_)) => ()
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
                SyntaxRulesKind::SyntaxCase => rules.def_env
                SyntaxRulesKind::SyntaxRules => env
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
            SyntaxRulesKind::SyntaxRules =>
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
            SyntaxRulesKind::SyntaxCase => {
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
          return MacroExpansion::{ datum: expanded, captures }
        }
    }
    continue i + 1
  }
  raise @core.EvalError("no matching syntax-rules pattern")
}

///|
fn expand_macro_proc(
  proc : Value,
  def_env : Env,
  call : Datum,
  env : Env,
) -> MacroExpansion raise EvalError {
  if !is_procedure_value(proc) {
    raise @core.EvalError("type error: procedure expected")
  }
  let temp_name = gensym("macro_proc")
  let fenv = env_extend(env)
  env_define(fenv, temp_name, proc)
  let call_ctx = next_syntax_context()
  let marked_call = syntax_add_scope(call, call_ctx)
  let stx = match marked_call {
    Datum::Value(Value::SyntaxObject(obj)) => Value::SyntaxObject(obj)
    _ =>
      Value::SyntaxObject(SyntaxObject::{
        datum: marked_call,
        scopes: [call_ctx],
        binding_id: None,
      })
  }
  let call_expr = list_from_array([Datum::Symbol(temp_name), Datum::Value(stx)])
  let value = eval_expr_transformer(call_expr, fenv)
  let datum = value_to_syntax_datum(value)
  rename_proc_expansion(datum, def_env, call_ctx)
}

///|
fn expand_macro(
  transformer : MacroTransformer,
  call : Datum,
  env : Env,
) -> MacroExpansion raise EvalError {
  match transformer {
    MacroTransformer::Rules(rules) => expand_macro_rules(rules, call, env)
    MacroTransformer::Procedure(proc, def_env) =>
      expand_macro_proc(proc, def_env, call, env)
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
fn syntax_add_scope_with_env(datum : Datum, scope : Int, env : Env) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{
          datum: Datum::Symbol(name),
          scopes: [scope],
          binding_id: env_binding_id_optional(env, name),
        }),
      )
    Datum::Value(Value::SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        Datum::Symbol(_) => obj.datum
        _ => syntax_add_scope_with_env(obj.datum, scope, env)
      }
      let binding_id = match obj.binding_id {
        Some(_) => obj.binding_id
        None =>
          match obj.datum {
            Datum::Symbol(name) => env_binding_id_optional(env, name)
            _ => None
          }
      }
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{
          datum: inner,
          scopes: next_scopes,
          binding_id,
        }),
      )
    }
    Datum::Pair(a, b) =>
      pair_new(
        syntax_add_scope_with_env(a.val, scope, env),
        syntax_add_scope_with_env(b.val, scope, env),
      )
    Datum::Vector(items) =>
      Datum::Vector(
        items.map((item) => syntax_add_scope_with_env(item, scope, env)),
      )
    Datum::Complex(real, imag) =>
      Datum::Complex(
        Ref::new(syntax_add_scope_with_env(real.val, scope, env)),
        Ref::new(syntax_add_scope_with_env(imag.val, scope, env)),
      )
    _ => datum
  }
}

///|
fn wrap_pattern_datum(value : Datum, scope : Int?, env : Env) -> Datum {
  match scope {
    Some(call_scope) => {
      let marked = syntax_add_scope_with_env(value, call_scope, env)
      match marked {
        Datum::Value(Value::SyntaxObject(_)) => marked
        _ =>
          Datum::Value(
            Value::SyntaxObject(SyntaxObject::{
              datum: marked,
              scopes: [call_scope],
              binding_id: None,
            }),
          )
      }
    }
    None =>
      match value {
        Datum::Value(Value::SyntaxObject(_)) => value
        _ => {
          let binding_id = match datum_unlabel(value) {
            Datum::Symbol(name) => env_binding_id_optional(env, name)
            _ => None
          }
          Datum::Value(
            Value::SyntaxObject(SyntaxObject::{
              datum: value,
              scopes: [],
              binding_id,
            }),
          )
        }
      }
  }
}

///|
fn rewrite_template_expr(
  expr : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  env : Env,
) -> Datum raise EvalError {
  match expr {
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(expr) {
        Some(items) => {
          if !items.is_empty() {
            let head = items[0]
            let mut is_template_form = false
            if datum_is_symbol(head, "syntax") {
              is_template_form = true
            } else if datum_is_symbol(head, "quasisyntax") {
              is_template_form = true
            } else if datum_is_symbol(head, "with-syntax") {
              is_template_form = true
            }
            if is_template_form {
              let indices : Array[Int] = []
              let expanded = expand_template_indexed(
                expr, bindings, renames, ellipsis, ellipsis_active, error_message,
                indices, env, true,
              )
              match expanded {
                Datum::Value(Value::SyntaxObject(obj)) =>
                  return Datum::Value(Value::SyntaxObject(obj))
                _ => {
                  let scope = next_syntax_context()
                  let scopes = [scope]
                  let datum = syntax_wrap_root(expanded, scopes)
                  let obj = SyntaxObject::{ datum, scopes, binding_id: None }
                  return Datum::Value(Value::SyntaxObject(obj))
                }
              }
            }
          }
          let rewritten : Array[Datum] = []
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
            Datum::Pair(a, b) =>
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
    Datum::Vector(items) =>
      Datum::Vector(
        items.map((item) =>
          rewrite_template_expr(
            item, bindings, renames, ellipsis, ellipsis_active, error_message, env,
          )
        ),
      )
    _ => expr
  }
}

///|
fn match_pattern(
  pattern : Datum,
  input : Datum,
  literals : Array[String],
  ellipsis : String,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : Env,
  use_env : Env,
) -> Bool {
  let pat = datum_unlabel(datum_unwrap_syntax(pattern))
  let inp = datum_unlabel(datum_unwrap_syntax(input))
  match pat {
    Datum::Label(_, _) => false
    Datum::Symbol(name) =>
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
    Datum::Nil => inp is Datum::Nil
    Datum::Bool(b) =>
      match inp {
        Datum::Bool(other) => other == b
        _ => false
      }
    Datum::Int(n) =>
      match inp {
        Datum::Int(other) => other == n
        _ => false
      }
    Datum::BigInt(n) =>
      match inp {
        Datum::BigInt(other) => other == n
        _ => false
      }
    Datum::Rat(n, d) =>
      match inp {
        Datum::Rat(on, od) => on == n && od == d
        _ => false
      }
    Datum::BigRat(n, d) =>
      match inp {
        Datum::BigRat(on, od) => on == n && od == d
        _ => false
      }
    Datum::Complex(real, imag) =>
      match inp {
        Datum::Complex(other_real, other_imag) =>
          datum_equal(real.val, other_real.val) &&
          datum_equal(imag.val, other_imag.val)
        _ => false
      }
    Datum::Float(f) =>
      match inp {
        Datum::Float(of) => of == f
        _ => false
      }
    Datum::Char(ch) =>
      match inp {
        Datum::Char(other) => other == ch
        _ => false
      }
    Datum::String(s) =>
      match inp {
        Datum::String(other) => other.val == s.val
        _ => false
      }
    Datum::Vector(items) =>
      match inp {
        Datum::Vector(other) =>
          match_pattern_list(
            items, other, literals, ellipsis, bindings, scope, def_env, use_env,
          )
        _ => false
      }
    Datum::ByteVector(items) =>
      match inp {
        Datum::ByteVector(other) => array_int_equal(items, other)
        _ => false
      }
    Datum::Record(rec) =>
      match inp {
        Datum::Record(other) => rec.id == other.id
        _ => false
      }
    Datum::Condition(cond) =>
      match inp {
        Datum::Condition(other) => cond.id == other.id
        _ => false
      }
    Datum::Value(value) => value_eqv(value, value_from_datum(inp))
    Datum::Pair(pa, pb) =>
      match (datum_list_to_array_opt(pat), datum_list_to_array_opt(inp)) {
        (Some(pats), Some(inputs)) =>
          match_pattern_list(
            pats, inputs, literals, ellipsis, bindings, scope, def_env, use_env,
          )
        _ =>
          match inp {
            Datum::Pair(ia, ib) =>
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
  pats : Array[Datum],
  inputs : Array[Datum],
  literals : Array[String],
  ellipsis : String,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : Env,
  use_env : Env,
) -> Bool {
  let ellipsis_active = !is_literal(literals, ellipsis)
  match_pattern_list_from(
    pats, inputs, 0, 0, literals, ellipsis, ellipsis_active, bindings, scope, def_env,
    use_env,
  )
}

///|
fn collect_repeat_bindings(
  repeat_pat : Datum,
  inputs : Array[Datum],
  start : Int,
  count : Int,
  literals : Array[String],
  ellipsis : String,
  scope : Int?,
  def_env : Env,
  use_env : Env,
) -> Map[String, BindingVal]? {
  let result : Map[String, BindingVal] = {}
  if count == 0 {
    let names : Map[String, Bool] = {}
    collect_pattern_vars(repeat_pat, literals, ellipsis, names)
    for name in names.keys() {
      result[name] = BindingVal::Seq([])
    }
    return Some(result)
  }
  let repeated : Map[String, Array[BindingVal]] = {}
  for i = 0; i < count; {
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
    for name in iter_bindings.keys() {
      match iter_bindings.get(name) {
        Some(value) =>
          match repeated.get(name) {
            Some(values) => values.push(value)
            None => {
              let values : Array[BindingVal] = []
              values.push(value)
              repeated[name] = values
            }
          }
        None => ()
      }
    }
    continue i + 1
  }
  for name in repeated.keys() {
    match repeated.get(name) {
      Some(values) => result[name] = BindingVal::Seq(values)
      None => ()
    }
  }
  Some(result)
}

///|
fn match_pattern_list_from(
  pats : Array[Datum],
  inputs : Array[Datum],
  pat_idx : Int,
  inp_idx : Int,
  literals : Array[String],
  ellipsis : String,
  ellipsis_active : Bool,
  bindings : Map[String, BindingVal],
  scope : Int?,
  def_env : Env,
  use_env : Env,
) -> Bool {
  if pat_idx >= pats.length() {
    return inp_idx == inputs.length()
  }
  if ellipsis_active &&
    pat_idx + 1 < pats.length() &&
    datum_is_symbol(pats[pat_idx + 1], ellipsis) {
    let max_count = inputs.length() - inp_idx
    let mut count = max_count
    while count >= 0 {
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
      count = count - 1
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
  value : Datum,
  bindings : Map[String, BindingVal],
) -> Bool {
  let leaf = BindingVal::Leaf(value)
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
  pattern : Datum,
  literals : Array[String],
  ellipsis : String,
  names : Map[String, Bool],
) -> Unit {
  match pattern {
    Datum::Symbol(name) =>
      if name != "_" && name != ellipsis && !is_literal(literals, name) {
        names[name] = true
      }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(pattern) {
        Some(items) =>
          for item in items {
            collect_pattern_vars(item, literals, ellipsis, names)
          }
        None =>
          match pattern {
            Datum::Pair(a, b) => {
              collect_pattern_vars(a.val, literals, ellipsis, names)
              collect_pattern_vars(b.val, literals, ellipsis, names)
            }
            _ => ()
          }
      }
    Datum::Vector(items) =>
      for item in items {
        collect_pattern_vars(item, literals, ellipsis, names)
      }
    _ => ()
  }
}

///|
fn datum_list_to_array_opt(list : Datum) -> Array[Datum]? {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match datum_unlabel(cur) {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a.val)
        cur = b.val
      }
      _ => return None
    }
  }
  Some(items)
}

///|
fn datum_unwrap_syntax(value : Datum) -> Datum {
  match value {
    Datum::Value(Value::SyntaxObject(obj)) => obj.datum
    _ => value
  }
}

///|
fn datum_is_symbol(value : Datum, name : String) -> Bool {
  match datum_unlabel(datum_unwrap_syntax(value)) {
    Datum::Symbol(sym) => sym == name
    _ => false
  }
}

///|
fn literal_matches(
  name : String,
  input : Datum,
  def_env : Env,
  use_env : Env,
) -> Bool {
  let unlabel = datum_unlabel(input)
  match unlabel {
    Datum::Value(Value::SyntaxObject(obj)) =>
      match datum_unlabel(obj.datum) {
        Datum::Symbol(other) => {
          if other != name {
            return false
          }
          let def_id = env_binding_id_optional(def_env, name)
          match (def_id, obj.binding_id) {
            (Some(left), Some(right)) => left == right
            (None, None) => scopes_equal_macro(obj.scopes, [])
            _ => false
          }
        }
        _ => false
      }
    _ =>
      match datum_unlabel(datum_unwrap_syntax(unlabel)) {
        Datum::Symbol(other) => {
          if other != name {
            return false
          }
          let def_id = env_binding_id_optional(def_env, name)
          let use_id = env_binding_id_optional(use_env, name)
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
fn array_datum_equal(left : Array[Datum], right : Array[Datum]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    for i = 0; i < left.length(); {
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
fn datum_equal(a : Datum, b : Datum) -> Bool {
  match (a, b) {
    (Datum::Label(_, x), _) => datum_equal(x.val, b)
    (_, Datum::Label(_, y)) => datum_equal(a, y.val)
    (Datum::Nil, Datum::Nil) => true
    (Datum::Bool(x), Datum::Bool(y)) => x == y
    (Datum::Int(x), Datum::Int(y)) => x == y
    (Datum::BigInt(x), Datum::BigInt(y)) => x == y
    (Datum::Rat(ax, ay), Datum::Rat(bx, by)) => ax == bx && ay == by
    (Datum::BigRat(ax, ay), Datum::BigRat(bx, by)) => ax == bx && ay == by
    (Datum::Float(x), Datum::Float(y)) => x == y
    (Datum::Complex(ar, ai), Datum::Complex(br, bi)) =>
      datum_equal(ar.val, br.val) && datum_equal(ai.val, bi.val)
    (Datum::Char(x), Datum::Char(y)) => x == y
    (Datum::String(x), Datum::String(y)) => x.val == y.val
    (Datum::Symbol(x), Datum::Symbol(y)) => x == y
    (Datum::Pair(ax, ay), Datum::Pair(bx, by)) =>
      datum_equal(ax.val, bx.val) && datum_equal(ay.val, by.val)
    (Datum::Vector(ax), Datum::Vector(bx)) => array_datum_equal(ax, bx)
    (Datum::ByteVector(ax), Datum::ByteVector(bx)) => array_int_equal(ax, bx)
    (Datum::Record(ax), Datum::Record(bx)) => ax.id == bx.id
    (Datum::Condition(ax), Datum::Condition(bx)) => ax.id == bx.id
    (Datum::Value(ax), _) => value_eqv(ax, value_from_datum(b))
    (_, Datum::Value(bx)) => value_eqv(value_from_datum(a), bx)
    _ => false
  }
}

///|
fn is_quote_form(items : Array[Datum]) -> Bool {
  if items.length() != 2 {
    false
  } else {
    match items[0] {
      Datum::Symbol("quote") => true
      _ => false
    }
  }
}

///|
fn should_rename(
  name : String,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  env : Env,
) -> Bool {
  if name == "_" || name == ellipsis {
    false
  } else if is_literal(literals, name) {
    false
  } else if is_core_identifier(name) {
    false
  } else if env_get_macro(env, name) is Some(_) {
    false
  } else if bindings.contains(name) {
    false
  } else {
    true
  }
}

///|
fn captured_value(def_env : Env, name : String) -> Value? {
  match env_lookup_optional(def_env, name) {
    Some(Value::Macro(_)) => None
    Some(Value::SyntaxKeyword(_)) => None
    Some(value) => Some(value)
    None => None
  }
}

///|
fn build_rename_map(
  template : Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  def_env : Env,
  env : Env,
) -> (Map[String, String], Map[String, Value]) {
  let renames : Map[String, String] = {}
  let captures : Map[String, Value] = {}
  collect_renames(
    template, bindings, literals, ellipsis, def_env, env, renames, captures,
  )
  (renames, captures)
}

///|
fn collect_renames(
  template : Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  def_env : Env,
  env : Env,
  renames : Map[String, String],
  captures : Map[String, Value],
) -> Unit {
  match template {
    Datum::Symbol(name) =>
      if should_rename(name, bindings, literals, ellipsis, env) &&
        !renames.contains(name) {
        let renamed = gensym(name)
        renames[name] = renamed
        match captured_value(def_env, name) {
          Some(value) => captures[renamed] = value
          None => ()
        }
      }
    Datum::Pair(_, _) =>
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
            Datum::Pair(a, b) => {
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
    Datum::Vector(items) =>
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
  template : Datum,
  symbols : Map[String, Bool],
) -> Unit {
  match template {
    Datum::Symbol(name) => symbols[name] = true
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if !is_quote_form(items) {
            for item in items {
              collect_template_symbols(item, symbols)
            }
          }
        None =>
          match template {
            Datum::Pair(a, b) => {
              collect_template_symbols(a.val, symbols)
              collect_template_symbols(b.val, symbols)
            }
            _ => ()
          }
      }
    Datum::Vector(items) =>
      for item in items {
        collect_template_symbols(item, symbols)
      }
    _ => ()
  }
}

///|
fn expand_with_syntax(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
  template_forms : Bool,
) -> Datum raise EvalError {
  if items.length() < 3 {
    raise @core.EvalError("invalid with-syntax")
  }
  let binding_specs = datum_list_to_array(items[1])
  let expanded = bindings_clone(bindings)
  let fenv = template_env_with_bindings(env, bindings)
  for spec in binding_specs {
    let parts = datum_list_to_array(spec)
    if parts.length() != 2 {
      raise @core.EvalError("invalid with-syntax")
    }
    let pattern = parts[0]
    let expr = parts[1]
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
      raise @core.EvalError("invalid with-syntax")
    }
    if !merge_bindings(expanded, local_bindings) {
      raise @core.EvalError("invalid with-syntax")
    }
  }
  if items.length() == 3 {
    expand_template_indexed(
      items[2],
      expanded,
      renames,
      ellipsis,
      ellipsis_active,
      error_message,
      indices,
      env,
      template_forms,
    )
  } else {
    let forms : Array[Datum] = [Datum::Symbol("begin")]
    for i = 2; i < items.length(); {
      forms.push(
        expand_template_indexed(
          items[i],
          expanded,
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
    list_from_array(forms)
  }
}

///|
fn rename_datum(datum : Datum, renames : Map[String, String]) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      match renames.get(name) {
        Some(rename) => Datum::Symbol(rename)
        None => datum
      }
    Datum::Pair(a, b) =>
      pair_new(rename_datum(a.val, renames), rename_datum(b.val, renames))
    Datum::Vector(items) =>
      Datum::Vector(items.map((item) => rename_datum(item, renames)))
    Datum::Label(tag, value) =>
      Datum::Label(tag, Ref::new(rename_datum(value.val, renames)))
    _ => datum
  }
}

///|
fn expand_template(
  template : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  env : Env,
  template_forms : Bool,
) -> Datum raise EvalError {
  let indices : Array[Int] = []
  expand_template_indexed(
    template, bindings, renames, ellipsis, ellipsis_active, error_message, indices,
    env, template_forms,
  )
}

///|
fn expand_template_indexed(
  template : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
  template_forms : Bool,
) -> Datum raise EvalError {
  match template {
    Datum::Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            template
          } else if template_forms &&
            items.length() == 2 &&
            datum_is_symbol(items[0], "syntax") {
            expand_template_indexed(
              items[1],
              bindings,
              renames,
              ellipsis,
              ellipsis_active,
              error_message,
              indices,
              env,
              template_forms,
            )
          } else if template_forms &&
            items.length() == 2 &&
            datum_is_symbol(items[0], "quasisyntax") {
            let fenv = template_env_with_bindings(env, bindings)
            let expanded_template = expand_template_quasisyntax(
              items[1],
              bindings,
              renames,
              ellipsis,
              ellipsis_active,
              error_message,
              indices,
              env,
            )
            let expanded = quasisyntax_expand(expanded_template, fenv, 1)
            rename_datum(expanded, renames)
          } else if template_forms &&
            items.length() >= 3 &&
            datum_is_symbol(items[0], "with-syntax") {
            expand_with_syntax(
              items, bindings, renames, ellipsis, ellipsis_active, error_message,
              indices, env, template_forms,
            )
          } else {
            expand_template_list(
              items, bindings, renames, ellipsis, ellipsis_active, error_message,
              indices, env, template_forms,
            )
          }
        None =>
          match template {
            Datum::Pair(a, b) =>
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
    Datum::Vector(items) =>
      expand_template_vector(
        items, bindings, renames, ellipsis, ellipsis_active, error_message, indices,
        env, template_forms,
      )
    _ => template
  }
}

///|
fn expand_template_quasisyntax(
  template : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
) -> Datum raise EvalError {
  match template {
    Datum::Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            template
          } else if items.length() == 2 && datum_is_symbol(items[0], "unsyntax") {
            template
          } else if items.length() == 2 &&
            datum_is_symbol(items[0], "unsyntax-splicing") {
            template
          } else {
            expand_template_list_quasisyntax(
              items, bindings, renames, ellipsis, ellipsis_active, error_message,
              indices, env,
            )
          }
        None =>
          match template {
            Datum::Pair(a, b) =>
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
    Datum::Vector(items) =>
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
) -> Datum raise EvalError {
  match bindings.get(name) {
    Some(value) =>
      match binding_select(value, indices, 0) {
        Some(datum) => datum
        None => raise @core.EvalError(error_message)
      }
    None =>
      match renames.get(name) {
        Some(rename) => Datum::Symbol(rename)
        None => Datum::Symbol(name)
      }
  }
}

///|
fn expand_template_list(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
  template_forms : Bool,
) -> Datum raise EvalError {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise @core.EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise @core.EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
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
        j = j + 1
      }
      i = i + 2
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
      i = i + 1
    }
  }
  list_from_array(expanded)
}

///|
fn expand_template_list_quasisyntax(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
) -> Datum raise EvalError {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise @core.EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise @core.EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
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
        j = j + 1
      }
      i = i + 2
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
      i = i + 1
    }
  }
  list_from_array(expanded)
}

///|
fn expand_template_vector(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
  template_forms : Bool,
) -> Datum raise EvalError {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise @core.EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise @core.EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
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
        j = j + 1
      }
      i = i + 2
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
      i = i + 1
    }
  }
  Datum::Vector(expanded)
}

///|
fn expand_template_vector_quasisyntax(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  indices : Array[Int],
  env : Env,
) -> Datum raise EvalError {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if ellipsis_active && datum_is_symbol(items[i], ellipsis) {
      raise @core.EvalError(error_message)
    }
    if ellipsis_active &&
      i + 1 < items.length() &&
      datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      if !template_has_repeat_binding(
          repeat_template, bindings, ellipsis, indices,
        ) {
        raise @core.EvalError(error_message)
      }
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
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
        j = j + 1
      }
      i = i + 2
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
      i = i + 1
    }
  }
  Datum::Vector(expanded)
}

///|
fn max_binding_len(
  template : Datum,
  bindings : Map[String, BindingVal],
  indices : Array[Int],
) -> Int {
  match template {
    Datum::Symbol(name) =>
      match bindings.get(name) {
        Some(value) => binding_len_at(value, indices, 0)
        None => 0
      }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            0
          } else {
            let mut max_len = 0
            for item in items {
              let len = max_binding_len(item, bindings, indices)
              if len > max_len {
                max_len = len
              }
            }
            max_len
          }
        None =>
          match template {
            Datum::Pair(a, b) => {
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
    Datum::Vector(items) => {
      let mut max_len = 0
      for item in items {
        let len = max_binding_len(item, bindings, indices)
        if len > max_len {
          max_len = len
        }
      }
      max_len
    }
    _ => 0
  }
}

///|
fn template_has_repeat_binding(
  template : Datum,
  bindings : Map[String, BindingVal],
  ellipsis : String,
  indices : Array[Int],
) -> Bool {
  match template {
    Datum::Symbol(name) =>
      if name == ellipsis {
        false
      } else {
        match bindings.get(name) {
          Some(value) => binding_has_repeat_at(value, indices, 0)
          None => false
        }
      }
    Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            false
          } else {
            let mut found = false
            for item in items {
              if template_has_repeat_binding(item, bindings, ellipsis, indices) {
                found = true
                break
              }
            }
            found
          }
        None =>
          match template {
            Datum::Pair(a, b) =>
              template_has_repeat_binding(a.val, bindings, ellipsis, indices) ||
              template_has_repeat_binding(b.val, bindings, ellipsis, indices)
            _ => false
          }
      }
    Datum::Vector(items) => {
      let mut found = false
      for item in items {
        if template_has_repeat_binding(item, bindings, ellipsis, indices) {
          found = true
          break
        }
      }
      found
    }
    _ => false
  }
}

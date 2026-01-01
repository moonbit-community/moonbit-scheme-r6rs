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
fn parse_syntax_rules(expr : @core.Datum, env : @core.Env) -> @core.SyntaxRules raise @core.EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 3 {
    raise @core.EvalError("invalid syntax-rules")
  }
  match parts[0] {
    @core.Datum::Symbol("syntax-rules") => ()
    _ => raise @core.EvalError("invalid syntax-rules")
  }
  let (ellipsis, literal_idx) = match parts[1] {
    @core.Datum::Symbol(name) => {
      if parts.length() < 4 {
        raise @core.EvalError("invalid syntax-rules")
      }
      (name, 2)
    }
    _ => ("...", 1)
  }
  let literals = parse_literal_list(parts[literal_idx])
  let ellipsis_active = !is_literal(literals, ellipsis)
  let rules : Array[@core.SyntaxRule] = []
  for i = literal_idx + 1; i < parts.length(); {
    // invariant : i >= literal_idx + 1 && i <= parts.length()
    // decreases : parts.length() - i
    // assert : i < parts.length()
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
    kind: @core.SyntaxRules,
    def_env: env,
  }
}

///|
fn parse_identifier_syntax(
  expr : @core.Datum,
  name : String,
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 2 {
    raise @core.EvalError("invalid identifier-syntax")
  }
  match parts[0] {
    @core.Datum::Symbol("identifier-syntax") => ()
    _ => raise @core.EvalError("invalid identifier-syntax")
  }
  let rules : Array[@core.SyntaxRule] = []
  if parts.length() == 2 {
    rules.push({
      pattern: @core.Datum::Symbol(name),
      template: parts[1],
      fender: None,
    })
  } else {
    for i = 1; i < parts.length(); {
      // invariant : i >= 1 && i <= parts.length()
      // decreases : parts.length() - i
      // assert : i < parts.length()
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
  let literals : Array[String] =
    literal_set
      .keys()
      .filter((key) => !pattern_vars.contains(key))
      .to_array()
  {
    literals,
    rules,
    ellipsis: "...",
    kind: @core.SyntaxRules,
    def_env: env,
  }
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
    kind: SyntaxCase,
    def_env: env,
  }
}

///|
fn parse_syntax_case_transformer(
  expr : @core.Datum,
  env : @core.Env,
) -> @core.SyntaxRules raise @core.EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 3 {
    raise @core.EvalError("invalid syntax-case")
  }
  match parts[0] {
    @core.Datum::Symbol("lambda") => ()
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
    @core.Datum::Symbol("syntax-case") => ()
    _ => raise @core.EvalError("invalid syntax-case")
  }
  let literals_expr = sc_parts[2]
  let clauses = sc_parts.sub(start=3).to_array()
  parse_syntax_case_rules(literals_expr, clauses, env)
}

///|
fn is_syntax_case_transformer_expr(expr : @core.Datum) -> Bool {
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
fn eval_transformer_expr(expr : @core.Datum, env : @core.Env) -> @core.Value raise @core.EvalError {
  let eval_env = @runtime.env_clone(env)
  eval_expr_transformer(expr, eval_env)
}

///|
fn parse_transformer(
  expr : @core.Datum,
  name : String,
  env : @core.Env,
) -> @core.MacroTransformer raise @core.EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid syntax transformer")
  }
  match parts[0] {
    @core.Datum::Symbol("syntax-rules") =>
      Rules(parse_syntax_rules(expr, env))
    @core.Datum::Symbol("identifier-syntax") =>
      Rules(parse_identifier_syntax(expr, name, env))
    @core.Datum::Symbol("make-variable-transformer") => {
      if parts.length() != 2 {
        raise @core.EvalError("invalid syntax transformer")
      }
      parse_transformer(parts[1], name, env)
    }
    @core.Datum::Symbol("lambda") if is_syntax_case_transformer_expr(expr) =>
      Rules(parse_syntax_case_transformer(expr, env))
    _ => {
      let value = eval_transformer_expr(expr, env)
      if !@runtime.is_procedure_value(value) {
        raise @core.EvalError("type error: procedure expected")
      }
      Procedure(value, env)
    }
  }
}

///|
fn parse_literal_list(expr : @core.Datum) -> Array[String] raise @core.EvalError {
  let items = datum_list_to_array(expr)
  let literals : Array[String] = []
  for item in items {
    match item {
      @core.Datum::Symbol(name) => literals.push(name)
      _ => raise @core.EvalError("invalid syntax-rules")
    }
  }
  literals
}

///|
fn parse_symbol_name(expr : @core.Datum) -> String raise @core.EvalError {
  match expr {
    @core.Datum::Symbol(name) => name
    _ => raise @core.EvalError("invalid syntax-case")
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
    @core.Datum::Symbol(name) =>
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
    @core.Datum::Pair(_, _) =>
      match datum_list_to_array_opt(pat) {
        Some(items) =>
          validate_pattern_list(
            items, literals, ellipsis, ellipsis_active, error_message,
          )
        None =>
          match pat {
            @core.Datum::Pair(a, b) => {
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
    @core.Datum::Vector(items) =>
      validate_pattern_list(
        items, literals, ellipsis, ellipsis_active, error_message,
      )
    @core.Datum::Label(_, value) =>
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
      let cloned : Array[BindingVal] = values.map((item) => binding_clone(item))
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
    Leaf(datum) =>
      if depth < indices.length() {
        None
      } else {
        Some(datum)
      }
    Seq(values) =>
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
    Leaf(_) => 0
    Seq(values) =>
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
    Leaf(_) => false
    Seq(values) =>
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
fn binding_to_datum(value : BindingVal) -> @core.Datum {
  match value {
    Leaf(datum) => datum
    Seq(values) => {
      let items : Array[@core.Datum] =
        values.map((item) => binding_to_datum(item))
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
  bindings.iter2().each((name, value) =>
    @runtime.env_define(fenv, name, value_from_datum(binding_to_datum(value)))
  )
  let result = eval_expr(expr, fenv)
  !is_false(result)
}

///|
fn template_env_with_bindings(
  env : @core.Env,
  bindings : Map[String, BindingVal],
) -> @core.Env {
  let fenv = @runtime.env_extend(env)
  bindings.iter2().each((name, value) =>
    @runtime.env_define(fenv, name, value_from_datum(binding_to_datum(value)))
  )
  fenv
}

///|
fn value_to_syntax_datum(value : @core.Value) -> @core.Datum raise @core.EvalError {
  match value {
    @core.Value::SyntaxObject(_) | @core.Value::Record(_) => value_to_datum_element(value)
    @core.Value::Datum(d) => d
    _ => raise @core.EvalError("type error: datum expected")
  }
}

///|
priv struct MacroExpansion {
  datum : @core.Datum
  captures : Map[String,@core.Value]
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
  captures : Map[String,@core.Value],
) -> @core.Datum {
  match renames.get(name) {
    Some(rename) => @core.Datum::Symbol(rename)
    None =>
      match captured_value(def_env, name) {
        Some(value) => {
          let renamed = @runtime.gensym(name)
          renames[name] = renamed
          captures[renamed] = value
          @core.Datum::Symbol(renamed)
        }
        None => @core.Datum::Symbol(name)
      }
  }
}

///|
fn rename_proc_datum(
  datum : @core.Datum,
  def_env : @core.Env,
  call_ctx : Int,
  renames : Map[String, String],
  captures : Map[String,@core.Value],
) -> @core.Datum {
  match datum {
    @core.Datum::Symbol(name) => rename_proc_symbol(name, def_env, renames, captures)
    @core.Datum::Pair(a, b) =>
      pair_new(
        rename_proc_datum(a.val, def_env, call_ctx, renames, captures),
        rename_proc_datum(b.val, def_env, call_ctx, renames, captures),
      )
    @core.Datum::Vector(items) =>
      @core.Datum::Vector(
        items.map((item) =>
          rename_proc_datum(item, def_env, call_ctx, renames, captures)
        ),
      )
    @core.Datum::Label(tag, value) =>
      @core.Datum::Label(
        tag,
        Ref::new(
          rename_proc_datum(value.val, def_env, call_ctx, renames, captures),
        ),
      )
    @core.Datum::Complex(real, imag) =>
      @core.Datum::Complex(
        Ref::new(
          rename_proc_datum(real.val, def_env, call_ctx, renames, captures),
        ),
        Ref::new(
          rename_proc_datum(imag.val, def_env, call_ctx, renames, captures),
        ),
      )
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) =>
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
        @core.Datum::Value(
          @core.Value::SyntaxObject(@core.SyntaxObject::{
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
  datum : @core.Datum,
  def_env : @core.Env,
  call_ctx : Int,
) -> MacroExpansion {
  let renames : Map[String, String] = {}
  let captures : Map[String,@core.Value] = {}
  let renamed = rename_proc_datum(datum, def_env, call_ctx, renames, captures)
  MacroExpansion::{ datum: renamed, captures }
}

///|
fn expand_macro_rules(
  rules : @core.SyntaxRules,
  call : @core.Datum,
  env : @core.Env,
) -> MacroExpansion raise @core.EvalError {
  let call_scope = match rules.kind {
   @core.SyntaxRules => Some(@runtime.next_syntax_context())
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
      (@core.Datum::Symbol(_), @core.Datum::Pair(_, _)) => ()
      (@core.Datum::Pair(_, _), @core.Datum::Symbol(_)) => ()
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
               @core.SyntaxRules => env
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
           @core.SyntaxRules =>
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
          return MacroExpansion::{ datum: expanded, captures }
        }
    }
    continue i + 1
  }
  raise @core.EvalError("no matching syntax-rules pattern")
}

///|
fn expand_macro_proc(
  proc : @core.Value,
  def_env : @core.Env,
  call : @core.Datum,
  env : @core.Env,
) -> MacroExpansion raise @core.EvalError {
  if !@runtime.is_procedure_value(proc) {
    raise @core.EvalError("type error: procedure expected")
  }
  let temp_name = @runtime.gensym("macro_proc")
  let fenv = @runtime.env_extend(env)
  @runtime.env_define(fenv, temp_name, proc)
  let call_ctx = @runtime.next_syntax_context()
  let marked_call = @runtime.syntax_add_scope(call, call_ctx)
  let stx = match marked_call {
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => @core.Value::SyntaxObject(obj)
    _ =>
      @core.Value::SyntaxObject(@core.SyntaxObject::{
        datum: marked_call,
        scopes: [call_ctx],
        binding_id: None,
      })
  }
  let call_expr = list_from_array([
    @core.Datum::Symbol(temp_name),
    @core.Datum::Value(stx),
  ])
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
    Procedure(proc, def_env) =>
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
fn syntax_add_scope_with_env(datum : @core.Datum, scope : Int, env : @core.Env) -> @core.Datum {
  match datum {
    @core.Datum::Symbol(name) =>
      @core.Datum::Value(
        @core.Value::SyntaxObject(@core.SyntaxObject::{
          datum: @core.Datum::Symbol(name),
          scopes: [scope],
          binding_id: @runtime.env_binding_id_optional(env, name),
        }),
      )
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        @core.Datum::Symbol(_) => obj.datum
        _ => syntax_add_scope_with_env(obj.datum, scope, env)
      }
      let binding_id = match obj.binding_id {
        Some(_) => obj.binding_id
        None =>
          match obj.datum {
            @core.Datum::Symbol(name) => @runtime.env_binding_id_optional(env, name)
            _ => None
          }
      }
      @core.Datum::Value(
        @core.Value::SyntaxObject(@core.SyntaxObject::{
          datum: inner,
          scopes: next_scopes,
          binding_id,
        }),
      )
    }
    @core.Datum::Pair(a, b) =>
      pair_new(
        syntax_add_scope_with_env(a.val, scope, env),
        syntax_add_scope_with_env(b.val, scope, env),
      )
    @core.Datum::Vector(items) =>
      @core.Datum::Vector(
        items.map((item) => syntax_add_scope_with_env(item, scope, env)),
      )
    @core.Datum::Complex(real, imag) =>
      @core.Datum::Complex(
        Ref::new(syntax_add_scope_with_env(real.val, scope, env)),
        Ref::new(syntax_add_scope_with_env(imag.val, scope, env)),
      )
    _ => datum
  }
}

///|
fn wrap_pattern_datum(value : @core.Datum, scope : Int?, env : @core.Env) -> @core.Datum {
  match scope {
    Some(call_scope) => {
      let marked = syntax_add_scope_with_env(value, call_scope, env)
      match marked {
        @core.Datum::Value(@core.Value::SyntaxObject(_)) => marked
        _ =>
          @core.Datum::Value(
            @core.Value::SyntaxObject(@core.SyntaxObject::{
              datum: marked,
              scopes: [call_scope],
              binding_id: None,
            }),
          )
      }
    }
    None =>
      match value {
        @core.Datum::Value(@core.Value::SyntaxObject(_)) => value
        _ => {
          let binding_id = match @runtime.datum_unlabel(value) {
            @core.Datum::Symbol(name) => @runtime.env_binding_id_optional(env, name)
            _ => None
          }
          @core.Datum::Value(
            @core.Value::SyntaxObject(@core.SyntaxObject::{
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
  expr : @core.Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  ellipsis_active : Bool,
  error_message : String,
  env : @core.Env,
) -> @core.Datum raise @core.EvalError {
  match expr {
    @core.Datum::Pair(_, _) =>
      match datum_list_to_array_opt(expr) {
        Some(items) => {
          if !items.is_empty() {
            let head = items[0]
            let is_template_form =
              datum_is_symbol(head, "syntax") ||
              datum_is_symbol(head, "quasisyntax") ||
              datum_is_symbol(head, "with-syntax")
            if is_template_form {
              let indices : Array[Int] = []
              let expanded = expand_template_indexed(
                expr, bindings, renames, ellipsis, ellipsis_active, error_message,
                indices, env, true,
              )
              match expanded {
                @core.Datum::Value(@core.Value::SyntaxObject(obj)) =>
                  return @core.Datum::Value(@core.Value::SyntaxObject(obj))
                _ => {
                  let scope = @runtime.next_syntax_context()
                  let scopes = [scope]
                  let datum = @runtime.syntax_wrap_root(expanded, scopes)
                  let obj =@core.SyntaxObject::{ datum, scopes, binding_id: None }
                  return @core.Datum::Value(@core.Value::SyntaxObject(obj))
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
            @core.Datum::Pair(a, b) =>
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
    @core.Datum::Vector(items) =>
      @core.Datum::Vector(
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
  match pat {
    @core.Datum::Label(_, _) => false
    @core.Datum::Symbol(name) =>
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
    @core.Datum::Nil => inp is @core.Datum::Nil
    @core.Datum::Bool(b) =>
      match inp {
        @core.Datum::Bool(other) => other == b
        _ => false
      }
    @core.Datum::Int(n) =>
      match inp {
        @core.Datum::Int(other) => other == n
        _ => false
      }
    @core.Datum::BigInt(n) =>
      match inp {
        @core.Datum::BigInt(other) => other == n
        _ => false
      }
    @core.Datum::Rat(n, d) =>
      match inp {
        @core.Datum::Rat(on, od) => on == n && od == d
        _ => false
      }
    @core.Datum::BigRat(n, d) =>
      match inp {
        @core.Datum::BigRat(on, od) => on == n && od == d
        _ => false
      }
    @core.Datum::Complex(real, imag) =>
      match inp {
        @core.Datum::Complex(other_real, other_imag) =>
          datum_equal(real.val, other_real.val) &&
          datum_equal(imag.val, other_imag.val)
        _ => false
      }
    @core.Datum::Float(f) =>
      match inp {
        @core.Datum::Float(of) => of == f
        _ => false
      }
    @core.Datum::Char(ch) =>
      match inp {
        @core.Datum::Char(other) => other == ch
        _ => false
      }
    @core.Datum::String(s) =>
      match inp {
        @core.Datum::String(other) => other.val == s.val
        _ => false
      }
    @core.Datum::Vector(items) =>
      match inp {
        @core.Datum::Vector(other) =>
          match_pattern_list(
            items, other, literals, ellipsis, bindings, scope, def_env, use_env,
          )
        _ => false
      }
    @core.Datum::ByteVector(items) =>
      match inp {
        @core.Datum::ByteVector(other) => array_int_equal(items, other)
        _ => false
      }
    @core.Datum::Record(rec) =>
      match inp {
        @core.Datum::Record(other) => rec.id == other.id
        _ => false
      }
    @core.Datum::Condition(cond) =>
      match inp {
        @core.Datum::Condition(other) => cond.id == other.id
        _ => false
      }
    @core.Datum::Value(value) => value_eqv(value, value_from_datum(inp))
    @core.Datum::Pair(pa, pb) =>
      match (datum_list_to_array_opt(pat), datum_list_to_array_opt(inp)) {
        (Some(pats), Some(inputs)) =>
          match_pattern_list(
            pats, inputs, literals, ellipsis, bindings, scope, def_env, use_env,
          )
        _ =>
          match inp {
            @core.Datum::Pair(ia, ib) =>
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
    names.keys().each((name) => result[name] = Seq([]))
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
    iter_bindings.iter2().each((name, value) =>
      match repeated.get(name) {
        Some(values) => values.push(value)
        None => {
          let values : Array[BindingVal] = []
          values.push(value)
          repeated[name] = values
        }
      }
    )
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
    @core.Datum::Symbol(name) =>
      if name != "_" && name != ellipsis && !is_literal(literals, name) {
        names[name] = true
      }
    @core.Datum::Pair(_, _) =>
      match datum_list_to_array_opt(pattern) {
        Some(items) =>
          for item in items {
            collect_pattern_vars(item, literals, ellipsis, names)
          }
        None =>
          match pattern {
            @core.Datum::Pair(a, b) => {
              collect_pattern_vars(a.val, literals, ellipsis, names)
              collect_pattern_vars(b.val, literals, ellipsis, names)
            }
            _ => ()
          }
      }
    @core.Datum::Vector(items) =>
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
    //     @core.Datum::Pair(_, _) | @core.Datum::Nil => true
    //     _ => false
    //   }
    match @runtime.datum_unlabel(cur) {
      @core.Datum::Nil => break true
      @core.Datum::Pair(a, b) => {
        items.push(a.val)
        continue b.val
      }
      _ => break false
    }
  } else {
    false
  }
  if ok { Some(items) } else { None }
}

///|
fn datum_unwrap_syntax(value : @core.Datum) -> @core.Datum {
  match value {
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => obj.datum
    _ => value
  }
}

///|
fn datum_is_symbol(value : @core.Datum, name : String) -> Bool {
  match @runtime.datum_unlabel(datum_unwrap_syntax(value)) {
    @core.Datum::Symbol(sym) => sym == name
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
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) =>
      match @runtime.datum_unlabel(obj.datum) {
        @core.Datum::Symbol(other) => {
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
        @core.Datum::Symbol(other) => {
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
fn array_datum_equal(left : Array[@core.Datum], right : Array[@core.Datum]) -> Bool {
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
    (@core.Datum::Label(_, x), _) => datum_equal(x.val, b)
    (_, @core.Datum::Label(_, y)) => datum_equal(a, y.val)
    (@core.Datum::Nil, @core.Datum::Nil) => true
    (@core.Datum::Bool(x), @core.Datum::Bool(y)) => x == y
    (@core.Datum::Int(x), @core.Datum::Int(y)) => x == y
    (@core.Datum::BigInt(x), @core.Datum::BigInt(y)) => x == y
    (@core.Datum::Rat(ax, ay), @core.Datum::Rat(bx, by)) => ax == bx && ay == by
    (@core.Datum::BigRat(ax, ay), @core.Datum::BigRat(bx, by)) => ax == bx && ay == by
    (@core.Datum::Float(x), @core.Datum::Float(y)) => x == y
    (@core.Datum::Complex(ar, ai), @core.Datum::Complex(br, bi)) =>
      datum_equal(ar.val, br.val) && datum_equal(ai.val, bi.val)
    (@core.Datum::Char(x), @core.Datum::Char(y)) => x == y
    (@core.Datum::String(x), @core.Datum::String(y)) => x.val == y.val
    (@core.Datum::Symbol(x), @core.Datum::Symbol(y)) => x == y
    (@core.Datum::Pair(ax, ay), @core.Datum::Pair(bx, by)) =>
      datum_equal(ax.val, bx.val) && datum_equal(ay.val, by.val)
    (@core.Datum::Vector(ax), @core.Datum::Vector(bx)) => array_datum_equal(ax, bx)
    (@core.Datum::ByteVector(ax), @core.Datum::ByteVector(bx)) => array_int_equal(ax, bx)
    (@core.Datum::Record(ax), @core.Datum::Record(bx)) => ax.id == bx.id
    (@core.Datum::Condition(ax), @core.Datum::Condition(bx)) => ax.id == bx.id
    (@core.Datum::Value(ax), _) => value_eqv(ax, value_from_datum(b))
    (_, @core.Datum::Value(bx)) => value_eqv(value_from_datum(a), bx)
    _ => false
  }
}

///|
fn is_quote_form(items : Array[@core.Datum]) -> Bool {
  if items.length() != 2 {
    false
  } else {
    match items[0] {
      @core.Datum::Symbol("quote") => true
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
    Some(@core.Value::Macro(_)) => None
    Some(@core.Value::SyntaxKeyword(_)) => None
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
) -> (Map[String, String], Map[String,@core.Value]) {
  let renames : Map[String, String] = {}
  let captures : Map[String,@core.Value] = {}
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
  captures : Map[String,@core.Value],
) -> Unit {
  match template {
    @core.Datum::Symbol(name) =>
      if should_rename(name, bindings, literals, ellipsis, env) &&
        !renames.contains(name) {
        let renamed = @runtime.gensym(name)
        renames[name] = renamed
        match captured_value(def_env, name) {
          Some(value) => captures[renamed] = value
          None => ()
        }
      }
    @core.Datum::Pair(_, _) =>
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
            @core.Datum::Pair(a, b) => {
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
    @core.Datum::Vector(items) =>
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
    @core.Datum::Symbol(name) => symbols[name] = true
    @core.Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if !is_quote_form(items) {
            for item in items {
              collect_template_symbols(item, symbols)
            }
          }
        None =>
          match template {
            @core.Datum::Pair(a, b) => {
              collect_template_symbols(a.val, symbols)
              collect_template_symbols(b.val, symbols)
            }
            _ => ()
          }
      }
    @core.Datum::Vector(items) =>
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
    let forms : Array[@core.Datum] = [@core.Datum::Symbol("begin")]
    for i = 2; i < items.length(); {
      // invariant : i >= 2 && i <= items.length()
      // decreases : items.length() - i
      // assert : i <= items.length()
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
fn rename_datum(datum : @core.Datum, renames : Map[String, String]) -> @core.Datum {
  match datum {
    @core.Datum::Symbol(name) =>
      match renames.get(name) {
        Some(rename) => @core.Datum::Symbol(rename)
        None => datum
      }
    @core.Datum::Pair(a, b) =>
      pair_new(rename_datum(a.val, renames), rename_datum(b.val, renames))
    @core.Datum::Vector(items) =>
      @core.Datum::Vector(items.map((item) => rename_datum(item, renames)))
    @core.Datum::Label(tag, value) =>
      @core.Datum::Label(tag, Ref::new(rename_datum(value.val, renames)))
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
    @core.Datum::Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    @core.Datum::Pair(_, _) =>
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
            @core.Datum::Pair(a, b) =>
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
    @core.Datum::Vector(items) =>
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
    @core.Datum::Symbol(name) => {
      if ellipsis_active && name == ellipsis {
        raise @core.EvalError(error_message)
      }
      expand_symbol(name, bindings, renames, indices, error_message)
    }
    @core.Datum::Pair(_, _) =>
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
            @core.Datum::Pair(a, b) =>
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
    @core.Datum::Vector(items) =>
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
        None => raise @core.EvalError(error_message)
      }
    None =>
      match renames.get(name) {
        Some(rename) => @core.Datum::Symbol(rename)
        None => @core.Datum::Symbol(name)
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
  @core.Datum::Vector(expanded)
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
  @core.Datum::Vector(expanded)
}

///|
fn max_binding_len(
  template : @core.Datum,
  bindings : Map[String, BindingVal],
  indices : Array[Int],
) -> Int {
  match template {
    @core.Datum::Symbol(name) =>
      match bindings.get(name) {
        Some(value) => binding_len_at(value, indices, 0)
        None => 0
      }
    @core.Datum::Pair(_, _) =>
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
            @core.Datum::Pair(a, b) => {
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
    @core.Datum::Vector(items) => {
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
    @core.Datum::Symbol(name) =>
      if name == ellipsis {
        false
      } else {
        match bindings.get(name) {
          Some(value) => binding_has_repeat_at(value, indices, 0)
          None => false
        }
      }
    @core.Datum::Pair(_, _) =>
      match datum_list_to_array_opt(template) {
        Some(items) =>
          if is_quote_form(items) {
            false
          } else {
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
        None =>
          match template {
            @core.Datum::Pair(a, b) =>
              template_has_repeat_binding(a.val, bindings, ellipsis, indices) ||
              template_has_repeat_binding(b.val, bindings, ellipsis, indices)
            _ => false
          }
      }
    @core.Datum::Vector(items) => {
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

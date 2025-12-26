///|
fn is_core_identifier(name : String) -> Bool {
  let core : Array[String] = [
    "quote",
    "quasiquote",
    "unquote",
    "unquote-splicing",
    "quasisyntax",
    "unsyntax",
    "unsyntax-splicing",
    "if",
    "begin",
    "lambda",
    "case-lambda",
    "define",
    "define-syntax",
    "library",
    "import",
    "export",
    "begin-for-syntax",
    "syntax-case",
    "syntax",
    "syntax?",
    "identifier-syntax",
    "syntax-rules",
    "set!",
    "let",
    "let*",
    "letrec",
    "letrec*",
    "let-values",
    "let*-values",
    "define-values",
    "let-syntax",
    "letrec-syntax",
    "define-record-type",
    "define-condition-type",
    "define-enumeration",
    "and",
    "or",
    "cond",
    "cond-expand",
    "do",
    "case",
    "else",
    "=>",
    "guard",
    "+",
    "-",
    "*",
    "/",
    "=",
    "<",
    ">",
    "<=",
    ">=",
    "eq?",
    "eqv?",
    "equal?",
    "cons",
    "car",
    "cdr",
    "list",
    "make-list",
    "null?",
    "pair?",
    "list?",
    "symbol?",
    "identifier?",
    "free-identifier=?",
    "bound-identifier=?",
    "symbol->string",
    "string->symbol",
    "syntax->datum",
    "datum->syntax",
    "boolean?",
    "number?",
    "integer?",
    "rational?",
    "real?",
    "complex?",
    "exact?",
    "inexact?",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "finite?",
    "infinite?",
    "nan?",
    "procedure?",
    "record?",
    "record-rtd",
    "record-type-descriptor?",
    "record-constructor-descriptor?",
    "record-type-name",
    "record-type-parent",
    "record-type-uid",
    "record-type-generative?",
    "record-type-sealed?",
    "record-type-opaque?",
    "record-type-field-names",
    "record-type-field-mutable?",
    "record-constructor-descriptor",
    "record-constructor",
    "record-predicate",
    "record-accessor",
    "record-mutator",
    "make-record-type-descriptor",
    "make-record-constructor-descriptor",
    "&condition",
    "condition",
    "condition?",
    "simple-conditions",
    "condition-predicate",
    "condition-accessor",
    "make-eq-hashtable",
    "make-eqv-hashtable",
    "make-hashtable",
    "hashtable?",
    "hashtable-size",
    "hashtable-ref",
    "hashtable-set!",
    "hashtable-delete!",
    "hashtable-contains?",
    "hashtable-update!",
    "hashtable-copy",
    "hashtable-clear!",
    "hashtable-keys",
    "hashtable-entries",
    "hashtable-equivalence-function",
    "hashtable-hash-function",
    "hashtable-mutable?",
    "make-enumeration",
    "enum-set-universe",
    "enum-set-indexer",
    "enum-set-constructor",
    "enum-set?",
    "enum-set-member?",
    "enum-set-subset?",
    "enum-set=?",
    "enum-set-union",
    "enum-set-intersection",
    "enum-set-difference",
    "enum-set-complement",
    "enum-set-projection",
    "enum-set->list",
    "not",
    "apply",
    "call/cc",
    "call-with-current-continuation",
    "values",
    "call-with-values",
    "make-parameter",
    "dynamic-wind",
    "eval",
    "environment",
    "promise?",
    "make-promise",
    "force",
    "delay",
    "parameterize",
    "list-ref",
    "list-tail",
    "member",
    "memq",
    "memv",
    "assoc",
    "assq",
    "assv",
    "map",
    "for-each",
    "set-car!",
    "set-cdr!",
    "list-copy",
    "caar",
    "cadr",
    "cdar",
    "cddr",
    "caaar",
    "caadr",
    "cadar",
    "caddr",
    "cdaar",
    "cdadr",
    "cddar",
    "cdddr",
    "caaaar",
    "caaadr",
    "caadar",
    "caaddr",
    "cadaar",
    "cadadr",
    "caddar",
    "cadddr",
    "cdaaar",
    "cdaadr",
    "cdadar",
    "cdaddr",
    "cddaar",
    "cddadr",
    "cdddar",
    "cddddr",
    "length",
    "append",
    "reverse",
    "char=?",
    "char<?",
    "char>?",
    "char<=?",
    "char>=?",
    "char-ci=?",
    "char-ci<?",
    "char-ci>?",
    "char-ci<=?",
    "char-ci>=?",
    "char?",
    "char->integer",
    "integer->char",
    "char-alphabetic?",
    "char-numeric?",
    "char-whitespace?",
    "char-upper-case?",
    "char-lower-case?",
    "char-upcase",
    "char-downcase",
    "char-foldcase",
    "string=?",
    "string<?",
    "string>?",
    "string<=?",
    "string>=?",
    "string-ci=?",
    "string-ci<?",
    "string-ci>?",
    "string-ci<=?",
    "string-ci>=?",
    "string",
    "make-string",
    "string?",
    "string-length",
    "string-append",
    "string-ref",
    "string-set!",
    "string-copy",
    "substring",
    "string-copy!",
    "string-fill!",
    "string->list",
    "list->string",
    "string-map",
    "string-for-each",
    "string-upcase",
    "string-downcase",
    "string-foldcase",
    "vector",
    "make-vector",
    "vector?",
    "vector-length",
    "vector-ref",
    "vector-set!",
    "vector-fill!",
    "vector-copy",
    "vector-copy!",
    "vector-append",
    "vector-map",
    "vector-for-each",
    "vector->list",
    "list->vector",
    "bytevector",
    "make-bytevector",
    "bytevector?",
    "bytevector-length",
    "bytevector-u8-ref",
    "bytevector-u8-set!",
    "bytevector-copy",
    "bytevector-copy!",
    "bytevector-append",
    "bytevector-fill!",
    "bytevector->u8-list",
    "u8-list->bytevector",
    "exact->inexact",
    "inexact->exact",
    "exact-integer-sqrt",
    "rationalize",
    "number->string",
    "string->number",
    "make-rectangular",
    "make-polar",
    "real-part",
    "imag-part",
    "magnitude",
    "angle",
    "sqrt",
    "exp",
    "log",
    "expt",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "numerator",
    "denominator",
    "abs",
    "quotient",
    "remainder",
    "modulo",
    "gcd",
    "lcm",
    "max",
    "min",
    "floor",
    "ceiling",
    "truncate",
    "round",
    "display",
    "write",
    "newline",
    "open-output-string",
    "get-output-string",
    "current-output-port",
    "with-exception-handler",
    "raise",
    "raise-continuable",
    "error",
    "assertion-violation",
    "implementation-restriction-violation",
    "undefined-violation",
    "syntax-violation",
  ]
  let mut i = 0
  while i < core.length() {
    if core[i] == name {
      return true
    }
    i = i + 1
  }
  false
}

///|
fn parse_syntax_rules(expr : Datum) -> SyntaxRules raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.length() < 3 {
    raise @core.EvalError("invalid syntax-rules")
  }
  match parts[0] {
    Datum::Symbol("syntax-rules") => ()
    _ => raise @core.EvalError("invalid syntax-rules")
  }
  let mut ellipsis = "..."
  let mut literal_idx = 1
  match parts[1] {
    Datum::Symbol(name) => {
      ellipsis = name
      literal_idx = 2
      if parts.length() < 4 {
        raise @core.EvalError("invalid syntax-rules")
      }
    }
    _ => ()
  }
  let literals = parse_literal_list(parts[literal_idx])
  let rules : Array[SyntaxRule] = []
  let mut i = literal_idx + 1
  while i < parts.length() {
    let rule_parts = datum_list_to_array(parts[i])
    if rule_parts.length() != 2 {
      raise @core.EvalError("invalid syntax-rules")
    }
    rules.push({ pattern: rule_parts[0], template: rule_parts[1], fender: None })
    i = i + 1
  }
  { literals, rules, ellipsis }
}

///|
fn parse_identifier_syntax(expr : Datum, name : String) -> SyntaxRules raise EvalError {
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
    rules.push({ pattern: Datum::Symbol(name), template: parts[1], fender: None })
  } else {
    let mut i = 1
    while i < parts.length() {
      let rule_parts = datum_list_to_array(parts[i])
      if rule_parts.length() != 2 {
        raise @core.EvalError("invalid identifier-syntax")
      }
      rules.push({ pattern: rule_parts[0], template: rule_parts[1], fender: None })
      i = i + 1
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
  { literals, rules, ellipsis: "..." }
}

///|
fn parse_syntax_case_transformer(expr : Datum) -> SyntaxRules raise EvalError {
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
  let literals = parse_literal_list(sc_parts[2])
  let rules : Array[SyntaxRule] = []
  let mut i = 3
  while i < sc_parts.length() {
    let rule_parts = datum_list_to_array(sc_parts[i])
    if rule_parts.length() == 2 {
      rules.push({ pattern: rule_parts[0], template: rule_parts[1], fender: None })
    } else if rule_parts.length() == 3 {
      rules.push({
        pattern: rule_parts[0],
        template: rule_parts[2],
        fender: Some(rule_parts[1]),
      })
    } else {
      raise @core.EvalError("invalid syntax-case clause")
    }
    i = i + 1
  }
  { literals, rules, ellipsis: "..." }
}

///|
fn parse_transformer(expr : Datum, name : String) -> SyntaxRules raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid syntax transformer")
  }
  match parts[0] {
    Datum::Symbol("syntax-rules") => parse_syntax_rules(expr)
    Datum::Symbol("identifier-syntax") => parse_identifier_syntax(expr, name)
    Datum::Symbol("lambda") => parse_syntax_case_transformer(expr)
    Datum::Symbol("make-variable-transformer") => {
      if parts.length() != 2 {
        raise @core.EvalError("invalid syntax transformer")
      }
      parse_transformer(parts[1], name)
    }
    _ => raise @core.EvalError("invalid syntax transformer")
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
  let mut i = 0
  while i < literals.length() {
    if literals[i] == name {
      return true
    }
    i = i + 1
  }
  false
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
    (BindingVal::Seq(a), BindingVal::Seq(b)) => {
      if a.length() != b.length() {
        false
      } else {
        let mut i = 0
        while i < a.length() {
          if !binding_equal(a[i], b[i]) {
            return false
          }
          i = i + 1
        }
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
fn bindings_clone(bindings : Map[String, BindingVal]) -> Map[String, BindingVal] {
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
fn bindings_overwrite(dest : Map[String, BindingVal], src : Map[String, BindingVal]) -> Unit {
  dest.clear()
  for name in src.keys() {
    match src.get(name) {
      Some(value) => dest[name] = value
      None => ()
    }
  }
}

///|
fn merge_bindings(dest : Map[String, BindingVal], src : Map[String, BindingVal]) -> Bool {
  for name in src.keys() {
    match src.get(name) {
      Some(value) => {
        match dest.get(name) {
          Some(existing) => {
            if !binding_equal(existing, value) {
              return false
            }
          }
          None => dest[name] = value
        }
      }
      None => ()
    }
  }
  true
}

///|
fn binding_select(value : BindingVal, indices : Array[Int], depth : Int) -> Datum? {
  match value {
    BindingVal::Leaf(datum) => Some(datum)
    BindingVal::Seq(values) => {
      if values.is_empty() {
        None
      } else {
        let idx = if depth < indices.length() { indices[depth] } else { 0 }
        let actual = if idx < values.length() { idx } else { values.length() - 1 }
        binding_select(values[actual], indices, depth + 1)
      }
    }
  }
}

///|
fn binding_len_at(value : BindingVal, indices : Array[Int], depth : Int) -> Int {
  match value {
    BindingVal::Leaf(_) => 0
    BindingVal::Seq(values) => {
      if depth < indices.length() {
        if values.is_empty() {
          0
        } else {
          let idx = indices[depth]
          let actual = if idx < values.length() { idx } else { values.length() - 1 }
          binding_len_at(values[actual], indices, depth + 1)
        }
      } else {
        values.length()
      }
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
fn fender_allows(expr : Datum, bindings : Map[String, BindingVal], env : Env) -> Bool raise EvalError {
  let fenv = env_extend(env)
  for name in bindings.keys() {
    match bindings.get(name) {
      Some(value) => env_define(fenv, name, Value::Datum(binding_to_datum(value)))
      None => ()
    }
  }
  let result = eval_expr(expr, fenv)
  !is_false(result)
}

///|
fn expand_macro(rules : SyntaxRules, call : Datum, env : Env) -> Datum raise EvalError {
  let mut i = 0
  while i < rules.rules.length() {
    let rule = rules.rules[i]
    let bindings : Map[String, BindingVal] = {}
    match (rule.pattern, call) {
      (Datum::Symbol(_), Datum::Pair(_, _)) => ()
      (Datum::Pair(_, _), Datum::Symbol(_)) => ()
      _ => {
        if match_pattern(rule.pattern, call, rules.literals, rules.ellipsis, bindings) {
          match rule.fender {
            Some(fender) => {
              if !fender_allows(fender, bindings, env) {
                i = i + 1
                continue
              }
            }
            None => ()
          }
          let renames = build_rename_map(rule.template, bindings, rules.literals, rules.ellipsis, env)
          return expand_template(rule.template, bindings, renames, rules.ellipsis)
        }
      }
    }
    i = i + 1
  }
  raise @core.EvalError("no matching syntax-rules pattern")
}

///|
fn match_pattern(
  pattern : Datum,
  input : Datum,
  literals : Array[String],
  ellipsis : String,
  bindings : Map[String, BindingVal],
) -> Bool {
  let pat = datum_unlabel(pattern)
  let inp = datum_unlabel(input)
  match pat {
    Datum::Label(_, _) => false
    Datum::Symbol(name) => {
      if name == "_" {
        true
      } else if is_literal(literals, name) {
        match inp {
          Datum::Symbol(other) => other == name
          _ => false
        }
      } else {
        bind_var(name, inp, bindings)
      }
    }
    Datum::Nil => inp is Datum::Nil
    Datum::Bool(b) => {
      match inp {
        Datum::Bool(other) => other == b
        _ => false
      }
    }
    Datum::Int(n) => {
      match inp {
        Datum::Int(other) => other == n
        _ => false
      }
    }
    Datum::BigInt(n) => {
      match inp {
        Datum::BigInt(other) => other == n
        _ => false
      }
    }
    Datum::Rat(n, d) => {
      match inp {
        Datum::Rat(on, od) => on == n && od == d
        _ => false
      }
    }
    Datum::BigRat(n, d) => {
      match inp {
        Datum::BigRat(on, od) => on == n && od == d
        _ => false
      }
    }
    Datum::Complex(real, imag) => {
      match inp {
        Datum::Complex(other_real, other_imag) =>
          datum_equal(real.val, other_real.val) && datum_equal(imag.val, other_imag.val)
        _ => false
      }
    }
    Datum::Float(f) => {
      match inp {
        Datum::Float(of) => of == f
        _ => false
      }
    }
    Datum::Char(ch) => {
      match inp {
        Datum::Char(other) => other == ch
        _ => false
      }
    }
    Datum::String(s) => {
      match inp {
        Datum::String(other) => other.val == s.val
        _ => false
      }
    }
    Datum::Vector(items) => {
      match inp {
        Datum::Vector(other) =>
          match_pattern_list(items, other, literals, ellipsis, bindings)
        _ => false
      }
    }
    Datum::ByteVector(items) => {
      match inp {
        Datum::ByteVector(other) => array_int_equal(items, other)
        _ => false
      }
    }
    Datum::Record(rec) => {
      match inp {
        Datum::Record(other) => rec.id == other.id
        _ => false
      }
    }
    Datum::Condition(cond) => {
      match inp {
        Datum::Condition(other) => cond.id == other.id
        _ => false
      }
    }
    Datum::Value(value) => value_eqv(value, value_from_datum(inp))
    Datum::Pair(pa, pb) => {
      match (datum_list_to_array_opt(pat), datum_list_to_array_opt(inp)) {
        (Some(pats), Some(inputs)) => match_pattern_list(pats, inputs, literals, ellipsis, bindings)
        _ => {
          match inp {
            Datum::Pair(ia, ib) =>
              if match_pattern(pa.val, ia.val, literals, ellipsis, bindings) {
                match_pattern(pb.val, ib.val, literals, ellipsis, bindings)
              } else {
                false
              }
            _ => false
          }
        }
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
) -> Bool {
  match_pattern_list_from(pats, inputs, 0, 0, literals, ellipsis, bindings)
}

///|
fn collect_repeat_bindings(
  repeat_pat : Datum,
  inputs : Array[Datum],
  start : Int,
  count : Int,
  literals : Array[String],
  ellipsis : String,
)
 -> Map[String, BindingVal]? {
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
  let mut i = 0
  while i < count {
    let iter_bindings : Map[String, BindingVal] = {}
    if !match_pattern(repeat_pat, inputs[start + i], literals, ellipsis, iter_bindings) {
      return None
    }
    for name in iter_bindings.keys() {
      match iter_bindings.get(name) {
        Some(value) => {
          match repeated.get(name) {
            Some(values) => values.push(value)
            None => {
              let values : Array[BindingVal] = []
              values.push(value)
              repeated[name] = values
            }
          }
        }
        None => ()
      }
    }
    i = i + 1
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
  bindings : Map[String, BindingVal],
) -> Bool {
  if pat_idx >= pats.length() {
    return inp_idx == inputs.length()
  }
  if pat_idx + 1 < pats.length() && datum_is_symbol(pats[pat_idx + 1], ellipsis) {
    let max_count = inputs.length() - inp_idx
    let mut count = max_count
    while count >= 0 {
      let base = bindings_clone(bindings)
      match collect_repeat_bindings(pats[pat_idx], inputs, inp_idx, count, literals, ellipsis) {
        Some(repeat_bindings) =>
          if merge_bindings(base, repeat_bindings) {
            if match_pattern_list_from(
              pats,
              inputs,
              pat_idx + 2,
              inp_idx + count,
              literals,
              ellipsis,
              base,
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
    if !match_pattern(pats[pat_idx], inputs[inp_idx], literals, ellipsis, bindings) {
      return false
    }
    match_pattern_list_from(pats, inputs, pat_idx + 1, inp_idx + 1, literals, ellipsis, bindings)
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
    Datum::Symbol(name) => {
      if name != "_" && name != ellipsis && !is_literal(literals, name) {
        names[name] = true
      }
    }
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(pattern) {
        Some(items) => {
          for item in items {
            collect_pattern_vars(item, literals, ellipsis, names)
          }
        }
        None => {
          match pattern {
            Datum::Pair(a, b) => {
              collect_pattern_vars(a.val, literals, ellipsis, names)
              collect_pattern_vars(b.val, literals, ellipsis, names)
            }
            _ => ()
          }
        }
      }
    }
    Datum::Vector(items) => {
      for item in items {
        collect_pattern_vars(item, literals, ellipsis, names)
      }
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
fn datum_is_symbol(value : Datum, name : String) -> Bool {
  match datum_unlabel(value) {
    Datum::Symbol(sym) => sym == name
    _ => false
  }
}

///|
fn array_datum_equal(left : Array[Datum], right : Array[Datum]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    let mut i = 0
    while i < left.length() {
      if !datum_equal(left[i], right[i]) {
        return false
      }
      i = i + 1
    }
    true
  }
}

///|
fn array_int_equal(left : Array[Int], right : Array[Int]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    let mut i = 0
    while i < left.length() {
      if left[i] != right[i] {
        return false
      }
      i = i + 1
    }
    true
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
fn build_rename_map(
  template : Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  env : Env,
) -> Map[String, String] {
  let renames : Map[String, String] = {}
  collect_renames(template, bindings, literals, ellipsis, env, renames)
  renames
}

///|
fn collect_renames(
  template : Datum,
  bindings : Map[String, BindingVal],
  literals : Array[String],
  ellipsis : String,
  env : Env,
  renames : Map[String, String],
) -> Unit {
  match template {
    Datum::Symbol(name) => {
      if should_rename(name, bindings, literals, ellipsis, env) && !renames.contains(name) {
        renames[name] = gensym(name)
      }
    }
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
          if !is_quote_form(items) {
            for item in items {
              collect_renames(item, bindings, literals, ellipsis, env, renames)
            }
          }
        }
        None => {
          match template {
            Datum::Pair(a, b) => {
              collect_renames(a.val, bindings, literals, ellipsis, env, renames)
              collect_renames(b.val, bindings, literals, ellipsis, env, renames)
            }
            _ => ()
          }
        }
      }
    }
    Datum::Vector(items) => {
      for item in items {
        collect_renames(item, bindings, literals, ellipsis, env, renames)
      }
    }
    _ => ()
  }
}

///|
fn collect_template_symbols(template : Datum, symbols : Map[String, Bool]) -> Unit {
  match template {
    Datum::Symbol(name) => symbols[name] = true
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
          if !is_quote_form(items) {
            for item in items {
              collect_template_symbols(item, symbols)
            }
          }
        }
        None => {
          match template {
            Datum::Pair(a, b) => {
              collect_template_symbols(a.val, symbols)
              collect_template_symbols(b.val, symbols)
            }
            _ => ()
          }
        }
      }
    }
    Datum::Vector(items) => {
      for item in items {
        collect_template_symbols(item, symbols)
      }
    }
    _ => ()
  }
}

///|
fn expand_template(
  template : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
) -> Datum {
  let indices : Array[Int] = []
  expand_template_indexed(template, bindings, renames, ellipsis, indices)
}

///|
fn expand_template_indexed(
  template : Datum,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  indices : Array[Int],
) -> Datum {
  match template {
    Datum::Symbol(name) => expand_symbol(name, bindings, renames, indices)
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
          if is_quote_form(items) {
            template
          } else {
            expand_template_list(items, bindings, renames, ellipsis, indices)
          }
        }
        None => {
          match template {
            Datum::Pair(a, b) => pair_new(
              expand_template_indexed(a.val, bindings, renames, ellipsis, indices),
              expand_template_indexed(b.val, bindings, renames, ellipsis, indices),
            )
            _ => template
          }
        }
      }
    }
    Datum::Vector(items) => expand_template_vector(items, bindings, renames, ellipsis, indices)
    _ => template
  }
}

///|
fn expand_symbol(
  name : String,
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  indices : Array[Int],
) -> Datum {
  match bindings.get(name) {
    Some(value) => {
      match binding_select(value, indices, 0) {
        Some(datum) => datum
        None => Datum::Symbol(name)
      }
    }
    None => {
      match renames.get(name) {
        Some(rename) => Datum::Symbol(rename)
        None => Datum::Symbol(name)
      }
    }
  }
}

///|
fn expand_template_list(
  items : Array[Datum],
  bindings : Map[String, BindingVal],
  renames : Map[String, String],
  ellipsis : String,
  indices : Array[Int],
) -> Datum {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if i + 1 < items.length() && datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_indexed(repeat_template, bindings, renames, ellipsis, next_indices),
        )
        j = j + 1
      }
      i = i + 2
    } else {
      expanded.push(expand_template_indexed(items[i], bindings, renames, ellipsis, indices))
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
  indices : Array[Int],
) -> Datum {
  let expanded : Array[Datum] = []
  let mut i = 0
  while i < items.length() {
    if i + 1 < items.length() && datum_is_symbol(items[i + 1], ellipsis) {
      let repeat_template = items[i]
      let count = max_binding_len(repeat_template, bindings, indices)
      let mut j = 0
      while j < count {
        let next_indices : Array[Int] = []
        for idx in indices {
          next_indices.push(idx)
        }
        next_indices.push(j)
        expanded.push(
          expand_template_indexed(repeat_template, bindings, renames, ellipsis, next_indices),
        )
        j = j + 1
      }
      i = i + 2
    } else {
      expanded.push(expand_template_indexed(items[i], bindings, renames, ellipsis, indices))
      i = i + 1
    }
  }
  Datum::Vector(expanded)
}

///|
fn max_binding_len(template : Datum, bindings : Map[String, BindingVal], indices : Array[Int]) -> Int {
  match template {
    Datum::Symbol(name) => {
      match bindings.get(name) {
        Some(value) => binding_len_at(value, indices, 0)
        None => 0
      }
    }
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
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
        }
        None => {
          match template {
            Datum::Pair(a, b) => {
              let left = max_binding_len(a.val, bindings, indices)
              let right = max_binding_len(b.val, bindings, indices)
              if left > right { left } else { right }
            }
            _ => 0
          }
        }
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

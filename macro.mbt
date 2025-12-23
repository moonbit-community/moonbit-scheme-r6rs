///|
let gensym_counter : Ref[Int] = Ref::new(0)

///|
fn gensym(base : String) -> String {
  let idx = gensym_counter.val
  gensym_counter.val = idx + 1
  "\{base}__gs\{idx}"
}

///|
fn is_core_identifier(name : String) -> Bool {
  let core : Array[String] = [
    "...",
    "quote",
    "if",
    "begin",
    "lambda",
    "define",
    "define-syntax",
    "syntax-rules",
    "set!",
    "let",
    "let*",
    "letrec",
    "and",
    "or",
    "cond",
    "else",
    "+",
    "-",
    "*",
    "=",
    "<",
    "cons",
    "car",
    "cdr",
    "list",
    "null?",
    "pair?",
    "symbol?",
    "boolean?",
    "number?",
    "procedure?",
    "not",
    "apply",
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
    raise EvalError("invalid syntax-rules")
  }
  match parts[0] {
    Datum::Symbol("syntax-rules") => ()
    _ => raise EvalError("invalid syntax-rules")
  }
  let literals = parse_literal_list(parts[1])
  let rules : Array[SyntaxRule] = []
  let mut i = 2
  while i < parts.length() {
    let rule_parts = datum_list_to_array(parts[i])
    if rule_parts.length() != 2 {
      raise EvalError("invalid syntax-rules")
    }
    rules.push({ pattern: rule_parts[0], template: rule_parts[1] })
    i = i + 1
  }
  { literals, rules }
}

///|
fn parse_literal_list(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  let literals : Array[String] = []
  for item in items {
    match item {
      Datum::Symbol(name) => literals.push(name)
      _ => raise EvalError("invalid syntax-rules")
    }
  }
  literals
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
fn expand_macro(rules : SyntaxRules, call : Datum) -> Datum raise EvalError {
  let mut i = 0
  while i < rules.rules.length() {
    let rule = rules.rules[i]
    let bindings : Map[String, Array[Datum]] = {}
    if match_pattern(rule.pattern, call, rules.literals, bindings, false) {
      let renames = build_rename_map(rule.template, bindings, rules.literals)
      return expand_template(rule.template, bindings, renames)
    }
    i = i + 1
  }
  raise EvalError("no matching syntax-rules pattern")
}

///|
fn match_pattern(
  pattern : Datum,
  input : Datum,
  literals : Array[String],
  bindings : Map[String, Array[Datum]],
  repeated : Bool,
) -> Bool {
  match pattern {
    Datum::Symbol(name) => {
      if name == "_" {
        true
      } else if is_literal(literals, name) {
        match input {
          Datum::Symbol(other) => other == name
          _ => false
        }
      } else {
        bind_var(name, input, bindings, repeated)
      }
    }
    Datum::Nil => input is Datum::Nil
    Datum::Bool(b) => {
      match input {
        Datum::Bool(other) => other == b
        _ => false
      }
    }
    Datum::Int(n) => {
      match input {
        Datum::Int(other) => other == n
        _ => false
      }
    }
    Datum::String(s) => {
      match input {
        Datum::String(other) => other == s
        _ => false
      }
    }
    Datum::Pair(pa, pb) => {
      match (datum_list_to_array_opt(pattern), datum_list_to_array_opt(input)) {
        (Some(pats), Some(inputs)) => match_pattern_list(pats, inputs, literals, bindings, repeated)
        _ => {
          match input {
            Datum::Pair(ia, ib) =>
              if match_pattern(pa, ia, literals, bindings, repeated) {
                match_pattern(pb, ib, literals, bindings, repeated)
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
  bindings : Map[String, Array[Datum]],
  repeated : Bool,
) -> Bool {
  if pats.length() >= 2 && pats[pats.length() - 1] is Datum::Symbol("...") {
    let repeat_pat = pats[pats.length() - 2]
    let prefix = pats.sub(end=pats.length() - 2).to_array()
    if inputs.length() < prefix.length() {
      false
    } else if !match_pattern_prefix(prefix, inputs, literals, bindings, repeated) {
      false
    } else {
      let mut i = prefix.length()
      while i < inputs.length() {
        if !match_pattern(repeat_pat, inputs[i], literals, bindings, true) {
          return false
        }
        i = i + 1
      }
      true
    }
  } else {
    if pats.length() != inputs.length() {
      false
    } else if !match_pattern_prefix(pats, inputs, literals, bindings, repeated) {
      false
    } else {
      true
    }
  }
}

///|
fn match_pattern_prefix(
  pats : Array[Datum],
  inputs : Array[Datum],
  literals : Array[String],
  bindings : Map[String, Array[Datum]],
  repeated : Bool,
) -> Bool {
  let mut i = 0
  while i < pats.length() {
    if !match_pattern(pats[i], inputs[i], literals, bindings, repeated) {
      return false
    }
    i = i + 1
  }
  true
}

///|
fn bind_var(
  name : String,
  value : Datum,
  bindings : Map[String, Array[Datum]],
  repeated : Bool,
) -> Bool {
  match bindings.get(name) {
    Some(values) => {
      if repeated {
        values.push(value)
        true
      } else {
        if values.is_empty() {
          values.push(value)
          true
        } else {
          datum_equal(values[0], value)
        }
      }
    }
    None => {
      let values : Array[Datum] = []
      values.push(value)
      bindings[name] = values
      true
    }
  }
}

///|
fn datum_list_to_array_opt(list : Datum) -> Array[Datum]? {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a)
        cur = b
      }
      _ => return None
    }
  }
  Some(items)
}

///|
fn datum_equal(a : Datum, b : Datum) -> Bool {
  match (a, b) {
    (Datum::Nil, Datum::Nil) => true
    (Datum::Bool(x), Datum::Bool(y)) => x == y
    (Datum::Int(x), Datum::Int(y)) => x == y
    (Datum::String(x), Datum::String(y)) => x == y
    (Datum::Symbol(x), Datum::Symbol(y)) => x == y
    (Datum::Pair(ax, ay), Datum::Pair(bx, by)) => datum_equal(ax, bx) && datum_equal(ay, by)
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
  bindings : Map[String, Array[Datum]],
  literals : Array[String],
) -> Bool {
  if name == "_" || name == "..." {
    false
  } else if is_literal(literals, name) {
    false
  } else if is_core_identifier(name) {
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
  bindings : Map[String, Array[Datum]],
  literals : Array[String],
) -> Map[String, String] {
  let renames : Map[String, String] = {}
  collect_renames(template, bindings, literals, renames)
  renames
}

///|
fn collect_renames(
  template : Datum,
  bindings : Map[String, Array[Datum]],
  literals : Array[String],
  renames : Map[String, String],
) -> Unit {
  match template {
    Datum::Symbol(name) => {
      if should_rename(name, bindings, literals) && !renames.contains(name) {
        renames[name] = gensym(name)
      }
    }
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
          if !is_quote_form(items) {
            for item in items {
              collect_renames(item, bindings, literals, renames)
            }
          }
        }
        None => {
          match template {
            Datum::Pair(a, b) => {
              collect_renames(a, bindings, literals, renames)
              collect_renames(b, bindings, literals, renames)
            }
            _ => ()
          }
        }
      }
    }
    _ => ()
  }
}

///|
fn expand_template(
  template : Datum,
  bindings : Map[String, Array[Datum]],
  renames : Map[String, String],
) -> Datum {
  expand_template_indexed(template, bindings, renames, None)
}

///|
fn expand_template_indexed(
  template : Datum,
  bindings : Map[String, Array[Datum]],
  renames : Map[String, String],
  index : Int?,
) -> Datum {
  match template {
    Datum::Symbol(name) => expand_symbol(name, bindings, renames, index)
    Datum::Pair(_, _) => {
      match datum_list_to_array_opt(template) {
        Some(items) => {
          if is_quote_form(items) {
            template
          } else {
            expand_template_list(items, bindings, renames, index)
          }
        }
        None => {
          match template {
            Datum::Pair(a, b) => Datum::Pair(
              expand_template_indexed(a, bindings, renames, index),
              expand_template_indexed(b, bindings, renames, index),
            )
            _ => template
          }
        }
      }
    }
    _ => template
  }
}

///|
fn expand_symbol(
  name : String,
  bindings : Map[String, Array[Datum]],
  renames : Map[String, String],
  index : Int?,
) -> Datum {
  match bindings.get(name) {
    Some(values) => {
      if values.is_empty() {
        Datum::Symbol(name)
      } else {
        match index {
          Some(i) if i < values.length() => values[i]
          Some(_) => values[values.length() - 1]
          None => values[0]
        }
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
  bindings : Map[String, Array[Datum]],
  renames : Map[String, String],
  index : Int?,
) -> Datum {
  if items.length() >= 2 && items[items.length() - 1] is Datum::Symbol("...") {
    let repeat_template = items[items.length() - 2]
    let prefix = items.sub(end=items.length() - 2).to_array()
    let expanded : Array[Datum] = []
    for item in prefix {
      expanded.push(expand_template_indexed(item, bindings, renames, index))
    }
    let count = max_binding_len(repeat_template, bindings)
    let mut i = 0
    while i < count {
      expanded.push(expand_template_indexed(repeat_template, bindings, renames, Some(i)))
      i = i + 1
    }
    list_from_array(expanded)
  } else {
    let expanded : Array[Datum] = []
    for item in items {
      expanded.push(expand_template_indexed(item, bindings, renames, index))
    }
    list_from_array(expanded)
  }
}

///|
fn max_binding_len(template : Datum, bindings : Map[String, Array[Datum]]) -> Int {
  match template {
    Datum::Symbol(name) => {
      match bindings.get(name) {
        Some(values) => values.length()
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
              let len = max_binding_len(item, bindings)
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
              let left = max_binding_len(a, bindings)
              let right = max_binding_len(b, bindings)
              if left > right { left } else { right }
            }
            _ => 0
          }
        }
      }
    }
    _ => 0
  }
}

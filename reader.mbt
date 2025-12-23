///|
priv struct Reader {
  chars : Array[Char]
  mut pos : Int
}

///|
fn make_reader(src : String) -> Reader {
  let chars : Array[Char] = []
  for ch in src {
    chars.push(ch)
  }
  { chars, pos: 0 }
}

///|
fn reader_peek(r : Reader) -> Char? {
  if r.pos < r.chars.length() {
    Some(r.chars[r.pos])
  } else {
    None
  }
}

///|
fn reader_next(r : Reader) -> Char? {
  let ch = reader_peek(r)
  match ch {
    Some(_) => r.pos = r.pos + 1
    None => ()
  }
  ch
}

///|
fn is_delim(ch : Char) -> Bool {
  if ch.is_ascii_whitespace() {
    true
  } else {
    ch == '(' || ch == ')' || ch == '\'' || ch == '`' || ch == ',' || ch == '"' || ch == ';'
  }
}

///|
fn skip_ws_and_comments(r : Reader) -> Unit {
  while true {
    match reader_peek(r) {
      Some(ch) if ch.is_ascii_whitespace() => {
        r.pos = r.pos + 1
        continue
      }
      Some(';') => {
        // Line comment
        r.pos = r.pos + 1
        while true {
          match reader_peek(r) {
            Some('\n') => {
              r.pos = r.pos + 1
              break
            }
            Some(_) => {
              r.pos = r.pos + 1
              continue
            }
            None => break
          }
        }
        continue
      }
      _ => break
    }
  }
}

///|
fn is_ellipsis_start(r : Reader) -> Bool {
  if r.pos + 2 >= r.chars.length() {
    false
  } else if r.chars[r.pos] == '.' && r.chars[r.pos + 1] == '.' && r.chars[r.pos + 2] == '.' {
    if r.pos + 3 >= r.chars.length() {
      true
    } else {
      is_delim(r.chars[r.pos + 3])
    }
  } else {
    false
  }
}

///|
fn read_token(r : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  while true {
    match reader_peek(r) {
      Some(ch) if !is_delim(ch) => {
        chars.push(ch)
        r.pos = r.pos + 1
        continue
      }
      _ => break
    }
  }
  if chars.is_empty() {
    raise ParseError("expected token")
  }
  String::from_array(chars)
}

///|
fn parse_int_token(tok : String) -> Int? {
  let mut sign = 1
  let mut acc = 0
  let mut i = 0
  let mut has_digit = false
  for ch in tok {
    if i == 0 && (ch == '+' || ch == '-') {
      sign = if ch == '-' { -1 } else { 1 }
      i = i + 1
      continue
    }
    if ch.is_ascii_digit() {
      acc = acc * 10 + (ch.to_int() - '0'.to_int())
      has_digit = true
      i = i + 1
      continue
    }
    return None
  }
  if has_digit {
    Some(sign * acc)
  } else {
    None
  }
}

///|
fn read_string(r : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  while true {
    match reader_next(r) {
      None => raise ParseError("unterminated string")
      Some('"') => break
      Some('\\') => {
        match reader_next(r) {
          Some('n') => chars.push('\n')
          Some('t') => chars.push('\t')
          Some('r') => chars.push('\r')
          Some('"') => chars.push('"')
          Some('\\') => chars.push('\\')
          Some(ch) => chars.push(ch)
          None => raise ParseError("unterminated string")
        }
      }
      Some(ch) => chars.push(ch)
    }
  }
  String::from_array(chars)
}

///|
fn make_quote(expr : Datum) -> Datum {
  Datum::Pair(
    Datum::Symbol("quote"),
    Datum::Pair(expr, Datum::Nil),
  )
}

///|
fn make_quasiquote(expr : Datum) -> Datum {
  Datum::Pair(
    Datum::Symbol("quasiquote"),
    Datum::Pair(expr, Datum::Nil),
  )
}

///|
fn make_unquote(expr : Datum) -> Datum {
  Datum::Pair(
    Datum::Symbol("unquote"),
    Datum::Pair(expr, Datum::Nil),
  )
}

///|
fn make_unquote_splicing(expr : Datum) -> Datum {
  Datum::Pair(
    Datum::Symbol("unquote-splicing"),
    Datum::Pair(expr, Datum::Nil),
  )
}

///|
fn read_list(r : Reader) -> Datum raise ParseError {
  let items : Array[Datum] = []
  let mut dotted_tail : Datum? = None
  while true {
    skip_ws_and_comments(r)
    match reader_peek(r) {
      None => raise ParseError("unexpected EOF while reading list")
      Some(')') => {
        r.pos = r.pos + 1
        break
      }
      Some('.') => {
        if is_ellipsis_start(r) {
          let tok = read_token(r)
          items.push(Datum::Symbol(tok))
        } else {
          if items.is_empty() {
            raise ParseError("unexpected dot")
          }
          r.pos = r.pos + 1
          skip_ws_and_comments(r)
          let tail = read_expr(r)
          skip_ws_and_comments(r)
          match reader_peek(r) {
            Some(')') => r.pos = r.pos + 1
            _ => raise ParseError("expected ) after dotted tail")
          }
          dotted_tail = Some(tail)
          break
        }
      }
      _ => {
        let expr = read_expr(r)
        items.push(expr)
      }
    }
  }
  let mut tail = match dotted_tail {
    Some(t) => t
    None => Datum::Nil
  }
  for item in items.rev_iter() {
    tail = Datum::Pair(item, tail)
  }
  tail
}

///|
fn read_expr(r : Reader) -> Datum raise ParseError {
  skip_ws_and_comments(r)
  match reader_peek(r) {
    None => raise ParseError("unexpected EOF")
    Some('(') => {
      r.pos = r.pos + 1
      read_list(r)
    }
    Some(')') => {
      r.pos = r.pos + 1
      raise ParseError("unexpected )")
    }
    Some('\'') => {
      r.pos = r.pos + 1
      let expr = read_expr(r)
      make_quote(expr)
    }
    Some('`') => {
      r.pos = r.pos + 1
      let expr = read_expr(r)
      make_quasiquote(expr)
    }
    Some(',') => {
      r.pos = r.pos + 1
      let mut splicing = false
      match reader_peek(r) {
        Some('@') => {
          r.pos = r.pos + 1
          splicing = true
        }
        _ => ()
      }
      let expr = read_expr(r)
      if splicing {
        make_unquote_splicing(expr)
      } else {
        make_unquote(expr)
      }
    }
    Some('"') => {
      r.pos = r.pos + 1
      Datum::String(read_string(r))
    }
    Some(_) => {
      let tok = read_token(r)
      if tok == "#t" {
        Datum::Bool(true)
      } else if tok == "#f" {
        Datum::Bool(false)
      } else {
        match parse_int_token(tok) {
          Some(n) => Datum::Int(n)
          None => Datum::Symbol(tok)
        }
      }
    }
  }
}

///|
fn parse_program(src : String) -> Array[Datum] raise ParseError {
  let r = make_reader(src)
  let exprs : Array[Datum] = []
  while true {
    skip_ws_and_comments(r)
    match reader_peek(r) {
      None => break
      Some(_) => {
        let expr = read_expr(r)
        exprs.push(expr)
      }
    }
  }
  exprs
}

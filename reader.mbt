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
fn is_vector_start(r : Reader) -> Bool {
  r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '('
}

///|
fn is_bytevector_start(r : Reader) -> Bool {
  if r.pos + 4 >= r.chars.length() {
    false
  } else {
    let mut ok = r.chars[r.pos + 1] == 'v'
    ok = ok && r.chars[r.pos + 2] == 'u'
    ok = ok && r.chars[r.pos + 3] == '8'
    ok = ok && r.chars[r.pos + 4] == '('
    ok
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
fn digit_value(ch : Char) -> Int? {
  if ch.is_ascii_digit() {
    Some(ch.to_int() - '0'.to_int())
  } else if ch.is_ascii_lowercase() {
    Some(ch.to_int() - 'a'.to_int() + 10)
  } else if ch.is_ascii_uppercase() {
    Some(ch.to_int() - 'A'.to_int() + 10)
  } else {
    None
  }
}

///|
fn parse_int_token_radix(tok : String, radix : Int) -> Int? {
  if radix < 2 || radix > 36 {
    return None
  }
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
    match digit_value(ch) {
      Some(digit) => {
        if digit >= radix {
          return None
        }
        acc = acc * radix + digit
        has_digit = true
        i = i + 1
      }
      None => return None
    }
  }
  if has_digit {
    Some(sign * acc)
  } else {
    None
  }
}

///|
fn parse_rat_token(tok : String) -> Datum? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  let mut slash = -1
  let mut i = 0
  while i < chars.length() {
    if chars[i] == '/' {
      if slash != -1 {
        return None
      }
      slash = i
    }
    i = i + 1
  }
  if slash <= 0 || slash >= chars.length() - 1 {
    None
  } else {
    let num_str = String::from_array(chars.sub(end=slash).to_array())
    let den_str = String::from_array(chars.sub(start=slash + 1).to_array())
    match (parse_int_token(num_str), parse_int_token(den_str)) {
      (Some(num), Some(den)) => normalize_rat(num, den)
      _ => None
    }
  }
}

///|
fn parse_float_token(tok : String) -> Float? {
  let mut sign = Float::from_int(1)
  let mut int_part = 0
  let mut frac_part = 0
  let mut frac_div = 1
  let mut seen_dot = false
  let mut seen_digit = false
  let mut i = 0
  for ch in tok {
    if i == 0 && (ch == '+' || ch == '-') {
      sign = if ch == '-' { Float::from_int(-1) } else { Float::from_int(1) }
      i = i + 1
      continue
    }
    if ch == '.' {
      if seen_dot {
        return None
      }
      seen_dot = true
      i = i + 1
      continue
    }
    if ch.is_ascii_digit() {
      let digit = ch.to_int() - '0'.to_int()
      seen_digit = true
      if seen_dot {
        frac_part = frac_part * 10 + digit
        frac_div = frac_div * 10
      } else {
        int_part = int_part * 10 + digit
      }
      i = i + 1
      continue
    }
    return None
  }
  if !seen_dot || !seen_digit {
    None
  } else {
    let int_value = Float::from_int(int_part)
    let frac_value = Float::from_int(frac_part)
    let div_value = Float::from_int(frac_div)
    let value = sign * (int_value + frac_value / div_value)
    Some(value)
  }
}

///|
fn parse_char_token(tok : String) -> Char? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  if chars.length() < 3 {
    None
  } else if chars[0] != '#' || chars[1] != '\\' {
    None
  } else {
    let rest = chars.sub(start=2).to_array()
    if rest.length() == 1 {
      Some(rest[0])
    } else {
      let name = String::from_array(rest)
      match name {
        "space" => Some(' ')
        "newline" => Some('\n')
        "tab" => Some('\t')
        _ => None
      }
    }
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
  pair_new(Datum::Symbol("quote"), pair_new(expr, Datum::Nil))
}

///|
fn make_quasiquote(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("quasiquote"), pair_new(expr, Datum::Nil))
}

///|
fn make_unquote(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("unquote"), pair_new(expr, Datum::Nil))
}

///|
fn make_unquote_splicing(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("unquote-splicing"), pair_new(expr, Datum::Nil))
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
    tail = pair_new(item, tail)
  }
  tail
}

///|
fn datum_list_to_array_parse(list : Datum) -> Array[Datum] raise ParseError {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a.val)
        cur = b.val
      }
      _ => raise ParseError("expected proper list")
    }
  }
  items
}

///|
fn read_vector_literal(r : Reader) -> Datum raise ParseError {
  let list = read_list(r)
  let items = datum_list_to_array_parse(list)
  Datum::Vector(items)
}

///|
fn read_bytevector_literal(r : Reader) -> Datum raise ParseError {
  let list = read_list(r)
  let items = datum_list_to_array_parse(list)
  let bytes : Array[Int] = []
  for item in items {
    match item {
      Datum::Int(n) => {
        if n < 0 || n > 255 {
          raise ParseError("bytevector element out of range")
        }
        bytes.push(n)
      }
      _ => raise ParseError("bytevector element must be integer")
    }
  }
  Datum::ByteVector(bytes)
}

///|
fn token_to_datum(tok : String) -> Datum {
  if tok == "#t" {
    Datum::Bool(true)
  } else if tok == "#f" {
    Datum::Bool(false)
  } else {
    match parse_char_token(tok) {
      Some(ch) => Datum::Char(ch)
      None => {
        match parse_int_token(tok) {
          Some(n) => Datum::Int(n)
          None => {
            match parse_rat_token(tok) {
              Some(rat) => rat
              None => {
                match parse_float_token(tok) {
                  Some(f) => Datum::Float(f)
                  None => Datum::Symbol(tok)
                }
              }
            }
          }
        }
      }
    }
  }
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
    Some('#') => {
      if is_vector_start(r) {
        r.pos = r.pos + 2
        read_vector_literal(r)
      } else if is_bytevector_start(r) {
        r.pos = r.pos + 5
        read_bytevector_literal(r)
      } else {
        let tok = read_token(r)
        token_to_datum(tok)
      }
    }
    Some('"') => {
      r.pos = r.pos + 1
      Datum::String(Ref::new(read_string(r)))
    }
    Some(_) => {
      let tok = read_token(r)
      token_to_datum(tok)
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

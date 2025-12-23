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
fn pow10_int(exp : Int) -> Int {
  let mut result = 1
  let mut i = 0
  while i < exp {
    result = result * 10
    i = i + 1
  }
  result
}

///|
fn pow10_float(exp : Int) -> Float {
  let mut result = Float::from_int(1)
  let mut i = 0
  let limit = int_abs(exp)
  while i < limit {
    result = result * Float::from_int(10)
    i = i + 1
  }
  if exp < 0 {
    Float::from_int(1) / result
  } else {
    result
  }
}

///|
fn split_exp_token(tok : String) -> (String, Int)? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  let mut idx = -1
  let mut i = 0
  while i < chars.length() {
    let ch = chars[i]
    if ch == 'e' || ch == 'E' {
      idx = i
      break
    }
    i = i + 1
  }
  if idx == -1 {
    None
  } else if idx == 0 || idx == chars.length() - 1 {
    None
  } else {
    let base = String::from_array(chars.sub(end=idx).to_array())
    let exp_str = String::from_array(chars.sub(start=idx + 1).to_array())
    match parse_int_token(exp_str) {
      Some(exp) => Some((base, exp))
      None => None
    }
  }
}

///|
fn parse_float_token_with_exp(tok : String) -> Float? {
  match split_exp_token(tok) {
    Some((base, exp)) => {
      let base_val = match parse_float_token(base) {
        Some(f) => Some(f)
        None => {
          match parse_int_token(base) {
            Some(n) => Some(Float::from_int(n))
            None => None
          }
        }
      }
      match base_val {
        Some(value) => Some(value * pow10_float(exp))
        None => None
      }
    }
    None => parse_float_token(tok)
  }
}

///|
fn parse_exact_decimal(tok : String) -> Datum? {
  let mut base = tok
  let mut exp = 0
  match split_exp_token(tok) {
    Some((b, e)) => {
      base = b
      exp = e
    }
    None => ()
  }
  let chars : Array[Char] = []
  for ch in base {
    chars.push(ch)
  }
  if chars.is_empty() {
    return None
  }
  let mut sign = 1
  let mut i = 0
  if chars[0] == '+' || chars[0] == '-' {
    sign = if chars[0] == '-' { -1 } else { 1 }
    i = 1
  }
  let mut seen_dot = false
  let mut frac_len = 0
  let digits : Array[Char] = []
  while i < chars.length() {
    let ch = chars[i]
    if ch == '.' {
      if seen_dot {
        return None
      }
      seen_dot = true
    } else if ch.is_ascii_digit() {
      digits.push(ch)
      if seen_dot {
        frac_len = frac_len + 1
      }
    } else {
      return None
    }
    i = i + 1
  }
  if digits.is_empty() {
    return None
  }
  let mut acc = 0
  for ch in digits {
    acc = acc * 10 + (ch.to_int() - '0'.to_int())
  }
  let mut num = sign * acc
  let mut den = pow10_int(frac_len)
  if exp >= 0 {
    num = num * pow10_int(exp)
  } else {
    den = den * pow10_int(-exp)
  }
  normalize_rat(num, den)
}

///|
fn parse_rat_token_radix(tok : String, radix : Int) -> Datum? {
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
    match (parse_int_token_radix(num_str, radix), parse_int_token_radix(den_str, radix)) {
      (Some(num), Some(den)) => normalize_rat(num, den)
      _ => None
    }
  }
}

///|
fn datum_to_float(value : Datum) -> Float? {
  match value {
    Datum::Int(n) => Some(Float::from_int(n))
    Datum::Rat(n, d) => Some(Float::from_int(n) / Float::from_int(d))
    _ => None
  }
}

///|
fn parse_number_token_internal(tok : String, radix_override : Int?) -> Datum? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  let mut radix = match radix_override {
    Some(r) => r
    None => 10
  }
  let mut exactness : Char? = None
  let mut i = 0
  while i + 1 < chars.length() && chars[i] == '#' {
    let tag = chars[i + 1]
    match tag {
      'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' => {
        if radix_override is None {
          if radix != 10 {
            return None
          }
          radix = match tag {
            'b' | 'B' => 2
            'o' | 'O' => 8
            'd' | 'D' => 10
            _ => 16
          }
        } else {
          return None
        }
      }
      'e' | 'E' | 'i' | 'I' => {
        if exactness is Some(_) {
          return None
        }
        exactness = Some(tag)
      }
      _ => return None
    }
    i = i + 2
  }
  if i >= chars.length() {
    return None
  }
  let rest = String::from_array(chars.sub(start=i).to_array())
  let mut has_dot = false
  let mut has_exp = false
  let mut has_slash = false
  for ch in rest {
    if ch == '.' {
      has_dot = true
    } else if ch == 'e' || ch == 'E' {
      has_exp = true
    } else if ch == '/' {
      has_slash = true
    }
  }
  let allow_rat = radix_override is None
  if radix != 10 {
    if has_dot || has_exp {
      return None
    }
    if has_slash && !allow_rat {
      return None
    }
    let parsed = if has_slash {
      parse_rat_token_radix(rest, radix)
    } else {
      match parse_int_token_radix(rest, radix) {
        Some(n) => Some(Datum::Int(n))
        None => None
      }
    }
    match parsed {
      Some(datum) => {
        match exactness {
          Some('i') | Some('I') =>
            match datum_to_float(datum) {
              Some(f) => Some(Datum::Float(f))
              None => None
            }
          _ => Some(datum)
        }
      }
      None => None
    }
  } else {
    if has_slash {
      if !allow_rat {
        return None
      }
      match parse_rat_token(rest) {
        Some(datum) => {
          match exactness {
            Some('i') | Some('I') =>
              match datum_to_float(datum) {
                Some(f) => Some(Datum::Float(f))
                None => None
              }
            _ => Some(datum)
          }
        }
        None => None
      }
    } else if has_dot || has_exp {
      match exactness {
        Some('e') | Some('E') => parse_exact_decimal(rest)
        _ => {
          match parse_float_token_with_exp(rest) {
            Some(f) => Some(Datum::Float(f))
            None => None
          }
        }
      }
    } else {
      match parse_int_token(rest) {
        Some(n) => {
          match exactness {
            Some('i') | Some('I') => Some(Datum::Float(Float::from_int(n)))
            _ => Some(Datum::Int(n))
          }
        }
        None => None
      }
    }
  }
}

///|
fn parse_number_token(tok : String) -> Datum? {
  parse_number_token_internal(tok, None)
}

///|
fn parse_number_token_with_radix(tok : String, radix : Int) -> Datum? {
  parse_number_token_internal(tok, Some(radix))
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
        match parse_number_token(tok) {
          Some(num) => num
          None => Datum::Symbol(tok)
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

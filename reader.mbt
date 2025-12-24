///|
priv struct Reader {
  chars : Array[Char]
  mut pos : Int
  mut fold_case : Bool
  labels : Map[Int, Ref[Datum]]
}

///|
fn make_reader(src : String) -> Reader {
  let chars : Array[Char] = []
  for ch in src {
    chars.push(ch)
  }
  { chars, pos: 0, fold_case: false, labels: Map::new() }
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
fn skip_ws_and_comments(r : Reader) -> Unit raise ParseError {
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
      Some('#') =>
        if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '|' {
          skip_block_comment(r)
          continue
        } else {
          break
        }
      _ => break
    }
  }
}

///|
fn skip_block_comment(r : Reader) -> Unit raise ParseError {
  r.pos = r.pos + 2
  let mut depth = 1
  while depth > 0 {
    if r.pos >= r.chars.length() {
      raise ParseError("unterminated block comment")
    }
    if r.chars[r.pos] == '#' && r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '|' {
      depth = depth + 1
      r.pos = r.pos + 2
      continue
    }
    if r.chars[r.pos] == '|' && r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '#' {
      depth = depth - 1
      r.pos = r.pos + 2
      continue
    }
    r.pos = r.pos + 1
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
  let mut in_bar = false
  let mut started = false
  let mut fold_case = r.fold_case
  while true {
    match reader_peek(r) {
      Some(ch) => {
        if !in_bar && is_delim(ch) {
          break
        }
        started = true
        r.pos = r.pos + 1
        if !in_bar && chars.is_empty() && ch == '#' {
          if r.pos < r.chars.length() && r.chars[r.pos] == '\\' {
            fold_case = false
          }
        }
        if in_bar {
          if ch == '|' {
            in_bar = false
          } else if ch == '\\' {
            chars.push(read_identifier_escape(r))
          } else {
            chars.push(ch)
          }
        } else {
          if ch == '|' {
            in_bar = true
          } else if ch == '\\' {
            if chars.length() == 1 && chars[0] == '#' {
              chars.push(ch)
            } else {
              let escaped = read_identifier_escape(r)
              chars.push(if fold_case { unicode_char_foldcase(escaped) } else { escaped })
            }
          } else {
            let next_char =
              if fold_case { unicode_char_foldcase(ch) } else { ch }
            chars.push(next_char)
          }
        }
        continue
      }
      _ => break
    }
  }
  if in_bar {
    raise ParseError("unterminated identifier")
  }
  if !started {
    raise ParseError("expected token")
  }
  String::from_array(chars)
}

///|
fn read_identifier_escape(r : Reader) -> Char raise ParseError {
  match reader_next(r) {
    Some('x') | Some('X') => read_hex_escape(r)
    Some(ch) => ch
    None => raise ParseError("unterminated identifier")
  }
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
fn parse_label_token(tok : String) -> (Bool, Int)? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  if chars.length() < 3 || chars[0] != '#' {
    return None
  }
  let last = chars[chars.length() - 1]
  if last != '=' && last != '#' {
    return None
  }
  let mut acc = 0
  let mut i = 1
  while i < chars.length() - 1 {
    let ch = chars[i]
    if !ch.is_ascii_digit() {
      return None
    }
    acc = acc * 10 + (ch.to_int() - '0'.to_int())
    i = i + 1
  }
  Some((last == '=', acc))
}

///|
///|
fn parse_bigint_token_radix(tok : String, radix : Int) -> @bigint.BigInt? {
  if radix < 2 || radix > 36 {
    return None
  }
  let mut sign = 1
  let mut acc = bigint_from_int(0)
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
        acc = acc * bigint_from_int(radix) + bigint_from_int(digit)
        has_digit = true
        i = i + 1
      }
      None => return None
    }
  }
  if has_digit {
    Some(if sign < 0 { -acc } else { acc })
  } else {
    None
  }
}

///|
fn integer_datum_from_bigint(value : @bigint.BigInt) -> Datum {
  match bigint_to_int_option(value) {
    Some(v) => Datum::Int(v)
    None => Datum::BigInt(value)
  }
}

///|
fn parse_integer_token(tok : String) -> Datum? {
  match parse_bigint_token_radix(tok, 10) {
    Some(value) => Some(integer_datum_from_bigint(value))
    None => None
  }
}

///|
fn parse_integer_token_radix(tok : String, radix : Int) -> Datum? {
  match parse_bigint_token_radix(tok, radix) {
    Some(value) => Some(integer_datum_from_bigint(value))
    None => None
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
    match (parse_integer_token(num_str), parse_integer_token(den_str)) {
      (Some(num), Some(den)) => normalize_rat_datum(num, den)
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
///|
fn pow10_bigint(exp : Int) -> @bigint.BigInt {
  let mut result = bigint_from_int(1)
  let mut i = 0
  while i < exp {
    result = result * bigint_from_int(10)
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
  let mut acc = bigint_from_int(0)
  for ch in digits {
    acc = acc * bigint_from_int(10) + bigint_from_int(ch.to_int() - '0'.to_int())
  }
  let mut num = if sign < 0 { -acc } else { acc }
  let mut den = pow10_bigint(frac_len)
  if exp >= 0 {
    num = num * pow10_bigint(exp)
  } else {
    den = den * pow10_bigint(-exp)
  }
  normalize_big_rat(num, den)
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
    match (parse_integer_token_radix(num_str, radix), parse_integer_token_radix(den_str, radix)) {
      (Some(num), Some(den)) => normalize_rat_datum(num, den)
      _ => None
    }
  }
}

///|
fn normalize_rat_datum(num : Datum, den : Datum) -> Datum? {
  match (num, den) {
    (Datum::Int(n), Datum::Int(d)) => normalize_rat(n, d)
    (Datum::BigInt(n), Datum::BigInt(d)) => normalize_big_rat(n, d)
    (Datum::Int(n), Datum::BigInt(d)) => normalize_big_rat(bigint_from_int(n), d)
    (Datum::BigInt(n), Datum::Int(d)) => normalize_big_rat(n, bigint_from_int(d))
    _ => None
  }
}

///|
fn datum_to_float(value : Datum) -> Float? {
  match value {
    Datum::Int(n) => Some(Float::from_int(n))
    Datum::BigInt(n) =>
      try @strconv.parse_double(n.to_string()) catch {
        _ => None
      } noraise {
        v => Some(Float::from_double(v))
      }
    Datum::Rat(n, d) => Some(Float::from_int(n) / Float::from_int(d))
    Datum::BigRat(n, d) =>
      match (try? @strconv.parse_double(n.to_string()), try? @strconv.parse_double(d.to_string())) {
        (Ok(nn), Ok(dd)) => Some(Float::from_double(nn / dd))
        _ => None
      }
    Datum::Float(f) => Some(f)
    _ => None
  }
}

///|
fn reader_datum_is_zero(value : Datum) -> Bool {
  match value {
    Datum::Int(n) => n == 0
    Datum::BigInt(n) => n.is_zero()
    Datum::Rat(n, _) => n == 0
    Datum::BigRat(n, _) => n.is_zero()
    Datum::Float(f) => f == 0.0
    _ => false
  }
}

///|
fn reader_datum_is_inexact(value : Datum) -> Bool {
  match value {
    Datum::Float(_) => true
    _ => false
  }
}

///|
fn make_complex_datum(real : Datum, imag : Datum) -> Datum {
  if reader_datum_is_zero(imag) {
    if reader_datum_is_inexact(imag) && !reader_datum_is_inexact(real) {
      match datum_to_float(real) {
        Some(value) => Datum::Float(value)
        None => real
      }
    } else {
      real
    }
  } else {
    Datum::Complex(Ref::new(real), Ref::new(imag))
  }
}

///|
fn parse_special_float_token(tok : String, radix : Int, exactness : Char?) -> Datum? {
  if radix != 10 {
    return None
  }
  match exactness {
    Some('e') | Some('E') => return None
    _ => ()
  }
  let lower = tok.to_lower()
  if lower == "+inf.0" || lower == "inf.0" {
    Some(Datum::Float(Float::from_int(1) / 0.0))
  } else if lower == "-inf.0" {
    Some(Datum::Float(Float::from_int(-1) / 0.0))
  } else if lower == "+nan.0" || lower == "-nan.0" || lower == "nan.0" {
    Some(Datum::Float(Float::from_int(0) / 0.0))
  } else {
    None
  }
}

///|
fn parse_real_token_with_settings(
  tok : String,
  radix : Int,
  exactness : Char?,
  allow_rat : Bool
) -> Datum? {
  if tok.length() == 0 {
    return None
  }
  match parse_special_float_token(tok, radix, exactness) {
    Some(value) => return Some(value)
    None => ()
  }
  let mut has_dot = false
  let mut has_exp = false
  let mut has_slash = false
  for ch in tok {
    if ch == '.' {
      has_dot = true
    } else if ch == 'e' || ch == 'E' {
      has_exp = true
    } else if ch == '/' {
      has_slash = true
    }
  }
  if radix != 10 {
    if has_dot || has_exp {
      return None
    }
    if has_slash && !allow_rat {
      return None
    }
    let parsed = if has_slash {
      parse_rat_token_radix(tok, radix)
    } else {
      parse_integer_token_radix(tok, radix)
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
      match parse_rat_token(tok) {
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
        Some('e') | Some('E') => parse_exact_decimal(tok)
        _ => {
          match parse_float_token_with_exp(tok) {
            Some(f) => Some(Datum::Float(f))
            None => None
          }
        }
      }
    } else {
      match parse_integer_token(tok) {
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
    }
  }
}

///|
fn parse_complex_token(
  tok : String,
  radix : Int,
  exactness : Char?,
  allow_rat : Bool
) -> Datum? {
  let chars : Array[Char] = []
  for ch in tok {
    chars.push(ch)
  }
  let mut at = -1
  let mut idx = 0
  while idx < chars.length() {
    if chars[idx] == '@' {
      if at != -1 {
        return None
      }
      at = idx
    }
    idx = idx + 1
  }
  if at != -1 {
    if at == 0 || at == chars.length() - 1 {
      return None
    }
    let left = String::from_array(chars.sub(end=at).to_array())
    let right = String::from_array(chars.sub(start=at + 1).to_array())
    let r = match parse_real_token_with_settings(left, radix, exactness, allow_rat) {
      Some(value) => value
      None => return None
    }
    let theta = match parse_real_token_with_settings(right, radix, exactness, allow_rat) {
      Some(value) => value
      None => return None
    }
    let rf = match datum_to_float(r) {
      Some(value) => value
      None => return None
    }
    let tf = match datum_to_float(theta) {
      Some(value) => value
      None => return None
    }
    let real = Datum::Float(rf * @math.cosf(tf))
    let imag = Datum::Float(rf * @math.sinf(tf))
    return Some(make_complex_datum(real, imag))
  }
  if chars.length() == 0 {
    return None
  }
  let last = chars[chars.length() - 1]
  if last != 'i' && last != 'I' {
    return None
  }
  let core = String::from_array(chars.sub(end=chars.length() - 1).to_array())
  if core.length() == 0 || core == "+" || core == "-" {
    let sign = if core == "-" { -1 } else { 1 }
    let imag = match exactness {
      Some('i') | Some('I') => Datum::Float(Float::from_int(sign))
      _ => Datum::Int(sign)
    }
    return Some(make_complex_datum(Datum::Int(0), imag))
  }
  let core_chars : Array[Char] = []
  for ch in core {
    core_chars.push(ch)
  }
  let mut split = -1
  if core_chars.length() > 1 {
    let mut i = core_chars.length() - 1
    while i > 0 {
      let ch = core_chars[i]
      if ch == '+' || ch == '-' {
        if radix == 10 && i > 0 {
          let prev = core_chars[i - 1]
          if prev == 'e' || prev == 'E' {
            i = i - 1
            continue
          }
        }
        split = i
        break
      }
      i = i - 1
    }
  }
  if split != -1 {
    let real_str = String::from_array(core_chars.sub(end=split).to_array())
    let imag_str = String::from_array(core_chars.sub(start=split).to_array())
    let real = match parse_real_token_with_settings(real_str, radix, exactness, allow_rat) {
      Some(value) => value
      None => return None
    }
    let imag = if imag_str == "+" || imag_str == "-" {
      let sign = if imag_str == "-" { -1 } else { 1 }
      match exactness {
        Some('i') | Some('I') => Datum::Float(Float::from_int(sign))
        _ => Datum::Int(sign)
      }
    } else {
      match parse_real_token_with_settings(imag_str, radix, exactness, allow_rat) {
        Some(value) => value
        None => return None
      }
    }
    return Some(make_complex_datum(real, imag))
  }
  let imag = match parse_real_token_with_settings(core, radix, exactness, allow_rat) {
    Some(value) => value
    None => return None
  }
  Some(make_complex_datum(Datum::Int(0), imag))
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
  let allow_rat = radix_override is None
  match parse_complex_token(rest, radix, exactness, allow_rat) {
    Some(datum) => Some(datum)
    None => parse_real_token_with_settings(rest, radix, exactness, allow_rat)
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
      let lower = ascii_lower_string(name)
      if lower.length() >= 2 {
        match lower.get_char(0) {
          Some('x') => {
            let hex_chars = rest.sub(start=1).to_array()
            if hex_chars.is_empty() {
              return None
            }
            let mut acc = 0
            for ch in hex_chars {
              match digit_value(ch) {
                Some(digit) =>
                  if digit < 16 {
                    acc = acc * 16 + digit
                  } else {
                    return None
                  }
                None => return None
              }
            }
            return acc.to_char()
          }
          _ => ()
        }
      }
      match lower {
        "space" => Some(' ')
        "newline" => Some('\n')
        "linefeed" => Some('\n')
        "tab" => Some('\t')
        "return" => Some('\r')
        "alarm" => Some('\u{7}')
        "backspace" => Some('\u{8}')
        "escape" => Some('\u{1b}')
        "delete" => Some('\u{7f}')
        "null" => Some('\u{0}')
        _ => None
      }
    }
  }
}

///|
fn ascii_lower_string(value : String) -> String {
  let chars : Array[Char] = []
  for ch in value {
    chars.push(ch.to_ascii_lowercase())
  }
  String::from_array(chars)
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
          Some('\n') => skip_string_line_continuation(r)
          Some('\r') => {
            if reader_peek(r) == Some('\n') {
              r.pos = r.pos + 1
            }
            skip_string_line_continuation(r)
          }
          Some('n') => chars.push('\n')
          Some('t') => chars.push('\t')
          Some('r') => chars.push('\r')
          Some('a') => chars.push('\u{7}')
          Some('b') => chars.push('\u{8}')
          Some('v') => chars.push('\u{0b}')
          Some('f') => chars.push('\u{0c}')
          Some('x') | Some('X') => chars.push(read_hex_escape(r))
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
fn skip_string_line_continuation(r : Reader) -> Unit {
  while true {
    match reader_peek(r) {
      Some(ch) if ch.is_ascii_whitespace() => {
        r.pos = r.pos + 1
        continue
      }
      _ => break
    }
  }
}

///|
fn read_hex_escape(r : Reader) -> Char raise ParseError {
  let mut acc = 0
  let mut has_digit = false
  while true {
    match reader_next(r) {
      None => raise ParseError("unterminated string")
      Some(';') => {
        if !has_digit {
          raise ParseError("invalid hex escape")
        }
        match acc.to_char() {
          Some(ch) => return ch
          None => raise ParseError("invalid hex escape")
        }
      }
      Some(ch) =>
        match digit_value(ch) {
          Some(digit) if digit < 16 => {
            acc = acc * 16 + digit
            has_digit = true
          }
          _ => raise ParseError("invalid hex escape")
        }
    }
  }
  raise ParseError("invalid hex escape")
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
fn make_syntax(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("syntax"), pair_new(expr, Datum::Nil))
}

///|
fn make_quasisyntax(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("quasisyntax"), pair_new(expr, Datum::Nil))
}

///|
fn make_unsyntax(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("unsyntax"), pair_new(expr, Datum::Nil))
}

///|
fn make_unsyntax_splicing(expr : Datum) -> Datum {
  pair_new(Datum::Symbol("unsyntax-splicing"), pair_new(expr, Datum::Nil))
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
      if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '!' {
        let tok = read_token(r)
        let directive = ascii_lower_string(tok)
        if directive == "#!fold-case" {
          r.fold_case = true
        } else if directive == "#!no-fold-case" {
          r.fold_case = false
        }
        read_expr(r)
      } else if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1].is_ascii_digit() {
        let tok = read_token(r)
        match parse_label_token(tok) {
          Some((true, label)) => {
            match r.labels.get(label) {
              Some(_) => raise ParseError("duplicate label")
              None => ()
            }
            let cell = Ref::new(Datum::Nil)
            r.labels[label] = cell
            let datum = read_expr(r)
            cell.val = datum
            Datum::Label(label, cell)
          }
          Some((false, label)) =>
            match r.labels.get(label) {
              Some(cell) => Datum::Label(label, cell)
              None => raise ParseError("undefined label")
            }
          None => token_to_datum(tok)
        }
      } else if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == ';' {
        r.pos = r.pos + 2
        let _ = read_expr(r)
        read_expr(r)
      } else if r.pos + 1 < r.chars.length() {
        match r.chars[r.pos + 1] {
          '\'' => {
            r.pos = r.pos + 2
            let expr = read_expr(r)
            make_syntax(expr)
          }
          '`' => {
            r.pos = r.pos + 2
            let expr = read_expr(r)
            make_quasisyntax(expr)
          }
          ',' => {
            r.pos = r.pos + 2
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
              make_unsyntax_splicing(expr)
            } else {
              make_unsyntax(expr)
            }
          }
          _ => {
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
        }
      } else if is_vector_start(r) {
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

///|
using @core {type Datum, type ParseError}

///|
using @lexer {
  type Reader,
  digit_value,
  make_reader,
}

///|
fn string_to_chars(value : String) -> Array[Char] {
  value.to_array()
}

///|
fn parse_int_token(tok : String) -> Int? {
  let chars = tok.to_array()
  if chars.is_empty() {
    return None
  }
  let (start, sign) = match chars[0] {
    '+' => (1, 1)
    '-' => (1, -1)
    _ => (0, 1)
  }
  let (acc, has_digit) = for i = start, acc = 0, has_digit = false;
    i < chars.length(); {
      let ch = chars[i]
      if !ch.is_ascii_digit() {
        return None
      }
      continue i + 1, acc * 10 + (ch.to_int() - '0'.to_int()), true
    } else {
      (acc, has_digit)
    }
  if has_digit { Some(sign * acc) } else { None }
}

///|

///|
fn parse_label_token(tok : String) -> (Bool, Int)? {
  let chars = string_to_chars(tok)
  if chars.length() < 3 || chars[0] != '#' {
    return None
  }
  let last = chars[chars.length() - 1]
  if last != '=' && last != '#' {
    return None
  }
  let end = chars.length() - 1
  let acc = for i = 1, acc = 0; i < end; {
    let ch = chars[i]
    if !ch.is_ascii_digit() {
      return None
    }
    continue i + 1, acc * 10 + (ch.to_int() - '0'.to_int())
  } else {
    acc
  }
  Some((last == '=', acc))
}

///|
fn int_abs(value : Int) -> Int {
  if value < 0 {
    -value
  } else {
    value
  }
}

///|
fn gcd(a : Int, b : Int) -> Int {
  for x = int_abs(a), y = int_abs(b); y != 0; {
    continue y, x % y
  } else {
    x
  }
}

///|
fn normalize_rat(num : Int, den : Int) -> Datum? {
  if den == 0 {
    None
  } else if num == 0 {
    Some(Datum::Int(0))
  } else {
    let (n, d) = if den < 0 { (-num, -den) } else { (num, den) }
    let g = gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd == 1 {
      Some(Datum::Int(nn))
    } else {
      Some(Datum::Rat(nn, dd))
    }
  }
}

///|
fn bigint_from_int(value : Int) -> @bigint.BigInt {
  @bigint.BigInt::from_int(value)
}

///|
fn bigint_is_zero(value : @bigint.BigInt) -> Bool {
  value.is_zero()
}

///|
fn bigint_abs(value : @bigint.BigInt) -> @bigint.BigInt {
  if value.compare_int(0) < 0 {
    -value
  } else {
    value
  }
}

///|
fn bigint_to_int_option(value : @bigint.BigInt) -> Int? {
  let int_max = 2147483647
  let int_min = -2147483648
  if value.compare_int(int_max) > 0 || value.compare_int(int_min) < 0 {
    None
  } else {
    Some(value.to_int())
  }
}

///|
fn bigint_gcd(a : @bigint.BigInt, b : @bigint.BigInt) -> @bigint.BigInt {
  for x = bigint_abs(a), y = bigint_abs(b); !bigint_is_zero(y); {
    continue y, x % y
  } else {
    x
  }
}

///|
fn normalize_big_rat(num : @bigint.BigInt, den : @bigint.BigInt) -> Datum? {
  if bigint_is_zero(den) {
    None
  } else if bigint_is_zero(num) {
    Some(Datum::Int(0))
  } else {
    let (n, d) = if den.compare_int(0) < 0 {
      (-num, -den)
    } else {
      (num, den)
    }
    let g = bigint_gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd.compare_int(1) == 0 {
      Some(
        match bigint_to_int_option(nn) {
          Some(v) => Datum::Int(v)
          None => Datum::BigInt(nn)
        },
      )
    } else {
      match (bigint_to_int_option(nn), bigint_to_int_option(dd)) {
        (Some(nv), Some(dv)) => Some(Datum::Rat(nv, dv))
        _ => Some(Datum::BigRat(nn, dd))
      }
    }
  }
}

///|
fn pair_new(car : Datum, cdr : Datum) -> Datum {
  Datum::Pair(Ref::new(car), Ref::new(cdr))
}

///|

///|
fn parse_bigint_token_radix(tok : String, radix : Int) -> @bigint.BigInt? {
  if radix < 2 || radix > 36 {
    return None
  }
  let mut sign = 1
  let mut acc = bigint_from_int(0)
  let mut first = true
  let mut has_digit = false
  for ch in tok {
    if first && (ch == '+' || ch == '-') {
      sign = if ch == '-' { -1 } else { 1 }
      first = false
      continue
    }
    first = false
    match digit_value(ch) {
      Some(digit) => {
        if digit >= radix {
          return None
        }
        acc = acc * bigint_from_int(radix) + bigint_from_int(digit)
        has_digit = true
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
fn find_unique_char(chars : Array[Char], target : Char) -> Int? {
  let mut found : Int? = None
  for i, ch in chars {
    if ch == target {
      if found is Some(_) {
        return None
      }
      found = Some(i)
    }
  }
  found
}

///|
fn parse_rat_token(tok : String) -> Datum? {
  let chars = string_to_chars(tok)
  match find_unique_char(chars, '/') {
    Some(slash) if slash > 0 && slash < chars.length() - 1 => {
      let num_str = String::from_array(chars.sub(end=slash).to_array())
      let den_str = String::from_array(chars.sub(start=slash + 1).to_array())
      match (parse_integer_token(num_str), parse_integer_token(den_str)) {
        (Some(num), Some(den)) => normalize_rat_datum(num, den)
        _ => None
      }
    }
    _ => None
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
  let mut first = true
  for ch in tok {
    if first && (ch == '+' || ch == '-') {
      sign = if ch == '-' { Float::from_int(-1) } else { Float::from_int(1) }
      first = false
      continue
    }
    first = false
    if ch == '.' {
      if seen_dot {
        return None
      }
      seen_dot = true
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
  let ten = bigint_from_int(10)
  for i = 0, acc = bigint_from_int(1); i < exp; {
    continue i + 1, acc * ten
  } else {
    acc
  }
}

///|
fn pow10_float(exp : Int) -> Float {
  let limit = int_abs(exp)
  let ten = Float::from_int(10)
  let result = for i = 0, acc = Float::from_int(1); i < limit; {
    continue i + 1, acc * ten
  } else {
    acc
  }
  if exp < 0 {
    Float::from_int(1) / result
  } else {
    result
  }
}

///|
fn split_exp_token(tok : String) -> (String, Int)? {
  let chars = string_to_chars(tok)
  let mut idx = -1
  for i, ch in chars {
    if ch == 'e' || ch == 'E' {
      idx = i
      break
    }
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
        None =>
          match parse_int_token(base) {
            Some(n) => Some(Float::from_int(n))
            None => None
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
  let (base, exp) = match split_exp_token(tok) {
    Some((b, e)) => (b, e)
    None => (tok, 0)
  }
  let chars = string_to_chars(base)
  if chars.is_empty() {
    return None
  }
  let mut sign = 1
  let mut start = 0
  if chars[0] == '+' || chars[0] == '-' {
    sign = if chars[0] == '-' { -1 } else { 1 }
    start = 1
  }
  let mut seen_dot = false
  let mut frac_len = 0
  let mut acc = bigint_from_int(0)
  let mut has_digit = false
  let ten = bigint_from_int(10)
  for ch in chars.sub(start=start) {
    if ch == '.' {
      if seen_dot {
        return None
      }
      seen_dot = true
    } else if ch.is_ascii_digit() {
      has_digit = true
      acc = acc * ten + bigint_from_int(ch.to_int() - '0'.to_int())
      if seen_dot {
        frac_len = frac_len + 1
      }
    } else {
      return None
    }
  }
  if !has_digit {
    return None
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
  let chars = string_to_chars(tok)
  match find_unique_char(chars, '/') {
    Some(slash) if slash > 0 && slash < chars.length() - 1 => {
      let num_str = String::from_array(chars.sub(end=slash).to_array())
      let den_str = String::from_array(chars.sub(start=slash + 1).to_array())
      match
        (
          parse_integer_token_radix(num_str, radix),
          parse_integer_token_radix(den_str, radix),
        ) {
        (Some(num), Some(den)) => normalize_rat_datum(num, den)
        _ => None
      }
    }
    _ => None
  }
}

///|
fn normalize_rat_datum(num : Datum, den : Datum) -> Datum? {
  match (num, den) {
    (Datum::Int(n), Datum::Int(d)) => normalize_rat(n, d)
    (Datum::BigInt(n), Datum::BigInt(d)) => normalize_big_rat(n, d)
    (Datum::Int(n), Datum::BigInt(d)) =>
      normalize_big_rat(bigint_from_int(n), d)
    (Datum::BigInt(n), Datum::Int(d)) =>
      normalize_big_rat(n, bigint_from_int(d))
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
      match
        (
          try? @strconv.parse_double(n.to_string()),
          try? @strconv.parse_double(d.to_string()),
        ) {
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
fn parse_special_float_token(
  tok : String,
  radix : Int,
  exactness : Char?,
) -> Datum? {
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
  allow_rat : Bool,
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
      Some(datum) =>
        match exactness {
          Some('i') | Some('I') =>
            match datum_to_float(datum) {
              Some(f) => Some(Datum::Float(f))
              None => None
            }
          _ => Some(datum)
        }
      None => None
    }
  } else if has_slash {
    if !allow_rat {
      return None
    }
    match parse_rat_token(tok) {
      Some(datum) =>
        match exactness {
          Some('i') | Some('I') =>
            match datum_to_float(datum) {
              Some(f) => Some(Datum::Float(f))
              None => None
            }
          _ => Some(datum)
        }
      None => None
    }
  } else if has_dot || has_exp {
    match exactness {
      Some('e') | Some('E') => parse_exact_decimal(tok)
      _ =>
        match parse_float_token_with_exp(tok) {
          Some(f) => Some(Datum::Float(f))
          None => None
        }
    }
  } else {
    match parse_integer_token(tok) {
      Some(datum) =>
        match exactness {
          Some('i') | Some('I') =>
            match datum_to_float(datum) {
              Some(f) => Some(Datum::Float(f))
              None => None
            }
          _ => Some(datum)
        }
      None => None
    }
  }
}

///|
fn find_complex_split(core_chars : Array[Char], radix : Int) -> Int? {
  if core_chars.length() <= 1 {
    return None
  }
  for i = core_chars.length(); i > 1; {
    let idx = i - 1
    let ch = core_chars[idx]
    if ch == '+' || ch == '-' {
      if radix == 10 {
        let prev = core_chars[idx - 1]
        if prev == 'e' || prev == 'E' {
          continue i - 1
        }
      }
      return Some(idx)
    }
    continue i - 1
  } else {
    None
  }
}

///|
fn parse_complex_token(
  tok : String,
  radix : Int,
  exactness : Char?,
  allow_rat : Bool,
) -> Datum? {
  let chars = string_to_chars(tok)
  let mut at = -1
  for i, ch in chars {
    if ch == '@' {
      if at != -1 {
        return None
      }
      at = i
    }
  }
  if at != -1 {
    if at == 0 || at == chars.length() - 1 {
      return None
    }
    let left = String::from_array(chars.sub(end=at).to_array())
    let right = String::from_array(chars.sub(start=at + 1).to_array())
    let r = match
      parse_real_token_with_settings(left, radix, exactness, allow_rat) {
      Some(value) => value
      None => return None
    }
    let theta = match
      parse_real_token_with_settings(right, radix, exactness, allow_rat) {
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
  let core_chars = string_to_chars(core)
  match find_complex_split(core_chars, radix) {
    Some(split) => {
      let real_str = String::from_array(core_chars.sub(end=split).to_array())
      let imag_str = String::from_array(core_chars.sub(start=split).to_array())
      let real = match
        parse_real_token_with_settings(real_str, radix, exactness, allow_rat) {
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
        match
          parse_real_token_with_settings(imag_str, radix, exactness, allow_rat) {
          Some(value) => value
          None => return None
        }
      }
      Some(make_complex_datum(real, imag))
    }
    None => {
      let imag = match
        parse_real_token_with_settings(core, radix, exactness, allow_rat) {
        Some(value) => value
        None => return None
      }
      Some(make_complex_datum(Datum::Int(0), imag))
    }
  }
}

///|
fn parse_number_token_internal(tok : String, radix_override : Int?) -> Datum? {
  let chars = string_to_chars(tok)
  let len = chars.length()
  let mut radix = match radix_override {
    Some(r) => r
    None => 10
  }
  let mut exactness : Char? = None
  let start = for i = 0; i + 1 < len && chars[i] == '#'; {
    let tag = chars[i + 1]
    match tag {
      'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' =>
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
      'e' | 'E' | 'i' | 'I' => {
        if exactness is Some(_) {
          return None
        }
        exactness = Some(tag)
      }
      _ => return None
    }
    continue i + 2
  } else {
    i
  }
  if start >= len {
    return None
  }
  let rest = String::from_array(chars.sub(start=start).to_array())
  let allow_rat = radix_override is None
  match parse_complex_token(rest, radix, exactness, allow_rat) {
    Some(datum) => Some(datum)
    None => parse_real_token_with_settings(rest, radix, exactness, allow_rat)
  }
}

///|
/// Parse a numeric token using default radix and exactness rules.
///
/// # Example
/// ```mbt check
/// test "parse number token" {
///   match parse_number_token("10") {
///     Some(@core.Datum::Int(10)) => ()
///     _ => fail("expected integer")
///   }
/// }
/// ```
pub fn parse_number_token(tok : String) -> Datum? {
  parse_number_token_internal(tok, None)
}

///|
/// Parse a numeric token using a specific radix override.
///
/// # Example
/// ```mbt check
/// test "parse number token with radix" {
///   match parse_number_token_with_radix("ff", 16) {
///     Some(@core.Datum::Int(255)) => ()
///     _ => fail("expected 255")
///   }
/// }
/// ```
pub fn parse_number_token_with_radix(tok : String, radix : Int) -> Datum? {
  parse_number_token_internal(tok, Some(radix))
}

///|
fn parse_char_token(tok : String) -> Char? {
  let chars = string_to_chars(tok)
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
  String::from_array(value.to_array().map((ch) => ch.to_ascii_lowercase()))
}

///|
fn make_unary_form(name : String, expr : Datum) -> Datum {
  pair_new(Datum::Symbol(name), pair_new(expr, Datum::Nil))
}

///|
fn make_quote(expr : Datum) -> Datum {
  make_unary_form("quote", expr)
}

///|
fn make_quasiquote(expr : Datum) -> Datum {
  make_unary_form("quasiquote", expr)
}

///|
fn make_unquote(expr : Datum) -> Datum {
  make_unary_form("unquote", expr)
}

///|
fn make_unquote_splicing(expr : Datum) -> Datum {
  make_unary_form("unquote-splicing", expr)
}

///|
fn make_syntax(expr : Datum) -> Datum {
  make_unary_form("syntax", expr)
}

///|
fn make_quasisyntax(expr : Datum) -> Datum {
  make_unary_form("quasisyntax", expr)
}

///|
fn make_unsyntax(expr : Datum) -> Datum {
  make_unary_form("unsyntax", expr)
}

///|
fn make_unsyntax_splicing(expr : Datum) -> Datum {
  make_unary_form("unsyntax-splicing", expr)
}

///|
fn read_list(r : Reader) -> Datum raise ParseError {
  let items : Array[Datum] = []
  let mut dotted_tail : Datum? = None
  while true {
    match r..skip_ws_and_comments().peek() {
      None => raise @core.ParseError("unexpected EOF while reading list")
      Some(')') => {
        r.pos = r.pos + 1
        break
      }
      Some('.') =>
        if r.is_ellipsis_start() {
          let tok = r.read_token()
          items.push(Datum::Symbol(tok))
        } else {
          if items.is_empty() {
            raise @core.ParseError("unexpected dot")
          }
          r.pos = r.pos + 1
          r.skip_ws_and_comments()
          let tail = read_expr(r)
          match r..skip_ws_and_comments().peek() {
            Some(')') => r.pos = r.pos + 1
            _ => raise @core.ParseError("expected ) after dotted tail")
          }
          dotted_tail = Some(tail)
          break
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
      _ => raise @core.ParseError("expected proper list")
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
          raise @core.ParseError("bytevector element out of range")
        }
        bytes.push(n)
      }
      _ => raise @core.ParseError("bytevector element must be integer")
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
      None =>
        match parse_number_token(tok) {
          Some(num) => num
          None => Datum::Symbol(tok)
        }
    }
  }
}

///|
fn read_expr(r : Reader) -> Datum raise ParseError {
  match r..skip_ws_and_comments().peek() {
    None => raise @core.ParseError("unexpected EOF")
    Some('(') => {
      r.pos = r.pos + 1
      read_list(r)
    }
    Some(')') => {
      r.pos = r.pos + 1
      raise @core.ParseError("unexpected )")
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
      match r.peek() {
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
    Some('#') =>
      if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '!' {
        let tok = r.read_token()
        let directive = ascii_lower_string(tok)
        if directive == "#!fold-case" {
          r.fold_case = true
        } else if directive == "#!no-fold-case" {
          r.fold_case = false
        }
        read_expr(r)
      } else if r.pos + 1 < r.chars.length() &&
        r.chars[r.pos + 1].is_ascii_digit() {
        let tok = r.read_token()
        match parse_label_token(tok) {
          Some((true, label)) => {
            match r.labels.get(label) {
              Some(_) => raise @core.ParseError("duplicate label")
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
              None => raise @core.ParseError("undefined label")
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
            match r.peek() {
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
          _ =>
            if r.is_vector_start() {
              r.pos = r.pos + 2
              read_vector_literal(r)
            } else if r.is_bytevector_start() {
              r.pos = r.pos + 5
              read_bytevector_literal(r)
            } else {
              let tok = r.read_token()
              token_to_datum(tok)
            }
        }
      } else if r.is_vector_start() {
        r.pos = r.pos + 2
        read_vector_literal(r)
      } else if r.is_bytevector_start() {
        r.pos = r.pos + 5
        read_bytevector_literal(r)
      } else {
        let tok = r.read_token()
        token_to_datum(tok)
      }
    Some('"') => {
      r.pos = r.pos + 1
      Datum::String(Ref::new(r.read_string()))
    }
    Some(_) => {
      let tok = r.read_token()
      token_to_datum(tok)
    }
  }
}

///|
/// Parse all top-level datums from a source string.
///
/// # Example
/// ```mbt check
/// test "parse program" {
///   let forms = parse_program("#\\a")
///   match forms {
///     [@core.Datum::Char('a'), ..] => ()
///     _ => fail("expected char datum")
///   }
/// }
/// ```
pub fn parse_program(src : String) -> Array[Datum] raise ParseError {
  parse_program_with_fold_case(src, false)
}

///|
/// Parse all top-level datums with optional case folding.
///
/// # Example
/// ```mbt check
/// test "parse program with fold case" {
///   let forms = parse_program_with_fold_case("ABC", true)
///   match forms {
///     [@core.Datum::Symbol("abc"), ..] => ()
///     _ => fail("expected folded symbol")
///   }
/// }
/// ```
pub fn parse_program_with_fold_case(
  src : String,
  fold_case : Bool,
) -> Array[Datum] raise ParseError {
  let r = make_reader(src)
  r.fold_case = fold_case
  let exprs : Array[Datum] = []
  while true {
    match r..skip_ws_and_comments().peek() {
      None => break
      Some(_) => {
        let expr = read_expr(r)
        exprs.push(expr)
      }
    }
  }
  exprs
}

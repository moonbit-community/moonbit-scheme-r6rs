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
      // invariant : i >= start && i <= chars.length()
      // decreases : chars.length() - i
      // assert : i <= chars.length()
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
    // invariant : i >= 1 && i <= end
    // decreases : end - i
    // assert : i <= end
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
    // invariant : x >= 0 && y >= 0
    // decreases : y
    // assert : y >= 0
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
    // invariant : x.compare_int(0) >= 0 && y.compare_int(0) >= 0
    // TODO(decreases) : BigInt measure not explicit; possible bug
    // assert : y.compare_int(0) >= 0
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
  let base = bigint_from_int(radix)
  let (sign, acc, has_digit) = for i = 0, sign = 1, acc = bigint_from_int(0),
    first = true, has_digit = false; i < tok.length(); {
      // invariant : i >= 0 && i <= tok.length()
      // decreases : tok.length() - i
      // assert : i <= tok.length()
      match tok.get_char(i) {
        Some(ch) => {
          if first && (ch == '+' || ch == '-') {
            let next_sign = if ch == '-' { -1 } else { 1 }
            continue i + 1, next_sign, acc, false, has_digit
          }
          match digit_value(ch) {
            Some(digit) => {
              if digit >= radix {
                return None
              }
              let next_acc = acc * base + bigint_from_int(digit)
              continue i + 1, sign, next_acc, false, true
            }
            None => return None
          }
        }
        None => break (sign, acc, has_digit)
      }
    } else {
      (sign, acc, has_digit)
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
  let found = for i = 0, found = -1; i < chars.length(); {
    // invariant : i >= 0 && i <= chars.length()
    // decreases : chars.length() - i
    // assert : i <= chars.length()
    let ch = chars[i]
    if ch == target {
      if found != -1 {
        return None
      }
      continue i + 1, i
    }
    continue i + 1, found
  } else {
    found
  }
  if found == -1 { None } else { Some(found) }
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
  let (sign, int_part, frac_part, frac_div, seen_dot, seen_digit) = for i = 0,
    sign = Float::from_int(1), int_part = 0, frac_part = 0, frac_div = 1,
    seen_dot = false, seen_digit = false, first = true; i < tok.length(); {
      // invariant : i >= 0 && i <= tok.length()
      // decreases : tok.length() - i
      // assert : i <= tok.length()
      match tok.get_char(i) {
        Some(ch) => {
          if first && (ch == '+' || ch == '-') {
            let next_sign =
              if ch == '-' { Float::from_int(-1) } else { Float::from_int(1) }
            continue i + 1, next_sign, int_part, frac_part, frac_div, seen_dot,
              seen_digit, false
          }
          if ch == '.' {
            if seen_dot {
              return None
            }
            continue i + 1, sign, int_part, frac_part, frac_div, true, seen_digit,
              false
          }
          if ch.is_ascii_digit() {
            let digit = ch.to_int() - '0'.to_int()
            let next_seen = true
            if seen_dot {
              let next_frac = frac_part * 10 + digit
              let next_div = frac_div * 10
              continue i + 1, sign, int_part, next_frac, next_div, seen_dot,
                next_seen, false
            } else {
              let next_int = int_part * 10 + digit
              continue i + 1, sign, next_int, frac_part, frac_div, seen_dot,
                next_seen, false
            }
          }
          return None
        }
        None => break (sign, int_part, frac_part, frac_div, seen_dot, seen_digit)
      }
    } else {
      (sign, int_part, frac_part, frac_div, seen_dot, seen_digit)
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
    // invariant : i >= 0 && i <= exp
    // decreases : exp - i
    // assert : i <= exp
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
    // invariant : i >= 0 && i <= limit
    // decreases : limit - i
    // assert : i <= limit
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
  let idx = for i = 0; i < chars.length(); {
    // invariant : i >= 0 && i <= chars.length()
    // decreases : chars.length() - i
    // assert : i <= chars.length()
    let ch = chars[i]
    if ch == 'e' || ch == 'E' {
      break i
    }
    continue i + 1
  } else {
    -1
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
  let (sign, start) = match chars[0] {
    '+' => (1, 1)
    '-' => (-1, 1)
    _ => (1, 0)
  }
  let ten = bigint_from_int(10)
  let (acc, frac_len, has_digit) = for i = start,
    acc = bigint_from_int(0), frac_len = 0, seen_dot = false, has_digit = false;
    i < chars.length(); {
      let ch = chars[i]
      if ch == '.' {
        if seen_dot {
          return None
        }
        continue i + 1, acc, frac_len, true, has_digit
      }
      if ch.is_ascii_digit() {
        let next_acc = acc * ten + bigint_from_int(ch.to_int() - '0'.to_int())
        let next_frac = if seen_dot { frac_len + 1 } else { frac_len }
        continue i + 1, next_acc, next_frac, seen_dot, true
      }
      return None
    } else {
      (acc, frac_len, has_digit)
    }
  if !has_digit {
    return None
  }
  let num = if sign < 0 { -acc } else { acc }
  let den = pow10_bigint(frac_len)
  let (num, den) = if exp >= 0 {
    (num * pow10_bigint(exp), den)
  } else {
    (num, den * pow10_bigint(-exp))
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
  let (has_dot, has_exp, has_slash) = for i = 0, has_dot = false, has_exp = false,
    has_slash = false; i < tok.length(); {
      match tok.get_char(i) {
        Some(ch) =>
          continue i + 1, has_dot || ch == '.',
            has_exp || ch == 'e' || ch == 'E', has_slash || ch == '/'
        None => break (has_dot, has_exp, has_slash)
      }
    } else {
      (has_dot, has_exp, has_slash)
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
    // invariant : i >= 1 && i <= core_chars.length()
    // decreases : i - 1
    // assert : i <= core_chars.length()
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
  let at = find_unique_char(chars, '@')
  if at is None && chars.contains('@') {
    return None
  }
  match at {
    Some(at) => {
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
    None => ()
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
  let base_radix = match radix_override {
    Some(r) => r
    None => 10
  }
  let default_exactness : Char? = None
  let (start, radix, exactness) = for i = 0, radix = base_radix,
    exactness = default_exactness; i + 1 < len && chars[i] == '#'; {
    let tag = chars[i + 1]
    match tag {
      'b' | 'B' | 'o' | 'O' | 'd' | 'D' | 'x' | 'X' =>
        if radix_override is None {
          if radix != 10 {
            return None
          }
          let next_radix = match tag {
            'b' | 'B' => 2
            'o' | 'O' => 8
            'd' | 'D' => 10
            _ => 16
          }
          continue i + 2, next_radix, exactness
        } else {
          return None
        }
      'e' | 'E' | 'i' | 'I' => {
        if exactness is Some(_) {
          return None
        }
        continue i + 2, radix, Some(tag)
      }
      _ => return None
    }
  } else {
    (i, radix, exactness)
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
fn parse_hex_digits(chars : Array[Char]) -> Int? {
  let (acc, has_digit) = for i = 0, acc = 0, has_digit = false;
    i < chars.length(); {
      // invariant : i >= 0 && i <= chars.length()
      // invariant : acc >= 0
      // decreases : chars.length() - i
      // assert : i <= chars.length()
      match digit_value(chars[i]) {
        Some(digit) if digit < 16 =>
          continue i + 1, acc * 16 + digit, true
        _ => return None
      }
    } else {
      (acc, has_digit)
    }
  if has_digit { Some(acc) } else { None }
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
            return match parse_hex_digits(hex_chars) {
              Some(value) => value.to_char()
              None => None
            }
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
fn list_from_items(items : Array[Datum], base : Datum) -> Datum {
  for i = items.length(), tail = base; i > 0; {
    // invariant : i >= 0 && i <= items.length()
    // decreases : i
    // assert : i <= items.length()
    let idx = i - 1
    continue i - 1, pair_new(items[idx], tail)
  } else {
    tail
  }
}

///|
/// Read a list or dotted list after consuming the opening parenthesis.
fn read_list(r : Reader) -> Datum raise ParseError {
  let items : Array[Datum] = []
  let list = for cur = r..skip_ws_and_comments().peek(); true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : reader position not exposed; possible bug
    // assert :
    //   match cur {
    //     Some('.') => r.is_ellipsis_start() || items.length() > 0
    //     _ => true
    //   }
    match cur {
      None => raise @core.ParseError("unexpected EOF while reading list")
      Some(')') => {
        ignore(r.next())
        break list_from_items(items, Datum::Nil)
      }
      Some('.') =>
        if r.is_ellipsis_start() {
          let tok = r.read_token()
          items.push(Datum::Symbol(tok))
          continue r..skip_ws_and_comments().peek()
        } else {
          if items.is_empty() {
            raise @core.ParseError("unexpected dot")
          }
          ignore(r.next())
          r.skip_ws_and_comments()
          let tail = read_expr(r)
          match r..skip_ws_and_comments().peek() {
            Some(')') => ignore(r.next())
            _ => raise @core.ParseError("expected ) after dotted tail")
          }
          break list_from_items(items, tail)
        }
      _ => {
        let expr = read_expr(r)
        items.push(expr)
        continue r..skip_ws_and_comments().peek()
      }
    }
  } else {
    Datum::Nil
  }
  list
}

///|
fn datum_list_to_array_parse(list : Datum) -> Array[Datum] raise ParseError {
  let items : Array[Datum] = []
  for cur = list; true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert :
    //   match cur {
    //     Datum::Pair(_, _) | Datum::Nil => true
    //     _ => false
    //   }
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a.val)
        continue b.val
      }
      _ => raise @core.ParseError("expected proper list")
    }
  } else {
    ()
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
  let bytes = items.map((item) =>
    match item {
      Datum::Int(n) => {
        if n < 0 || n > 255 {
          raise @core.ParseError("bytevector element out of range")
        }
        n
      }
      _ => raise @core.ParseError("bytevector element must be integer")
    }
  )
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
      ignore(r.next())
      read_list(r)
    }
    Some(')') => {
      ignore(r.next())
      raise @core.ParseError("unexpected )")
    }
    Some('\'') => {
      ignore(r.next())
      let expr = read_expr(r)
      make_quote(expr)
    }
    Some('`') => {
      ignore(r.next())
      let expr = read_expr(r)
      make_quasiquote(expr)
    }
    Some(',') => {
      ignore(r.next())
      let splicing = match r.peek() {
        Some('@') => {
          ignore(r.next())
          true
        }
        _ => false
      }
      let expr = read_expr(r)
      if splicing {
        make_unquote_splicing(expr)
      } else {
        make_unquote(expr)
      }
    }
    Some('#') =>
      match r.peek_next() {
        Some('!') => {
          let tok = r.read_token()
          let directive = ascii_lower_string(tok)
          if directive == "#!fold-case" {
            r.set_fold_case(true)
          } else if directive == "#!no-fold-case" {
            r.set_fold_case(false)
          }
          read_expr(r)
        }
        Some(ch) if ch.is_ascii_digit() => {
          let tok = r.read_token()
          match parse_label_token(tok) {
            Some((true, label)) => {
              match r.label_get(label) {
                Some(_) => raise @core.ParseError("duplicate label")
                None => ()
              }
              let cell = Ref::new(Datum::Nil)
              r.label_set(label, cell)
              let datum = read_expr(r)
              cell.val = datum
              Datum::Label(label, cell)
            }
            Some((false, label)) =>
              match r.label_get(label) {
                Some(cell) => Datum::Label(label, cell)
                None => raise @core.ParseError("undefined label")
              }
            None => token_to_datum(tok)
          }
        }
        Some(';') => {
          r.advance(2)
          let _ = read_expr(r)
          read_expr(r)
        }
        Some('\'') => {
          r.advance(2)
          let expr = read_expr(r)
          make_syntax(expr)
        }
        Some('`') => {
          r.advance(2)
          let expr = read_expr(r)
          make_quasisyntax(expr)
        }
        Some(',') => {
          r.advance(2)
          let splicing = match r.peek() {
            Some('@') => {
              ignore(r.next())
              true
            }
            _ => false
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
            r.advance(2)
            read_vector_literal(r)
          } else if r.is_bytevector_start() {
            r.advance(5)
            read_bytevector_literal(r)
          } else {
            let tok = r.read_token()
            token_to_datum(tok)
          }
      }
    Some('"') => {
      ignore(r.next())
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
  let r = {
    let r = make_reader(src)
    r..set_fold_case(fold_case)
    r
  }
  let exprs : Array[Datum] = []
  for cur = r..skip_ws_and_comments().peek(); cur is Some(_); {
    // invariant : exprs.length() >= 0
    // TODO(decreases) : reader position not explicit; possible bug
    // assert : exprs.length() >= 0
    let expr = read_expr(r)
    exprs.push(expr)
    continue r..skip_ws_and_comments().peek()
  } else {
    ()
  }
  exprs
}

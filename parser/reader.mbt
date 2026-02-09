///|
fn digit_value(ch : Char) -> Int? {
  match ch {
    '0'..='9' => Some(ch.to_int() - '0'.to_int())
    'a'..='f' => Some(ch.to_int() - 'a'.to_int() + 10)
    'A'..='F' => Some(ch.to_int() - 'A'.to_int() + 10)
    _ => None
  }
}

///|
fn parse_decimal_digits(chars : StringView) -> Int? {
  match chars {
    [] => None
    _ => {
      let mut acc = 0
      for ch in chars {
        if !ch.is_ascii_digit() {
          return None
        }
        acc = acc * 10 + (ch.to_int() - '0'.to_int())
      }
      Some(acc)
    }
  }
}

///|
fn parse_int_token(tok : String) -> Int? {
  let view = tok[:]
  match view {
    [] => None
    ['+', .. rest] => parse_decimal_digits(rest)
    ['-', .. rest] => parse_decimal_digits(rest).map(value => -value)
    _ => parse_decimal_digits(view)
  }
}

///|
/// Create an imaginary unit (1 or -1) based on exactness.
fn make_imag_unit(sign : Int, exactness : Char?) -> @core.Datum {
  match exactness {
    Some('i') | Some('I') => Float(Float::from_int(sign))
    _ => Int(sign)
  }
}

///|

///|
fn parse_label_token(tok : String) -> (Bool, Int)? {
  let view = tok[:]
  match view {
    ['#', .. digits, last] =>
      match last {
        '=' | '#' => parse_decimal_digits(digits).map(acc => (last == '=', acc))
        _ => None
      }
    _ => None
  }
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
  for x = Int::abs(a), y = Int::abs(b); y != 0; {
    // invariant : x >= 0 && y >= 0
    // decreases : y
    // assert : y >= 0
    continue y, x % y
  } else {
    x
  }
}

///|
fn normalize_rat(num : Int, den : Int) -> @core.Datum? {
  if den == 0 {
    None
  } else if num == 0 {
    Some(Int(0))
  } else {
    let (n, d) = if den < 0 { (-num, -den) } else { (num, den) }
    let g = gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd == 1 {
      Some(Int(nn))
    } else {
      Some(Rat(nn, dd))
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
fn normalize_big_rat(
  num : @bigint.BigInt,
  den : @bigint.BigInt,
) -> @core.Datum? {
  if bigint_is_zero(den) {
    None
  } else if bigint_is_zero(num) {
    Some(Int(0))
  } else {
    let (n, d) = if den.compare_int(0) < 0 { (-num, -den) } else { (num, den) }
    let g = bigint_gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd.compare_int(1) == 0 {
      Some(
        match bigint_to_int_option(nn) {
          Some(v) => Int(v)
          None => BigInt(nn)
        },
      )
    } else {
      match (bigint_to_int_option(nn), bigint_to_int_option(dd)) {
        (Some(nv), Some(dv)) => Some(Rat(nv, dv))
        _ => Some(BigRat(nn, dd))
      }
    }
  }
}

///|
fn pair_new(car : @core.Datum, cdr : @core.Datum) -> @core.Datum {
  Pair(Ref::new(car), Ref::new(cdr))
}

///|

///|
fn parse_bigint_token_radix(tok : String, radix : Int) -> @bigint.BigInt? {
  if radix < 2 || radix > 36 {
    return None
  }
  let base = bigint_from_int(radix)
  let (sign, acc, has_digit) = for i = 0, sign = 1, acc = bigint_from_int(0), first = true, has_digit = false; i <
                                  tok.length(); {
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
fn integer_datum_from_bigint(value : @bigint.BigInt) -> @core.Datum {
  match bigint_to_int_option(value) {
    Some(v) => Int(v)
    None => BigInt(value)
  }
}

///|
fn parse_integer_token(tok : String) -> @core.Datum? {
  parse_bigint_token_radix(tok, 10).map(value => integer_datum_from_bigint(
    value,
  ))
}

///|
fn parse_integer_token_radix(tok : String, radix : Int) -> @core.Datum? {
  parse_bigint_token_radix(tok, radix).map(value => integer_datum_from_bigint(
    value,
  ))
}

///|
fn view_contains_char(chars : StringView, target : Char) -> Bool {
  for ch in chars {
    if ch == target {
      return true
    }
  }
  false
}

///|
fn find_unique_char(chars : StringView, target : Char) -> Int? {
  let target_code = target.to_int()
  let found : Int? = for i = 0, found = None; i < chars.length(); {
    // invariant : i >= 0 && i <= chars.length()
    // invariant : found is None || (found is Some(idx) && idx < i)
    // decreases : chars.length() - i
    // assert : i <= chars.length()
    if chars[i].to_int() == target_code {
      match found {
        Some(_) => return None
        None => continue i + 1, Some(i)
      }
    }
    continue i + 1, found
  } else {
    found
  }
  found
}

///|
fn parse_rat_token(tok : String) -> @core.Datum? {
  let view = tok[:]
  match find_unique_char(view, '/') {
    Some(slash) if slash > 0 && slash + 1 < view.length() => {
      let num_view = try view.sub(end=slash) catch {
        _ => return None
      } noraise {
        view => view
      }
      let den_view = try view.sub(start=slash + 1) catch {
        _ => return None
      } noraise {
        view => view
      }
      let num_str = num_view.to_string()
      let den_str = den_view.to_string()
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
  let (sign, int_part, frac_part, frac_div, seen_dot, seen_digit) = for i = 0, sign = Float::from_int(
                                                                            1,
                                                                          ), int_part = 0, frac_part = 0, frac_div = 1, seen_dot = false, seen_digit = false, first = true; i <
                                                                       tok.length(); {
    // invariant : i >= 0 && i <= tok.length()
    // decreases : tok.length() - i
    // assert : i <= tok.length()
    match tok.get_char(i) {
      Some(ch) => {
        if first && (ch == '+' || ch == '-') {
          let next_sign = if ch == '-' {
            Float::from_int(-1)
          } else {
            Float::from_int(1)
          }
          continue i + 1,
            next_sign,
            int_part,
            frac_part,
            frac_div,
            seen_dot,
            seen_digit,
            false
        }
        if ch == '.' {
          if seen_dot {
            return None
          }
          continue i + 1,
            sign,
            int_part,
            frac_part,
            frac_div,
            true,
            seen_digit,
            false
        }
        if ch.is_ascii_digit() {
          let digit = ch.to_int() - '0'.to_int()
          let next_seen = true
          if seen_dot {
            let next_frac = frac_part * 10 + digit
            let next_div = frac_div * 10
            continue i + 1,
              sign,
              int_part,
              next_frac,
              next_div,
              seen_dot,
              next_seen,
              false
          } else {
            let next_int = int_part * 10 + digit
            continue i + 1,
              sign,
              next_int,
              frac_part,
              frac_div,
              seen_dot,
              next_seen,
              false
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
  let view = tok[:]
  let lower_e = 'e'.to_int()
  let upper_e = 'E'.to_int()
  let idx = for i = 0; i < view.length(); {
    // invariant : i >= 0 && i <= view.length()
    // decreases : view.length() - i
    // assert : i <= view.length()
    let code = view[i].to_int()
    if code == lower_e || code == upper_e {
      break i
    }
    continue i + 1
  } else {
    -1
  }
  if idx == -1 {
    None
  } else if idx == 0 || idx + 1 == view.length() {
    None
  } else {
    let base_view = try view.sub(end=idx) catch {
      _ => return None
    } noraise {
      view => view
    }
    let exp_view = try view.sub(start=idx + 1) catch {
      _ => return None
    } noraise {
      view => view
    }
    let base = base_view.to_string()
    let exp_str = exp_view.to_string()
    parse_int_token(exp_str).map(exp => (base, exp))
  }
}

///|
fn parse_float_token_with_exp(tok : String) -> Float? {
  match split_exp_token(tok) {
    Some((base, exp)) => {
      let base_val = match parse_float_token(base) {
        Some(f) => Some(f)
        None =>
          if parse_int_token(base) is Some(n) {
            Some(Float::from_int(n))
          } else {
            None
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
fn parse_exact_decimal(tok : String) -> @core.Datum? {
  let (base, exp) = match split_exp_token(tok) {
    Some((b, e)) => (b, e)
    None => (tok, 0)
  }
  let (sign, digits) = match base[:] {
    [] => return None
    ['+', .. rest] => (1, rest)
    ['-', .. rest] => (-1, rest)
    _ => (1, base[:])
  }
  let ten = bigint_from_int(10)
  let mut acc = bigint_from_int(0)
  let mut frac_len = 0
  let mut seen_dot = false
  let mut has_digit = false
  for ch in digits {
    if ch == '.' {
      if seen_dot {
        return None
      }
      seen_dot = true
      continue
    }
    if ch.is_ascii_digit() {
      acc = acc * ten + bigint_from_int(ch.to_int() - '0'.to_int())
      if seen_dot {
        frac_len = frac_len + 1
      }
      has_digit = true
      continue
    }
    return None
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
fn parse_rat_token_radix(tok : String, radix : Int) -> @core.Datum? {
  let view = tok[:]
  match find_unique_char(view, '/') {
    Some(slash) if slash > 0 && slash + 1 < view.length() => {
      let num_view = try view.sub(end=slash) catch {
        _ => return None
      } noraise {
        view => view
      }
      let den_view = try view.sub(start=slash + 1) catch {
        _ => return None
      } noraise {
        view => view
      }
      let num_str = num_view.to_string()
      let den_str = den_view.to_string()
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
fn normalize_rat_datum(num : @core.Datum, den : @core.Datum) -> @core.Datum? {
  match (num, den) {
    (Int(n), Int(d)) => normalize_rat(n, d)
    (BigInt(n), BigInt(d)) => normalize_big_rat(n, d)
    (Int(n), BigInt(d)) => normalize_big_rat(bigint_from_int(n), d)
    (BigInt(n), Int(d)) => normalize_big_rat(n, bigint_from_int(d))
    _ => None
  }
}

///|
fn datum_to_float(value : @core.Datum) -> Float? {
  match value {
    Int(n) => Some(Float::from_int(n))
    BigInt(n) =>
      try @strconv.parse_double(n.to_string()) catch {
        _ => None
      } noraise {
        v => Some(Float::from_double(v))
      }
    Rat(n, d) => Some(Float::from_int(n) / Float::from_int(d))
    BigRat(n, d) =>
      match
        (
          try? @strconv.parse_double(n.to_string()),
          try? @strconv.parse_double(d.to_string()),
        ) {
        (Ok(nn), Ok(dd)) => Some(Float::from_double(nn / dd))
        _ => None
      }
    Float(f) => Some(f)
    _ => None
  }
}

///|
fn apply_inexact_exactness(
  exactness : Char?,
  datum : @core.Datum,
) -> @core.Datum? {
  match exactness {
    Some('i') | Some('I') =>
      match datum_to_float(datum) {
        Some(f) => Some(Float(f))
        None => None
      }
    _ => Some(datum)
  }
}

///|
fn reader_datum_is_zero(value : @core.Datum) -> Bool {
  match value {
    Int(n) => n == 0
    BigInt(n) => n.is_zero()
    Rat(n, _) => n == 0
    BigRat(n, _) => n.is_zero()
    Float(f) => f == 0.0
    _ => false
  }
}

///|
fn reader_datum_is_inexact(value : @core.Datum) -> Bool {
  match value {
    Float(_) => true
    _ => false
  }
}

///|
fn make_complex_datum(real : @core.Datum, imag : @core.Datum) -> @core.Datum {
  if reader_datum_is_zero(imag) {
    if reader_datum_is_inexact(imag) && !reader_datum_is_inexact(real) {
      match datum_to_float(real) {
        Some(value) => Float(value)
        None => real
      }
    } else {
      real
    }
  } else {
    Complex(Ref::new(real), Ref::new(imag))
  }
}

///|
fn parse_special_float_token(
  tok : String,
  radix : Int,
  exactness : Char?,
) -> @core.Datum? {
  if radix != 10 {
    return None
  }
  match exactness {
    Some('e') | Some('E') => return None
    _ => ()
  }
  let lower = tok.to_lower()
  if lower == "+inf.0" || lower == "inf.0" {
    Some(Float(Float::from_int(1) / 0.0))
  } else if lower == "-inf.0" {
    Some(Float(Float::from_int(-1) / 0.0))
  } else if lower == "+nan.0" || lower == "-nan.0" || lower == "nan.0" {
    Some(Float(Float::from_int(0) / 0.0))
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
) -> @core.Datum? {
  if tok.length() == 0 {
    return None
  }
  match parse_special_float_token(tok, radix, exactness) {
    Some(value) => return Some(value)
    None => ()
  }
  let (has_dot, has_exp, has_slash) = for i = 0, has_dot = false, has_exp = false, has_slash = false; i <
                                         tok.length(); {
    // invariant : i >= 0 && i <= tok.length()
    // decreases : tok.length() - i
    // assert : i <= tok.length()
    match tok.get_char(i) {
      Some(ch) =>
        continue i + 1,
          has_dot || ch == '.',
          has_exp || ch == 'e' || ch == 'E',
          has_slash || ch == '/'
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
      Some(datum) => apply_inexact_exactness(exactness, datum)
      None => None
    }
  } else if has_slash {
    if !allow_rat {
      return None
    }
    match parse_rat_token(tok) {
      Some(datum) => apply_inexact_exactness(exactness, datum)
      None => None
    }
  } else if has_dot || has_exp {
    match exactness {
      Some('e') | Some('E') => parse_exact_decimal(tok)
      _ =>
        match parse_float_token_with_exp(tok) {
          Some(f) => Some(Float(f))
          None => None
        }
    }
  } else {
    match parse_integer_token(tok) {
      Some(datum) => apply_inexact_exactness(exactness, datum)
      None => None
    }
  }
}

///|
fn find_complex_split(core_chars : StringView, radix : Int) -> Int? {
  match core_chars {
    [] | [_] => None
    _ => {
      let plus_code = '+'.to_int()
      let minus_code = '-'.to_int()
      let lower_e = 'e'.to_int()
      let upper_e = 'E'.to_int()
      for i = core_chars.length(); i > 1; {
        // invariant : i >= 1 && i <= core_chars.length()
        // decreases : i - 1
        // assert : i <= core_chars.length()
        let idx = i - 1
        let code = core_chars[idx].to_int()
        if code == plus_code || code == minus_code {
          if radix == 10 {
            let prev = core_chars[idx - 1].to_int()
            if prev == lower_e || prev == upper_e {
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
  }
}

///|
fn parse_complex_token(
  tok : String,
  radix : Int,
  exactness : Char?,
  allow_rat : Bool,
) -> @core.Datum? {
  let view = tok[:]
  let at = find_unique_char(view, '@')
  if at is None && view_contains_char(view, '@') {
    return None
  }
  match at {
    Some(at) => {
      if at == 0 || at + 1 == view.length() {
        return None
      }
      let left_view = try view.sub(end=at) catch {
        _ => return None
      } noraise {
        view => view
      }
      let right_view = try view.sub(start=at + 1) catch {
        _ => return None
      } noraise {
        view => view
      }
      let left = left_view.to_string()
      let right = right_view.to_string()
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
  let (core_chars, last) = match view {
    [] => return None
    [.. core_chars, last] => (core_chars, last)
  }
  if last != 'i' && last != 'I' {
    return None
  }
  match core_chars {
    [] | ['+'] =>
      return Some(make_complex_datum(Int(0), make_imag_unit(1, exactness)))
    ['-'] =>
      return Some(make_complex_datum(Int(0), make_imag_unit(-1, exactness)))
    _ => ()
  }
  match find_complex_split(core_chars, radix) {
    Some(split) => {
      let real_view = try core_chars.sub(end=split) catch {
        _ => return None
      } noraise {
        view => view
      }
      let imag_view = try core_chars.sub(start=split) catch {
        _ => return None
      } noraise {
        view => view
      }
      let real_str = real_view.to_string()
      let real = match
        parse_real_token_with_settings(real_str, radix, exactness, allow_rat) {
        Some(value) => value
        None => return None
      }
      let imag = match imag_view {
        ['+'] => make_imag_unit(1, exactness)
        ['-'] => make_imag_unit(-1, exactness)
        _ => {
          let imag_str = imag_view.to_string()
          match
            parse_real_token_with_settings(
              imag_str, radix, exactness, allow_rat,
            ) {
            Some(value) => value
            None => return None
          }
        }
      }
      Some(make_complex_datum(real, imag))
    }
    None => {
      let core_str = core_chars.to_string()
      let imag = match
        parse_real_token_with_settings(core_str, radix, exactness, allow_rat) {
        Some(value) => value
        None => return None
      }
      Some(make_complex_datum(Int(0), imag))
    }
  }
}

///|
fn parse_number_token_internal(
  tok : String,
  radix_override : Int?,
) -> @core.Datum? {
  let view = tok[:]
  let (rest_view, radix, exactness) = match
    parse_number_prefix(view, radix_override) {
    Some(value) => value
    None => return None
  }
  match rest_view {
    [] => return None
    _ => ()
  }
  let rest = rest_view.to_string()
  let allow_rat = radix_override is None
  match parse_complex_token(rest, radix, exactness, allow_rat) {
    Some(datum) => Some(datum)
    None => parse_real_token_with_settings(rest, radix, exactness, allow_rat)
  }
}

///|
fn parse_number_prefix(
  view : StringView,
  radix_override : Int?,
) -> (StringView, Int, Char?)? {
  let base_radix = match radix_override {
    Some(r) => r
    None => 10
  }
  let disallow_radix = radix_override is Some(_)
  fn step(
    view : StringView,
    radix : Int,
    exactness : Char?,
    disallow_radix : Bool,
  ) -> (StringView, Int, Char?)? {
    match view {
      ['#', tag, .. rest] =>
        match tag {
          'b' | 'B' =>
            if disallow_radix || radix != 10 {
              None
            } else {
              step(rest, 2, exactness, disallow_radix)
            }
          'o' | 'O' =>
            if disallow_radix || radix != 10 {
              None
            } else {
              step(rest, 8, exactness, disallow_radix)
            }
          'd' | 'D' =>
            if disallow_radix || radix != 10 {
              None
            } else {
              step(rest, 10, exactness, disallow_radix)
            }
          'x' | 'X' =>
            if disallow_radix || radix != 10 {
              None
            } else {
              step(rest, 16, exactness, disallow_radix)
            }
          'e' | 'E' | 'i' | 'I' =>
            match exactness {
              Some(_) => None
              None => step(rest, radix, Some(tag), disallow_radix)
            }
          _ => None
        }
      _ => Some((view, radix, exactness))
    }
  }

  step(view, base_radix, None, disallow_radix)
}

///|
/// Parse a numeric token using default radix and exactness rules.
/// Optionally override the radix.
///
/// # Example
/// ```mbt check
/// test "parse number token" {
///   match parse_number_token("10") {
///     Some(Int(10)) => ()
///     _ => fail("expected integer")
///   }
///   match parse_number_token("ff", radix=16) {
///     Some(Int(255)) => ()
///     _ => fail("expected 255")
///   }
/// }
/// ```
pub fn parse_number_token(tok : String, radix? : Int) -> @core.Datum? {
  parse_number_token_internal(tok, radix)
}

///|
fn parse_hex_digits(chars : StringView) -> Int? {
  match chars {
    [] => None
    _ => {
      let mut acc = 0
      for ch in chars {
        match digit_value(ch) {
          Some(digit) if digit < 16 => acc = acc * 16 + digit
          _ => return None
        }
      }
      Some(acc)
    }
  }
}

///|
fn parse_char_token(tok : String) -> Char? {
  match tok[:] {
    ['#', '\\', .. rest] =>
      match rest {
        [] => None
        [ch] => Some(ch)
        _ => {
          let name = rest.to_string()
          let lower = ascii_lower_string(name)
          if lower.length() >= 2 {
            match lower.get_char(0) {
              Some('x') => {
                let hex_view = try rest.sub(start=1) catch {
                  _ => return None
                } noraise {
                  view => view
                }
                return match parse_hex_digits(hex_view) {
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
    _ => None
  }
}

///|
fn ascii_lower_string(value : String) -> String {
  String::from_array(value.to_array().map(ch => ch.to_ascii_lowercase()))
}

///|
fn make_unary_form(name : String, expr : @core.Datum) -> @core.Datum {
  pair_new(Symbol(name), pair_new(expr, Nil))
}

///|
fn make_quote(expr : @core.Datum) -> @core.Datum {
  make_unary_form("quote", expr)
}

///|
fn make_quasiquote(expr : @core.Datum) -> @core.Datum {
  make_unary_form("quasiquote", expr)
}

///|
fn make_unquote(expr : @core.Datum) -> @core.Datum {
  make_unary_form("unquote", expr)
}

///|
fn make_unquote_splicing(expr : @core.Datum) -> @core.Datum {
  make_unary_form("unquote-splicing", expr)
}

///|
fn make_syntax(expr : @core.Datum) -> @core.Datum {
  make_unary_form("syntax", expr)
}

///|
fn make_quasisyntax(expr : @core.Datum) -> @core.Datum {
  make_unary_form("quasisyntax", expr)
}

///|
fn make_unsyntax(expr : @core.Datum) -> @core.Datum {
  make_unary_form("unsyntax", expr)
}

///|
fn make_unsyntax_splicing(expr : @core.Datum) -> @core.Datum {
  make_unary_form("unsyntax-splicing", expr)
}

///|
fn list_from_items(
  items : Array[@core.Datum],
  base : @core.Datum,
) -> @core.Datum {
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
fn read_list(r : @lexer.Reader) -> @core.Datum raise @core.ParseError {
  let items : Array[@core.Datum] = []
  let list = for cur = r..skip_ws_and_comments().peek(); true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : reader position not exposed; possible bug
    // assert :
    //   match cur {
    //     Some('.') => r.is_ellipsis_start() || items.length() > 0
    //     _ => true
    //   }
    match cur {
      None => raise ParseError("unexpected EOF while reading list")
      Some(')') => {
        ignore(r.next())
        break list_from_items(items, Nil)
      }
      Some('.') =>
        if r.is_ellipsis_start() {
          let tok = r.read_token()
          items.push(Symbol(tok))
          continue r..skip_ws_and_comments().peek()
        } else {
          if items.is_empty() {
            raise ParseError("unexpected dot")
          }
          ignore(r.next())
          r.skip_ws_and_comments()
          let tail = read_expr(r)
          match r..skip_ws_and_comments().peek() {
            Some(')') => ignore(r.next())
            _ => raise ParseError("expected ) after dotted tail")
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
    Nil
  }
  list
}

///|
fn datum_list_to_array_parse(
  list : @core.Datum,
) -> Array[@core.Datum] raise @core.ParseError {
  let items : Array[@core.Datum] = []
  for cur = list; true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert :
    //   match cur {
    //     Pair(_, _) | Nil => true
    //     _ => false
    //   }
    match cur {
      Nil => break
      Pair(a, b) => {
        items.push(a.val)
        continue b.val
      }
      _ => raise ParseError("expected proper list")
    }
  } else {
    ()
  }
  items
}

///|
fn read_vector_literal(r : @lexer.Reader) -> @core.Datum raise @core.ParseError {
  let list = read_list(r)
  let items = datum_list_to_array_parse(list)
  Vector(items)
}

///|
fn read_bytevector_literal(
  r : @lexer.Reader,
) -> @core.Datum raise @core.ParseError {
  let list = read_list(r)
  let items = datum_list_to_array_parse(list)
  let bytes = items.map(item => match item {
    Int(n) => {
      if n < 0 || n > 255 {
        raise ParseError::ParseError("bytevector element out of range")
      }
      n
    }
    _ => raise ParseError::ParseError("bytevector element must be integer")
  })
  ByteVector(bytes)
}

///|
fn token_to_datum(tok : String) -> @core.Datum {
  if tok == "#t" {
    Bool(true)
  } else if tok == "#f" {
    Bool(false)
  } else if parse_char_token(tok) is Some(ch) {
    Char(ch)
  } else if parse_number_token(tok) is Some(num) {
    num
  } else {
    Symbol(tok)
  }
}

///|
priv enum CommaKind {
  Unquote
  Unsyntax
}

///|
fn read_comma_form(
  r : @lexer.Reader,
  kind : CommaKind,
) -> @core.Datum raise @core.ParseError {
  let splicing = match r.peek() {
    Some('@') => {
      ignore(r.next())
      true
    }
    _ => false
  }
  let expr = read_expr(r)
  match (kind, splicing) {
    (Unquote, true) => make_unquote_splicing(expr)
    (Unquote, false) => make_unquote(expr)
    (Unsyntax, true) => make_unsyntax_splicing(expr)
    (Unsyntax, false) => make_unsyntax(expr)
  }
}

///|
fn read_expr(r : @lexer.Reader) -> @core.Datum raise @core.ParseError {
  match r..skip_ws_and_comments().peek() {
    None => raise ParseError("unexpected EOF")
    Some('(') => {
      ignore(r.next())
      read_list(r)
    }
    Some(')') => {
      ignore(r.next())
      raise ParseError("unexpected )")
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
      read_comma_form(r, Unquote)
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
              if r.label_get(label) is Some(_) {
                raise ParseError("duplicate label")
              }
              let cell = Ref::new(Datum::Nil)
              r.label_set(label, cell)
              let datum = read_expr(r)
              cell.val = datum
              Label(label, cell)
            }
            Some((false, label)) =>
              if r.label_get(label) is Some(cell) {
                Label(label, cell)
              } else {
                raise ParseError("undefined label")
              }
            None => token_to_datum(tok)
          }
        }
        Some(';') => {
          ignore(r.next())
          ignore(r.next())
          let _ = read_expr(r)
          read_expr(r)
        }
        Some('\'') => {
          ignore(r.next())
          ignore(r.next())
          let expr = read_expr(r)
          make_syntax(expr)
        }
        Some('`') => {
          ignore(r.next())
          ignore(r.next())
          let expr = read_expr(r)
          make_quasisyntax(expr)
        }
        Some(',') => {
          ignore(r.next())
          ignore(r.next())
          read_comma_form(r, Unsyntax)
        }
        Some('(') => {
          ignore(r.next())
          ignore(r.next())
          read_vector_literal(r)
        }
        _ => {
          let tok = r.read_token()
          match tok {
            "#vu8" =>
              match r.peek() {
                Some('(') => {
                  ignore(r.next())
                  read_bytevector_literal(r)
                }
                _ => token_to_datum(tok)
              }
            _ => token_to_datum(tok)
          }
        }
      }
    Some('"') => {
      ignore(r.next())
      String(Ref::new(r.read_string()))
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
///     [Char('a'), ..] => ()
///     _ => fail("expected char datum")
///   }
/// }
/// ```
///
/// # Example
/// ```mbt check
/// test "parse program fold case" {
///   let forms = parse_program("ABC", fold_case=true)
///   match forms {
///     [Symbol("abc"), ..] => ()
///     _ => fail("expected folded symbol")
///   }
/// }
/// ```
pub fn parse_program(
  src : String,
  fold_case? : Bool = false,
) -> Array[@core.Datum] raise @core.ParseError {
  let r = {
    let r = @lexer.Reader::new(src)
    r..set_fold_case(fold_case)
    r
  }
  let exprs : Array[@core.Datum] = []
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

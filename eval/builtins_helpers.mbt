///|
/// Build a standardized arity mismatch error.
fn arity_mismatch(expected : Int, got : Int) -> EvalError {
  @core.EvalError("arity mismatch: expected \{expected} got \{got}")
}

///|
/// Return true if the datum is a proper list.
fn is_proper_list(value : Datum) -> Bool {
  let mut cur = value
  while true {
    match datum_unlabel(cur) {
      Datum::Nil => return true
      Datum::Pair(_, cdr) => cur = cdr.val
      _ => return false
    }
  }
  false
}

///|
priv enum CompareMode {
  Eq
  Lt
  Gt
  Le
  Ge
}

///|
fn compare_ok(mode : CompareMode, cmp : Int) -> Bool {
  match mode {
    CompareMode::Eq => cmp == 0
    CompareMode::Lt => cmp < 0
    CompareMode::Gt => cmp > 0
    CompareMode::Le => cmp <= 0
    CompareMode::Ge => cmp >= 0
  }
}

///|
fn string_to_chars(value : String) -> Array[Char] {
  let chars : Array[Char] = []
  for ch in value {
    chars.push(ch)
  }
  chars
}

///|
fn scopes_equal(left : Array[Int], right : Array[Int]) -> Bool {
  if left.length() != right.length() {
    false
  } else {
    for i, value in left {
      if value != right[i] {
        return false
      }
    }
    true
  }
}

///|
fn strip_gensym_suffix(name : String) -> String? {
  let chars = string_to_chars(name)
  let len = chars.length()
  if len < 5 {
    return None
  }
  let mut i = len
  while i > 0 {
    let ch = chars[i - 1]
    if ch >= '0' && ch <= '9' {
      i = i - 1
    } else {
      break
    }
  }
  if i == len || i < 4 {
    return None
  }
  if
    chars[i - 1] != 's' ||
    chars[i - 2] != 'g' ||
    chars[i - 3] != '_' ||
    chars[i - 4] != '_' {
    return None
  }
  let base_len = i - 4
  if base_len == 0 {
    return None
  }
  Some(String::from_array(chars.sub(end=base_len).to_array()))
}

///|
fn strip_all_gensym_suffixes(name : String) -> String {
  let mut cur = name
  while true {
    match strip_gensym_suffix(cur) {
      Some(next) => cur = next
      None => break
    }
  }
  cur
}

///|
/// eqv? semantics for runtime values.
fn value_eqv(left : Value, right : Value) -> Bool {
  match (left, right) {
    (Value::Datum(a), Value::Datum(b)) => datum_equal(a, b)
    (Value::Primitive(a), Value::Primitive(b)) => a == b
    (Value::Closure(a), Value::Closure(b)) => a.id == b.id
    (Value::CaseClosure(a), Value::CaseClosure(b)) => a.id == b.id
    (Value::GuardHandler(a), Value::GuardHandler(b)) => a.id == b.id
    (Value::Parameter(a), Value::Parameter(b)) => a.id == b.id
    (Value::Promise(a), Value::Promise(b)) => a.id == b.id
    (Value::EvalEnv(a), Value::EvalEnv(b)) => a.id == b.id
    (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id
    (Value::Port(a), Value::Port(b)) => a.id == b.id
    (Value::Record(a), Value::Record(b)) => a.id == b.id
    (Value::RecordProc(a), Value::RecordProc(b)) => a.id == b.id
    (Value::ConditionProc(a), Value::ConditionProc(b)) => a.id == b.id
    (Value::Hashtable(a), Value::Hashtable(b)) => a.id == b.id
    (Value::EnumSet(a), Value::EnumSet(b)) => a.id == b.id
    (Value::EnumSetProc(a), Value::EnumSetProc(b)) => a.id == b.id
    (Value::RecordTypeDescriptor(a), Value::RecordTypeDescriptor(b)) =>
      a.id == b.id
    (
      Value::RecordConstructorDescriptor(a),
      Value::RecordConstructorDescriptor(b),
    ) => a.id == b.id
    (Value::SyntaxObject(a), Value::SyntaxObject(b)) =>
      scopes_equal(a.scopes, b.scopes) && datum_equal(a.datum, b.datum)
    (Value::SyntaxKeyword(a), Value::SyntaxKeyword(b)) => a == b
    (Value::Void, Value::Void) => true
    _ => false
  }
}

///|
/// equal? semantics for runtime values.
fn value_equal(left : Value, right : Value) -> Bool {
  value_eqv(left, right)
}

///|
fn identifier_info(value : Value) -> (String, Array[Int], Int?) raise EvalError {
  match value {
    Value::Datum(d) =>
      match datum_unlabel(d) {
        Datum::Symbol(name) => (name, [], None)
        _ => raise @core.EvalError("type error: identifier expected")
      }
    Value::SyntaxObject(obj) =>
      match datum_unlabel(obj.datum) {
        Datum::Symbol(name) => (name, obj.scopes, obj.binding_id)
        _ => raise @core.EvalError("type error: identifier expected")
      }
    _ => raise @core.EvalError("type error: identifier expected")
  }
}

///|
fn is_identifier_value(value : Value) -> Bool {
  match value {
    Value::Datum(d) =>
      match datum_unlabel(d) {
        Datum::Symbol(_) => true
        _ => false
      }
    Value::SyntaxObject(obj) =>
      match datum_unlabel(obj.datum) {
        Datum::Symbol(_) => true
        _ => false
      }
    _ => false
  }
}

///|
fn value_as_number(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => {
      let unlabel = datum_unlabel(d)
      match unlabel {
        Datum::Int(_)
        | Datum::BigInt(_)
        | Datum::Rat(_, _)
        | Datum::BigRat(_, _)
        | Datum::Float(_)
        | Datum::Complex(_, _) => unlabel
        _ => raise @core.EvalError("type error: number expected")
      }
    }
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn bigint_to_float(value : @bigint.BigInt) -> Float raise EvalError {
  try @strconv.parse_double(value.to_string()) catch {
    _ => raise @core.EvalError("number too large")
  } noraise {
    v => Float::from_double(v)
  }
}

///|
fn bigint_to_float_unchecked(value : @bigint.BigInt) -> Float {
  try @strconv.parse_double(value.to_string()) catch {
    _ => Float::from_int(0)
  } noraise {
    v => Float::from_double(v)
  }
}

///|
fn number_to_float(value : Datum) -> Float raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => Float::from_int(n)
    Datum::BigInt(n) => bigint_to_float(n)
    Datum::Rat(n, d) => Float::from_int(n) / Float::from_int(d)
    Datum::BigRat(n, d) => bigint_to_float(n) / bigint_to_float(d)
    Datum::Float(f) => f
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn number_to_rat(value : Datum) -> (Int, Int) raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => (n, 1)
    Datum::Rat(n, d) => (n, d)
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn number_to_bigrat(
  value : Datum,
) -> (@bigint.BigInt, @bigint.BigInt) raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => (bigint_from_int(n), bigint_from_int(1))
    Datum::BigInt(n) => (n, bigint_from_int(1))
    Datum::Rat(n, d) => (bigint_from_int(n), bigint_from_int(d))
    Datum::BigRat(n, d) => (n, d)
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn float_to_exact_datum(value : Float) -> Datum raise EvalError {
  if !float_is_finite(value) {
    raise @core.EvalError("inexact->exact: non-finite")
  }
  // Decode IEEE-754 f32 into an exact rational.
  let bits = Float::reinterpret_as_uint(value)
  let sign_bit = (bits >> 31) & 0x1U
  let exp_bits = (bits >> 23) & 0xFFU
  let frac_bits = bits & 0x7FFFFFU
  if exp_bits == 0U && frac_bits == 0U {
    return Datum::Int(0)
  }
  let mantissa_bits = if exp_bits == 0U {
    frac_bits
  } else {
    (1U << 23) | frac_bits
  }
  let exp_adjusted = if exp_bits == 0U {
    -149
  } else {
    exp_bits.reinterpret_as_int() - 150
  }
  let mut num = bigint_from_int(mantissa_bits.reinterpret_as_int())
  let mut den = bigint_from_int(1)
  if exp_adjusted >= 0 {
    num = num * pow_bigint_int(bigint_from_int(2), exp_adjusted)
  } else {
    den = pow_bigint_int(bigint_from_int(2), -exp_adjusted)
  }
  if sign_bit != 0U {
    num = -num
  }
  match normalize_big_rat(num, den) {
    Some(datum) => datum
    None => raise @core.EvalError("inexact->exact: unsupported")
  }
}

///|
fn inexact_component_to_exact(value : Datum) -> Datum raise EvalError {
  match value {
    Datum::Float(f) => float_to_exact_datum(f)
    Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
      value
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn normalize_rat_or_error(num : Int, den : Int) -> Datum raise EvalError {
  match normalize_rat(num, den) {
    Some(result) => result
    None => raise @core.EvalError("division by zero")
  }
}

///|
fn normalize_big_rat_or_error(
  num : @bigint.BigInt,
  den : @bigint.BigInt,
) -> Datum raise EvalError {
  match normalize_big_rat(num, den) {
    Some(result) => result
    None => raise @core.EvalError("division by zero")
  }
}

///|
fn datum_is_zero(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(n) => n == 0
    Datum::BigInt(n) => n.is_zero()
    Datum::Rat(n, _) => n == 0
    Datum::BigRat(n, _) => n.is_zero()
    Datum::Float(f) => f == 0.0
    _ => false
  }
}

///|
fn datum_is_inexact(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Float(_) => true
    _ => false
  }
}

///|
fn datum_is_big_exact(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::BigInt(_) | Datum::BigRat(_, _) => true
    _ => false
  }
}

///|
fn datum_is_exact_integer(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(_) | Datum::BigInt(_) => true
    Datum::Complex(real, imag) =>
      datum_is_zero(imag.val) &&
      !datum_is_inexact(imag.val) &&
      datum_is_exact_integer(real.val)
    _ => false
  }
}

///|
fn datum_is_integer_valued(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(_) | Datum::BigInt(_) => true
    Datum::Float(f) => float_is_finite(f) && float_to_int_exact(f) is Some(_)
    Datum::Complex(real, imag) =>
      datum_is_zero(imag.val) && datum_is_integer_valued(real.val)
    _ => false
  }
}

///|
fn datum_is_rational_valued(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
      true
    Datum::Float(f) => float_is_finite(f)
    Datum::Complex(real, imag) =>
      datum_is_zero(imag.val) && datum_is_rational_valued(real.val)
    _ => false
  }
}

///|
fn datum_to_real(value : Datum) -> Datum raise EvalError {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) =>
      if datum_is_zero(imag.val) {
        real.val
      } else {
        raise @core.EvalError("type error: real expected")
      }
    _ => value
  }
}

///|
fn datum_to_complex_parts(value : Datum) -> (Datum, Datum) {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) => (real.val, imag.val)
    _ => (value, Datum::Int(0))
  }
}

///|
fn datum_to_complex_float(value : Datum) -> (Float, Float) raise EvalError {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) =>
      (number_to_float(real.val), number_to_float(imag.val))
    _ => (number_to_float(value), 0.0)
  }
}

///|
fn make_complex(real : Datum, imag : Datum) -> Datum {
  if datum_is_zero(imag) {
    if datum_is_inexact(imag) && !datum_is_inexact(real) {
      match real {
        Datum::Int(n) => Datum::Float(Float::from_int(n))
        Datum::Rat(n, d) =>
          Datum::Float(Float::from_int(n) / Float::from_int(d))
        Datum::BigInt(n) => Datum::Float(bigint_to_float_unchecked(n))
        Datum::BigRat(n, d) =>
          Datum::Float(
            bigint_to_float_unchecked(n) / bigint_to_float_unchecked(d),
          )
        Datum::Float(f) => Datum::Float(f)
        _ => real
      }
    } else {
      real
    }
  } else {
    Datum::Complex(Ref::new(real), Ref::new(imag))
  }
}

///|
fn complex_from_float(real : Float, imag : Float) -> Datum {
  make_complex(Datum::Float(real), Datum::Float(imag))
}

///|
fn float_to_int_exact(value : Float) -> Int? {
  let n = value.to_int()
  if value == Float::from_int(n) {
    Some(n)
  } else {
    None
  }
}

///|
fn pow_int(base : Int, exp : Int) -> Int {
  let mut result = 1
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn pow_bigint_int(base : @bigint.BigInt, exp : Int) -> @bigint.BigInt {
  let mut result = bigint_from_int(1)
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn pow_bigint_bigint(
  base : @bigint.BigInt,
  exp : @bigint.BigInt,
) -> @bigint.BigInt {
  let mut result = bigint_from_int(1)
  let mut b = base
  let mut e = exp
  let two = bigint_from_int(2)
  while e.compare_int(0) > 0 {
    if !bigint_is_zero(e % two) {
      result = result * b
    }
    b = b * b
    e = e / two
  }
  result
}

///|
fn pow_float_int(base : Float, exp : Int) -> Float {
  let mut result = Float::from_int(1)
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn complex_add_f(
  ar : Float,
  ai : Float,
  br : Float,
  bi : Float,
) -> (Float, Float) {
  (ar + br, ai + bi)
}

///|
fn complex_sub_f(
  ar : Float,
  ai : Float,
  br : Float,
  bi : Float,
) -> (Float, Float) {
  (ar - br, ai - bi)
}

///|
fn complex_mul_f(
  ar : Float,
  ai : Float,
  br : Float,
  bi : Float,
) -> (Float, Float) {
  (ar * br - ai * bi, ar * bi + ai * br)
}

///|
fn complex_div_f(
  ar : Float,
  ai : Float,
  br : Float,
  bi : Float,
) -> (Float, Float) raise EvalError {
  let denom = br * br + bi * bi
  if denom == 0.0 {
    raise @core.EvalError("division by zero")
  }
  ((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)
}

///|
fn complex_exp_f(ar : Float, ai : Float) -> (Float, Float) {
  let exp_a = @math.expf(ar)
  (exp_a * @math.cosf(ai), exp_a * @math.sinf(ai))
}

///|
fn complex_log_f(ar : Float, ai : Float) -> (Float, Float) {
  let r = (ar * ar + ai * ai).sqrt()
  (@math.lnf(r), @math.atan2f(ai, ar))
}

///|
fn complex_sqrt_f(ar : Float, ai : Float) -> (Float, Float) {
  let r = (ar * ar + ai * ai).sqrt()
  let real = ((r + ar) / 2.0).sqrt()
  let imag_mag = ((r - ar) / 2.0).sqrt()
  let imag = if ai < 0.0 { -imag_mag } else { imag_mag }
  (real, imag)
}

///|
fn complex_sin_f(ar : Float, ai : Float) -> (Float, Float) {
  let sin_a = @math.sinf(ar)
  let cos_a = @math.cosf(ar)
  let exp_b = @math.expf(ai)
  let exp_neg_b = @math.expf(-ai)
  let cosh_b = (exp_b + exp_neg_b) / 2.0
  let sinh_b = (exp_b - exp_neg_b) / 2.0
  (sin_a * cosh_b, cos_a * sinh_b)
}

///|
fn complex_cos_f(ar : Float, ai : Float) -> (Float, Float) {
  let sin_a = @math.sinf(ar)
  let cos_a = @math.cosf(ar)
  let exp_b = @math.expf(ai)
  let exp_neg_b = @math.expf(-ai)
  let cosh_b = (exp_b + exp_neg_b) / 2.0
  let sinh_b = (exp_b - exp_neg_b) / 2.0
  (cos_a * cosh_b, -sin_a * sinh_b)
}

///|
fn complex_tan_f(ar : Float, ai : Float) -> (Float, Float) raise EvalError {
  let (sr, si) = complex_sin_f(ar, ai)
  let (cr, ci) = complex_cos_f(ar, ai)
  complex_div_f(sr, si, cr, ci)
}

///|
fn complex_asin_f(ar : Float, ai : Float) -> (Float, Float) {
  let (zr, zi) = (ar, ai)
  let (z2r, z2i) = complex_mul_f(zr, zi, zr, zi)
  let (one_minus_r, one_minus_i) = complex_sub_f(1.0, 0.0, z2r, z2i)
  let (sqrt_r, sqrt_i) = complex_sqrt_f(one_minus_r, one_minus_i)
  let (iz_r, iz_i) = (-zi, zr)
  let (inside_r, inside_i) = complex_add_f(iz_r, iz_i, sqrt_r, sqrt_i)
  let (log_r, log_i) = complex_log_f(inside_r, inside_i)
  (log_i, -log_r)
}

///|
fn complex_acos_f(ar : Float, ai : Float) -> (Float, Float) {
  let (asin_r, asin_i) = complex_asin_f(ar, ai)
  let pi_over_2 = Float::from_double(@math.PI / 2.0)
  (pi_over_2 - asin_r, -asin_i)
}

///|
fn complex_atan_f(ar : Float, ai : Float) -> (Float, Float) {
  let (iz_r, iz_i) = (-ai, ar)
  let (one_minus_r, one_minus_i) = complex_sub_f(1.0, 0.0, iz_r, iz_i)
  let (one_plus_r, one_plus_i) = complex_add_f(1.0, 0.0, iz_r, iz_i)
  let (log1_r, log1_i) = complex_log_f(one_minus_r, one_minus_i)
  let (log2_r, log2_i) = complex_log_f(one_plus_r, one_plus_i)
  let (diff_r, diff_i) = complex_sub_f(log1_r, log1_i, log2_r, log2_i)
  (-diff_i / 2.0, diff_r / 2.0)
}

///|
fn complex_expt_f(
  base_r : Float,
  base_i : Float,
  exp_r : Float,
  exp_i : Float,
) -> (Float, Float) {
  let (log_r, log_i) = complex_log_f(base_r, base_i)
  let (mul_r, mul_i) = complex_mul_f(exp_r, exp_i, log_r, log_i)
  complex_exp_f(mul_r, mul_i)
}

///|
fn num_add(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_add(lr, rr)
    let imag = num_add(li, ri)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) + number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * d2 + n2 * d1, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 + n2 * d1, d1 * d2)
  }
}

///|
fn num_sub(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_sub(lr, rr)
    let imag = num_sub(li, ri)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) - number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * d2 - n2 * d1, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 - n2 * d1, d1 * d2)
  }
}

///|
fn num_mul(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_sub(num_mul(lr, rr), num_mul(li, ri))
    let imag = num_add(num_mul(lr, ri), num_mul(li, rr))
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) * number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * n2, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * n2, d1 * d2)
  }
}

///|
fn num_div(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let denom = num_add(num_mul(rr, rr), num_mul(ri, ri))
    let real_num = num_add(num_mul(lr, rr), num_mul(li, ri))
    let imag_num = num_sub(num_mul(li, rr), num_mul(lr, ri))
    let real = num_div(real_num, denom)
    let imag = num_div(imag_num, denom)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    let denom = number_to_float(right)
    if denom == 0.0 {
      raise @core.EvalError("division by zero")
    }
    Datum::Float(number_to_float(left) / denom)
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    if bigint_is_zero(n2) {
      raise @core.EvalError("division by zero")
    }
    normalize_big_rat_or_error(n1 * d2, d1 * n2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    if n2 == 0 {
      raise @core.EvalError("division by zero")
    }
    normalize_rat_or_error(n1 * d2, d1 * n2)
  }
}

///|
fn num_equal(left : Datum, right : Datum) -> Bool raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    return num_equal(lr, rr) && num_equal(li, ri)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    number_to_float(left) == number_to_float(right)
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    n1 * d2 == n2 * d1
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    n1 * d2 == n2 * d1
  }
}

///|
fn num_less(left : Datum, right : Datum) -> Bool raise EvalError {
  let lreal = datum_to_real(left)
  let rreal = datum_to_real(right)
  if lreal is Datum::Float(_) || rreal is Datum::Float(_) {
    number_to_float(lreal) < number_to_float(rreal)
  } else if datum_is_big_exact(lreal) || datum_is_big_exact(rreal) {
    let (n1, d1) = number_to_bigrat(lreal)
    let (n2, d2) = number_to_bigrat(rreal)
    n1 * d2 < n2 * d1
  } else {
    let (n1, d1) = number_to_rat(lreal)
    let (n2, d2) = number_to_rat(rreal)
    n1 * d2 < n2 * d1
  }
}

///|
fn num_greater(left : Datum, right : Datum) -> Bool raise EvalError {
  let lreal = datum_to_real(left)
  let rreal = datum_to_real(right)
  if lreal is Datum::Float(_) || rreal is Datum::Float(_) {
    number_to_float(lreal) > number_to_float(rreal)
  } else if datum_is_big_exact(lreal) || datum_is_big_exact(rreal) {
    let (n1, d1) = number_to_bigrat(lreal)
    let (n2, d2) = number_to_bigrat(rreal)
    n1 * d2 > n2 * d1
  } else {
    let (n1, d1) = number_to_rat(lreal)
    let (n2, d2) = number_to_rat(rreal)
    n1 * d2 > n2 * d1
  }
}

///|
fn num_less_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_less(left, right) || num_equal(left, right)
}

///|
fn num_greater_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_greater(left, right) || num_equal(left, right)
}

///|
fn rat_floor(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else if num < 0 {
    q - 1
  } else {
    q
  }
}

///|
fn rat_ceiling(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else if num > 0 {
    q + 1
  } else {
    q
  }
}

///|
fn rat_round(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else {
    let abs_r = int_abs(r)
    let twice = abs_r * 2
    if twice < den {
      q
    } else if twice > den {
      if num < 0 {
        q - 1
      } else {
        q + 1
      }
    } else if q % 2 == 0 {
      q
    } else if num < 0 {
      q - 1
    } else {
      q + 1
    }
  }
}

///|
fn bigrat_floor(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else if num.compare_int(0) < 0 {
    q - bigint_from_int(1)
  } else {
    q
  }
}

///|
fn bigrat_ceiling(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else if num.compare_int(0) > 0 {
    q + bigint_from_int(1)
  } else {
    q
  }
}

///|
fn bigrat_round(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else {
    let abs_r = bigint_abs(r)
    let twice = abs_r * bigint_from_int(2)
    if twice < den {
      q
    } else if twice > den {
      if num.compare_int(0) < 0 {
        q - bigint_from_int(1)
      } else {
        q + bigint_from_int(1)
      }
    } else if bigint_is_zero(q % bigint_from_int(2)) {
      q
    } else if num.compare_int(0) < 0 {
      q - bigint_from_int(1)
    } else {
      q + bigint_from_int(1)
    }
  }
}

///|
fn float_floor_int(value : Float) -> Int {
  let n = value.to_int()
  if value == Float::from_int(n) {
    n
  } else if value > 0.0 {
    n
  } else {
    n - 1
  }
}

///|
fn float_ceiling_int(value : Float) -> Int {
  let n = value.to_int()
  if value == Float::from_int(n) {
    n
  } else if value > 0.0 {
    n + 1
  } else {
    n
  }
}

///|
fn float_round_int(value : Float) -> Int {
  let floor_n = float_floor_int(value)
  let diff = value - Float::from_int(floor_n)
  if diff < 0.5 {
    floor_n
  } else if diff > 0.5 {
    floor_n + 1
  } else if floor_n % 2 == 0 {
    floor_n
  } else {
    floor_n + 1
  }
}

///|
fn float_is_nan(value : Float) -> Bool {
  value != value
}

///|
fn float_is_infinite(value : Float) -> Bool {
  if float_is_nan(value) {
    false
  } else {
    let diff = value - value
    diff != diff
  }
}

///|
fn float_is_finite(value : Float) -> Bool {
  !float_is_nan(value) && !float_is_infinite(value)
}

///|
let radix_digits : ReadOnlyArray[Char] = [
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
  'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
  'w', 'x', 'y', 'z',
]

///|
fn chars_rev_to_string(chars : Array[Char]) -> String {
  let out : Array[Char] = []
  for ch in chars.rev_iter() {
    out.push(ch)
  }
  String::from_array(out)
}

///|
fn int_to_string_radix(value : Int, radix : Int) -> String {
  if value == 0 {
    return "0"
  }
  let mut n = if value < 0 { -value } else { value }
  let chars : Array[Char] = []
  while n > 0 {
    let rem = n % radix
    chars.push(radix_digits[rem])
    n = n / radix
  }
  if value < 0 {
    chars.push('-')
  }
  chars_rev_to_string(chars)
}

///|
fn bigint_to_string_radix(value : @bigint.BigInt, radix : Int) -> String {
  if value.is_zero() {
    return "0"
  }
  let mut n = if value.compare_int(0) < 0 { -value } else { value }
  let radix_big = bigint_from_int(radix)
  let chars : Array[Char] = []
  while n.compare_int(0) > 0 {
    let rem = n % radix_big
    chars.push(radix_digits[rem.to_int()])
    n = n / radix_big
  }
  if value.compare_int(0) < 0 {
    chars.push('-')
  }
  chars_rev_to_string(chars)
}

///|
fn value_as_string(value : Value) -> String raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::String(s) => s.val
        _ => raise @core.EvalError("type error: string expected")
      }
    _ => raise @core.EvalError("type error: string expected")
  }
}

///|
fn value_as_string_ref(value : Value) -> Ref[String] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::String(s) => s
        _ => raise @core.EvalError("type error: string expected")
      }
    _ => raise @core.EvalError("type error: string expected")
  }
}

///|
fn string_replace_range(
  s : String,
  start : Int,
  end : Int,
  fill : Char,
) -> String raise EvalError {
  let len = s.length()
  if start < 0 || end < start || end > len {
    raise @core.EvalError("index out of range")
  }
  let buf = @buffer.new()
  let mut i = 0
  while i < len {
    match s.get_char(i) {
      Some(ch) =>
        if i >= start && i < end {
          buf.write_char(fill)
        } else {
          buf.write_char(ch)
        }
      None => raise @core.EvalError("index out of range")
    }
    i = i + 1
  }
  buf.to_string()
}

///|
fn value_as_char(value : Value) -> Char raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Char(ch) => ch
        _ => raise @core.EvalError("type error: char expected")
      }
    _ => raise @core.EvalError("type error: char expected")
  }
}

///|
fn char_from_int_value(value : Value) -> Char raise EvalError {
  let n = value_as_int_index(value)
  match n.to_char() {
    Some(ch) => ch
    None => raise @core.EvalError("type error: valid scalar value expected")
  }
}

///|
fn exact_integer_sqrt(n : Int) -> (Int, Int) {
  if n <= 0 {
    if n == 0 {
      return (0, 0)
    }
    // Caller ensures nonnegative; keep a safe fallback.
    return (0, n)
  }
  let mut low = 0
  let mut high = n
  let mut ans = 0
  while low <= high {
    let mid = (low + high) / 2
    if mid == 0 {
      ans = 0
      low = 1
    } else {
      let div = n / mid
      if mid <= div {
        ans = mid
        low = mid + 1
      } else {
        high = mid - 1
      }
    }
  }
  let rem = n - ans * ans
  (ans, rem)
}

///|
fn exact_integer_sqrt_bigint(
  n : @bigint.BigInt,
) -> (@bigint.BigInt, @bigint.BigInt) {
  if n.compare_int(0) <= 0 {
    if n.is_zero() {
      return (bigint_from_int(0), bigint_from_int(0))
    }
    return (bigint_from_int(0), n)
  }
  let mut low = bigint_from_int(0)
  let mut high = n
  let mut ans = bigint_from_int(0)
  let one = bigint_from_int(1)
  let two = bigint_from_int(2)
  while low <= high {
    let mid = (low + high) / two
    if mid.is_zero() {
      ans = bigint_from_int(0)
      low = one
    } else {
      let div = n / mid
      if mid <= div {
        ans = mid
        low = mid + one
      } else {
        high = mid - one
      }
    }
  }
  let rem = n - ans * ans
  (ans, rem)
}

///|
fn rationalize_float(value : Float, tol : Float) -> (Int, Int) {
  if value == 0.0 {
    return (0, 1)
  }
  let mut sign = 1
  let mut x = value
  if x < 0.0 {
    sign = -1
    x = -x
  }
  let mut a = float_floor_int(x)
  let mut p0 = 1
  let mut q0 = 0
  let mut p1 = a
  let mut q1 = 1
  let mut frac = x - Float::from_int(a)
  let mut iter = 0
  while iter < 64 {
    let approx = Float::from_int(p1) / Float::from_int(q1)
    let mut diff = x - approx
    if diff < 0.0 {
      diff = -diff
    }
    if diff <= tol || frac == 0.0 {
      break
    }
    frac = 1.0 / frac
    a = float_floor_int(frac)
    let p2 = a * p1 + p0
    let q2 = a * q1 + q0
    if q2 == 0 {
      break
    }
    p0 = p1
    q0 = q1
    p1 = p2
    q1 = q2
    frac = frac - Float::from_int(a)
    iter = iter + 1
  }
  (sign * p1, q1)
}

///|
fn value_as_int_index(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Int(n) => n
        Datum::BigInt(n) =>
          match bigint_to_int_option(n) {
            Some(v) => v
            None => raise @core.EvalError("index out of range")
          }
        Datum::Complex(real, imag) =>
          if datum_is_zero(imag.val) {
            match datum_unlabel(real.val) {
              Datum::Int(n) => n
              Datum::BigInt(n) =>
                match bigint_to_int_option(n) {
                  Some(v) => v
                  None => raise @core.EvalError("index out of range")
                }
              _ => raise @core.EvalError("type error: exact integer expected")
            }
          } else {
            raise @core.EvalError("type error: exact integer expected")
          }
        _ => raise @core.EvalError("type error: exact integer expected")
      }
    _ => raise @core.EvalError("type error: exact integer expected")
  }
}

///|
fn value_as_exact_integer(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Int(n) => Datum::Int(n)
        Datum::BigInt(n) => Datum::BigInt(n)
        Datum::Complex(real, imag) =>
          if datum_is_zero(imag.val) {
            match datum_unlabel(real.val) {
              Datum::Int(n) => Datum::Int(n)
              Datum::BigInt(n) => Datum::BigInt(n)
              _ => raise @core.EvalError("type error: integer expected")
            }
          } else {
            raise @core.EvalError("type error: integer expected")
          }
        _ => raise @core.EvalError("type error: integer expected")
      }
    _ => raise @core.EvalError("type error: integer expected")
  }
}

///|
fn datum_to_bigint(value : Datum) -> @bigint.BigInt raise EvalError {
  match datum_unlabel(value) {
    Datum::Int(n) => bigint_from_int(n)
    Datum::BigInt(n) => n
    _ => raise @core.EvalError("type error: integer expected")
  }
}

///|
fn bigint_to_integer_datum(value : @bigint.BigInt) -> Datum {
  match bigint_to_int_option(value) {
    Some(v) => Datum::Int(v)
    None => Datum::BigInt(value)
  }
}

///|
fn exact_sqrt_real(value : Datum) -> Datum? raise EvalError {
  match datum_unlabel(value) {
    Datum::Int(n) =>
      if n < 0 {
        None
      } else {
        let (s, r) = exact_integer_sqrt(n)
        if r == 0 {
          Some(Datum::Int(s))
        } else {
          None
        }
      }
    Datum::BigInt(n) =>
      if n.compare_int(0) < 0 {
        None
      } else {
        let (s, r) = exact_integer_sqrt_bigint(n)
        if bigint_is_zero(r) {
          Some(bigint_to_integer_datum(s))
        } else {
          None
        }
      }
    Datum::Rat(n, d) =>
      if n < 0 {
        None
      } else {
        let (sn, rn) = exact_integer_sqrt(n)
        if rn != 0 {
          None
        } else {
          let (sd, rd) = exact_integer_sqrt(d)
          if rd != 0 {
            None
          } else {
            Some(normalize_rat_or_error(sn, sd))
          }
        }
      }
    Datum::BigRat(n, d) =>
      if n.compare_int(0) < 0 {
        None
      } else {
        let (sn, rn) = exact_integer_sqrt_bigint(n)
        if !bigint_is_zero(rn) {
          None
        } else {
          let (sd, rd) = exact_integer_sqrt_bigint(d)
          if !bigint_is_zero(rd) {
            None
          } else {
            Some(normalize_big_rat_or_error(sn, sd))
          }
        }
      }
    _ => None
  }
}

///|
fn value_as_nonnegative_int(value : Value) -> Int raise EvalError {
  let n = value_as_int_index(value)
  if n < 0 {
    raise @core.EvalError("type error: exact nonnegative integer expected")
  }
  n
}

///|
let fixnum_width_cache : Ref[Int] = Ref::new(0)

///|
fn fixnum_width_value() -> Int {
  let cached = fixnum_width_cache.val
  if cached != 0 {
    return cached
  }
  let mut exp = 0
  let mut value = bigint_from_int(1)
  let two = bigint_from_int(2)
  while true {
    match bigint_to_int_option(value) {
      Some(_) => {
        exp = exp + 1
        value = value * two
      }
      None => break
    }
  }
  let width = exp + 1
  fixnum_width_cache.val = width
  width
}

///|
fn fixnum_base_bigint() -> @bigint.BigInt {
  bigint_pow2(fixnum_width_value())
}

///|
fn fixnum_half_bigint() -> @bigint.BigInt {
  bigint_pow2(fixnum_width_value() - 1)
}

///|
fn datum_fixnum_bigint(datum : Datum) -> @bigint.BigInt? {
  match datum_unlabel(datum) {
    Datum::Int(n) => Some(bigint_from_int(n))
    Datum::BigInt(n) =>
      match bigint_to_int_option(n) {
        Some(_) => Some(n)
        None => None
      }
    Datum::Complex(real, imag) =>
      if datum_is_zero(imag.val) {
        datum_fixnum_bigint(real.val)
      } else {
        None
      }
    _ => None
  }
}

///|
fn value_is_fixnum(value : Value) -> Bool {
  match value {
    Value::Datum(datum) => datum_fixnum_bigint(datum) is Some(_)
    _ => false
  }
}

///|
fn value_as_fixnum_bigint(value : Value) -> @bigint.BigInt raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_fixnum_bigint(datum) {
        Some(n) => n
        None => raise @core.EvalError("type error: fixnum expected")
      }
    _ => raise @core.EvalError("type error: fixnum expected")
  }
}

///|
fn value_as_fixnum_datum(value : Value) -> Datum raise EvalError {
  fixnum_from_bigint(value_as_fixnum_bigint(value))
}

///|
fn value_as_fixnum_int(value : Value) -> Int raise EvalError {
  let big = value_as_fixnum_bigint(value)
  match bigint_to_int_option(big) {
    Some(v) => v
    None => raise @core.EvalError("type error: fixnum expected")
  }
}

///|
fn value_as_nonnegative_fixnum_int(value : Value) -> Int raise EvalError {
  let n = value_as_fixnum_int(value)
  if n < 0 {
    raise @core.EvalError("type error: exact nonnegative integer expected")
  }
  n
}

///|
fn fixnum_from_bigint(value : @bigint.BigInt) -> Datum raise EvalError {
  match bigint_to_int_option(value) {
    Some(v) => Datum::Int(v)
    None => raise @core.EvalError("fixnum overflow")
  }
}

///|
fn fixnum_from_datum(value : Datum) -> Datum raise EvalError {
  fixnum_from_bigint(datum_to_bigint(value))
}

///|
fn bigint_floor_div(
  value : @bigint.BigInt,
  base : @bigint.BigInt,
) -> @bigint.BigInt {
  let q = value / base
  let r = value % base
  if r.compare_int(0) < 0 {
    q - bigint_from_int(1)
  } else {
    q
  }
}

///|
fn value_as_flonum(value : Value) -> Float raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Float(f) => f
        _ => raise @core.EvalError("type error: flonum expected")
      }
    _ => raise @core.EvalError("type error: flonum expected")
  }
}

///|
fn value_as_integer_flonum(value : Value) -> Int raise EvalError {
  let f = value_as_flonum(value)
  if !float_is_finite(f) {
    raise @core.EvalError("type error: integer-valued flonum expected")
  }
  match float_to_int_exact(f) {
    Some(n) => n
    None => raise @core.EvalError("type error: integer-valued flonum expected")
  }
}

///|
fn flonum_div_and_mod(left : Float, right : Float) -> (Float, Float) {
  let quotient = left / right
  if !float_is_finite(quotient) {
    return (quotient, left - right * quotient)
  }
  let q_int = float_floor_int(quotient)
  let q = Float::from_int(q_int)
  (q, left - right * q)
}

///|
fn flonum_div0_and_mod0(left : Float, right : Float) -> (Float, Float) {
  let quotient = left / right
  if !float_is_finite(quotient) {
    return (quotient, left - right * quotient)
  }
  let q_int = quotient.to_int()
  let q = Float::from_int(q_int)
  (q, left - right * q)
}

///|
fn flonum_numerator(value : Float) -> Float raise EvalError {
  if float_is_nan(value) || float_is_infinite(value) {
    return value
  }
  if value == 0.0 {
    let neg_zero = Float::from_int(1) / value < 0.0
    let zero = Float::from_int(0)
    return if neg_zero { -zero } else { zero }
  }
  let exact = float_to_exact_datum(value)
  match exact {
    Datum::Int(n) => Float::from_int(n)
    Datum::BigInt(n) => bigint_to_float(n)
    Datum::Rat(n, _) => Float::from_int(n)
    Datum::BigRat(n, _) => bigint_to_float(n)
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
fn flonum_denominator(value : Float) -> Float raise EvalError {
  if float_is_nan(value) {
    return value
  }
  if float_is_infinite(value) || value == 0.0 {
    return Float::from_int(1)
  }
  let exact = float_to_exact_datum(value)
  match exact {
    Datum::Int(_) | Datum::BigInt(_) => Float::from_int(1)
    Datum::Rat(_, d) => Float::from_int(d)
    Datum::BigRat(_, d) => bigint_to_float(d)
    _ => raise @core.EvalError("type error: number expected")
  }
}

///|
priv enum BitOp {
  And
  Or
  Xor
}

///|
fn bigint_pow2(exp : Int) -> @bigint.BigInt {
  pow_bigint_int(bigint_from_int(2), exp)
}

///|
fn bigint_ge(left : @bigint.BigInt, right : @bigint.BigInt) -> Bool {
  (left - right).compare_int(0) >= 0
}

///|
fn bigint_bit_length_nonneg(value : @bigint.BigInt) -> Int {
  if bigint_is_zero(value) {
    return 0
  }
  let mut n = value
  let two = bigint_from_int(2)
  let mut count = 0
  while n.compare_int(0) > 0 {
    n = n / two
    count = count + 1
  }
  count
}

///|
fn bigint_bit_count_nonneg(value : @bigint.BigInt) -> Int {
  let mut n = value
  let two = bigint_from_int(2)
  let mut count = 0
  while n.compare_int(0) > 0 {
    let bit = n % two
    if !bigint_is_zero(bit) {
      count = count + 1
    }
    n = n / two
  }
  count
}

///|
fn bigint_twos_length(value : @bigint.BigInt) -> Int {
  if value.compare_int(0) >= 0 {
    bigint_bit_length_nonneg(value)
  } else {
    bigint_bit_length_nonneg(-value - bigint_from_int(1))
  }
}

///|
fn bigint_to_twos(value : @bigint.BigInt, width : Int) -> @bigint.BigInt {
  if value.compare_int(0) >= 0 {
    value
  } else {
    value + bigint_pow2(width)
  }
}

///|
fn bigint_from_twos(value : @bigint.BigInt, width : Int) -> @bigint.BigInt {
  let limit = bigint_pow2(width - 1)
  if bigint_ge(value, limit) {
    value - bigint_pow2(width)
  } else {
    value
  }
}

///|
fn bigint_bitwise_nonneg(
  left : @bigint.BigInt,
  right : @bigint.BigInt,
  op : BitOp,
) -> @bigint.BigInt {
  let mut a = left
  let mut b = right
  let two = bigint_from_int(2)
  let mut bit = bigint_from_int(1)
  let mut result = bigint_from_int(0)
  while !bigint_is_zero(a) || !bigint_is_zero(b) {
    let a_bit = a % two
    let b_bit = b % two
    let a_one = !bigint_is_zero(a_bit)
    let b_one = !bigint_is_zero(b_bit)
    let out_one = match op {
      BitOp::And => a_one && b_one
      BitOp::Or => a_one || b_one
      BitOp::Xor => a_one != b_one
    }
    if out_one {
      result = result + bit
    }
    a = a / two
    b = b / two
    bit = bit * two
  }
  result
}

///|
fn bigint_bitwise_binop(
  left : @bigint.BigInt,
  right : @bigint.BigInt,
  op : BitOp,
) -> @bigint.BigInt {
  let len_left = bigint_twos_length(left)
  let len_right = bigint_twos_length(right)
  let width = (if len_left > len_right { len_left } else { len_right }) + 1
  let left_rep = bigint_to_twos(left, width)
  let right_rep = bigint_to_twos(right, width)
  let result_rep = bigint_bitwise_nonneg(left_rep, right_rep, op)
  bigint_from_twos(result_rep, width)
}

///|
fn datum_bitwise_binop(
  left : Datum,
  right : Datum,
  op : BitOp,
) -> Datum raise EvalError {
  let left_big = datum_to_bigint(left)
  let right_big = datum_to_bigint(right)
  bigint_to_integer_datum(bigint_bitwise_binop(left_big, right_big, op))
}

///|
fn datum_bitwise_not(value : Datum) -> Datum raise EvalError {
  let big = datum_to_bigint(value)
  bigint_to_integer_datum(-big - bigint_from_int(1))
}

///|
fn datum_arithmetic_shift(value : Datum, shift : Int) -> Datum raise EvalError {
  let big = datum_to_bigint(value)
  if shift == 0 {
    return bigint_to_integer_datum(big)
  }
  if shift > 0 {
    let factor = bigint_pow2(shift)
    return bigint_to_integer_datum(big * factor)
  }
  let count = -shift
  let divisor = bigint_pow2(count)
  let q = big / divisor
  let r = big % divisor
  if big.compare_int(0) < 0 && !bigint_is_zero(r) {
    bigint_to_integer_datum(q - bigint_from_int(1))
  } else {
    bigint_to_integer_datum(q)
  }
}

///|
fn datum_bitwise_length(value : Datum) -> Int raise EvalError {
  let big = datum_to_bigint(value)
  if big.compare_int(0) >= 0 {
    bigint_bit_length_nonneg(big)
  } else {
    bigint_bit_length_nonneg(-big - bigint_from_int(1))
  }
}

///|
fn datum_bitwise_bit_count(value : Datum) -> Int raise EvalError {
  let big = datum_to_bigint(value)
  if big.compare_int(0) >= 0 {
    bigint_bit_count_nonneg(big)
  } else {
    bigint_bit_count_nonneg(-big - bigint_from_int(1))
  }
}

///|
fn datum_bitwise_bit_set(value : Datum, index : Int) -> Bool raise EvalError {
  let big = datum_to_bigint(value)
  let mask = bigint_pow2(index)
  let res = bigint_bitwise_binop(big, mask, BitOp::And)
  !bigint_is_zero(res)
}

///|
fn datum_bitwise_bit_field(
  value : Datum,
  start : Int,
  end : Int,
) -> Datum raise EvalError {
  if end < start {
    raise @core.EvalError("index out of range")
  }
  let width = end - start
  if width == 0 {
    return Datum::Int(0)
  }
  let shifted = datum_arithmetic_shift(value, -start)
  let mask = bigint_pow2(width) - bigint_from_int(1)
  let field = bigint_bitwise_binop(datum_to_bigint(shifted), mask, BitOp::And)
  bigint_to_integer_datum(field)
}

///|
fn datum_bitwise_copy_bit_field(
  value : Datum,
  start : Int,
  end : Int,
  from : Datum,
) -> Datum raise EvalError {
  if end < start {
    raise @core.EvalError("index out of range")
  }
  let width = end - start
  if width == 0 {
    return value
  }
  let mask_low = bigint_pow2(width) - bigint_from_int(1)
  let shift_factor = bigint_pow2(start)
  let mask = mask_low * shift_factor
  let mask_not = -mask - bigint_from_int(1)
  let cleared_value = bigint_bitwise_binop(
    datum_to_bigint(value),
    mask_not,
    BitOp::And,
  )
  let from_shifted = datum_arithmetic_shift(from, start)
  let insert = bigint_bitwise_binop(
    datum_to_bigint(from_shifted),
    mask,
    BitOp::And,
  )
  let result = bigint_bitwise_binop(cleared_value, insert, BitOp::Or)
  bigint_to_integer_datum(result)
}

///|
fn datum_bitwise_rotate_bit_field(
  value : Datum,
  start : Int,
  end : Int,
  count : Int,
) -> Datum raise EvalError {
  if end < start {
    raise @core.EvalError("index out of range")
  }
  let width = end - start
  if width == 0 {
    return value
  }
  let mut shift = count % width
  if shift < 0 {
    shift = shift + width
  }
  let field = datum_bitwise_bit_field(value, start, end)
  let mask = bigint_pow2(width) - bigint_from_int(1)
  let field_big = datum_to_bigint(field)
  let left = field_big * bigint_pow2(shift)
  let right = if shift == 0 {
    bigint_from_int(0)
  } else {
    field_big / bigint_pow2(width - shift)
  }
  let rotated = bigint_bitwise_binop(left, right, BitOp::Or)
  let rotated_masked = bigint_bitwise_binop(rotated, mask, BitOp::And)
  let rotated_value = bigint_to_integer_datum(rotated_masked)
  datum_bitwise_copy_bit_field(value, start, end, rotated_value)
}

///|
fn datum_bitwise_reverse_bit_field(
  value : Datum,
  start : Int,
  end : Int,
) -> Datum raise EvalError {
  if end < start {
    raise @core.EvalError("index out of range")
  }
  let width = end - start
  if width == 0 {
    return value
  }
  let field = datum_bitwise_bit_field(value, start, end)
  let mut reversed = bigint_from_int(0)
  let mut i = 0
  while i < width {
    if datum_bitwise_bit_set(field, i) {
      reversed = reversed + bigint_pow2(width - 1 - i)
    }
    i = i + 1
  }
  datum_bitwise_copy_bit_field(
    value,
    start,
    end,
    bigint_to_integer_datum(reversed),
  )
}

///|
fn check_index(idx : Int, len : Int) -> Unit raise EvalError {
  if idx < 0 || idx >= len {
    raise @core.EvalError("index out of range")
  }
}

///|
fn foldcase_char_if(ch : Char, case_insensitive : Bool) -> Char {
  if case_insensitive { unicode_char(ch).foldcase() } else { ch }
}

///|
fn foldcase_string_if(s : String, case_insensitive : Bool) -> String {
  if case_insensitive { unicode_string(s).foldcase().into_string() } else { s }
}

///|
fn compare_chain_char(
  args : Array[Value],
  mode : CompareMode,
  case_insensitive : Bool,
) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let prev = foldcase_char_if(value_as_char(args[0]), case_insensitive)
  for i = 1, prev = prev; i < args.length(); {
    let cur = foldcase_char_if(value_as_char(args[i]), case_insensitive)
    let cmp = prev.to_int().compare(cur.to_int())
    if !compare_ok(mode, cmp) {
      return false
    }
    continue i + 1, cur
  } else {
    true
  }
}

///|
fn compare_chain_string(
  args : Array[Value],
  mode : CompareMode,
  case_insensitive : Bool,
) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let prev = foldcase_string_if(value_as_string(args[0]), case_insensitive)
  for i = 1, prev = prev; i < args.length(); {
    let cur = foldcase_string_if(value_as_string(args[i]), case_insensitive)
    let cmp = prev.lexical_compare(cur)
    if !compare_ok(mode, cmp) {
      return false
    }
    continue i + 1, cur
  } else {
    true
  }
}

///|
fn compare_chain_bool(args : Array[Value]) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let prev = value_as_bool(args[0])
  for i = 1, prev = prev; i < args.length(); {
    let cur = value_as_bool(args[i])
    if prev != cur {
      return false
    }
    continue i + 1, cur
  } else {
    true
  }
}

///|
fn compare_chain_symbol(args : Array[Value]) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let prev = value_as_symbol(args[0])
  for i = 1, prev = prev; i < args.length(); {
    let cur = value_as_symbol(args[i])
    if prev != cur {
      return false
    }
    continue i + 1, cur
  } else {
    true
  }
}

///|
fn hash_string_value(s : String) -> Int {
  let modulus = 2147483647
  let factor = 65599
  for i = 0, hash = 0; i < s.length(); {
    match s.get_char(i) {
      Some(ch) => {
        let next = (hash * factor + ch.to_int()) % modulus
        let next_hash = if next < 0 { next + modulus } else { next }
        continue i + 1, next_hash
      }
      None => return hash
    }
  } else {
    hash
  }
}

///|
priv enum EqualityMode {
  Eqv
  Equal
}

///|
fn equality_match(mode : EqualityMode, left : Value, right : Value) -> Bool {
  match mode {
    EqualityMode::Eqv => value_eqv(left, right)
    EqualityMode::Equal => value_equal(left, right)
  }
}

///|
fn list_member(
  mode : EqualityMode,
  item : Value,
  list : Datum,
) -> Value raise EvalError {
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => return bool_value(false)
      Datum::Pair(car, cdr) => {
        if equality_match(mode, item, value_from_datum(car.val)) {
          return Value::Datum(cur)
        }
        cur = cdr.val
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  }
  bool_value(false)
}

///|
fn list_assoc(
  mode : EqualityMode,
  key : Value,
  list : Datum,
) -> Value raise EvalError {
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => return bool_value(false)
      Datum::Pair(entry, rest) => {
        let entry_val = entry.val
        match entry_val {
          Datum::Pair(car, _) =>
            if equality_match(mode, key, value_from_datum(car.val)) {
              return Value::Datum(entry_val)
            }
          _ => raise @core.EvalError("type error: pair expected")
        }
        cur = rest.val
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  }
  bool_value(false)
}

///|
fn value_as_vector(value : Value) -> Array[Datum] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Vector(items) => items
        _ => raise @core.EvalError("type error: vector expected")
      }
    _ => raise @core.EvalError("type error: vector expected")
  }
}

///|
fn value_as_bool(value : Value) -> Bool raise EvalError {
  match value {
    Value::Datum(Datum::Bool(flag)) => flag
    _ => raise @core.EvalError("type error: boolean expected")
  }
}

///|
fn value_as_symbol(value : Value) -> String raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Symbol(name) => name
        _ => raise @core.EvalError("type error: symbol expected")
      }
    _ => raise @core.EvalError("type error: symbol expected")
  }
}

///|
fn parse_field_spec(item : Datum) -> RecordField raise EvalError {
  let parts = datum_list_to_array(item)
  if parts.length() != 2 {
    raise @core.EvalError("invalid record field spec")
  }
  let mut_flag = match symbol_name(parts[0]) {
    Some("mutable") => true
    Some("immutable") => false
    _ => raise @core.EvalError("invalid record field spec")
  }
  let name = match symbol_name(parts[1]) {
    Some(field_name) => field_name
    None => raise @core.EvalError("invalid record field spec")
  }
  RecordField::{ name, mutable: mut_flag }
}

///|
fn parse_field_specs(
  items : Array[Datum],
) -> Array[RecordField] raise EvalError {
  let fields : Array[RecordField] = []
  for item in items {
    fields.push(parse_field_spec(item))
  }
  fields
}

///|
fn value_as_bytevector(value : Value) -> Array[Int] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::ByteVector(items) => items
        _ => raise @core.EvalError("type error: bytevector expected")
      }
    _ => raise @core.EvalError("type error: bytevector expected")
  }
}

///|
fn value_as_byte(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(Datum::Int(n)) => {
      if n < 0 || n > 255 {
        raise @core.EvalError("type error: byte expected")
      }
      n
    }
    _ => raise @core.EvalError("type error: byte expected")
  }
}

///|
priv enum Endian {
  Big
  Little
}

///|
fn native_endianness_mode() -> Endian {
  Endian::Little
}

///|
fn native_endianness_value() -> Value {
  Value::Datum(Datum::Symbol("little"))
}

///|
fn endianness_from_value(value : Value) -> Endian raise EvalError {
  let name = value_as_symbol(value)
  match name {
    "big" => Endian::Big
    "little" => Endian::Little
    "native" => native_endianness_mode()
    _ => raise @core.EvalError("type error: endianness expected")
  }
}

///|
fn bytevector_size_from_value(value : Value) -> Int raise EvalError {
  let size = value_as_nonnegative_int(value)
  if size <= 0 || size > 8 {
    raise @core.EvalError("size out of range")
  }
  size
}

///|
fn check_bytevector_range(
  start : Int,
  size : Int,
  len : Int,
) -> Unit raise EvalError {
  if start < 0 || start + size > len {
    raise @core.EvalError("index out of range")
  }
}

///|
fn utf8_raise_invalid() -> Unit raise EvalError {
  raise @core.EvalError("invalid utf8 sequence")
}

///|
fn utf8_encode_char(
  codepoint : Int,
  bytes : Array[Int],
) -> Unit raise EvalError {
  if codepoint < 0 || codepoint > 0x10FFFF {
    utf8_raise_invalid()
  }
  if codepoint >= 0xD800 && codepoint <= 0xDFFF {
    utf8_raise_invalid()
  }
  if codepoint <= 0x7F {
    bytes.push(codepoint)
  } else if codepoint <= 0x7FF {
    bytes.push(0xC0 | (codepoint >> 6))
    bytes.push(0x80 | (codepoint & 0x3F))
  } else if codepoint <= 0xFFFF {
    bytes.push(0xE0 | (codepoint >> 12))
    bytes.push(0x80 | ((codepoint >> 6) & 0x3F))
    bytes.push(0x80 | (codepoint & 0x3F))
  } else {
    bytes.push(0xF0 | (codepoint >> 18))
    bytes.push(0x80 | ((codepoint >> 12) & 0x3F))
    bytes.push(0x80 | ((codepoint >> 6) & 0x3F))
    bytes.push(0x80 | (codepoint & 0x3F))
  }
}

///|
fn string_char_length(s : String) -> Int {
  let mut len = 0
  for _ in s {
    len = len + 1
  }
  len
}

///|
fn string_to_utf8_bytes(
  s : String,
  start : Int,
  end : Int,
) -> Array[Int] raise EvalError {
  if start < 0 || end < start {
    raise @core.EvalError("index out of range")
  }
  let bytes : Array[Int] = []
  let mut idx = 0
  for ch in s {
    if idx >= start && idx < end {
      utf8_encode_char(ch.to_int(), bytes)
    }
    idx = idx + 1
  }
  if end > idx {
    raise @core.EvalError("index out of range")
  }
  bytes
}

///|
fn utf8_decode_to_string(
  items : Array[Int],
  start : Int,
  end : Int,
) -> String raise EvalError {
  if start < 0 || end < start || end > items.length() {
    raise @core.EvalError("index out of range")
  }
  let buf = @buffer.new()
  let mut i = start
  while i < end {
    let b0 = items[i]
    if b0 < 0 || b0 > 255 {
      utf8_raise_invalid()
    }
    let (codepoint, size) = if b0 <= 0x7F {
      (b0, 1)
    } else if b0 >= 0xC2 && b0 <= 0xDF {
      if i + 1 >= end {
        utf8_raise_invalid()
      }
      let b1 = items[i + 1]
      if b1 < 0x80 || b1 > 0xBF {
        utf8_raise_invalid()
      }
      (((b0 & 0x1F) << 6) | (b1 & 0x3F), 2)
    } else if b0 >= 0xE0 && b0 <= 0xEF {
      if i + 2 >= end {
        utf8_raise_invalid()
      }
      let b1 = items[i + 1]
      let b2 = items[i + 2]
      if b1 < 0x80 || b1 > 0xBF || b2 < 0x80 || b2 > 0xBF {
        utf8_raise_invalid()
      }
      if b0 == 0xE0 && b1 < 0xA0 {
        utf8_raise_invalid()
      }
      if b0 == 0xED && b1 > 0x9F {
        utf8_raise_invalid()
      }
      (((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F), 3)
    } else if b0 >= 0xF0 && b0 <= 0xF4 {
      if i + 3 >= end {
        utf8_raise_invalid()
      }
      let b1 = items[i + 1]
      let b2 = items[i + 2]
      let b3 = items[i + 3]
      if b1 < 0x80 ||
        b1 > 0xBF ||
        b2 < 0x80 ||
        b2 > 0xBF ||
        b3 < 0x80 ||
        b3 > 0xBF {
        utf8_raise_invalid()
      }
      if b0 == 0xF0 && b1 < 0x90 {
        utf8_raise_invalid()
      }
      if b0 == 0xF4 && b1 > 0x8F {
        utf8_raise_invalid()
      }
      (
        ((b0 & 0x07) << 18) |
        ((b1 & 0x3F) << 12) |
        ((b2 & 0x3F) << 6) |
        (b3 & 0x3F),
        4,
      )
    } else {
      utf8_raise_invalid()
      (0, 1)
    }
    match codepoint.to_char() {
      Some(ch) => buf.write_char(ch)
      None => utf8_raise_invalid()
    }
    i = i + size
  }
  buf.to_string()
}

///|
fn bytevector_uint_ref(
  items : Array[Int],
  start : Int,
  size : Int,
  endian : Endian,
) -> @bigint.BigInt {
  let base = bigint_from_int(256)
  let mut result = bigint_from_int(0)
  match endian {
    Endian::Big => {
      let mut i = 0
      while i < size {
        result = result * base + bigint_from_int(items[start + i])
        i = i + 1
      }
    }
    Endian::Little => {
      let mut factor = bigint_from_int(1)
      let mut i = 0
      while i < size {
        result = result + bigint_from_int(items[start + i]) * factor
        factor = factor * base
        i = i + 1
      }
    }
  }
  result
}

///|
fn bytevector_uint_set(
  items : Array[Int],
  start : Int,
  size : Int,
  endian : Endian,
  value : @bigint.BigInt,
) -> Unit raise EvalError {
  let limit = bigint_pow2(size * 8)
  if value.compare_int(0) < 0 || value >= limit {
    raise @core.EvalError("integer out of range")
  }
  let base = bigint_from_int(256)
  let bytes : Array[Int] = []
  let mut v = value
  let mut i = 0
  while i < size {
    let byte = v % base
    let byte_int = match bigint_to_int_option(byte) {
      Some(n) => n
      None => raise @core.EvalError("integer out of range")
    }
    bytes.push(byte_int)
    v = v / base
    i = i + 1
  }
  match endian {
    Endian::Big => {
      let mut j = 0
      while j < size {
        items[start + j] = bytes[size - 1 - j]
        j = j + 1
      }
    }
    Endian::Little => {
      let mut j = 0
      while j < size {
        items[start + j] = bytes[j]
        j = j + 1
      }
    }
  }
}

///|
fn bytevector_sint_ref(
  items : Array[Int],
  start : Int,
  size : Int,
  endian : Endian,
) -> @bigint.BigInt {
  let unsigned = bytevector_uint_ref(items, start, size, endian)
  let sign_bit = bigint_pow2(size * 8 - 1)
  if unsigned >= sign_bit {
    unsigned - bigint_pow2(size * 8)
  } else {
    unsigned
  }
}

///|
fn bytevector_sint_to_uint(
  value : @bigint.BigInt,
  size : Int,
) -> @bigint.BigInt raise EvalError {
  let sign_bit = bigint_pow2(size * 8 - 1)
  let limit = bigint_pow2(size * 8)
  let min = -sign_bit
  if value < min || value >= sign_bit {
    raise @core.EvalError("integer out of range")
  }
  if value.compare_int(0) < 0 {
    value + limit
  } else {
    value
  }
}

///|
fn value_as_port(value : Value) -> Port raise EvalError {
  match value {
    Value::Port(port) => port
    _ => raise @core.EvalError("type error: port expected")
  }
}

///|
fn value_as_record_type_descriptor(
  value : Value,
) -> RecordTypeDescriptor raise EvalError {
  match value {
    Value::RecordTypeDescriptor(desc) => desc
    _ => raise @core.EvalError("type error: record type descriptor expected")
  }
}

///|
fn value_as_record_constructor_descriptor(
  value : Value,
) -> RecordConstructorDescriptor raise EvalError {
  match value {
    Value::RecordConstructorDescriptor(desc) => desc
    _ =>
      raise @core.EvalError(
        "type error: record constructor descriptor expected",
      )
  }
}

///|
fn value_as_hashtable(value : Value) -> Hashtable raise EvalError {
  match value {
    Value::Hashtable(table) => table
    _ => raise @core.EvalError("type error: hashtable expected")
  }
}

///|
fn value_as_enum_set(value : Value) -> EnumSet raise EvalError {
  match value {
    Value::EnumSet(set) => set
    _ => raise @core.EvalError("type error: enum-set expected")
  }
}

///|
fn hashtable_equiv_from_value(value : Value) -> HashtableEquiv raise EvalError {
  match value {
    Value::Primitive(Primitive::Eq) => HashtableEquiv::Eq
    Value::Primitive(Primitive::Eqv) => HashtableEquiv::Eqv
    Value::Primitive(Primitive::Equal) => HashtableEquiv::Equal
    _ => {
      if !is_procedure_value(value) {
        raise @core.EvalError("type error: procedure expected")
      }
      HashtableEquiv::Proc(value)
    }
  }
}

///|
fn hashtable_equiv_to_value(equiv : HashtableEquiv) -> Value {
  match equiv {
    HashtableEquiv::Eq => Value::Primitive(Primitive::Eq)
    HashtableEquiv::Eqv => Value::Primitive(Primitive::Eqv)
    HashtableEquiv::Equal => Value::Primitive(Primitive::Equal)
    HashtableEquiv::Proc(proc) => proc
  }
}

///|
fn validate_hashtable_size_arg(value : Value) -> Unit raise EvalError {
  let n = value_as_int_index(value)
  if n < 0 {
    raise @core.EvalError("type error: non-negative integer expected")
  }
}

///|
fn enum_set_require_same_universe(
  a : EnumSet,
  b : EnumSet,
) -> Unit raise EvalError {
  if !enum_set_universe_equal(a, b) {
    raise @core.EvalError("type error: enum-set universe mismatch")
  }
}

///|
fn enum_set_symbol_list(value : Value) -> Array[String] raise EvalError {
  let datum = value_as_datum(value)
  let items = datum_list_to_array(datum)
  parse_symbol_list(items)
}

///|
fn ensure_unique_symbols(names : Array[String]) -> Unit raise EvalError {
  let seen : Map[String, Bool] = {}
  for name in names {
    if seen.contains(name) {
      raise @core.EvalError("type error: duplicate symbol")
    }
    seen[name] = true
  }
}

///|
fn value_as_condition_type_descriptor(
  value : Value,
) -> RecordTypeDescriptor raise EvalError {
  let desc = value_as_record_type_descriptor(value)
  let base = condition_base_type()
  if !record_type_is_a(desc.record_type, base) {
    raise @core.EvalError("type error: condition type expected")
  }
  desc
}

///|
fn record_accessor_info(value : Value) -> (RecordType, Int) raise EvalError {
  match value {
    Value::RecordProc(proc) =>
      match proc.kind {
        RecordProcKind::Accessor(record_type, idx) => (record_type, idx)
        _ => raise @core.EvalError("type error: record accessor expected")
      }
    _ => raise @core.EvalError("type error: record accessor expected")
  }
}

///|
fn char_to_raw_string(ch : Char) -> String {
  let buf = @buffer.new()
  buf.write_char(ch)
  buf.to_string()
}

///|
fn display_string(value : Value) -> String {
  match value {
    Value::Datum(Datum::String(s)) => s.val
    Value::Datum(Datum::Char(ch)) => char_to_raw_string(ch)
    Value::Datum(Datum::Symbol(name)) => name
    _ => value_to_string(value)
  }
}

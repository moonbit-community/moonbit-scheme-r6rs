///|
/// Apply a primitive to evaluated arguments.
fn apply_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
    Primitive::Add => {
      let mut acc = Datum::Int(0)
      for arg in args {
        acc = num_add(acc, value_as_number(arg))
      }
      Value::Datum(acc)
    }
    Primitive::Sub =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_sub(Datum::Int(0), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_sub(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    Primitive::Mul => {
      let mut product = Datum::Int(1)
      for arg in args {
        product = num_mul(product, value_as_number(arg))
      }
      Value::Datum(product)
    }
    Primitive::Div =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_div(Datum::Int(1), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_div(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    Primitive::NumEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_number(args[0])
        let mut ok = true
        let mut i = 1
        while i < args.length() {
          if !num_equal(value_as_number(args[i]), first) {
            ok = false
            break
          } else {
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::Less =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::Greater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::LessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::GreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::ExactToInexact =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(_)) => args[0]
            Value::Datum(Datum::Complex(real, imag)) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              Value::Datum(
                make_complex(Datum::Float(real_f), Datum::Float(imag_f)),
              )
            }
            _ => {
              let value = value_as_number(args[0])
              Value::Datum(Datum::Float(number_to_float(value)))
            }
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::InexactToExact =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(f)) =>
              Value::Datum(float_to_exact_datum(f))
            Value::Datum(Datum::Complex(real, imag)) => {
              let real_exact = inexact_component_to_exact(real.val)
              let imag_exact = inexact_component_to_exact(imag.val)
              Value::Datum(make_complex(real_exact, imag_exact))
            }
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => args[0]
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ExactIntegerSqrt =>
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          match value {
            Datum::Int(n) => {
              if n < 0 {
                raise @core.EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt(n)
              Value::Values([
                Value::Datum(Datum::Int(s)),
                Value::Datum(Datum::Int(r)),
              ])
            }
            Datum::BigInt(n) => {
              if n.compare_int(0) < 0 {
                raise @core.EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt_bigint(n)
              Value::Values([
                Value::Datum(Datum::BigInt(s)),
                Value::Datum(Datum::BigInt(r)),
              ])
            }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Rationalize =>
      match args.length() {
        2 => {
          let x = value_as_number(args[0])
          let tol_value = value_as_number(args[1])
          let tol = number_to_float(tol_value)
          if tol < 0.0 {
            raise @core.EvalError("type error: nonnegative real expected")
          }
          match x {
            Datum::Int(_)
            | Datum::BigInt(_)
            | Datum::Rat(_, _)
            | Datum::BigRat(_, _) => Value::Datum(x)
            Datum::Float(f) => {
              let (n, d) = rationalize_float(f, tol)
              Value::Datum(normalize_rat_or_error(n, d))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::NumberToString =>
      match args.length() {
        1 | 2 => {
          let num = value_as_number(args[0])
          let radix = if args.length() == 2 {
            value_as_int_index(args[1])
          } else {
            10
          }
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          match num {
            Datum::Int(n) => {
              let text = int_to_string_radix(n, radix)
              Value::Datum(Datum::String(Ref::new(text)))
            }
            Datum::BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              Value::Datum(Datum::String(Ref::new(text)))
            }
            Datum::Rat(_, _)
            | Datum::BigRat(_, _)
            | Datum::Float(_)
            | Datum::Complex(_, _) => {
              if radix != 10 {
                raise @core.EvalError("type error: exact integer expected")
              }
              let text = value_to_string(Value::Datum(num))
              Value::Datum(Datum::String(Ref::new(text)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::StringToNumber =>
      match args.length() {
        1 | 2 => {
          let s = value_as_string(args[0])
          let radix = if args.length() == 2 {
            value_as_int_index(args[1])
          } else {
            10
          }
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          let parsed = if args.length() == 2 {
            parse_number_token_with_radix(s, radix)
          } else {
            parse_number_token(s)
          }
          match parsed {
            Some(num) => Value::Datum(num)
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::MakeRectangular =>
      match args.length() {
        2 => {
          let real = datum_to_real(value_as_number(args[0]))
          let imag = datum_to_real(value_as_number(args[1]))
          Value::Datum(make_complex(real, imag))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::MakePolar =>
      match args.length() {
        2 => {
          let r = number_to_float(datum_to_real(value_as_number(args[0])))
          let theta = number_to_float(datum_to_real(value_as_number(args[1])))
          let real = Datum::Float(r * @math.cosf(theta))
          let imag = Datum::Float(r * @math.sinf(theta))
          Value::Datum(make_complex(real, imag))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::RealPart =>
      match args.length() {
        1 =>
          match value_as_number(args[0]) {
            Datum::Complex(real, _) => Value::Datum(real.val)
            datum => Value::Datum(datum)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ImagPart =>
      match args.length() {
        1 =>
          match value_as_number(args[0]) {
            Datum::Complex(_, imag) => Value::Datum(imag.val)
            Datum::Float(_) => Value::Datum(Datum::Float(0.0))
            Datum::Int(_)
            | Datum::BigInt(_)
            | Datum::Rat(_, _)
            | Datum::BigRat(_, _) => Value::Datum(Datum::Int(0))
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Magnitude =>
      match args.length() {
        1 =>
          match value_as_number(args[0]) {
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Value::Datum(Datum::Float(mag))
            }
            Datum::Int(n) => Value::Datum(Datum::Int(int_abs(n)))
            Datum::Rat(n, d) =>
              Value::Datum(normalize_rat_or_error(int_abs(n), d))
            Datum::BigInt(n) => Value::Datum(Datum::BigInt(bigint_abs(n)))
            Datum::BigRat(n, d) =>
              Value::Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Datum::Float(f) =>
              if f < 0.0 {
                Value::Datum(Datum::Float(-f))
              } else {
                Value::Datum(Datum::Float(f))
              }
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Angle =>
      match args.length() {
        1 =>
          match value_as_number(args[0]) {
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              Value::Datum(Datum::Float(@math.atan2f(imag_f, real_f)))
            }
            datum => {
              let real_f = number_to_float(datum)
              Value::Datum(Datum::Float(@math.atan2f(0.0, real_f)))
            }
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Sqrt =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let mut real_value = value
          let mut is_real = true
          match datum_unlabel(value) {
            Datum::Complex(real, imag) =>
              if datum_is_zero(imag.val) {
                real_value = real.val
              } else {
                is_real = false
              }
            _ => ()
          }
          let exact_opt = if is_real {
            exact_sqrt_real(real_value)
          } else {
            None
          }
          match exact_opt {
            Some(result) => Value::Datum(result)
            None => {
              let (real_f, imag_f) = datum_to_complex_float(value)
              if imag_f == 0.0 && real_f >= 0.0 {
                Value::Datum(Datum::Float(real_f.sqrt()))
              } else {
                let (sr, si) = complex_sqrt_f(real_f, imag_f)
                Value::Datum(complex_from_float(sr, si))
              }
            }
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Exp =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.expf(real_f)))
          } else {
            let (er, ei) = complex_exp_f(real_f, imag_f)
            Value::Datum(complex_from_float(er, ei))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Log =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= 0.0 {
            Value::Datum(Datum::Float(@math.lnf(real_f)))
          } else {
            let (lr, li) = complex_log_f(real_f, imag_f)
            Value::Datum(complex_from_float(lr, li))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Expt =>
      match args.length() {
        2 => {
          let base = value_as_number(args[0])
          let exponent = value_as_number(args[1])
          match exponent {
            Datum::Int(exp_int) =>
              match base {
                Datum::Int(base_int) =>
                  if exp_int >= 0 {
                    Value::Datum(Datum::Int(pow_int(base_int, exp_int)))
                  } else {
                    Value::Datum(
                      normalize_rat_or_error(1, pow_int(base_int, -exp_int)),
                    )
                  }
                Datum::BigInt(base_big) =>
                  if exp_int >= 0 {
                    Value::Datum(
                      bigint_to_integer_datum(pow_bigint_int(base_big, exp_int)),
                    )
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_int(base_big, -exp_int)
                    Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Datum::Rat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_int(n, exp_int)
                    let den = pow_int(d, exp_int)
                    Value::Datum(normalize_rat_or_error(num, den))
                  } else {
                    let num = pow_int(d, -exp_int)
                    let den = pow_int(n, -exp_int)
                    Value::Datum(normalize_rat_or_error(num, den))
                  }
                Datum::BigRat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_bigint_int(n, exp_int)
                    let den = pow_bigint_int(d, exp_int)
                    Value::Datum(normalize_big_rat_or_error(num, den))
                  } else {
                    if bigint_is_zero(n) {
                      raise @core.EvalError("division by zero")
                    }
                    let num = pow_bigint_int(d, -exp_int)
                    let den = pow_bigint_int(n, -exp_int)
                    Value::Datum(normalize_big_rat_or_error(num, den))
                  }
                Datum::Float(f) => {
                  let pow = if exp_int >= 0 {
                    pow_float_int(f, exp_int)
                  } else {
                    1.0 / pow_float_int(f, -exp_int)
                  }
                  Value::Datum(Datum::Float(pow))
                }
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(
                    br,
                    bi,
                    Float::from_int(exp_int),
                    0.0,
                  )
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise @core.EvalError("type error: number expected")
              }
            Datum::BigInt(exp_big) => {
              let exp_sign = exp_big.compare_int(0)
              let abs_exp = if exp_sign < 0 {
                bigint_abs(exp_big)
              } else {
                exp_big
              }
              let exp_is_odd = !bigint_is_zero(abs_exp % bigint_from_int(2))
              match base {
                Datum::Int(base_int) => {
                  let base_big = bigint_from_int(base_int)
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                }
                Datum::BigInt(base_big) =>
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Datum::Rat(n, d) => {
                  let num = bigint_from_int(n)
                  let den = bigint_from_int(d)
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(num) {
                      raise @core.EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                }
                Datum::BigRat(n, d) =>
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(n) {
                      raise @core.EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                Datum::Float(f) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let abs_base = if f < 0.0 { -f } else { f }
                  let mut pow = @math.powf(abs_base, exp_f)
                  if exp_sign < 0 {
                    pow = 1.0 / pow
                  }
                  if f < 0.0 && exp_is_odd {
                    pow = -pow
                  }
                  Value::Datum(Datum::Float(pow))
                }
                Datum::Complex(_, _) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let (br, bi) = datum_to_complex_float(base)
                  let exp_real = if exp_sign < 0 { -exp_f } else { exp_f }
                  let (rr, ri) = complex_expt_f(br, bi, exp_real, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise @core.EvalError("type error: number expected")
              }
            }
            Datum::Float(exp_f) =>
              match base {
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  match float_to_int_exact(exp_f) {
                    Some(exp_int) => {
                      let pow = if exp_int >= 0 {
                        pow_float_int(base_f, exp_int)
                      } else {
                        1.0 / pow_float_int(base_f, -exp_int)
                      }
                      Value::Datum(Datum::Float(pow))
                    }
                    None =>
                      if base_f >= 0.0 {
                        Value::Datum(Datum::Float(@math.powf(base_f, exp_f)))
                      } else {
                        let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                        Value::Datum(complex_from_float(rr, ri))
                      }
                  }
                }
              }
            Datum::Rat(_, _) | Datum::BigRat(_, _) => {
              let exp_f = number_to_float(exponent)
              match base {
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  if base_f >= 0.0 {
                    Value::Datum(Datum::Float(@math.powf(base_f, exp_f)))
                  } else {
                    let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                    Value::Datum(complex_from_float(rr, ri))
                  }
                }
              }
            }
            Datum::Complex(_, _) => {
              let (br, bi) = datum_to_complex_float(base)
              let (er, ei) = datum_to_complex_float(exponent)
              let (rr, ri) = complex_expt_f(br, bi, er, ei)
              Value::Datum(complex_from_float(rr, ri))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Sin =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.sinf(real_f)))
          } else {
            let (sr, si) = complex_sin_f(real_f, imag_f)
            Value::Datum(complex_from_float(sr, si))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Cos =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.cosf(real_f)))
          } else {
            let (cr, ci) = complex_cos_f(real_f, imag_f)
            Value::Datum(complex_from_float(cr, ci))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Tan =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.tanf(real_f)))
          } else {
            let (tr, ti) = complex_tan_f(real_f, imag_f)
            Value::Datum(complex_from_float(tr, ti))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Asin =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Value::Datum(Datum::Float(@math.asinf(real_f)))
          } else {
            let (ar, ai) = complex_asin_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Acos =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Value::Datum(Datum::Float(@math.acosf(real_f)))
          } else {
            let (ar, ai) = complex_acos_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Atan =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.atanf(real_f)))
          } else {
            let (ar, ai) = complex_atan_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        2 => {
          let y = datum_to_real(value_as_number(args[0]))
          let x = datum_to_real(value_as_number(args[1]))
          let y_f = number_to_float(y)
          let x_f = number_to_float(x)
          Value::Datum(Datum::Float(@math.atan2f(y_f, x_f)))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::Numerator =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(n)) => Value::Datum(Datum::Int(n))
            Value::Datum(Datum::Rat(n, _)) => Value::Datum(Datum::Int(n))
            Value::Datum(Datum::BigInt(n)) => Value::Datum(Datum::BigInt(n))
            Value::Datum(Datum::BigRat(n, _)) => Value::Datum(Datum::BigInt(n))
            _ => raise @core.EvalError("type error: rational expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Denominator =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(_)) => Value::Datum(Datum::Int(1))
            Value::Datum(Datum::Rat(_, d)) => Value::Datum(Datum::Int(d))
            Value::Datum(Datum::BigInt(_)) => Value::Datum(Datum::Int(1))
            Value::Datum(Datum::BigRat(_, d)) => Value::Datum(Datum::BigInt(d))
            _ => raise @core.EvalError("type error: rational expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Abs =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(int_abs(n)))
            Datum::Rat(n, d) =>
              Value::Datum(normalize_rat_or_error(int_abs(n), d))
            Datum::BigInt(n) => Value::Datum(Datum::BigInt(bigint_abs(n)))
            Datum::BigRat(n, d) =>
              Value::Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Datum::Float(f) =>
              if f < 0.0 {
                Value::Datum(Datum::Float(-f))
              } else {
                Value::Datum(Datum::Float(f))
              }
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Value::Datum(Datum::Float(mag))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Quotient =>
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise @core.EvalError("division by zero")
              }
              Value::Datum(Datum::Int(left / right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise @core.EvalError("division by zero")
              }
              Value::Datum(Datum::BigInt(left / right))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Remainder =>
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise @core.EvalError("division by zero")
              }
              Value::Datum(Datum::Int(left % right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise @core.EvalError("division by zero")
              }
              Value::Datum(Datum::BigInt(left % right))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Modulo =>
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise @core.EvalError("division by zero")
              }
              let rem = left % right
              let result = if rem == 0 ||
                (rem > 0 && right > 0) ||
                (rem < 0 && right < 0) {
                rem
              } else {
                rem + right
              }
              Value::Datum(Datum::Int(result))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise @core.EvalError("division by zero")
              }
              let rem = left % right
              let rem_sign = rem.compare_int(0)
              let right_sign = right.compare_int(0)
              let result = if rem.is_zero() ||
                (rem_sign > 0 && right_sign > 0) ||
                (rem_sign < 0 && right_sign < 0) {
                rem
              } else {
                rem + right
              }
              Value::Datum(Datum::BigInt(result))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Gcd => {
      let mut acc_int = 0
      let mut acc_big = bigint_from_int(0)
      let mut use_big = false
      for arg in args {
        let value = value_as_exact_integer(arg)
        match value {
          Datum::Int(n) =>
            if use_big {
              acc_big = bigint_gcd(acc_big, bigint_from_int(n))
            } else {
              acc_int = gcd(acc_int, n)
            }
          Datum::BigInt(n) => {
            if !use_big {
              use_big = true
              acc_big = bigint_from_int(acc_int)
            }
            acc_big = bigint_gcd(acc_big, n)
          }
          _ => raise @core.EvalError("type error: integer expected")
        }
      }
      if use_big {
        Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Datum::Int(v)
            None => Datum::BigInt(acc_big)
          },
        )
      } else {
        Value::Datum(Datum::Int(acc_int))
      }
    }
    Primitive::Lcm => {
      let mut acc_int = 1
      let mut acc_big = bigint_from_int(1)
      let mut use_big = false
      for arg in args {
        let value = value_as_exact_integer(arg)
        match value {
          Datum::Int(n) =>
            if use_big {
              let n_big = bigint_from_int(n)
              if acc_big.is_zero() || n_big.is_zero() {
                acc_big = bigint_from_int(0)
              } else {
                let g = bigint_gcd(acc_big, n_big)
                acc_big = bigint_abs(acc_big / g * n_big)
              }
            } else if acc_int == 0 || n == 0 {
              acc_int = 0
            } else {
              let g = gcd(acc_int, n)
              acc_int = int_abs(acc_int / g * n)
            }
          Datum::BigInt(n) => {
            if !use_big {
              use_big = true
              acc_big = bigint_from_int(acc_int)
            }
            if acc_big.is_zero() || n.is_zero() {
              acc_big = bigint_from_int(0)
            } else {
              let g = bigint_gcd(acc_big, n)
              acc_big = bigint_abs(acc_big / g * n)
            }
          }
          _ => raise @core.EvalError("type error: integer expected")
        }
      }
      if use_big {
        Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Datum::Int(v)
            None => Datum::BigInt(acc_big)
          },
        )
      } else {
        Value::Datum(Datum::Int(acc_int))
      }
    }
    Primitive::Max => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut best = value_as_number(args[0])
      let mut has_inexact = best is Datum::Float(_)
      let mut i = 1
      while i < args.length() {
        let cur = value_as_number(args[i])
        if cur is Datum::Float(_) {
          has_inexact = true
        }
        if num_less(best, cur) {
          best = cur
        }
        i = i + 1
      }
      if has_inexact {
        Value::Datum(Datum::Float(number_to_float(best)))
      } else {
        Value::Datum(best)
      }
    }
    Primitive::Min => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut best = value_as_number(args[0])
      let mut has_inexact = best is Datum::Float(_)
      let mut i = 1
      while i < args.length() {
        let cur = value_as_number(args[i])
        if cur is Datum::Float(_) {
          has_inexact = true
        }
        if num_greater(best, cur) {
          best = cur
        }
        i = i + 1
      }
      if has_inexact {
        Value::Datum(Datum::Float(number_to_float(best)))
      } else {
        Value::Datum(best)
      }
    }
    Primitive::Floor =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_floor(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_floor(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_floor_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Ceiling =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_ceiling(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_ceiling(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_ceiling_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Truncate =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(n / d))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = n / d
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = f.to_int()
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Round =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_round(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_round(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_round_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BitwiseAnd =>
      if args.is_empty() {
        Value::Datum(Datum::Int(-1))
      } else {
        let mut acc = value_as_exact_integer(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_exact_integer(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::And)
          i = i + 1
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseIor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let mut acc = value_as_exact_integer(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_exact_integer(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::Or)
          i = i + 1
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseXor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let mut acc = value_as_exact_integer(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_exact_integer(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::Xor)
          i = i + 1
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseNot =>
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          Value::Datum(datum_bitwise_not(value))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BitwiseIf =>
      match args.length() {
        3 => {
          let mask = value_as_exact_integer(args[0])
          let yes_value = value_as_exact_integer(args[1])
          let no_value = value_as_exact_integer(args[2])
          let masked_yes = datum_bitwise_binop(mask, yes_value, BitOp::And)
          let masked_no = datum_bitwise_binop(
            datum_bitwise_not(mask),
            no_value,
            BitOp::And,
          )
          Value::Datum(datum_bitwise_binop(masked_yes, masked_no, BitOp::Or))
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::ArithmeticShift =>
      match args.length() {
        2 => {
          let value = value_as_exact_integer(args[0])
          let shift = value_as_int_index(args[1])
          Value::Datum(datum_arithmetic_shift(value, shift))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::BitwiseBitCount =>
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          let count = datum_bitwise_bit_count(value)
          Value::Datum(Datum::Int(count))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BitwiseLength =>
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          let count = datum_bitwise_length(value)
          Value::Datum(Datum::Int(count))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BitwiseFirstBitSet =>
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Value::Datum(Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitOp::And)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Value::Datum(Datum::Int(idx))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BitwiseBitSetP =>
      match args.length() {
        2 => {
          let value = value_as_exact_integer(args[0])
          let index = value_as_nonnegative_int(args[1])
          bool_value(datum_bitwise_bit_set(value, index))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::BitwiseCopyBit =>
      match args.length() {
        3 => {
          let value = value_as_exact_integer(args[0])
          let index = value_as_nonnegative_int(args[1])
          let bit_value = value_as_exact_integer(args[2])
          let bit = match bit_value {
            Datum::Int(n) => n
            Datum::BigInt(n) =>
              if bigint_is_zero(n) {
                0
              } else if n.compare_int(1) == 0 {
                1
              } else {
                raise @core.EvalError("type error: bit expected")
              }
            _ => raise @core.EvalError("type error: bit expected")
          }
          let mask = bigint_to_integer_datum(bigint_pow2(index))
          let result = if bit == 0 {
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitOp::And)
          } else if bit == 1 {
            datum_bitwise_binop(value, mask, BitOp::Or)
          } else {
            raise @core.EvalError("type error: bit expected")
          }
          Value::Datum(result)
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::BitwiseBitField =>
      match args.length() {
        3 => {
          let value = value_as_exact_integer(args[0])
          let start = value_as_nonnegative_int(args[1])
          let end = value_as_nonnegative_int(args[2])
          Value::Datum(datum_bitwise_bit_field(value, start, end))
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::BitwiseCopyBitField =>
      match args.length() {
        4 => {
          let value = value_as_exact_integer(args[0])
          let start = value_as_nonnegative_int(args[1])
          let end = value_as_nonnegative_int(args[2])
          let from = value_as_exact_integer(args[3])
          Value::Datum(datum_bitwise_copy_bit_field(value, start, end, from))
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::BitwiseRotateBitField =>
      match args.length() {
        4 => {
          let value = value_as_exact_integer(args[0])
          let start = value_as_nonnegative_int(args[1])
          let end = value_as_nonnegative_int(args[2])
          let count = value_as_int_index(args[3])
          Value::Datum(datum_bitwise_rotate_bit_field(value, start, end, count))
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::BitwiseReverseBitField =>
      match args.length() {
        3 => {
          let value = value_as_exact_integer(args[0])
          let start = value_as_nonnegative_int(args[1])
          let end = value_as_nonnegative_int(args[2])
          Value::Datum(datum_bitwise_reverse_bit_field(value, start, end))
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FixnumP =>
      match args.length() {
        1 => bool_value(value_is_fixnum(args[0]))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FixnumWidth =>
      match args.length() {
        0 => Value::Datum(Datum::Int(fixnum_width_value()))
        n => raise arity_mismatch(0, n)
      }
    Primitive::LeastFixnum =>
      match args.length() {
        0 => {
          let half = fixnum_half_bigint()
          Value::Datum(fixnum_from_bigint(-half))
        }
        n => raise arity_mismatch(0, n)
      }
    Primitive::GreatestFixnum =>
      match args.length() {
        0 => {
          let half = fixnum_half_bigint()
          Value::Datum(fixnum_from_bigint(half - bigint_from_int(1)))
        }
        n => raise arity_mismatch(0, n)
      }
    Primitive::FxEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_fixnum_int(args[0])
        let mut ok = true
        let mut i = 1
        while i < args.length() {
          if value_as_fixnum_int(args[i]) != first {
            ok = false
            break
          } else {
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FxLess =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_fixnum_int(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_fixnum_int(args[i])
          if prev >= cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FxGreater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_fixnum_int(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_fixnum_int(args[i])
          if prev <= cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FxLessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_fixnum_int(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_fixnum_int(args[i])
          if prev > cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FxGreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_fixnum_int(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_fixnum_int(args[i])
          if prev < cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FxZeroP =>
      match args.length() {
        1 => bool_value(value_as_fixnum_int(args[0]) == 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxPositiveP =>
      match args.length() {
        1 => bool_value(value_as_fixnum_int(args[0]) > 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxNegativeP =>
      match args.length() {
        1 => bool_value(value_as_fixnum_int(args[0]) < 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxOddP =>
      match args.length() {
        1 => bool_value(value_as_fixnum_int(args[0]) % 2 != 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxEvenP =>
      match args.length() {
        1 => bool_value(value_as_fixnum_int(args[0]) % 2 == 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxMin => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut current = value_as_fixnum_int(args[0])
      let mut i = 1
      while i < args.length() {
        let cur = value_as_fixnum_int(args[i])
        if cur < current {
          current = cur
        }
        i = i + 1
      }
      Value::Datum(Datum::Int(current))
    }
    Primitive::FxMax => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut current = value_as_fixnum_int(args[0])
      let mut i = 1
      while i < args.length() {
        let cur = value_as_fixnum_int(args[i])
        if cur > current {
          current = cur
        }
        i = i + 1
      }
      Value::Datum(Datum::Int(current))
    }
    Primitive::FxAdd => {
      let mut acc = bigint_from_int(0)
      for arg in args {
        acc = acc + value_as_fixnum_bigint(arg)
      }
      Value::Datum(fixnum_from_bigint(acc))
    }
    Primitive::FxSub =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_fixnum_bigint(args[0])
          Value::Datum(fixnum_from_bigint(-value))
        }
        _ => {
          let mut result = value_as_fixnum_bigint(args[0])
          let mut i = 1
          while i < args.length() {
            result = result - value_as_fixnum_bigint(args[i])
            i = i + 1
          }
          Value::Datum(fixnum_from_bigint(result))
        }
      }
    Primitive::FxMul => {
      let mut acc = bigint_from_int(1)
      for arg in args {
        acc = acc * value_as_fixnum_bigint(arg)
      }
      Value::Datum(fixnum_from_bigint(acc))
    }
    Primitive::FxDiv =>
      match args.length() {
        2 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          let mut q = left / right
          let r = left % right
          if !bigint_is_zero(r) {
            let r_sign = r.compare_int(0)
            let right_sign = right.compare_int(0)
            if (r_sign > 0 && right_sign < 0) || (r_sign < 0 && right_sign > 0) {
              q = q - bigint_from_int(1)
            }
          }
          Value::Datum(fixnum_from_bigint(q))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxMod =>
      match args.length() {
        2 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          let mut r = left % right
          if !bigint_is_zero(r) {
            let r_sign = r.compare_int(0)
            let right_sign = right.compare_int(0)
            if (r_sign > 0 && right_sign < 0) || (r_sign < 0 && right_sign > 0) {
              r = r + right
            }
          }
          Value::Datum(fixnum_from_bigint(r))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxDiv0 =>
      match args.length() {
        2 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          Value::Datum(fixnum_from_bigint(left / right))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxMod0 =>
      match args.length() {
        2 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          Value::Datum(fixnum_from_bigint(left % right))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxAddCarry =>
      match args.length() {
        3 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          let carry = value_as_fixnum_bigint(args[2])
          let total = left + right + carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Value::Values([
            Value::Datum(fixnum_from_bigint(sum)),
            Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxSubCarry =>
      match args.length() {
        3 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          let carry = value_as_fixnum_bigint(args[2])
          let total = left - right - carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Value::Values([
            Value::Datum(fixnum_from_bigint(sum)),
            Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxMulCarry =>
      match args.length() {
        3 => {
          let left = value_as_fixnum_bigint(args[0])
          let right = value_as_fixnum_bigint(args[1])
          let carry = value_as_fixnum_bigint(args[2])
          let total = left * right + carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Value::Values([
            Value::Datum(fixnum_from_bigint(sum)),
            Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxNot =>
      match args.length() {
        1 => {
          let value = value_as_fixnum_datum(args[0])
          let result = datum_bitwise_not(value)
          Value::Datum(fixnum_from_datum(result))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxAnd =>
      if args.is_empty() {
        Value::Datum(Datum::Int(-1))
      } else {
        let mut acc = value_as_fixnum_datum(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_fixnum_datum(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::And)
          i = i + 1
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxIor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let mut acc = value_as_fixnum_datum(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_fixnum_datum(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::Or)
          i = i + 1
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxXor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let mut acc = value_as_fixnum_datum(args[0])
        let mut i = 1
        while i < args.length() {
          let next = value_as_fixnum_datum(args[i])
          acc = datum_bitwise_binop(acc, next, BitOp::Xor)
          i = i + 1
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxIf =>
      match args.length() {
        3 => {
          let mask = value_as_fixnum_datum(args[0])
          let yes_value = value_as_fixnum_datum(args[1])
          let no_value = value_as_fixnum_datum(args[2])
          let masked_yes = datum_bitwise_binop(mask, yes_value, BitOp::And)
          let masked_no = datum_bitwise_binop(
            datum_bitwise_not(mask),
            no_value,
            BitOp::And,
          )
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_binop(masked_yes, masked_no, BitOp::Or),
            ),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxBitCount =>
      match args.length() {
        1 => {
          let value = value_as_fixnum_datum(args[0])
          let count = datum_bitwise_bit_count(value)
          Value::Datum(Datum::Int(count))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxLength =>
      match args.length() {
        1 => {
          let value = value_as_fixnum_datum(args[0])
          let count = datum_bitwise_length(value)
          Value::Datum(Datum::Int(count))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxFirstBitSet =>
      match args.length() {
        1 => {
          let value = value_as_fixnum_datum(args[0])
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Value::Datum(Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitOp::And)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Value::Datum(Datum::Int(idx))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FxBitSetP =>
      match args.length() {
        2 => {
          let value = value_as_fixnum_datum(args[0])
          let index = value_as_nonnegative_fixnum_int(args[1])
          bool_value(datum_bitwise_bit_set(value, index))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxCopyBit =>
      match args.length() {
        3 => {
          let value = value_as_fixnum_datum(args[0])
          let index = value_as_nonnegative_fixnum_int(args[1])
          let bit_value = value_as_fixnum_bigint(args[2])
          let bit = if bigint_is_zero(bit_value) {
            0
          } else if bit_value.compare_int(1) == 0 {
            1
          } else {
            raise @core.EvalError("type error: bit expected")
          }
          let mask = bigint_to_integer_datum(bigint_pow2(index))
          let result = if bit == 0 {
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitOp::And)
          } else {
            datum_bitwise_binop(value, mask, BitOp::Or)
          }
          Value::Datum(fixnum_from_datum(result))
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxBitField =>
      match args.length() {
        3 => {
          let value = value_as_fixnum_datum(args[0])
          let start = value_as_nonnegative_fixnum_int(args[1])
          let end = value_as_nonnegative_fixnum_int(args[2])
          Value::Datum(
            fixnum_from_datum(datum_bitwise_bit_field(value, start, end)),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxCopyBitField =>
      match args.length() {
        4 => {
          let value = value_as_fixnum_datum(args[0])
          let start = value_as_nonnegative_fixnum_int(args[1])
          let end = value_as_nonnegative_fixnum_int(args[2])
          let from = value_as_fixnum_datum(args[3])
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_copy_bit_field(value, start, end, from),
            ),
          )
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::FxRotateBitField =>
      match args.length() {
        4 => {
          let value = value_as_fixnum_datum(args[0])
          let start = value_as_nonnegative_fixnum_int(args[1])
          let end = value_as_nonnegative_fixnum_int(args[2])
          let count = value_as_fixnum_int(args[3])
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_rotate_bit_field(value, start, end, count),
            ),
          )
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::FxReverseBitField =>
      match args.length() {
        3 => {
          let value = value_as_fixnum_datum(args[0])
          let start = value_as_nonnegative_fixnum_int(args[1])
          let end = value_as_nonnegative_fixnum_int(args[2])
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_reverse_bit_field(value, start, end),
            ),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::FxArithmeticShift =>
      match args.length() {
        2 => {
          let value = value_as_fixnum_datum(args[0])
          let shift = value_as_fixnum_int(args[1])
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxArithmeticShiftLeft =>
      match args.length() {
        2 => {
          let value = value_as_fixnum_datum(args[0])
          let shift = value_as_nonnegative_fixnum_int(args[1])
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FxArithmeticShiftRight =>
      match args.length() {
        2 => {
          let value = value_as_fixnum_datum(args[0])
          let shift = value_as_nonnegative_fixnum_int(args[1])
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, -shift)))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlonumP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(datum) =>
              match datum_unlabel(datum) {
                Datum::Float(_) => bool_value(true)
                _ => bool_value(false)
              }
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RealToFlonum =>
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(Datum::Float(number_to_float(value)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FixnumToFlonum =>
      match args.length() {
        1 => {
          let value = value_as_fixnum_int(args[0])
          Value::Datum(Datum::Float(Float::from_int(value)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_flonum(args[0])
        let mut ok = true
        let mut i = 1
        while i < args.length() {
          if value_as_flonum(args[i]) != first {
            ok = false
            break
          } else {
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FlLess =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_flonum(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_flonum(args[i])
          if prev >= cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FlGreater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_flonum(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_flonum(args[i])
          if prev <= cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FlLessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_flonum(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_flonum(args[i])
          if prev > cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FlGreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_flonum(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_flonum(args[i])
          if prev < cur {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    Primitive::FlIntegerP =>
      match args.length() {
        1 => {
          let f = value_as_flonum(args[0])
          let ok = float_is_finite(f) && float_to_int_exact(f) is Some(_)
          bool_value(ok)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlZeroP =>
      match args.length() {
        1 => bool_value(value_as_flonum(args[0]) == 0.0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlPositiveP =>
      match args.length() {
        1 => bool_value(value_as_flonum(args[0]) > 0.0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlNegativeP =>
      match args.length() {
        1 => bool_value(value_as_flonum(args[0]) < 0.0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlOddP =>
      match args.length() {
        1 => bool_value(value_as_integer_flonum(args[0]) % 2 != 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlEvenP =>
      match args.length() {
        1 => bool_value(value_as_integer_flonum(args[0]) % 2 == 0)
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlFiniteP =>
      match args.length() {
        1 => bool_value(float_is_finite(value_as_flonum(args[0])))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlInfiniteP =>
      match args.length() {
        1 => bool_value(float_is_infinite(value_as_flonum(args[0])))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlNanP =>
      match args.length() {
        1 => bool_value(float_is_nan(value_as_flonum(args[0])))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlMax => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return Value::Datum(Datum::Float(current))
      }
      let mut i = 1
      while i < args.length() {
        let cur = value_as_flonum(args[i])
        if float_is_nan(cur) {
          return Value::Datum(Datum::Float(cur))
        }
        if cur > current {
          current = cur
        }
        i = i + 1
      }
      Value::Datum(Datum::Float(current))
    }
    Primitive::FlMin => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return Value::Datum(Datum::Float(current))
      }
      let mut i = 1
      while i < args.length() {
        let cur = value_as_flonum(args[i])
        if float_is_nan(cur) {
          return Value::Datum(Datum::Float(cur))
        }
        if cur < current {
          current = cur
        }
        i = i + 1
      }
      Value::Datum(Datum::Float(current))
    }
    Primitive::FlAdd => {
      let mut acc = Float::from_int(0)
      for arg in args {
        acc = acc + value_as_flonum(arg)
      }
      Value::Datum(Datum::Float(acc))
    }
    Primitive::FlMul => {
      let mut acc = Float::from_int(1)
      for arg in args {
        acc = acc * value_as_flonum(arg)
      }
      Value::Datum(Datum::Float(acc))
    }
    Primitive::FlSub =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => Value::Datum(Datum::Float(-value_as_flonum(args[0])))
        _ => {
          let mut result = value_as_flonum(args[0])
          let mut i = 1
          while i < args.length() {
            result = result - value_as_flonum(args[i])
            i = i + 1
          }
          Value::Datum(Datum::Float(result))
        }
      }
    Primitive::FlDiv =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let denom = value_as_flonum(args[0])
          Value::Datum(Datum::Float(Float::from_int(1) / denom))
        }
        _ => {
          let mut result = value_as_flonum(args[0])
          let mut i = 1
          while i < args.length() {
            result = result / value_as_flonum(args[i])
            i = i + 1
          }
          Value::Datum(Datum::Float(result))
        }
      }
    Primitive::FlAbs =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          Value::Datum(Datum::Float(if value < 0.0 { -value } else { value }))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlDivAndMod =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (q, r) = flonum_div_and_mod(left, right)
          Value::Values([
            Value::Datum(Datum::Float(q)),
            Value::Datum(Datum::Float(r)),
          ])
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlDivInt =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (q, _) = flonum_div_and_mod(left, right)
          Value::Datum(Datum::Float(q))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlMod =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (_, r) = flonum_div_and_mod(left, right)
          Value::Datum(Datum::Float(r))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlDiv0AndMod0 =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (q, r) = flonum_div0_and_mod0(left, right)
          Value::Values([
            Value::Datum(Datum::Float(q)),
            Value::Datum(Datum::Float(r)),
          ])
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlDiv0 =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (q, _) = flonum_div0_and_mod0(left, right)
          Value::Datum(Datum::Float(q))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlMod0 =>
      match args.length() {
        2 => {
          let left = value_as_flonum(args[0])
          let right = value_as_flonum(args[1])
          let (_, r) = flonum_div0_and_mod0(left, right)
          Value::Datum(Datum::Float(r))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlNumerator =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          Value::Datum(Datum::Float(flonum_numerator(value)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlDenominator =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          Value::Datum(Datum::Float(flonum_denominator(value)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlFloor =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          if !float_is_finite(value) {
            Value::Datum(Datum::Float(value))
          } else {
            Value::Datum(Datum::Float(Float::from_int(float_floor_int(value))))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlCeiling =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          if !float_is_finite(value) {
            Value::Datum(Datum::Float(value))
          } else {
            Value::Datum(
              Datum::Float(Float::from_int(float_ceiling_int(value))),
            )
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlTruncate =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          if !float_is_finite(value) {
            Value::Datum(Datum::Float(value))
          } else {
            Value::Datum(Datum::Float(Float::from_int(value.to_int())))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlRound =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          if !float_is_finite(value) {
            Value::Datum(Datum::Float(value))
          } else {
            Value::Datum(Datum::Float(Float::from_int(float_round_int(value))))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlExp =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.expf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlLog =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.lnf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlSin =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.sinf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlCos =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.cosf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlTan =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.tanf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlAsin =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.asinf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlAcos =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.acosf(value_as_flonum(args[0]))))
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlAtan =>
      match args.length() {
        1 => Value::Datum(Datum::Float(@math.atanf(value_as_flonum(args[0]))))
        2 => {
          let y = value_as_flonum(args[0])
          let x = value_as_flonum(args[1])
          Value::Datum(Datum::Float(@math.atan2f(y, x)))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::FlSqrt =>
      match args.length() {
        1 => {
          let value = value_as_flonum(args[0])
          Value::Datum(Datum::Float(value.sqrt()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FlExpt =>
      match args.length() {
        2 => {
          let base = value_as_flonum(args[0])
          let exp = value_as_flonum(args[1])
          Value::Datum(Datum::Float(@math.powf(base, exp)))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Eq =>
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    Primitive::Eqv =>
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    Primitive::Equal =>
      match args.length() {
        2 => bool_value(value_equal(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    Primitive::Cons =>
      match args.length() {
        2 => {
          let car = value_to_datum_element(args[0])
          let cdr = value_to_datum_element(args[1])
          Value::Datum(pair_new(car, cdr))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::SetCar =>
      match args.length() {
        2 => {
          let (car_cell, _) = value_as_pair_cells(args[0])
          car_cell.val = value_to_datum_element(args[1])
          Value::Void
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::SetCdr =>
      match args.length() {
        2 => {
          let (_, cdr_cell) = value_as_pair_cells(args[0])
          cdr_cell.val = value_to_datum_element(args[1])
          Value::Void
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Car =>
      match args.length() {
        1 => {
          let (car, _) = value_as_pair(args[0])
          value_from_datum(car)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Cdr =>
      match args.length() {
        1 => {
          let (_, cdr) = value_as_pair(args[0])
          value_from_datum(cdr)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Cxr(chain) =>
      match args.length() {
        1 => {
          let mut cur = value_as_datum(args[0])
          let mut i = chain.length()
          while i > 0 {
            i = i - 1
            let op = match chain.get_char(i) {
              Some(ch) => ch
              None => raise @core.EvalError("internal error: invalid cxr")
            }
            match cur {
              Datum::Pair(car, cdr) =>
                match op {
                  'a' => cur = car.val
                  'd' => cur = cdr.val
                  _ => raise @core.EvalError("internal error: invalid cxr")
                }
              _ => raise @core.EvalError("type error: pair expected")
            }
          }
          value_from_datum(cur)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::List => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_to_datum_element(arg))
      }
      Value::Datum(list_from_array(items))
    }
    Primitive::MakeList =>
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_to_datum_element(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::NullP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Nil) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::PairP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Pair(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SymbolP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Symbol(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SymbolEq => bool_value(compare_chain_symbol(args))
    Primitive::IdentifierP =>
      match args.length() {
        1 => bool_value(is_identifier_value(args[0]))
        n => raise arity_mismatch(1, n)
      }
    Primitive::SyntaxP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::SyntaxObject(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FreeIdentifierEq =>
      match args.length() {
        2 => {
          let (a, a_scopes, a_binding) = identifier_info(args[0])
          let (b, b_scopes, b_binding) = identifier_info(args[1])
          if a != b {
            bool_value(false)
          } else if a_binding is Some(_) && b_binding is Some(_) {
            bool_value(a_binding == b_binding)
          } else {
            bool_value(scopes_equal(a_scopes, b_scopes))
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::BoundIdentifierEq =>
      match args.length() {
        2 => {
          let (a, a_scopes, a_binding) = identifier_info(args[0])
          let (b, b_scopes, b_binding) = identifier_info(args[1])
          if a != b {
            bool_value(false)
          } else if a_binding is Some(_) && b_binding is Some(_) {
            bool_value(a_binding == b_binding)
          } else {
            bool_value(scopes_equal(a_scopes, b_scopes))
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::SymbolToString =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Symbol(name)) =>
              Value::Datum(Datum::String(Ref::new(name)))
            _ => raise @core.EvalError("type error: symbol expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringToSymbol =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::String(name)) =>
              Value::Datum(Datum::Symbol(name.val))
            _ => raise @core.EvalError("type error: string expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringHash =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::Int(hash_string_value(s)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringCiHash =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          let folded = unicode_string_foldcase(s)
          Value::Datum(Datum::Int(hash_string_value(folded)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SymbolHash =>
      match args.length() {
        1 => {
          let name = value_as_symbol(args[0])
          Value::Datum(Datum::Int(hash_string_value(name)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EqualHash =>
      match args.length() {
        1 => {
          let repr = value_to_string(args[0])
          Value::Datum(Datum::Int(hash_string_value(repr)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SyntaxToDatum =>
      match args.length() {
        1 =>
          match args[0] {
            Value::SyntaxObject(obj) =>
              Value::Datum(strip_syntax_datum(obj.datum))
            Value::Datum(d) => Value::Datum(strip_syntax_datum(d))
            _ => raise @core.EvalError("type error: syntax expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::DatumToSyntax =>
      match args.length() {
        2 | 3 => {
          let scopes = match args[0] {
            Value::SyntaxObject(obj) => obj.scopes
            _ => []
          }
          let context_binding = match args[0] {
            Value::SyntaxObject(obj) => obj.binding_id
            _ => None
          }
          let mut base = match args[1] {
            Value::SyntaxObject(obj) => obj.datum
            Value::Datum(d) => d
            _ => raise @core.EvalError("type error: datum expected")
          }
          let template_name = match args[0] {
            Value::SyntaxObject(obj) =>
              match datum_unlabel(obj.datum) {
                Datum::Symbol(name) => Some(name)
                _ => None
              }
            _ => None
          }
          match (template_name, datum_unlabel(base)) {
            (Some(template), Datum::Symbol(base_name)) =>
              match strip_gensym_suffix(template) {
                Some(immediate) =>
                  if strip_gensym_suffix(immediate) is Some(_) {
                    let original = strip_all_gensym_suffixes(template)
                    if original == base_name {
                      base = Datum::Symbol(immediate)
                    }
                  }
                None => ()
              }
            _ => ()
          }
          let datum = syntax_wrap_root(base, scopes)
          let binding_id = match datum_unlabel(base) {
            Datum::Symbol(_) => context_binding
            _ => None
          }
          Value::SyntaxObject(SyntaxObject::{ datum, scopes, binding_id })
        }
        n => raise @core.EvalError("arity mismatch: expected 2 or 3 got \{n}")
      }
    Primitive::BooleanP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Bool(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::BooleanEq => bool_value(compare_chain_bool(args))
    Primitive::NumberP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_))
            | Value::Datum(Datum::Complex(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::IntegerP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(datum) => bool_value(datum_is_integer_valued(datum))
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ExactIntegerP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(datum) => bool_value(datum_is_exact_integer(datum))
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RationalP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(datum) => bool_value(datum_is_rational_valued(datum))
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RealP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_)) => bool_value(true)
            Value::Datum(Datum::Complex(_, imag)) =>
              bool_value(datum_is_zero(imag.val))
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ComplexP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_))
            | Value::Datum(Datum::Complex(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ExactP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) =>
              match (real.val, imag.val) {
                (Datum::Int(_), Datum::Int(_)) => bool_value(true)
                (Datum::Int(_), Datum::BigInt(_)) => bool_value(true)
                (Datum::Int(_), Datum::Rat(_, _)) => bool_value(true)
                (Datum::Int(_), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::BigInt(_), Datum::Int(_)) => bool_value(true)
                (Datum::BigInt(_), Datum::BigInt(_)) => bool_value(true)
                (Datum::BigInt(_), Datum::Rat(_, _)) => bool_value(true)
                (Datum::BigInt(_), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::Rat(_, _), Datum::Int(_)) => bool_value(true)
                (Datum::Rat(_, _), Datum::BigInt(_)) => bool_value(true)
                (Datum::Rat(_, _), Datum::Rat(_, _)) => bool_value(true)
                (Datum::Rat(_, _), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::Int(_)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::BigInt(_)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::Rat(_, _)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::BigRat(_, _)) => bool_value(true)
                _ => bool_value(false)
              }
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::InexactP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(_)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) =>
              match (real.val, imag.val) {
                (Datum::Float(_), _) => bool_value(true)
                (_, Datum::Float(_)) => bool_value(true)
                _ => bool_value(false)
              }
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ZeroP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Int(n)) => bool_value(n == 0)
            Value::Datum(Datum::Rat(n, _)) => bool_value(n == 0)
            Value::Datum(Datum::BigInt(n)) => bool_value(n.is_zero())
            Value::Datum(Datum::BigRat(n, _)) => bool_value(n.is_zero())
            Value::Datum(Datum::Float(n)) => bool_value(n == 0.0)
            Value::Datum(Datum::Complex(real, imag)) =>
              bool_value(datum_is_zero(real.val) && datum_is_zero(imag.val))
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::PositiveP =>
      match args.length() {
        1 => {
          let value = datum_to_real(value_as_number(args[0]))
          match value {
            Datum::Int(n) => bool_value(n > 0)
            Datum::Rat(n, _) => bool_value(n > 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) > 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) > 0)
            Datum::Float(n) => bool_value(n > 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::NegativeP =>
      match args.length() {
        1 => {
          let value = datum_to_real(value_as_number(args[0]))
          match value {
            Datum::Int(n) => bool_value(n < 0)
            Datum::Rat(n, _) => bool_value(n < 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) < 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) < 0)
            Datum::Float(n) => bool_value(n < 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::OddP =>
      match args.length() {
        1 => {
          let value = match args[0] {
            Value::Datum(Datum::Int(n)) => Datum::Int(n)
            Value::Datum(Datum::BigInt(n)) => Datum::BigInt(n)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(n) => Datum::Int(n)
                  Datum::BigInt(n) => Datum::BigInt(n)
                  _ =>
                    raise @core.EvalError("type error: exact integer expected")
                }
              } else {
                raise @core.EvalError("type error: exact integer expected")
              }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
          match value {
            Datum::Int(n) => bool_value(n % 2 != 0)
            Datum::BigInt(n) =>
              bool_value(!bigint_is_zero(n % bigint_from_int(2)))
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EvenP =>
      match args.length() {
        1 => {
          let value = match args[0] {
            Value::Datum(Datum::Int(n)) => Datum::Int(n)
            Value::Datum(Datum::BigInt(n)) => Datum::BigInt(n)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(n) => Datum::Int(n)
                  Datum::BigInt(n) => Datum::BigInt(n)
                  _ =>
                    raise @core.EvalError("type error: exact integer expected")
                }
              } else {
                raise @core.EvalError("type error: exact integer expected")
              }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
          match value {
            Datum::Int(n) => bool_value(n % 2 == 0)
            Datum::BigInt(n) =>
              bool_value(bigint_is_zero(n % bigint_from_int(2)))
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::FiniteP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_finite(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_finite(f))
                  Datum::Int(_)
                  | Datum::BigInt(_)
                  | Datum::Rat(_, _)
                  | Datum::BigRat(_, _) => bool_value(true)
                  _ => raise @core.EvalError("type error: number expected")
                }
              } else {
                raise @core.EvalError("type error: real expected")
              }
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::InfiniteP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_infinite(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(false)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_infinite(f))
                  Datum::Int(_)
                  | Datum::BigInt(_)
                  | Datum::Rat(_, _)
                  | Datum::BigRat(_, _) => bool_value(false)
                  _ => raise @core.EvalError("type error: number expected")
                }
              } else {
                raise @core.EvalError("type error: real expected")
              }
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::NanP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_nan(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(false)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_nan(f))
                  Datum::Int(_)
                  | Datum::BigInt(_)
                  | Datum::Rat(_, _)
                  | Datum::BigRat(_, _) => bool_value(false)
                  _ => raise @core.EvalError("type error: number expected")
                }
              } else {
                raise @core.EvalError("type error: real expected")
              }
            _ => raise @core.EvalError("type error: number expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ProcedureP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Primitive(_)
            | Value::Closure(_)
            | Value::CaseClosure(_)
            | Value::GuardHandler(_)
            | Value::Parameter(_)
            | Value::Continuation(_)
            | Value::RecordProc(_)
            | Value::ConditionProc(_)
            | Value::EnumSetProc(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Record(record) => bool_value(!record.record_type.is_opaque)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordRtd =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Record(record) =>
              if record.record_type.is_opaque {
                raise @core.EvalError("record type is opaque")
              } else {
                match
                  lookup_record_type_descriptor_by_id(record.record_type.id) {
                  Some(desc) => Value::RecordTypeDescriptor(desc)
                  None =>
                    raise @core.EvalError("invalid record type descriptor")
                }
              }
            _ => raise @core.EvalError("type error: record expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeDescriptorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::RecordTypeDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructorDescriptorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::RecordConstructorDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeName =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::Datum(Datum::Symbol(desc.record_type.name))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeParent =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.parent {
            Some(parent) =>
              match lookup_record_type_descriptor(parent.name) {
                Some(parent_desc) => Value::RecordTypeDescriptor(parent_desc)
                None => Value::Datum(Datum::Bool(false))
              }
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeUid =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.uid {
            Some(uid) => Value::Datum(Datum::Symbol(uid))
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeGenerativeP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.uid is None)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeSealedP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_sealed)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeOpaqueP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_opaque)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeFieldNames =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          let items : Array[Datum] = []
          for field in desc.record_type.fields {
            items.push(Datum::Symbol(field.name))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructorDescriptor =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordConstructorDescriptor(desc.constructor_desc)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructor =>
      raise @core.EvalError("record-constructor requires evaluation context")
    Primitive::RecordTypeFieldMutableP =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          bool_value(field.mutable)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::RecordPredicate =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordProc(
            make_record_proc(RecordProcKind::Predicate(desc.record_type)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordAccessor =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          Value::RecordProc(
            make_record_proc(RecordProcKind::Accessor(desc.record_type, idx)),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::RecordMutator =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          if !field.mutable {
            raise @core.EvalError("record field is immutable: \{field.name}")
          }
          Value::RecordProc(
            make_record_proc(RecordProcKind::Mutator(desc.record_type, idx)),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::MakeRecordTypeDescriptor =>
      match args.length() {
        6 => {
          let name = value_as_symbol(args[0])
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) => None
            Value::RecordTypeDescriptor(desc) => Some(desc)
            _ =>
              raise @core.EvalError(
                "type error: record type descriptor expected",
              )
          }
          let parent_type = match parent_desc {
            Some(desc) => Some(desc.record_type)
            None => None
          }
          match parent_type {
            Some(parent) =>
              if parent.is_sealed {
                raise @core.EvalError("record type is sealed")
              }
            None => ()
          }
          let uid = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            Value::Datum(Datum::Symbol(uid_name)) => Some(uid_name)
            _ => raise @core.EvalError("type error: symbol expected")
          }
          let is_sealed = value_as_bool(args[3])
          let is_opaque = value_as_bool(args[4])
          let new_fields = parse_field_specs(value_as_vector(args[5]))
          let fields : Array[RecordField] = []
          match parent_type {
            Some(parent) =>
              for field in parent.fields {
                fields.push(field)
              }
            None => ()
          }
          for field in new_fields {
            fields.push(field)
          }
          let effective_opaque = match parent_type {
            Some(parent) => parent.is_opaque || is_opaque
            None => is_opaque
          }
          match uid {
            Some(uid_name) =>
              match lookup_record_type_descriptor_by_uid(uid_name) {
                Some(existing_desc) => {
                  if !record_type_matches(
                      existing_desc.record_type,
                      parent_type,
                      is_sealed,
                      is_opaque,
                      fields,
                    ) {
                    raise @core.EvalError("invalid record type descriptor")
                  }
                  register_record_type_alias(name, existing_desc)
                  Value::RecordTypeDescriptor(existing_desc)
                }
                None => {
                  let record_type = make_record_type(
                    name, parent_type, is_sealed, effective_opaque, uid, fields,
                  )
                  let ctor_desc = default_constructor_descriptor(record_type)
                  let rtd = make_record_type_descriptor(record_type, ctor_desc)
                  let _ = register_record_type(name, rtd)
                  Value::RecordTypeDescriptor(rtd)
                }
              }
            None => {
              let record_type = make_record_type(
                name, parent_type, is_sealed, effective_opaque, uid, fields,
              )
              let ctor_desc = default_constructor_descriptor(record_type)
              let rtd = make_record_type_descriptor(record_type, ctor_desc)
              let _ = register_record_type(name, rtd)
              Value::RecordTypeDescriptor(rtd)
            }
          }
        }
        n => raise arity_mismatch(6, n)
      }
    Primitive::MakeRecordConstructorDescriptor =>
      match args.length() {
        3 => {
          let rtd = value_as_record_type_descriptor(args[0])
          let record_type = rtd.record_type
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) =>
              match record_type.parent {
                Some(parent) => Some(default_constructor_descriptor(parent))
                None => None
              }
            _ => Some(value_as_record_constructor_descriptor(args[1]))
          }
          match record_type.parent {
            Some(parent) =>
              match parent_desc {
                Some(desc) =>
                  if desc.record_type.id != parent.id {
                    raise @core.EvalError(
                      "invalid record constructor descriptor",
                    )
                  }
                None =>
                  raise @core.EvalError("invalid record constructor descriptor")
              }
            None =>
              match parent_desc {
                Some(_) =>
                  raise @core.EvalError("invalid record constructor descriptor")
                None => ()
              }
          }
          let protocol = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            value => {
              if !is_procedure_value(value) {
                raise @core.EvalError("type error: procedure expected")
              }
              Some(value)
            }
          }
          match (protocol, parent_desc) {
            (None, Some(desc)) =>
              match desc.protocol {
                Some(_) =>
                  raise @core.EvalError("invalid record constructor descriptor")
                None => ()
              }
            _ => ()
          }
          Value::RecordConstructorDescriptor(
            make_record_constructor_descriptor(
              record_type, parent_desc, protocol,
            ),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::Condition => {
      if args.length() == 0 {
        raise arity_mismatch(1, 0)
      }
      let base = condition_base_type()
      let components : Array[Record] = []
      for arg in args {
        let parts = condition_components(arg, base)
        for record in parts {
          components.push(record)
        }
      }
      if components.length() == 1 {
        Value::Record(components[0])
      } else {
        Value::Datum(Datum::Condition(make_condition(components)))
      }
    }
    Primitive::ConditionP =>
      match args.length() {
        1 => {
          let base = condition_base_type()
          let ok = condition_components_opt(args[0], base) is Some(_)
          bool_value(ok)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SimpleConditions =>
      match args.length() {
        1 => {
          let base = condition_base_type()
          let components = condition_components(args[0], base)
          let items : Array[Datum] = []
          for record in components {
            items.push(Datum::Record(record))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ConditionPredicate =>
      match args.length() {
        1 => {
          let desc = value_as_condition_type_descriptor(args[0])
          Value::ConditionProc(
            make_condition_proc(ConditionProcKind::Predicate(desc.record_type)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ConditionAccessor =>
      match args.length() {
        2 => {
          let desc = value_as_condition_type_descriptor(args[0])
          let (accessor_type, idx) = record_accessor_info(args[1])
          if accessor_type.id != desc.record_type.id {
            raise @core.EvalError("type error: record accessor expected")
          }
          Value::ConditionProc(
            make_condition_proc(
              ConditionProcKind::Accessor(desc.record_type, idx),
            ),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::MakeEqHashtable =>
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeEqvHashtable =>
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeHashtable =>
      match args.length() {
        2 | 3 => {
          let hash = args[0]
          if !is_procedure_value(hash) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(args[1])
          if args.length() == 3 {
            validate_hashtable_size_arg(args[2])
          }
          Value::Hashtable(make_hashtable(equiv, Some(hash), true))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::HashtableP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Hashtable(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableSize =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          Value::Datum(Datum::Int(table.entries.val.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableRef =>
      raise @core.EvalError(
        "internal error: hashtable-ref should be handled in apply_proc",
      )
    Primitive::HashtableSet =>
      raise @core.EvalError(
        "internal error: hashtable-set! should be handled in apply_proc",
      )
    Primitive::HashtableDelete =>
      raise @core.EvalError(
        "internal error: hashtable-delete! should be handled in apply_proc",
      )
    Primitive::HashtableContainsP =>
      raise @core.EvalError(
        "internal error: hashtable-contains? should be handled in apply_proc",
      )
    Primitive::HashtableUpdate =>
      raise @core.EvalError(
        "internal error: hashtable-update! should be handled in apply_proc",
      )
    Primitive::HashtableCopy =>
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          let mutable = if args.length() == 2 {
            value_as_bool(args[1])
          } else {
            table.mutable
          }
          let entries : Array[HashtableEntry] = []
          for entry in table.entries.val {
            entries.push(HashtableEntry::{
              key: entry.key,
              value: Ref::new(entry.value.val),
            })
          }
          Value::Hashtable(Hashtable::{
            id: next_hashtable_id(),
            mutable,
            equiv: table.equiv,
            hash: table.hash,
            entries: Ref::new(entries),
          })
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableClear =>
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          if args.length() == 2 {
            validate_hashtable_size_arg(args[1])
          }
          table.entries.val = []
          Value::Void
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableKeys =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let items : Array[Datum] = []
          for entry in table.entries.val {
            items.push(value_to_datum_element(entry.key))
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableEntries =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let key_items : Array[Datum] = []
          let value_items : Array[Datum] = []
          for entry in table.entries.val {
            key_items.push(value_to_datum_element(entry.key))
            value_items.push(value_to_datum_element(entry.value.val))
          }
          Value::Values([
            Value::Datum(Datum::Vector(key_items)),
            Value::Datum(Datum::Vector(value_items)),
          ])
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableEquivalenceFunction =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          hashtable_equiv_to_value(table.equiv)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableHashFunction =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          match table.hash {
            Some(hash) => hash
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableMutableP =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          bool_value(table.mutable)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeEnumeration =>
      match args.length() {
        1 => {
          let names = enum_set_symbol_list(args[0])
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          Value::EnumSet(make_enum_set(names, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetUniverse =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_true(set.universe)
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetIndexer =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Indexer(set)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetConstructor =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(
            make_enum_set_proc(EnumSetProcKind::Constructor(set)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::EnumSet(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetMemberP =>
      match args.length() {
        2 => {
          let name = parse_symbol(value_as_datum(args[0]))
          let set = value_as_enum_set(args[1])
          bool_value(enum_set_member_by_name(set, name))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetSubsetP =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let mut i = 0
          while i < left.members.length() {
            if left.members[i] && !right.members[i] {
              return bool_value(false)
            }
            i = i + 1
          }
          bool_value(true)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetEq =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let mut i = 0
          while i < left.members.length() {
            if left.members[i] != right.members[i] {
              return bool_value(false)
            }
            i = i + 1
          }
          bool_value(true)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetUnion =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] || right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetIntersection =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] && right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetDifference =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] && !right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetComplement =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_false(set.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = !set.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetProjection =>
      match args.length() {
        2 => {
          let source = value_as_enum_set(args[0])
          let target = value_as_enum_set(args[1])
          let members = enum_set_members_all_false(target.universe)
          let mut i = 0
          while i < target.universe.length() {
            let name = target.universe[i]
            members[i] = enum_set_member_by_name(source, name)
            i = i + 1
          }
          Value::EnumSet(make_enum_set(target.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetToList =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let items : Array[Datum] = []
          let mut i = 0
          while i < set.universe.length() {
            if set.members[i] {
              items.push(Datum::Symbol(set.universe[i]))
            }
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharEq =>
      bool_value(compare_chain_char(args, CompareMode::Eq, false))
    Primitive::CharLess =>
      bool_value(compare_chain_char(args, CompareMode::Lt, false))
    Primitive::CharGreater =>
      bool_value(compare_chain_char(args, CompareMode::Gt, false))
    Primitive::CharLessEq =>
      bool_value(compare_chain_char(args, CompareMode::Le, false))
    Primitive::CharGreaterEq =>
      bool_value(compare_chain_char(args, CompareMode::Ge, false))
    Primitive::CharCiEq =>
      bool_value(compare_chain_char(args, CompareMode::Eq, true))
    Primitive::CharCiLess =>
      bool_value(compare_chain_char(args, CompareMode::Lt, true))
    Primitive::CharCiGreater =>
      bool_value(compare_chain_char(args, CompareMode::Gt, true))
    Primitive::CharCiLessEq =>
      bool_value(compare_chain_char(args, CompareMode::Le, true))
    Primitive::CharCiGreaterEq =>
      bool_value(compare_chain_char(args, CompareMode::Ge, true))
    Primitive::CharP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Char(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharToInteger =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Int(ch.to_int()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::IntegerToChar =>
      match args.length() {
        1 => {
          let ch = char_from_int_value(args[0])
          Value::Datum(Datum::Char(ch))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharAlphabeticP =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(unicode_char_is_alphabetic(ch))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharNumericP =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_numeric())
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharWhitespaceP =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_whitespace())
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharUpperCaseP =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(unicode_char_is_uppercase(ch))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharLowerCaseP =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(unicode_char_is_lowercase(ch))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharUpcase =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(unicode_char_upcase(ch)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharDowncase =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(unicode_char_downcase(ch)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharFoldcase =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(unicode_char_foldcase(ch)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharGeneralCategory =>
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Symbol(unicode_char_general_category(ch)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringEq =>
      bool_value(compare_chain_string(args, CompareMode::Eq, false))
    Primitive::StringLess =>
      bool_value(compare_chain_string(args, CompareMode::Lt, false))
    Primitive::StringGreater =>
      bool_value(compare_chain_string(args, CompareMode::Gt, false))
    Primitive::StringLessEq =>
      bool_value(compare_chain_string(args, CompareMode::Le, false))
    Primitive::StringGreaterEq =>
      bool_value(compare_chain_string(args, CompareMode::Ge, false))
    Primitive::StringCiEq =>
      bool_value(compare_chain_string(args, CompareMode::Eq, true))
    Primitive::StringCiLess =>
      bool_value(compare_chain_string(args, CompareMode::Lt, true))
    Primitive::StringCiGreater =>
      bool_value(compare_chain_string(args, CompareMode::Gt, true))
    Primitive::StringCiLessEq =>
      bool_value(compare_chain_string(args, CompareMode::Le, true))
    Primitive::StringCiGreaterEq =>
      bool_value(compare_chain_string(args, CompareMode::Ge, true))
    Primitive::String => {
      let buf = @buffer.new()
      for arg in args {
        let ch = value_as_char(arg)
        buf.write_char(ch)
      }
      Value::Datum(Datum::String(Ref::new(buf.to_string())))
    }
    Primitive::MakeString =>
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_as_char(args[1])
          } else {
            let zero = 0
            match zero.to_char() {
              Some(ch) => ch
              None =>
                raise @core.EvalError("internal error: invalid default char")
            }
          }
          let buf = @buffer.new()
          let mut i = 0
          while i < len {
            buf.write_char(fill)
            i = i + 1
          }
          Value::Datum(Datum::String(Ref::new(buf.to_string())))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::StringP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::String(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringLength =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::Int(s.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringAppend => {
      let buf = @buffer.new()
      for arg in args {
        buf.write_string(value_as_string(arg))
      }
      Value::Datum(Datum::String(Ref::new(buf.to_string())))
    }
    Primitive::StringRef =>
      match args.length() {
        2 => {
          let s = value_as_string(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, s.length())
          match s.get_char(idx) {
            Some(ch) => Value::Datum(Datum::Char(ch))
            None => raise @core.EvalError("index out of range")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::StringSet =>
      match args.length() {
        3 => {
          let sref = value_as_string_ref(args[0])
          let idx = value_as_int_index(args[1])
          let ch = value_as_char(args[2])
          let updated = string_replace_range(sref.val, idx, idx + 1, ch)
          sref.val = updated
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::StringCopy =>
      match args.length() {
        1 | 2 | 3 => {
          let s = value_as_string(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            s.length()
          }
          if start < 0 || end < start || end > s.length() {
            raise @core.EvalError("index out of range")
          }
          let view = try? s[start:end]
          match view {
            Ok(slice) =>
              Value::Datum(Datum::String(Ref::new(slice.to_string())))
            Err(_) => raise @core.EvalError("index out of range")
          }
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::Substring =>
      match args.length() {
        3 => {
          let s = value_as_string(args[0])
          let start = value_as_int_index(args[1])
          let end = value_as_int_index(args[2])
          if start < 0 || end < start || end > s.length() {
            raise @core.EvalError("index out of range")
          }
          let view = try? s[start:end]
          match view {
            Ok(slice) =>
              Value::Datum(Datum::String(Ref::new(slice.to_string())))
            Err(_) => raise @core.EvalError("index out of range")
          }
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::StringCopyBang =>
      match args.length() {
        3 | 4 | 5 => {
          let to_ref = value_as_string_ref(args[0])
          let at = value_as_int_index(args[1])
          let from_str = value_as_string(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_str.length()
          }
          if start < 0 || end < start || end > from_str.length() || at < 0 {
            raise @core.EvalError("index out of range")
          }
          let count = end - start
          let to_str = to_ref.val
          let to_len = to_str.length()
          if at + count > to_len {
            raise @core.EvalError("index out of range")
          }
          let buf = @buffer.new()
          let mut i = 0
          while i < to_len {
            match to_str.get_char(i) {
              Some(ch) =>
                if i >= at && i < at + count {
                  let from_idx = start + (i - at)
                  match from_str.get_char(from_idx) {
                    Some(fch) => buf.write_char(fch)
                    None => raise @core.EvalError("index out of range")
                  }
                } else {
                  buf.write_char(ch)
                }
              None => raise @core.EvalError("index out of range")
            }
            i = i + 1
          }
          to_ref.val = buf.to_string()
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    Primitive::StringFill =>
      match args.length() {
        2 | 3 | 4 => {
          let sref = value_as_string_ref(args[0])
          let fill = value_as_char(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            sref.val.length()
          }
          let updated = string_replace_range(sref.val, start, end, fill)
          sref.val = updated
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    Primitive::StringToList =>
      match args.length() {
        1 | 2 | 3 => {
          let s = value_as_string(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            s.length()
          }
          if start < 0 || end < start || end > s.length() {
            raise @core.EvalError("index out of range")
          }
          let items : Array[Datum] = []
          let mut i = start
          while i < end {
            match s.get_char(i) {
              Some(ch) => items.push(Datum::Char(ch))
              None => raise @core.EvalError("index out of range")
            }
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ListToString =>
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let buf = @buffer.new()
          for item in items {
            match item {
              Datum::Char(ch) => buf.write_char(ch)
              _ => raise @core.EvalError("type error: char expected")
            }
          }
          Value::Datum(Datum::String(Ref::new(buf.to_string())))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringUpcase =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(unicode_string_upcase(s))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringDowncase =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(unicode_string_downcase(s))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringFoldcase =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(unicode_string_foldcase(s))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfc =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(unicode_string_normalize_nfc(s))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfd =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(unicode_string_normalize_nfd(s))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfkc =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(
            Datum::String(Ref::new(unicode_string_normalize_nfkc(s))),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfkd =>
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(
            Datum::String(Ref::new(unicode_string_normalize_nfkd(s))),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Vector => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_to_datum_element(arg))
      }
      Value::Datum(Datum::Vector(items))
    }
    Primitive::MakeVector =>
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_to_datum_element(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::VectorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::Vector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::VectorLength =>
      match args.length() {
        1 => {
          let items = value_as_vector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::VectorRef =>
      match args.length() {
        2 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          value_from_datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::VectorSet =>
      match args.length() {
        3 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_to_datum_element(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::VectorFill =>
      match args.length() {
        2 | 3 | 4 => {
          let items = value_as_vector(args[0])
          let fill = value_to_datum_element(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          let mut i = start
          while i < end {
            items[i] = fill
            i = i + 1
          }
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    Primitive::VectorCopy =>
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_vector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          let slice = items.sub(start~, end~).to_array()
          Value::Datum(Datum::Vector(slice))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::VectorCopyBang =>
      match args.length() {
        3 | 4 | 5 => {
          let to_items = value_as_vector(args[0])
          let at = value_as_int_index(args[1])
          let from_items = value_as_vector(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_items.length()
          }
          if start < 0 || end < start || end > from_items.length() || at < 0 {
            raise @core.EvalError("index out of range")
          }
          let count = end - start
          if at + count > to_items.length() {
            raise @core.EvalError("index out of range")
          }
          let temp : Array[Datum] = []
          let mut i = 0
          while i < count {
            temp.push(from_items[start + i])
            i = i + 1
          }
          let mut j = 0
          while j < count {
            to_items[at + j] = temp[j]
            j = j + 1
          }
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    Primitive::VectorAppend => {
      let items : Array[Datum] = []
      for arg in args {
        let vec = value_as_vector(arg)
        for item in vec {
          items.push(item)
        }
      }
      Value::Datum(Datum::Vector(items))
    }
    Primitive::VectorToList =>
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_vector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          Value::Datum(list_from_array(items.sub(start~, end~).to_array()))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ListToVector =>
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ByteVector => {
      let items : Array[Int] = []
      for arg in args {
        items.push(value_as_byte(arg))
      }
      Value::Datum(Datum::ByteVector(items))
    }
    Primitive::MakeByteVector =>
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 { value_as_byte(args[1]) } else { 0 }
          let items : Array[Int] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(Datum::ByteVector(items))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::ByteVectorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(Datum::ByteVector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ByteVectorLength =>
      match args.length() {
        1 => {
          let items = value_as_bytevector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ByteVectorEq =>
      match args.length() {
        2 => {
          let left = value_as_bytevector(args[0])
          let right = value_as_bytevector(args[1])
          if left.length() != right.length() {
            bool_value(false)
          } else {
            let mut i = 0
            while i < left.length() {
              if left[i] != right[i] {
                return bool_value(false)
              }
              i = i + 1
            }
            bool_value(true)
          }
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::ByteVectorU8Ref =>
      match args.length() {
        2 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          Value::Datum(Datum::Int(items[idx]))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::ByteVectorU8Set =>
      match args.length() {
        3 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_as_byte(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::ByteVectorCopy =>
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_bytevector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          let slice = items.sub(start~, end~).to_array()
          Value::Datum(Datum::ByteVector(slice))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ByteVectorCopyBang =>
      match args.length() {
        3 | 4 | 5 => {
          let to_items = value_as_bytevector(args[0])
          let at = value_as_int_index(args[1])
          let from_items = value_as_bytevector(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_items.length()
          }
          if start < 0 || end < start || end > from_items.length() || at < 0 {
            raise @core.EvalError("index out of range")
          }
          let count = end - start
          if at + count > to_items.length() {
            raise @core.EvalError("index out of range")
          }
          let temp : Array[Int] = []
          let mut i = 0
          while i < count {
            temp.push(from_items[start + i])
            i = i + 1
          }
          let mut j = 0
          while j < count {
            to_items[at + j] = temp[j]
            j = j + 1
          }
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    Primitive::ByteVectorAppend => {
      let items : Array[Int] = []
      for arg in args {
        let vec = value_as_bytevector(arg)
        for item in vec {
          items.push(item)
        }
      }
      Value::Datum(Datum::ByteVector(items))
    }
    Primitive::ByteVectorFill =>
      match args.length() {
        2 | 3 | 4 => {
          let items = value_as_bytevector(args[0])
          let fill = value_as_byte(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          let mut i = start
          while i < end {
            items[i] = fill
            i = i + 1
          }
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    Primitive::ByteVectorToU8List =>
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_bytevector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise @core.EvalError("index out of range")
          }
          let datums : Array[Datum] = []
          let mut i = start
          while i < end {
            datums.push(Datum::Int(items[i]))
            i = i + 1
          }
          Value::Datum(list_from_array(datums))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::U8ListToByteVector =>
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let bytes : Array[Int] = []
          for item in items {
            match item {
              Datum::Int(n) => {
                if n < 0 || n > 255 {
                  raise @core.EvalError("type error: byte expected")
                }
                bytes.push(n)
              }
              _ => raise @core.EvalError("type error: byte expected")
            }
          }
          Value::Datum(Datum::ByteVector(bytes))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringToUtf8 =>
      match args.length() {
        1 | 2 | 3 => {
          let s = value_as_string(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            string_char_length(s)
          }
          let bytes = string_to_utf8_bytes(s, start, end)
          Value::Datum(Datum::ByteVector(bytes))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::Utf8ToString =>
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_bytevector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          let text = utf8_decode_to_string(items, start, end)
          Value::Datum(Datum::String(Ref::new(text)))
        }
        n => raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::NativeEndianness =>
      match args.length() {
        0 => native_endianness_value()
        n => raise arity_mismatch(0, n)
      }
    Primitive::ByteVectorUintRef =>
      match args.length() {
        4 => {
          let items = value_as_bytevector(args[0])
          let start = value_as_int_index(args[1])
          let endian = endianness_from_value(args[2])
          let size = bytevector_size_from_value(args[3])
          check_bytevector_range(start, size, items.length())
          let value = bytevector_uint_ref(items, start, size, endian)
          Value::Datum(bigint_to_integer_datum(value))
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::ByteVectorSintRef =>
      match args.length() {
        4 => {
          let items = value_as_bytevector(args[0])
          let start = value_as_int_index(args[1])
          let endian = endianness_from_value(args[2])
          let size = bytevector_size_from_value(args[3])
          check_bytevector_range(start, size, items.length())
          let value = bytevector_sint_ref(items, start, size, endian)
          Value::Datum(bigint_to_integer_datum(value))
        }
        n => raise arity_mismatch(4, n)
      }
    Primitive::ByteVectorUintSet =>
      match args.length() {
        5 => {
          let items = value_as_bytevector(args[0])
          let start = value_as_int_index(args[1])
          let endian = endianness_from_value(args[2])
          let size = bytevector_size_from_value(args[3])
          check_bytevector_range(start, size, items.length())
          let value = datum_to_bigint(value_as_exact_integer(args[4]))
          bytevector_uint_set(items, start, size, endian, value)
          Value::Void
        }
        n => raise arity_mismatch(5, n)
      }
    Primitive::ByteVectorSintSet =>
      match args.length() {
        5 => {
          let items = value_as_bytevector(args[0])
          let start = value_as_int_index(args[1])
          let endian = endianness_from_value(args[2])
          let size = bytevector_size_from_value(args[3])
          check_bytevector_range(start, size, items.length())
          let value = datum_to_bigint(value_as_exact_integer(args[4]))
          let unsigned = bytevector_sint_to_uint(value, size)
          bytevector_uint_set(items, start, size, endian, unsigned)
          Value::Void
        }
        n => raise arity_mismatch(5, n)
      }
    Primitive::Display =>
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, display_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, display_string(args[0]))
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::Write =>
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::Newline =>
      match args.length() {
        0 => {
          let port = get_current_output_port()
          port_write(port, "\n")
          Value::Void
        }
        1 => {
          let port = value_as_port(args[0])
          port_write(port, "\n")
          Value::Void
        }
        n => raise @core.EvalError("arity mismatch: expected 0 or 1 got \{n}")
      }
    Primitive::OpenOutputString =>
      match args.length() {
        0 => Value::Port(new_output_string_port())
        n => raise arity_mismatch(0, n)
      }
    Primitive::GetOutputString =>
      match args.length() {
        1 => {
          let port = value_as_port(args[0])
          Value::Datum(Datum::String(Ref::new(port_get_output_string(port))))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CurrentOutputPort =>
      match args.length() {
        0 => Value::Port(get_current_output_port())
        n => raise arity_mismatch(0, n)
      }
    Primitive::WithExceptionHandler =>
      raise @core.EvalError(
        "internal error: with-exception-handler should be handled in apply_proc",
      )
    Primitive::Raise =>
      raise @core.EvalError(
        "internal error: raise should be handled in apply_proc",
      )
    Primitive::RaiseContinuable =>
      raise @core.EvalError(
        "internal error: raise-continuable should be handled in apply_proc",
      )
    Primitive::Error =>
      raise @core.EvalError(
        "internal error: error should be handled in apply_proc",
      )
    Primitive::AssertionViolation =>
      raise @core.EvalError(
        "internal error: assertion-violation should be handled in apply_proc",
      )
    Primitive::ImplementationRestrictionViolation =>
      raise @core.EvalError(
        "internal error: implementation-restriction-violation should be handled in apply_proc",
      )
    Primitive::UndefinedViolation =>
      raise @core.EvalError(
        "internal error: undefined-violation should be handled in apply_proc",
      )
    Primitive::SyntaxViolation =>
      raise @core.EvalError(
        "internal error: syntax-violation should be handled in apply_proc",
      )
    Primitive::Not =>
      match args.length() {
        1 => bool_value(is_false(args[0]))
        n => raise arity_mismatch(1, n)
      }
    Primitive::Apply =>
      // handled by apply_proc to allow proper argument flattening
      raise @core.EvalError(
        "internal error: apply should be handled in apply_proc",
      )
    Primitive::CallCC =>
      raise @core.EvalError(
        "internal error: call/cc should be handled in apply_proc",
      )
    Primitive::Map =>
      raise @core.EvalError(
        "internal error: map should be handled in apply_proc",
      )
    Primitive::ForEach =>
      raise @core.EvalError(
        "internal error: for-each should be handled in apply_proc",
      )
    Primitive::VectorMap =>
      raise @core.EvalError(
        "internal error: vector-map should be handled in apply_proc",
      )
    Primitive::VectorForEach =>
      raise @core.EvalError(
        "internal error: vector-for-each should be handled in apply_proc",
      )
    Primitive::StringMap =>
      raise @core.EvalError(
        "internal error: string-map should be handled in apply_proc",
      )
    Primitive::StringForEach =>
      raise @core.EvalError(
        "internal error: string-for-each should be handled in apply_proc",
      )
    Primitive::Values => Value::Values(args)
    Primitive::MakeVariableTransformer =>
      match args.length() {
        1 => {
          if !is_procedure_value(args[0]) {
            raise @core.EvalError("type error: procedure expected")
          }
          args[0]
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::GenerateTemporaries =>
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let results : Array[Datum] = []
          for item in items {
            let (name, scopes, binding_id) = match datum_unlabel(item) {
              Datum::Symbol(name) => (name, [], None)
              Datum::Value(Value::SyntaxObject(obj)) =>
                match datum_unlabel(obj.datum) {
                  Datum::Symbol(name) => {
                    let copied : Array[Int] = []
                    for scope in obj.scopes {
                      copied.push(scope)
                    }
                    (name, copied, obj.binding_id)
                  }
                  _ => raise @core.EvalError("type error: identifier expected")
                }
              _ => raise @core.EvalError("type error: identifier expected")
            }
            let datum = Datum::Symbol(gensym(name))
            let temp = Value::SyntaxObject(SyntaxObject::{
              datum,
              scopes,
              binding_id,
            })
            results.push(value_to_datum_element(temp))
          }
          Value::Datum(list_from_array(results))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::CallWithValues =>
      raise @core.EvalError(
        "internal error: call-with-values should be handled in apply_proc",
      )
    Primitive::MakeParameter =>
      raise @core.EvalError(
        "internal error: make-parameter should be handled in apply_proc",
      )
    Primitive::DynamicWind =>
      raise @core.EvalError(
        "internal error: dynamic-wind should be handled in apply_proc",
      )
    Primitive::Eval =>
      raise @core.EvalError(
        "internal error: eval should be handled in apply_proc",
      )
    Primitive::Environment =>
      raise @core.EvalError(
        "internal error: environment should be handled in apply_proc",
      )
    Primitive::PromiseP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Promise(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakePromise =>
      raise @core.EvalError(
        "internal error: make-promise should be handled in apply_proc",
      )
    Primitive::Force =>
      raise @core.EvalError(
        "internal error: force should be handled in apply_proc",
      )
    Primitive::ListP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Datum(d) => bool_value(is_proper_list(d))
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Length =>
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise @core.EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Append =>
      match args.length() {
        0 => Value::Datum(Datum::Nil)
        1 => args[0]
        _ => {
          let items : Array[Datum] = []
          let mut i = 0
          while i < args.length() - 1 {
            let datum = match args[i] {
              Value::Datum(d) => d
              _ => raise @core.EvalError("type error: list expected")
            }
            let part = datum_list_to_array(datum)
            for item in part {
              items.push(item)
            }
            i = i + 1
          }
          let tail = value_to_datum_element(args[args.length() - 1])
          Value::Datum(list_with_tail(items, tail))
        }
      }
    Primitive::Reverse =>
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise @core.EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          let mut result = Datum::Nil
          for item in items {
            result = pair_new(item, result)
          }
          Value::Datum(result)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ListRef =>
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          check_index(idx, items.length())
          value_from_datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::ListTail =>
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          if idx < 0 || idx > items.length() {
            raise @core.EvalError("index out of range")
          }
          Value::Datum(list_from_array(items.sub(start=idx).to_array()))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::ListCopy =>
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::Member =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Equal, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Memq =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Memv =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assoc =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Equal, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assq =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assv =>
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
  }
}

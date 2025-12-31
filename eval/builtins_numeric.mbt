///|
/// Apply numeric primitives.
fn apply_numeric_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::Add
    | Primitive::Sub
    | Primitive::Mul
    | Primitive::Div
    | Primitive::NumEq
    | Primitive::Less
    | Primitive::Greater
    | Primitive::LessEq
    | Primitive::GreaterEq
    | Primitive::ExactToInexact
    | Primitive::InexactToExact
    | Primitive::ExactIntegerSqrt
    | Primitive::Rationalize
    | Primitive::NumberToString
    | Primitive::StringToNumber
    | Primitive::MakeRectangular
    | Primitive::MakePolar
    | Primitive::RealPart
    | Primitive::ImagPart
    | Primitive::Magnitude
    | Primitive::Angle
    | Primitive::Sqrt
    | Primitive::Exp
    | Primitive::Log
    | Primitive::Expt
    | Primitive::Sin
    | Primitive::Cos
    | Primitive::Tan
    | Primitive::Asin
    | Primitive::Acos
    | Primitive::Atan
    | Primitive::Numerator
    | Primitive::Denominator
    | Primitive::Abs
    | Primitive::Quotient
    | Primitive::Remainder
    | Primitive::Modulo
    | Primitive::Gcd
    | Primitive::Lcm
    | Primitive::Max
    | Primitive::Min
    | Primitive::Floor
    | Primitive::Ceiling
    | Primitive::Truncate
    | Primitive::Round
    | Primitive::BitwiseAnd
    | Primitive::BitwiseIor
    | Primitive::BitwiseXor
    | Primitive::BitwiseNot
    | Primitive::BitwiseIf
    | Primitive::ArithmeticShift
    | Primitive::BitwiseBitCount
    | Primitive::BitwiseLength
    | Primitive::BitwiseFirstBitSet
    | Primitive::BitwiseBitSetP
    | Primitive::BitwiseCopyBit
    | Primitive::BitwiseBitField
    | Primitive::BitwiseCopyBitField
    | Primitive::BitwiseRotateBitField
    | Primitive::BitwiseReverseBitField
    | Primitive::FixnumP
    | Primitive::FixnumWidth
    | Primitive::LeastFixnum
    | Primitive::GreatestFixnum
    | Primitive::FxEq
    | Primitive::FxLess
    | Primitive::FxGreater
    | Primitive::FxLessEq
    | Primitive::FxGreaterEq
    | Primitive::FxZeroP
    | Primitive::FxPositiveP
    | Primitive::FxNegativeP
    | Primitive::FxOddP
    | Primitive::FxEvenP
    | Primitive::FxMin
    | Primitive::FxMax
    | Primitive::FxAdd
    | Primitive::FxSub
    | Primitive::FxMul
    | Primitive::FxDiv
    | Primitive::FxMod
    | Primitive::FxDiv0
    | Primitive::FxMod0
    | Primitive::FxAddCarry
    | Primitive::FxSubCarry
    | Primitive::FxMulCarry
    | Primitive::FxNot
    | Primitive::FxAnd
    | Primitive::FxIor
    | Primitive::FxXor
    | Primitive::FxIf
    | Primitive::FxBitCount
    | Primitive::FxLength
    | Primitive::FxFirstBitSet
    | Primitive::FxBitSetP
    | Primitive::FxCopyBit
    | Primitive::FxBitField
    | Primitive::FxCopyBitField
    | Primitive::FxRotateBitField
    | Primitive::FxReverseBitField
    | Primitive::FxArithmeticShift
    | Primitive::FxArithmeticShiftLeft
    | Primitive::FxArithmeticShiftRight
    | Primitive::FlonumP
    | Primitive::RealToFlonum
    | Primitive::FixnumToFlonum
    | Primitive::FlEq
    | Primitive::FlLess
    | Primitive::FlGreater
    | Primitive::FlLessEq
    | Primitive::FlGreaterEq
    | Primitive::FlIntegerP
    | Primitive::FlZeroP
    | Primitive::FlPositiveP
    | Primitive::FlNegativeP
    | Primitive::FlOddP
    | Primitive::FlEvenP
    | Primitive::FlFiniteP
    | Primitive::FlInfiniteP
    | Primitive::FlNanP
    | Primitive::FlMax
    | Primitive::FlMin
    | Primitive::FlAdd
    | Primitive::FlMul
    | Primitive::FlSub
    | Primitive::FlDiv
    | Primitive::FlAbs
    | Primitive::FlDivAndMod
    | Primitive::FlDivInt
    | Primitive::FlMod
    | Primitive::FlDiv0AndMod0
    | Primitive::FlDiv0
    | Primitive::FlMod0
    | Primitive::FlNumerator
    | Primitive::FlDenominator
    | Primitive::FlFloor
    | Primitive::FlCeiling
    | Primitive::FlTruncate
    | Primitive::FlRound
    | Primitive::FlExp
    | Primitive::FlLog
    | Primitive::FlSin
    | Primitive::FlCos
    | Primitive::FlTan
    | Primitive::FlAsin
    | Primitive::FlAcos
    | Primitive::FlAtan
    | Primitive::FlSqrt
    | Primitive::FlExpt
    => Some(apply_numeric_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply numeric primitives (core match).
fn apply_numeric_primitive_core(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
    Primitive::Add => {
      let acc = for i = 0, acc = Datum::Int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_add(acc, value_as_number(args[i]))
      } else {
        acc
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
          let result = for i = 1, result = value_as_number(args[0]);
            i < args.length(); {
              // invariant : i >= 1 && i <= args.length()
              // decreases : args.length() - i
              // assert : i <= args.length()
              continue i + 1, num_sub(result, value_as_number(args[i]))
            } else {
              result
            }
          Value::Datum(result)
        }
      }
    Primitive::Mul => {
      let product = for i = 0, product = Datum::Int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_mul(product, value_as_number(args[i]))
      } else {
        product
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
          let result = for i = 1, result = value_as_number(args[0]);
            i < args.length(); {
              // invariant : i >= 1 && i <= args.length()
              // decreases : args.length() - i
              // assert : i <= args.length()
              continue i + 1, num_div(result, value_as_number(args[i]))
            } else {
              result
            }
          Value::Datum(result)
        }
      }
    Primitive::NumEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_number(args[0])
        let ok = for i = 1; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          if !num_equal(value_as_number(args[i]), first) {
            break false
          }
          continue i + 1
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::Less =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_number(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          if !num_less(prev, cur) {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::Greater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_number(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          if !num_greater(prev, cur) {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::LessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_number(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          if !num_less_eq(prev, cur) {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::GreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_number(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          if !num_greater_eq(prev, cur) {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::ExactToInexact =>
      match args {
        [Value::Datum(Datum::Float(f))] => Value::Datum(Datum::Float(f))
        [Value::Datum(Datum::Complex(real, imag))] => {
          let real_f = number_to_float(real.val)
          let imag_f = number_to_float(imag.val)
          Value::Datum(
            make_complex(Datum::Float(real_f), Datum::Float(imag_f)),
          )
        }
        [value] => {
          let value = value_as_number(value)
          Value::Datum(Datum::Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::InexactToExact =>
      match args {
        [Value::Datum(Datum::Float(f))] =>
          Value::Datum(float_to_exact_datum(f))
        [Value::Datum(Datum::Complex(real, imag))] => {
          let real_exact = inexact_component_to_exact(real.val)
          let imag_exact = inexact_component_to_exact(imag.val)
          Value::Datum(make_complex(real_exact, imag_exact))
        }
        [Value::Datum(Datum::Int(n))] => Value::Datum(Datum::Int(n))
        [Value::Datum(Datum::BigInt(n))] => Value::Datum(Datum::BigInt(n))
        [Value::Datum(Datum::Rat(n, d))] => Value::Datum(Datum::Rat(n, d))
        [Value::Datum(Datum::BigRat(n, d))] =>
          Value::Datum(Datum::BigRat(n, d))
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ExactIntegerSqrt =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Rationalize =>
      match args {
        [x_value, tol_value] => {
          let x = value_as_number(x_value)
          let tol_value = value_as_number(tol_value)
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
        _ => raise arity_mismatch(2, args.length())
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
          let (real_value, is_real) = match datum_unlabel(value) {
            Datum::Complex(real, imag) =>
              if datum_is_zero(imag.val) {
                (real.val, true)
              } else {
                (value, false)
              }
            _ => (value, true)
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
                  let pow = @math.powf(abs_base, exp_f)
                  let pow = if exp_sign < 0 { Float::from_int(1) / pow } else { pow }
                  let pow = if f < 0.0 && exp_is_odd { -pow } else { pow }
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
      // invariant : i >= 0 && i <= args.length()
      // decreases : args.length() - i
      // assert : use_big || acc_big == bigint_from_int(0)
      let (use_big, acc_int, acc_big) =
        for i = 0,
          use_big = false,
          acc_int = 0,
          acc_big = bigint_from_int(0);
          i < args.length(); {
          let value = value_as_exact_integer(args[i])
          match value {
            Datum::Int(n) =>
              if use_big {
                let next_acc_big = bigint_gcd(acc_big, bigint_from_int(n))
                continue i + 1, true, acc_int, next_acc_big
              } else {
                let next_acc_int = gcd(acc_int, n)
                continue i + 1, false, next_acc_int, acc_big
              }
            Datum::BigInt(n) => {
              let base_big = if use_big { acc_big } else { bigint_from_int(acc_int) }
              let next_acc_big = bigint_gcd(base_big, n)
              continue i + 1, true, acc_int, next_acc_big
            }
            _ => raise @core.EvalError("type error: integer expected")
          }
        } else {
          (use_big, acc_int, acc_big)
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
      // invariant : i >= 0 && i <= args.length()
      // decreases : args.length() - i
      // assert : use_big || acc_big == bigint_from_int(1)
      let (use_big, acc_int, acc_big) =
        for i = 0,
          use_big = false,
          acc_int = 1,
          acc_big = bigint_from_int(1);
          i < args.length(); {
          let value = value_as_exact_integer(args[i])
          match value {
            Datum::Int(n) =>
              if use_big {
                let n_big = bigint_from_int(n)
                let next_acc_big =
                  if acc_big.is_zero() || n_big.is_zero() {
                    bigint_from_int(0)
                  } else {
                    let g = bigint_gcd(acc_big, n_big)
                    bigint_abs(acc_big / g * n_big)
                  }
                continue i + 1, true, acc_int, next_acc_big
              } else if acc_int == 0 || n == 0 {
                continue i + 1, false, 0, acc_big
              } else {
                let g = gcd(acc_int, n)
                let next_acc_int = int_abs(acc_int / g * n)
                continue i + 1, false, next_acc_int, acc_big
              }
            Datum::BigInt(n) => {
              let base_big = if use_big { acc_big } else { bigint_from_int(acc_int) }
              let next_acc_big =
                if base_big.is_zero() || n.is_zero() {
                  bigint_from_int(0)
                } else {
                  let g = bigint_gcd(base_big, n)
                  bigint_abs(base_big / g * n)
                }
              continue i + 1, true, acc_int, next_acc_big
            }
            _ => raise @core.EvalError("type error: integer expected")
          }
        } else {
          (use_big, acc_int, acc_big)
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
      let best = value_as_number(args[0])
      let has_inexact = best is Datum::Float(_)
      let (best, has_inexact) =
        for i = 1, best = best, has_inexact = has_inexact; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          let next_best = if num_less(best, cur) { cur } else { best }
          let next_has_inexact = has_inexact || cur is Datum::Float(_)
          continue i + 1, next_best, next_has_inexact
        } else {
          (best, has_inexact)
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
      let best = value_as_number(args[0])
      let has_inexact = best is Datum::Float(_)
      let (best, has_inexact) =
        for i = 1, best = best, has_inexact = has_inexact; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          let next_best = if num_greater(best, cur) { cur } else { best }
          let next_has_inexact = has_inexact || cur is Datum::Float(_)
          continue i + 1, next_best, next_has_inexact
        } else {
          (best, has_inexact)
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
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::And)
        } else {
          acc
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseIor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::Or)
        } else {
          acc
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseXor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::Xor)
        } else {
          acc
        }
        Value::Datum(acc)
      }
    Primitive::BitwiseNot =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          Value::Datum(datum_bitwise_not(value))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::BitwiseIf =>
      match args {
        [mask_value, yes_value, no_value] => {
          let mask = value_as_exact_integer(mask_value)
          let yes_value = value_as_exact_integer(yes_value)
          let no_value = value_as_exact_integer(no_value)
          let masked_yes = datum_bitwise_binop(mask, yes_value, BitOp::And)
          let masked_no = datum_bitwise_binop(
            datum_bitwise_not(mask),
            no_value,
            BitOp::And,
          )
          Value::Datum(datum_bitwise_binop(masked_yes, masked_no, BitOp::Or))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::ArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_exact_integer(value)
          let shift = value_as_int_index(shift_value)
          Value::Datum(datum_arithmetic_shift(value, shift))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::BitwiseBitCount =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_bit_count(value)
          Value::Datum(Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::BitwiseLength =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_length(value)
          Value::Datum(Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::BitwiseFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Value::Datum(Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitOp::And)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Value::Datum(Datum::Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::BitwiseBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_exact_integer(value)
          let index = value_as_nonnegative_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::BitwiseCopyBit =>
      match args {
        [value, index_value, bit_value] => {
          let value = value_as_exact_integer(value)
          let index = value_as_nonnegative_int(index_value)
          let bit_value = value_as_exact_integer(bit_value)
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
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::BitwiseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          Value::Datum(datum_bitwise_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::BitwiseCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let from = value_as_exact_integer(from_value)
          Value::Datum(datum_bitwise_copy_bit_field(value, start, end, from))
        }
        _ => raise arity_mismatch(4, args.length())
      }
    Primitive::BitwiseRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let count = value_as_int_index(count_value)
          Value::Datum(datum_bitwise_rotate_bit_field(value, start, end, count))
        }
        _ => raise arity_mismatch(4, args.length())
      }
    Primitive::BitwiseReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          Value::Datum(datum_bitwise_reverse_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::FixnumP =>
      match args {
        [value] => bool_value(value_is_fixnum(value))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FixnumWidth =>
      match args {
        [] => Value::Datum(Datum::Int(fixnum_width_value()))
        _ => raise arity_mismatch(0, args.length())
      }
    Primitive::LeastFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          Value::Datum(fixnum_from_bigint(-half))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    Primitive::GreatestFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          Value::Datum(fixnum_from_bigint(half - bigint_from_int(1)))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    Primitive::FxEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_fixnum_int(args[0])
        let ok = for i = 1; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          if value_as_fixnum_int(args[i]) != first {
            break false
          }
          continue i + 1
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FxLess =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_fixnum_int(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_fixnum_int(args[i])
          if prev >= cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FxGreater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_fixnum_int(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_fixnum_int(args[i])
          if prev <= cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FxLessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_fixnum_int(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_fixnum_int(args[i])
          if prev > cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FxGreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_fixnum_int(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_fixnum_int(args[i])
          if prev < cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FxZeroP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxPositiveP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) > 0)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxNegativeP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) < 0)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxOddP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 != 0)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxEvenP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxMin => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let current = value_as_fixnum_int(args[0])
      let current = for i = 1, current = current; i < args.length(); {
        // invariant : i >= 1 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let cur = value_as_fixnum_int(args[i])
        let next = if cur < current { cur } else { current }
        continue i + 1, next
      } else {
        current
      }
      Value::Datum(Datum::Int(current))
    }
    Primitive::FxMax => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let current = value_as_fixnum_int(args[0])
      let current = for i = 1, current = current; i < args.length(); {
        // invariant : i >= 1 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let cur = value_as_fixnum_int(args[i])
        let next = if cur > current { cur } else { current }
        continue i + 1, next
      } else {
        current
      }
      Value::Datum(Datum::Int(current))
    }
    Primitive::FxAdd => {
      let acc = for i = 0, acc = bigint_from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc + value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
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
          let result = for i = 1, result = value_as_fixnum_bigint(args[0]);
            i < args.length(); {
              continue i + 1, result - value_as_fixnum_bigint(args[i])
            } else {
              result
            }
          Value::Datum(fixnum_from_bigint(result))
        }
      }
    Primitive::FxMul => {
      let acc = for i = 0, acc = bigint_from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc * value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
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
          let q = left / right
          let r = left % right
          let q = if !bigint_is_zero(r) {
            let r_sign = r.compare_int(0)
            let right_sign = right.compare_int(0)
            if (r_sign > 0 && right_sign < 0) || (r_sign < 0 && right_sign > 0) {
              q - bigint_from_int(1)
            } else {
              q
            }
          } else {
            q
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
          let r = left % right
          let r = if !bigint_is_zero(r) {
            let r_sign = r.compare_int(0)
            let right_sign = right.compare_int(0)
            if (r_sign > 0 && right_sign < 0) || (r_sign < 0 && right_sign > 0) {
              r + right
            } else {
              r
            }
          } else {
            r
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
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::And)
        } else {
          acc
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxIor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::Or)
        } else {
          acc
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxXor =>
      if args.is_empty() {
        Value::Datum(Datum::Int(0))
      } else {
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOp::Xor)
        } else {
          acc
        }
        Value::Datum(fixnum_from_datum(acc))
      }
    Primitive::FxIf =>
      match args {
        [mask_value, yes_value, no_value] => {
          let mask = value_as_fixnum_datum(mask_value)
          let yes_value = value_as_fixnum_datum(yes_value)
          let no_value = value_as_fixnum_datum(no_value)
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
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::FxBitCount =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_bit_count(value)
          Value::Datum(Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxLength =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_length(value)
          Value::Datum(Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Value::Datum(Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitOp::And)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Value::Datum(Datum::Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FxBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_fixnum_datum(value)
          let index = value_as_nonnegative_fixnum_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::FxCopyBit =>
      match args {
        [value, index_value, bit_value] => {
          let value = value_as_fixnum_datum(value)
          let index = value_as_nonnegative_fixnum_int(index_value)
          let bit_value = value_as_fixnum_bigint(bit_value)
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
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::FxBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          Value::Datum(
            fixnum_from_datum(datum_bitwise_bit_field(value, start, end)),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::FxCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let from = value_as_fixnum_datum(from_value)
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_copy_bit_field(value, start, end, from),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    Primitive::FxRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let count = value_as_fixnum_int(count_value)
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_rotate_bit_field(value, start, end, count),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    Primitive::FxReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          Value::Datum(
            fixnum_from_datum(
              datum_bitwise_reverse_bit_field(value, start, end),
            ),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::FxArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_fixnum_int(shift_value)
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::FxArithmeticShiftLeft =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::FxArithmeticShiftRight =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, -shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::FlonumP =>
      match args {
        [Value::Datum(datum)] =>
          match datum_unlabel(datum) {
            Datum::Float(_) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::RealToFlonum =>
      match args {
        [value] => {
          let value = value_as_number(value)
          Value::Datum(Datum::Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FixnumToFlonum =>
      match args {
        [value] => {
          let value = value_as_fixnum_int(value)
          Value::Datum(Datum::Float(Float::from_int(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FlEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_flonum(args[0])
        let ok = for i = 1; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          if value_as_flonum(args[i]) != first {
            break false
          }
          continue i + 1
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FlLess =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_flonum(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_flonum(args[i])
          if prev >= cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FlGreater =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_flonum(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_flonum(args[i])
          if prev <= cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FlLessEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_flonum(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_flonum(args[i])
          if prev > cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
        }
        bool_value(ok)
      }
    Primitive::FlGreaterEq =>
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let prev = value_as_flonum(args[0])
        let ok = for i = 1, prev = prev; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_flonum(args[i])
          if prev < cur {
            break false
          }
          continue i + 1, cur
        } else {
          true
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
      let current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return Value::Datum(Datum::Float(current))
      }
      let current = for i = 1, current = current; i < args.length(); {
        // invariant : i >= 1 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let cur = value_as_flonum(args[i])
        if float_is_nan(cur) {
          break cur
        }
        let next = if cur > current { cur } else { current }
        continue i + 1, next
      } else {
        current
      }
      Value::Datum(Datum::Float(current))
    }
    Primitive::FlMin => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return Value::Datum(Datum::Float(current))
      }
      let current = for i = 1, current = current; i < args.length(); {
        // invariant : i >= 1 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let cur = value_as_flonum(args[i])
        if float_is_nan(cur) {
          break cur
        }
        let next = if cur < current { cur } else { current }
        continue i + 1, next
      } else {
        current
      }
      Value::Datum(Datum::Float(current))
    }
    Primitive::FlAdd => {
      let acc = for i = 0, acc = Float::from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc + value_as_flonum(args[i])
      } else {
        acc
      }
      Value::Datum(Datum::Float(acc))
    }
    Primitive::FlMul => {
      let acc = for i = 0, acc = Float::from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc * value_as_flonum(args[i])
      } else {
        acc
      }
      Value::Datum(Datum::Float(acc))
    }
    Primitive::FlSub =>
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => Value::Datum(Datum::Float(-value_as_flonum(args[0])))
        _ => {
          let result = value_as_flonum(args[0])
          let result = for i = 1, result = result; i < args.length(); {
            // invariant : i >= 1 && i <= args.length()
            // decreases : args.length() - i
            // assert : i <= args.length()
            continue i + 1, result - value_as_flonum(args[i])
          } else {
            result
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
          let result = value_as_flonum(args[0])
          let result = for i = 1, result = result; i < args.length(); {
            // invariant : i >= 1 && i <= args.length()
            // decreases : args.length() - i
            // assert : i <= args.length()
            continue i + 1, result / value_as_flonum(args[i])
          } else {
            result
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
    _ =>
      raise @core.EvalError("internal error: numeric primitive dispatch")
  }
}

///|
/// Apply numeric primitives.
fn apply_numeric_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    Add
    | Sub
    | Mul
    | Div
    | NumEq
    | Less
    | Greater
    | LessEq
    | GreaterEq
    | ExactToInexact
    | InexactToExact
    | ExactIntegerSqrt
    | Rationalize
    | NumberToString
    | StringToNumber
    | MakeRectangular
    | MakePolar
    | RealPart
    | ImagPart
    | Magnitude
    | Angle
    | Sqrt
    | Exp
    | Log
    | Expt
    | Sin
    | Cos
    | Tan
    | Asin
    | Acos
    | Atan
    | Numerator
    | Denominator
    | Abs
    | Quotient
    | Remainder
    | Modulo
    | Gcd
    | Lcm
    | Max
    | Min
    | Floor
    | Ceiling
    | Truncate
    | Round
    | BitwiseAnd
    | BitwiseIor
    | BitwiseXor
    | BitwiseNot
    | BitwiseIf
    | ArithmeticShift
    | BitwiseBitCount
    | BitwiseLength
    | BitwiseFirstBitSet
    | BitwiseBitSetP
    | BitwiseCopyBit
    | BitwiseBitField
    | BitwiseCopyBitField
    | BitwiseRotateBitField
    | BitwiseReverseBitField
    | FixnumP
    | FixnumWidth
    | LeastFixnum
    | GreatestFixnum
    | FxEq
    | FxLess
    | FxGreater
    | FxLessEq
    | FxGreaterEq
    | FxZeroP
    | FxPositiveP
    | FxNegativeP
    | FxOddP
    | FxEvenP
    | FxMin
    | FxMax
    | FxAdd
    | FxSub
    | FxMul
    | FxDiv
    | FxMod
    | FxDiv0
    | FxMod0
    | FxAddCarry
    | FxSubCarry
    | FxMulCarry
    | FxNot
    | FxAnd
    | FxIor
    | FxXor
    | FxIf
    | FxBitCount
    | FxLength
    | FxFirstBitSet
    | FxBitSetP
    | FxCopyBit
    | FxBitField
    | FxCopyBitField
    | FxRotateBitField
    | FxReverseBitField
    | FxArithmeticShift
    | FxArithmeticShiftLeft
    | FxArithmeticShiftRight
    | FlonumP
    | RealToFlonum
    | FixnumToFlonum
    | FlEq
    | FlLess
    | FlGreater
    | FlLessEq
    | FlGreaterEq
    | FlIntegerP
    | FlZeroP
    | FlPositiveP
    | FlNegativeP
    | FlOddP
    | FlEvenP
    | FlFiniteP
    | FlInfiniteP
    | FlNanP
    | FlMax
    | FlMin
    | FlAdd
    | FlMul
    | FlSub
    | FlDiv
    | FlAbs
    | FlDivAndMod
    | FlDivInt
    | FlMod
    | FlDiv0AndMod0
    | FlDiv0
    | FlMod0
    | FlNumerator
    | FlDenominator
    | FlFloor
    | FlCeiling
    | FlTruncate
    | FlRound
    | FlExp
    | FlLog
    | FlSin
    | FlCos
    | FlTan
    | FlAsin
    | FlAcos
    | FlAtan
    | FlSqrt
    | FlExpt => Some(apply_numeric_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply numeric primitives (core match).
fn apply_numeric_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    Add => {
      let acc = for i = 0, acc = Datum::Int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_add(acc, value_as_number(args[i]))
      } else {
        acc
      }
      Datum(acc)
    }
    Sub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let number = value_as_number(value)
          Datum(num_sub(Int(0), number))
        }
        [first, .. rest] => {
          let result = for i = 0, result = value_as_number(first); i <
                          rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            continue i + 1, num_sub(result, value_as_number(rest[i]))
          } else {
            result
          }
          Datum(result)
        }
      }
    Mul => {
      let product = for i = 0, product = Datum::Int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_mul(product, value_as_number(args[i]))
      } else {
        product
      }
      Datum(product)
    }
    Div =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let number = value_as_number(value)
          Datum(num_div(Int(1), number))
        }
        [first, .. rest] => {
          let result = for i = 0, result = value_as_number(first); i <
                          rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            continue i + 1, num_div(result, value_as_number(rest[i]))
          } else {
            result
          }
          Datum(result)
        }
      }
    NumEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let first = value_as_number(first)
          let ok = for i = 0; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            if !num_equal(value_as_number(rest[i]), first) {
              break false
            }
            continue i + 1
          } else {
            true
          }
          bool_value(ok)
        }
      }
    Less => compare_chain(args, num_less)
    Greater => compare_chain(args, num_greater)
    LessEq => compare_chain(args, num_less_eq)
    GreaterEq => compare_chain(args, num_greater_eq)
    ExactToInexact =>
      match args {
        [Datum(Float(f))] => Datum(Float(f))
        [Datum(Complex(real, imag))] => {
          let real_f = number_to_float(real.val)
          let imag_f = number_to_float(imag.val)
          Datum(
            make_complex(Float(real_f), Float(imag_f)),
          )
        }
        [value] => {
          let value = value_as_number(value)
          Datum(Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    InexactToExact =>
      match args {
        [Datum(Float(f))] => Datum(float_to_exact_datum(f))
        [Datum(Complex(real, imag))] => {
          let real_exact = inexact_component_to_exact(real.val)
          let imag_exact = inexact_component_to_exact(imag.val)
          Datum(make_complex(real_exact, imag_exact))
        }
        [Datum(Int(n))] => Datum(Int(n))
        [Datum(BigInt(n))] => Datum(BigInt(n))
        [Datum(Rat(n, d))] => Datum(Rat(n, d))
        [Datum(BigRat(n, d))] => Datum(BigRat(n, d))
        [_] => raise EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    ExactIntegerSqrt =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          match value {
            Int(n) => {
              if n < 0 {
                raise EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt(n)
              Values([
                Datum(Int(s)),
                Datum(Int(r)),
              ])
            }
            BigInt(n) => {
              if n.compare_int(0) < 0 {
                raise EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt_bigint(n)
              Values([
                Datum(BigInt(s)),
                Datum(BigInt(r)),
              ])
            }
            _ => raise EvalError("type error: exact integer expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Rationalize =>
      match args {
        [x_value, tol_value] => {
          let x = value_as_number(x_value)
          let tol_value = value_as_number(tol_value)
          let tol = number_to_float(tol_value)
          if tol < 0.0 {
            raise EvalError("type error: nonnegative real expected")
          }
          match x {
            Int(_) | BigInt(_) | Rat(_, _) | BigRat(_, _) =>
              Datum(x)
            Float(f) => {
              let (n, d) = rationalize_float(f, tol)
              Datum(normalize_rat_or_error(n, d))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    NumberToString =>
      match args {
        [num_value] => {
          let num = value_as_number(num_value)
          let radix = 10
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          match num {
            Int(n) => {
              let text = int_to_string_radix(n, radix)
              Datum(String(Ref::new(text)))
            }
            BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              Datum(String(Ref::new(text)))
            }
            Rat(_, _) | BigRat(_, _) | Float(_) | Complex(_, _) => {
              if radix != 10 {
                raise EvalError("type error: exact integer expected")
              }
              let text = @runtime.value_to_string(Datum(num))
              Datum(String(Ref::new(text)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        [num_value, radix_value] => {
          let num = value_as_number(num_value)
          let radix = value_as_int_index(radix_value)
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          match num {
            Int(n) => {
              let text = int_to_string_radix(n, radix)
              Datum(String(Ref::new(text)))
            }
            BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              Datum(String(Ref::new(text)))
            }
            Rat(_, _) | BigRat(_, _) | Float(_) | Complex(_, _) => {
              if radix != 10 {
                raise EvalError("type error: exact integer expected")
              }
              let text = @runtime.value_to_string(Datum(num))
              Datum(String(Ref::new(text)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    StringToNumber =>
      match args {
        [string_value] => {
          let s = value_as_string(string_value)
          let radix = 10
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          let parsed = @parser.parse_number_token(s)
          match parsed {
            Some(num) => Datum(num)
            None => Datum(Bool(false))
          }
        }
        [string_value, radix_value] => {
          let s = value_as_string(string_value)
          let radix = value_as_int_index(radix_value)
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          let parsed = @parser.parse_number_token(s, radix~)
          match parsed {
            Some(num) => Datum(num)
            None => Datum(Bool(false))
          }
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    MakeRectangular =>
      match args {
        [real_value, imag_value] => {
          let real = datum_to_real(value_as_number(real_value))
          let imag = datum_to_real(value_as_number(imag_value))
          Datum(make_complex(real, imag))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    MakePolar =>
      match args {
        [r_value, theta_value] => {
          let r = number_to_float(datum_to_real(value_as_number(r_value)))
          let theta = number_to_float(
            datum_to_real(value_as_number(theta_value)),
          )
          let real = Datum::Float(r * @math.cosf(theta))
          let imag = Datum::Float(r * @math.sinf(theta))
          Datum(make_complex(real, imag))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    RealPart =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, _) => Datum(real.val)
            datum => Datum(datum)
          }
        _ => raise arity_mismatch(1, args.length())
      }
    ImagPart =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(_, imag) => Datum(imag.val)
            Float(_) => Datum(Float(0.0))
            Int(_) | BigInt(_) | Rat(_, _) | BigRat(_, _) =>
              Datum(Int(0))
            _ => raise EvalError("type error: number expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Magnitude =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Datum(Float(mag))
            }
            Int(n) => Datum(Int(int_abs(n)))
            Rat(n, d) =>
              Datum(normalize_rat_or_error(int_abs(n), d))
            BigInt(n) => Datum(BigInt(bigint_abs(n)))
            BigRat(n, d) =>
              Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Float(f) =>
              if f < 0.0 {
                Datum(Float(-f))
              } else {
                Datum(Float(f))
              }
            _ => raise EvalError("type error: number expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Angle =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              Datum(
                Float(@math.atan2f(imag_f, real_f)),
              )
            }
            datum => {
              let real_f = number_to_float(datum)
              Datum(Float(@math.atan2f(0.0, real_f)))
            }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Sqrt =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_value, is_real) = match @runtime.datum_unlabel(value) {
            Complex(real, imag) =>
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
            Some(result) => Datum(result)
            None => {
              let (real_f, imag_f) = datum_to_complex_float(value)
              if imag_f == 0.0 && real_f >= 0.0 {
                Datum(Float(real_f.sqrt()))
              } else {
                let (sr, si) = complex_sqrt_f(real_f, imag_f)
                Datum(complex_from_float(sr, si))
              }
            }
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Exp =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Datum(Float(@math.expf(real_f)))
          } else {
            let (er, ei) = complex_exp_f(real_f, imag_f)
            Datum(complex_from_float(er, ei))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Log =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= 0.0 {
            Datum(Float(@math.lnf(real_f)))
          } else {
            let (lr, li) = complex_log_f(real_f, imag_f)
            Datum(complex_from_float(lr, li))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Expt =>
      match args {
        [base_value, exponent_value] => {
          let base = value_as_number(base_value)
          let exponent = value_as_number(exponent_value)
          match exponent {
            Int(exp_int) =>
              match base {
                Int(base_int) =>
                  if exp_int >= 0 {
                    Datum(
                      Int(pow_int(base_int, exp_int)),
                    )
                  } else {
                    Datum(
                      normalize_rat_or_error(1, pow_int(base_int, -exp_int)),
                    )
                  }
                BigInt(base_big) =>
                  if exp_int >= 0 {
                    Datum(
                      bigint_to_integer_datum(pow_bigint_int(base_big, exp_int)),
                    )
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_int(base_big, -exp_int)
                    Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Rat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_int(n, exp_int)
                    let den = pow_int(d, exp_int)
                    Datum(normalize_rat_or_error(num, den))
                  } else {
                    let num = pow_int(d, -exp_int)
                    let den = pow_int(n, -exp_int)
                    Datum(normalize_rat_or_error(num, den))
                  }
                BigRat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_bigint_int(n, exp_int)
                    let den = pow_bigint_int(d, exp_int)
                    Datum(normalize_big_rat_or_error(num, den))
                  } else {
                    if bigint_is_zero(n) {
                      raise EvalError("division by zero")
                    }
                    let num = pow_bigint_int(d, -exp_int)
                    let den = pow_bigint_int(n, -exp_int)
                    Datum(normalize_big_rat_or_error(num, den))
                  }
                Float(f) => {
                  let pow = if exp_int >= 0 {
                    pow_float_int(f, exp_int)
                  } else {
                    1.0 / pow_float_int(f, -exp_int)
                  }
                  Datum(Float(pow))
                }
                Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(
                    br,
                    bi,
                    Float::from_int(exp_int),
                    0.0,
                  )
                  Datum(complex_from_float(rr, ri))
                }
                _ => raise EvalError("type error: number expected")
              }
            BigInt(exp_big) => {
              let exp_sign = exp_big.compare_int(0)
              let abs_exp = if exp_sign < 0 {
                bigint_abs(exp_big)
              } else {
                exp_big
              }
              let exp_is_odd = !bigint_is_zero(abs_exp % bigint_from_int(2))
              match base {
                Int(base_int) => {
                  let base_big = bigint_from_int(base_int)
                  if exp_sign == 0 {
                    Datum(Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                }
                BigInt(base_big) =>
                  if exp_sign == 0 {
                    Datum(Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Rat(n, d) => {
                  let num = bigint_from_int(n)
                  let den = bigint_from_int(d)
                  if exp_sign == 0 {
                    Datum(Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(num_pow, den_pow),
                    )
                  } else {
                    if bigint_is_zero(num) {
                      raise EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(den_pow, num_pow),
                    )
                  }
                }
                BigRat(n, d) =>
                  if exp_sign == 0 {
                    Datum(Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(num_pow, den_pow),
                    )
                  } else {
                    if bigint_is_zero(n) {
                      raise EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Datum(
                      normalize_big_rat_or_error(den_pow, num_pow),
                    )
                  }
                Float(f) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let abs_base = if f < 0.0 { -f } else { f }
                  let pow = @math.powf(abs_base, exp_f)
                  let pow = if exp_sign < 0 {
                    Float::from_int(1) / pow
                  } else {
                    pow
                  }
                  let pow = if f < 0.0 && exp_is_odd { -pow } else { pow }
                  Datum(Float(pow))
                }
                Complex(_, _) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let (br, bi) = datum_to_complex_float(base)
                  let exp_real = if exp_sign < 0 { -exp_f } else { exp_f }
                  let (rr, ri) = complex_expt_f(br, bi, exp_real, 0.0)
                  Datum(complex_from_float(rr, ri))
                }
                _ => raise EvalError("type error: number expected")
              }
            }
            Float(exp_f) =>
              match base {
                Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Datum(complex_from_float(rr, ri))
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
                      Datum(Float(pow))
                    }
                    None =>
                      if base_f >= 0.0 {
                        Datum(
                          Float(@math.powf(base_f, exp_f)),
                        )
                      } else {
                        let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                        Datum(complex_from_float(rr, ri))
                      }
                  }
                }
              }
            Rat(_, _) | BigRat(_, _) => {
              let exp_f = number_to_float(exponent)
              match base {
                Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  if base_f >= 0.0 {
                    Datum(
                      Float(@math.powf(base_f, exp_f)),
                    )
                  } else {
                    let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                    Datum(complex_from_float(rr, ri))
                  }
                }
              }
            }
            Complex(_, _) => {
              let (br, bi) = datum_to_complex_float(base)
              let (er, ei) = datum_to_complex_float(exponent)
              let (rr, ri) = complex_expt_f(br, bi, er, ei)
              Datum(complex_from_float(rr, ri))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Sin =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Datum(Float(@math.sinf(real_f)))
          } else {
            let (sr, si) = complex_sin_f(real_f, imag_f)
            Datum(complex_from_float(sr, si))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Cos =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Datum(Float(@math.cosf(real_f)))
          } else {
            let (cr, ci) = complex_cos_f(real_f, imag_f)
            Datum(complex_from_float(cr, ci))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Tan =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Datum(Float(@math.tanf(real_f)))
          } else {
            let (tr, ti) = complex_tan_f(real_f, imag_f)
            Datum(complex_from_float(tr, ti))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Asin =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Datum(Float(@math.asinf(real_f)))
          } else {
            let (ar, ai) = complex_asin_f(real_f, imag_f)
            Datum(complex_from_float(ar, ai))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Acos =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Datum(Float(@math.acosf(real_f)))
          } else {
            let (ar, ai) = complex_acos_f(real_f, imag_f)
            Datum(complex_from_float(ar, ai))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Atan =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Datum(Float(@math.atanf(real_f)))
          } else {
            let (ar, ai) = complex_atan_f(real_f, imag_f)
            Datum(complex_from_float(ar, ai))
          }
        }
        [y_value, x_value] => {
          let y = datum_to_real(value_as_number(y_value))
          let x = datum_to_real(value_as_number(x_value))
          let y_f = number_to_float(y)
          let x_f = number_to_float(x)
          Datum(Float(@math.atan2f(y_f, x_f)))
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Numerator =>
      match args {
        [value] =>
          match value {
            Datum(Int(n)) => Datum(Int(n))
            Datum(Rat(n, _)) => Datum(Int(n))
            Datum(BigInt(n)) => Datum(BigInt(n))
            Datum(BigRat(n, _)) => Datum(BigInt(n))
            _ => raise EvalError("type error: rational expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Denominator =>
      match args {
        [value] =>
          match value {
            Datum(Int(_)) => Datum(Int(1))
            Datum(Rat(_, d)) => Datum(Int(d))
            Datum(BigInt(_)) => Datum(Int(1))
            Datum(BigRat(_, d)) => Datum(BigInt(d))
            _ => raise EvalError("type error: rational expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Abs =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => Datum(Int(int_abs(n)))
            Rat(n, d) =>
              Datum(normalize_rat_or_error(int_abs(n), d))
            BigInt(n) => Datum(BigInt(bigint_abs(n)))
            BigRat(n, d) =>
              Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Float(f) =>
              if f < 0.0 {
                Datum(Float(-f))
              } else {
                Datum(Float(f))
              }
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Datum(Float(mag))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Quotient =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              Datum(Int(left / right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
              }
              Datum(BigInt(left / right))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Remainder =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              Datum(Int(left % right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
              }
              Datum(BigInt(left % right))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Modulo =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              let rem = left % right
              let result = if rem == 0 ||
                (rem > 0 && right > 0) ||
                (rem < 0 && right < 0) {
                rem
              } else {
                rem + right
              }
              Datum(Int(result))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
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
              Datum(BigInt(result))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Gcd => {
      let (use_big, acc_int, acc_big) = for i = 0, use_big = false, acc_int = 0, acc_big = bigint_from_int(
                                                0,
                                              ); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : use_big || acc_big == bigint_from_int(0)
        let value = value_as_exact_integer(args[i])
        match value {
          Int(n) =>
            if use_big {
              let next_acc_big = bigint_gcd(acc_big, bigint_from_int(n))
              continue i + 1, true, acc_int, next_acc_big
            } else {
              let next_acc_int = gcd(acc_int, n)
              continue i + 1, false, next_acc_int, acc_big
            }
          BigInt(n) => {
            let base_big = if use_big {
              acc_big
            } else {
              bigint_from_int(acc_int)
            }
            let next_acc_big = bigint_gcd(base_big, n)
            continue i + 1, true, acc_int, next_acc_big
          }
          _ => raise EvalError("type error: integer expected")
        }
      } else {
        (use_big, acc_int, acc_big)
      }
      if use_big {
        Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Int(v)
            None => BigInt(acc_big)
          },
        )
      } else {
        Datum(Int(acc_int))
      }
    }
    Lcm => {
      let (use_big, acc_int, acc_big) = for i = 0, use_big = false, acc_int = 1, acc_big = bigint_from_int(
                                                1,
                                              ); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : use_big || acc_big == bigint_from_int(1)
        let value = value_as_exact_integer(args[i])
        match value {
          Int(n) =>
            if use_big {
              let n_big = bigint_from_int(n)
              let next_acc_big = if acc_big.is_zero() || n_big.is_zero() {
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
          BigInt(n) => {
            let base_big = if use_big {
              acc_big
            } else {
              bigint_from_int(acc_int)
            }
            let next_acc_big = if base_big.is_zero() || n.is_zero() {
              bigint_from_int(0)
            } else {
              let g = bigint_gcd(base_big, n)
              bigint_abs(base_big / g * n)
            }
            continue i + 1, true, acc_int, next_acc_big
          }
          _ => raise EvalError("type error: integer expected")
        }
      } else {
        (use_big, acc_int, acc_big)
      }
      if use_big {
        Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Int(v)
            None => BigInt(acc_big)
          },
        )
      } else {
        Datum(Int(acc_int))
      }
    }
    Max =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let best = value_as_number(first)
          let has_inexact = best is Float(_)
          let (best, has_inexact) = for i = 0, best = best, has_inexact = has_inexact; i <
                                       rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            let next_best = if num_less(best, cur) { cur } else { best }
            let next_has_inexact = has_inexact || cur is Float(_)
            continue i + 1, next_best, next_has_inexact
          } else {
            (best, has_inexact)
          }
          if has_inexact {
            Datum(Float(number_to_float(best)))
          } else {
            Datum(best)
          }
        }
      }
    Min =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let best = value_as_number(first)
          let has_inexact = best is Float(_)
          let (best, has_inexact) = for i = 0, best = best, has_inexact = has_inexact; i <
                                       rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            let next_best = if num_greater(best, cur) { cur } else { best }
            let next_has_inexact = has_inexact || cur is Float(_)
            continue i + 1, next_best, next_has_inexact
          } else {
            (best, has_inexact)
          }
          if has_inexact {
            Datum(Float(number_to_float(best)))
          } else {
            Datum(best)
          }
        }
      }
    Floor =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => Datum(Int(n))
            Rat(n, d) => Datum(Int(rat_floor(n, d)))
            BigInt(n) => Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_floor(n, d)
              Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_floor_int(f)
              Datum(Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Ceiling =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => Datum(Int(n))
            Rat(n, d) => Datum(Int(rat_ceiling(n, d)))
            BigInt(n) => Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_ceiling(n, d)
              Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_ceiling_int(f)
              Datum(Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Truncate =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => Datum(Int(n))
            Rat(n, d) => Datum(Int(n / d))
            BigInt(n) => Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = n / d
              Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = f.to_int()
              Datum(Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Round =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => Datum(Int(n))
            Rat(n, d) => Datum(Int(rat_round(n, d)))
            BigInt(n) => Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_round(n, d)
              Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_round_int(f)
              Datum(Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    BitwiseAnd =>
      match args {
        [] => Datum(Int(-1))
        [first, .. rest] => {
          let acc = value_as_exact_integer(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_exact_integer(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitAnd)
          } else {
            acc
          }
          Datum(acc)
        }
      }
    BitwiseIor =>
      match args {
        [] => Datum(Int(0))
        [first, .. rest] => {
          let acc = value_as_exact_integer(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_exact_integer(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitOr)
          } else {
            acc
          }
          Datum(acc)
        }
      }
    BitwiseXor =>
      match args {
        [] => Datum(Int(0))
        [first, .. rest] => {
          let acc = value_as_exact_integer(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_exact_integer(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitXor)
          } else {
            acc
          }
          Datum(acc)
        }
      }
    BitwiseNot =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          Datum(datum_bitwise_not(value))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    BitwiseIf =>
      match args {
        [mask_value, yes_value, no_value] => {
          let mask = value_as_exact_integer(mask_value)
          let yes_value = value_as_exact_integer(yes_value)
          let no_value = value_as_exact_integer(no_value)
          let masked_yes = datum_bitwise_binop(mask, yes_value, BitAnd)
          let masked_no = datum_bitwise_binop(
            datum_bitwise_not(mask),
            no_value,
            BitAnd,
          )
          Datum(datum_bitwise_binop(masked_yes, masked_no, BitOr))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    ArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_exact_integer(value)
          let shift = value_as_int_index(shift_value)
          Datum(datum_arithmetic_shift(value, shift))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    BitwiseBitCount =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_bit_count(value)
          Datum(Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    BitwiseLength =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_length(value)
          Datum(Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    BitwiseFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Datum(Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitAnd)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Datum(Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    BitwiseBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_exact_integer(value)
          let index = value_as_nonnegative_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    BitwiseCopyBit =>
      match args {
        [value, index_value, bit_value] => {
          let value = value_as_exact_integer(value)
          let index = value_as_nonnegative_int(index_value)
          let bit_value = value_as_exact_integer(bit_value)
          let bit = match bit_value {
            Int(n) => n
            BigInt(n) =>
              if bigint_is_zero(n) {
                0
              } else if n.compare_int(1) == 0 {
                1
              } else {
                raise EvalError("type error: bit expected")
              }
            _ => raise EvalError("type error: bit expected")
          }
          let mask = bigint_to_integer_datum(bigint_pow2(index))
          let result = if bit == 0 {
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitAnd)
          } else if bit == 1 {
            datum_bitwise_binop(value, mask, BitOr)
          } else {
            raise EvalError("type error: bit expected")
          }
          Datum(result)
        }
        _ => raise arity_mismatch(3, args.length())
      }
    BitwiseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          Datum(datum_bitwise_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    BitwiseCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let from = value_as_exact_integer(from_value)
          Datum(
            datum_bitwise_copy_bit_field(value, start, end, from),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    BitwiseRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let count = value_as_int_index(count_value)
          Datum(
            datum_bitwise_rotate_bit_field(value, start, end, count),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    BitwiseReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          Datum(datum_bitwise_reverse_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FixnumP =>
      match args {
        [value] => bool_value(value_is_fixnum(value))
        _ => raise arity_mismatch(1, args.length())
      }
    FixnumWidth =>
      match args {
        [] => Datum(Int(fixnum_width_value()))
        _ => raise arity_mismatch(0, args.length())
      }
    LeastFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          Datum(fixnum_from_bigint(-half))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    GreatestFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          Datum(fixnum_from_bigint(half - bigint_from_int(1)))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    FxEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let first = value_as_fixnum_int(first)
          let ok = for i = 0; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            if value_as_fixnum_int(rest[i]) != first {
              break false
            }
            continue i + 1
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FxLess =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_fixnum_int(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            if prev >= cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FxGreater =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_fixnum_int(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            if prev <= cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FxLessEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_fixnum_int(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            if prev > cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FxGreaterEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_fixnum_int(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            if prev < cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FxZeroP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FxPositiveP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) > 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FxNegativeP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) < 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FxOddP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 != 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FxEvenP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FxMin =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let current = value_as_fixnum_int(first)
          let current = for i = 0, current = current; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            let next = if cur < current { cur } else { current }
            continue i + 1, next
          } else {
            current
          }
          Datum(Int(current))
        }
      }
    FxMax =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let current = value_as_fixnum_int(first)
          let current = for i = 0, current = current; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_fixnum_int(rest[i])
            let next = if cur > current { cur } else { current }
            continue i + 1, next
          } else {
            current
          }
          Datum(Int(current))
        }
      }
    FxAdd => {
      let acc = for i = 0, acc = bigint_from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc + value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
      }
      Datum(fixnum_from_bigint(acc))
    }
    FxSub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let result = value_as_fixnum_bigint(value)
          Datum(fixnum_from_bigint(-result))
        }
        [first, .. rest] => {
          let result = for i = 0, result = value_as_fixnum_bigint(first); i <
                          rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            continue i + 1, result - value_as_fixnum_bigint(rest[i])
          } else {
            result
          }
          Datum(fixnum_from_bigint(result))
        }
      }
    FxMul => {
      let acc = for i = 0, acc = bigint_from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc * value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
      }
      Datum(fixnum_from_bigint(acc))
    }
    FxDiv =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise EvalError("division by zero")
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
          Datum(fixnum_from_bigint(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise EvalError("division by zero")
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
          Datum(fixnum_from_bigint(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxDiv0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise EvalError("division by zero")
          }
          Datum(fixnum_from_bigint(left / right))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise EvalError("division by zero")
          }
          Datum(fixnum_from_bigint(left % right))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxAddCarry =>
      match args {
        [left_value, right_value, carry_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          let carry = value_as_fixnum_bigint(carry_value)
          let total = left + right + carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Values([
            Datum(fixnum_from_bigint(sum)),
            Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxSubCarry =>
      match args {
        [left_value, right_value, carry_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          let carry = value_as_fixnum_bigint(carry_value)
          let total = left - right - carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Values([
            Datum(fixnum_from_bigint(sum)),
            Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxMulCarry =>
      match args {
        [left_value, right_value, carry_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          let carry = value_as_fixnum_bigint(carry_value)
          let total = left * right + carry
          let base = fixnum_base_bigint()
          let half = fixnum_half_bigint()
          let carry_out = bigint_floor_div(total + half, base)
          let sum = total - carry_out * base
          Values([
            Datum(fixnum_from_bigint(sum)),
            Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxNot =>
      match args {
        [value] => {
          let datum = value_as_fixnum_datum(value)
          let result = datum_bitwise_not(datum)
          Datum(fixnum_from_datum(result))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FxAnd =>
      match args {
        [] => Datum(Int(-1))
        [first, .. rest] => {
          let acc = value_as_fixnum_datum(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_fixnum_datum(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitAnd)
          } else {
            acc
          }
          Datum(fixnum_from_datum(acc))
        }
      }
    FxIor =>
      match args {
        [] => Datum(Int(0))
        [first, .. rest] => {
          let acc = value_as_fixnum_datum(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_fixnum_datum(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitOr)
          } else {
            acc
          }
          Datum(fixnum_from_datum(acc))
        }
      }
    FxXor =>
      match args {
        [] => Datum(Int(0))
        [first, .. rest] => {
          let acc = value_as_fixnum_datum(first)
          let acc = for i = 0, acc = acc; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let next = value_as_fixnum_datum(rest[i])
            continue i + 1, datum_bitwise_binop(acc, next, BitXor)
          } else {
            acc
          }
          Datum(fixnum_from_datum(acc))
        }
      }
    FxIf =>
      match args {
        [mask_value, yes_value, no_value] => {
          let mask = value_as_fixnum_datum(mask_value)
          let yes_value = value_as_fixnum_datum(yes_value)
          let no_value = value_as_fixnum_datum(no_value)
          let masked_yes = datum_bitwise_binop(mask, yes_value, BitAnd)
          let masked_no = datum_bitwise_binop(
            datum_bitwise_not(mask),
            no_value,
            BitAnd,
          )
          Datum(
            fixnum_from_datum(datum_bitwise_binop(masked_yes, masked_no, BitOr)),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxBitCount =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_bit_count(value)
          Datum(Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FxLength =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_length(value)
          Datum(Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FxFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            Datum(Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitAnd)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            Datum(Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FxBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_fixnum_datum(value)
          let index = value_as_nonnegative_fixnum_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxCopyBit =>
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
            raise EvalError("type error: bit expected")
          }
          let mask = bigint_to_integer_datum(bigint_pow2(index))
          let result = if bit == 0 {
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitAnd)
          } else {
            datum_bitwise_binop(value, mask, BitOr)
          }
          Datum(fixnum_from_datum(result))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          Datum(
            fixnum_from_datum(datum_bitwise_bit_field(value, start, end)),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let from = value_as_fixnum_datum(from_value)
          Datum(
            fixnum_from_datum(
              datum_bitwise_copy_bit_field(value, start, end, from),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    FxRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let count = value_as_fixnum_int(count_value)
          Datum(
            fixnum_from_datum(
              datum_bitwise_rotate_bit_field(value, start, end, count),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    FxReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          Datum(
            fixnum_from_datum(
              datum_bitwise_reverse_bit_field(value, start, end),
            ),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    FxArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_fixnum_int(shift_value)
          Datum(
            fixnum_from_datum(datum_arithmetic_shift(value, shift)),
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxArithmeticShiftLeft =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          Datum(
            fixnum_from_datum(datum_arithmetic_shift(value, shift)),
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FxArithmeticShiftRight =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          Datum(
            fixnum_from_datum(datum_arithmetic_shift(value, -shift)),
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlonumP =>
      match args {
        [Datum(datum)] =>
          match @runtime.datum_unlabel(datum) {
            Float(_) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RealToFlonum =>
      match args {
        [value] => {
          let value = value_as_number(value)
          Datum(Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FixnumToFlonum =>
      match args {
        [value] => {
          let value = value_as_fixnum_int(value)
          Datum(Float(Float::from_int(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let first = value_as_flonum(first)
          let ok = for i = 0; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            if value_as_flonum(rest[i]) != first {
              break false
            }
            continue i + 1
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FlLess =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_flonum(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if prev >= cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FlGreater =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_flonum(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if prev <= cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FlLessEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_flonum(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if prev > cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FlGreaterEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, .. rest] => {
          let prev = value_as_flonum(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if prev < cur {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    FlIntegerP =>
      match args {
        [value] => {
          let f = value_as_flonum(value)
          let ok = float_is_finite(f) && float_to_int_exact(f) is Some(_)
          bool_value(ok)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlZeroP =>
      match args {
        [value] => bool_value(value_as_flonum(value) == 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    FlPositiveP =>
      match args {
        [value] => bool_value(value_as_flonum(value) > 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    FlNegativeP =>
      match args {
        [value] => bool_value(value_as_flonum(value) < 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    FlOddP =>
      match args {
        [value] => bool_value(value_as_integer_flonum(value) % 2 != 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FlEvenP =>
      match args {
        [value] => bool_value(value_as_integer_flonum(value) % 2 == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    FlFiniteP =>
      match args {
        [value] => bool_value(float_is_finite(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    FlInfiniteP =>
      match args {
        [value] => bool_value(float_is_infinite(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    FlNanP =>
      match args {
        [value] => bool_value(float_is_nan(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    FlMax =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let current = value_as_flonum(first)
          if float_is_nan(current) {
            return Datum(Float(current))
          }
          let current = for i = 0, current = current; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if float_is_nan(cur) {
              break cur
            }
            let next = if cur > current { cur } else { current }
            continue i + 1, next
          } else {
            current
          }
          Datum(Float(current))
        }
      }
    FlMin =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [first, .. rest] => {
          let current = value_as_flonum(first)
          if float_is_nan(current) {
            return Datum(Float(current))
          }
          let current = for i = 0, current = current; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_flonum(rest[i])
            if float_is_nan(cur) {
              break cur
            }
            let next = if cur < current { cur } else { current }
            continue i + 1, next
          } else {
            current
          }
          Datum(Float(current))
        }
      }
    FlAdd => {
      let acc = for i = 0, acc = Float::from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc + value_as_flonum(args[i])
      } else {
        acc
      }
      Datum(Float(acc))
    }
    FlMul => {
      let acc = for i = 0, acc = Float::from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc * value_as_flonum(args[i])
      } else {
        acc
      }
      Datum(Float(acc))
    }
    FlSub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] =>
          Datum(Float(-value_as_flonum(value)))
        [first, .. rest] => {
          let result = value_as_flonum(first)
          let result = for i = 0, result = result; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            continue i + 1, result - value_as_flonum(rest[i])
          } else {
            result
          }
          Datum(Float(result))
        }
      }
    FlDiv =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let denom = value_as_flonum(value)
          Datum(Float(Float::from_int(1) / denom))
        }
        [first, .. rest] => {
          let result = value_as_flonum(first)
          let result = for i = 0, result = result; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            continue i + 1, result / value_as_flonum(rest[i])
          } else {
            result
          }
          Datum(Float(result))
        }
      }
    FlAbs =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          Datum(
            Float(if num < 0.0 { -num } else { num }),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlDivAndMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, r) = flonum_div_and_mod(left, right)
          Values([
            Datum(Float(q)),
            Datum(Float(r)),
          ])
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlDivInt =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, _) = flonum_div_and_mod(left, right)
          Datum(Float(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (_, r) = flonum_div_and_mod(left, right)
          Datum(Float(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlDiv0AndMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, r) = flonum_div0_and_mod0(left, right)
          Values([
            Datum(Float(q)),
            Datum(Float(r)),
          ])
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlDiv0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, _) = flonum_div0_and_mod0(left, right)
          Datum(Float(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (_, r) = flonum_div0_and_mod0(left, right)
          Datum(Float(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlNumerator =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          Datum(Float(flonum_numerator(num)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlDenominator =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          Datum(Float(flonum_denominator(num)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlFloor =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            Datum(Float(num))
          } else {
            Datum(
              Float(Float::from_int(float_floor_int(num))),
            )
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlCeiling =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            Datum(Float(num))
          } else {
            Datum(
              Float(Float::from_int(float_ceiling_int(num))),
            )
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlTruncate =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            Datum(Float(num))
          } else {
            Datum(
              Float(Float::from_int(num.to_int())),
            )
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlRound =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            Datum(Float(num))
          } else {
            Datum(
              Float(Float::from_int(float_round_int(num))),
            )
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlExp =>
      match args {
        [value] =>
          Datum(
            Float(@math.expf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlLog =>
      match args {
        [value] =>
          Datum(
            Float(@math.lnf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlSin =>
      match args {
        [value] =>
          Datum(
            Float(@math.sinf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlCos =>
      match args {
        [value] =>
          Datum(
            Float(@math.cosf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlTan =>
      match args {
        [value] =>
          Datum(
            Float(@math.tanf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlAsin =>
      match args {
        [value] =>
          Datum(
            Float(@math.asinf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlAcos =>
      match args {
        [value] =>
          Datum(
            Float(@math.acosf(value_as_flonum(value))),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    FlAtan =>
      match args {
        [value] =>
          Datum(
            Float(@math.atanf(value_as_flonum(value))),
          )
        [y_value, x_value] => {
          let y = value_as_flonum(y_value)
          let x = value_as_flonum(x_value)
          Datum(Float(@math.atan2f(y, x)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    FlSqrt =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          Datum(Float(num.sqrt()))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    FlExpt =>
      match args {
        [base_value, exp_value] => {
          let base = value_as_flonum(base_value)
          let exp = value_as_flonum(exp_value)
          Datum(Float(@math.powf(base, exp)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    _ => raise EvalError("internal error: numeric primitive dispatch")
  }
}

///|
/// Helper for chained numeric comparisons (e.g., < <= > >=).
fn compare_chain(
  args : Array[@core.Value],
  cmp : (@core.Datum, @core.Datum) -> Bool raise @core.EvalError,
) -> @core.Value raise @core.EvalError {
  match args {
    [] | [_] => bool_value(true)
    [first, .. rest] => {
      let prev = value_as_number(first)
      let ok = for i = 0, prev = prev; i < rest.length(); {
        let cur = value_as_number(rest[i])
        if !cmp(prev, cur) {
          break false
        }
        continue i + 1, cur
      } else {
        true
      }
      bool_value(ok)
    }
  }
}

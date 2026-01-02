///|
/// @core.Primitive::Apply numeric primitives.
fn apply_numeric_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    @core.Primitive::Add
    | @core.Primitive::Sub
    | @core.Primitive::Mul
    | @core.Primitive::Div
    | @core.Primitive::NumEq
    | @core.Primitive::Less
    | @core.Primitive::Greater
    | @core.Primitive::LessEq
    | @core.Primitive::GreaterEq
    | @core.Primitive::ExactToInexact
    | @core.Primitive::InexactToExact
    | @core.Primitive::ExactIntegerSqrt
    | @core.Primitive::Rationalize
    | @core.Primitive::NumberToString
    | @core.Primitive::StringToNumber
    | @core.Primitive::MakeRectangular
    | @core.Primitive::MakePolar
    | @core.Primitive::RealPart
    | @core.Primitive::ImagPart
    | @core.Primitive::Magnitude
    | @core.Primitive::Angle
    | @core.Primitive::Sqrt
    | @core.Primitive::Exp
    | @core.Primitive::Log
    | @core.Primitive::Expt
    | @core.Primitive::Sin
    | @core.Primitive::Cos
    | @core.Primitive::Tan
    | @core.Primitive::Asin
    | @core.Primitive::Acos
    | @core.Primitive::Atan
    | @core.Primitive::Numerator
    | @core.Primitive::Denominator
    | @core.Primitive::Abs
    | @core.Primitive::Quotient
    | @core.Primitive::Remainder
    | @core.Primitive::Modulo
    | @core.Primitive::Gcd
    | @core.Primitive::Lcm
    | @core.Primitive::Max
    | @core.Primitive::Min
    | @core.Primitive::Floor
    | @core.Primitive::Ceiling
    | @core.Primitive::Truncate
    | @core.Primitive::Round
    | @core.Primitive::BitwiseAnd
    | @core.Primitive::BitwiseIor
    | @core.Primitive::BitwiseXor
    | @core.Primitive::BitwiseNot
    | @core.Primitive::BitwiseIf
    | @core.Primitive::ArithmeticShift
    | @core.Primitive::BitwiseBitCount
    | @core.Primitive::BitwiseLength
    | @core.Primitive::BitwiseFirstBitSet
    | @core.Primitive::BitwiseBitSetP
    | @core.Primitive::BitwiseCopyBit
    | @core.Primitive::BitwiseBitField
    | @core.Primitive::BitwiseCopyBitField
    | @core.Primitive::BitwiseRotateBitField
    | @core.Primitive::BitwiseReverseBitField
    | @core.Primitive::FixnumP
    | @core.Primitive::FixnumWidth
    | @core.Primitive::LeastFixnum
    | @core.Primitive::GreatestFixnum
    | @core.Primitive::FxEq
    | @core.Primitive::FxLess
    | @core.Primitive::FxGreater
    | @core.Primitive::FxLessEq
    | @core.Primitive::FxGreaterEq
    | @core.Primitive::FxZeroP
    | @core.Primitive::FxPositiveP
    | @core.Primitive::FxNegativeP
    | @core.Primitive::FxOddP
    | @core.Primitive::FxEvenP
    | @core.Primitive::FxMin
    | @core.Primitive::FxMax
    | @core.Primitive::FxAdd
    | @core.Primitive::FxSub
    | @core.Primitive::FxMul
    | @core.Primitive::FxDiv
    | @core.Primitive::FxMod
    | @core.Primitive::FxDiv0
    | @core.Primitive::FxMod0
    | @core.Primitive::FxAddCarry
    | @core.Primitive::FxSubCarry
    | @core.Primitive::FxMulCarry
    | @core.Primitive::FxNot
    | @core.Primitive::FxAnd
    | @core.Primitive::FxIor
    | @core.Primitive::FxXor
    | @core.Primitive::FxIf
    | @core.Primitive::FxBitCount
    | @core.Primitive::FxLength
    | @core.Primitive::FxFirstBitSet
    | @core.Primitive::FxBitSetP
    | @core.Primitive::FxCopyBit
    | @core.Primitive::FxBitField
    | @core.Primitive::FxCopyBitField
    | @core.Primitive::FxRotateBitField
    | @core.Primitive::FxReverseBitField
    | @core.Primitive::FxArithmeticShift
    | @core.Primitive::FxArithmeticShiftLeft
    | @core.Primitive::FxArithmeticShiftRight
    | @core.Primitive::FlonumP
    | @core.Primitive::RealToFlonum
    | @core.Primitive::FixnumToFlonum
    | @core.Primitive::FlEq
    | @core.Primitive::FlLess
    | @core.Primitive::FlGreater
    | @core.Primitive::FlLessEq
    | @core.Primitive::FlGreaterEq
    | @core.Primitive::FlIntegerP
    | @core.Primitive::FlZeroP
    | @core.Primitive::FlPositiveP
    | @core.Primitive::FlNegativeP
    | @core.Primitive::FlOddP
    | @core.Primitive::FlEvenP
    | @core.Primitive::FlFiniteP
    | @core.Primitive::FlInfiniteP
    | @core.Primitive::FlNanP
    | @core.Primitive::FlMax
    | @core.Primitive::FlMin
    | @core.Primitive::FlAdd
    | @core.Primitive::FlMul
    | @core.Primitive::FlSub
    | @core.Primitive::FlDiv
    | @core.Primitive::FlAbs
    | @core.Primitive::FlDivAndMod
    | @core.Primitive::FlDivInt
    | @core.Primitive::FlMod
    | @core.Primitive::FlDiv0AndMod0
    | @core.Primitive::FlDiv0
    | @core.Primitive::FlMod0
    | @core.Primitive::FlNumerator
    | @core.Primitive::FlDenominator
    | @core.Primitive::FlFloor
    | @core.Primitive::FlCeiling
    | @core.Primitive::FlTruncate
    | @core.Primitive::FlRound
    | @core.Primitive::FlExp
    | @core.Primitive::FlLog
    | @core.Primitive::FlSin
    | @core.Primitive::FlCos
    | @core.Primitive::FlTan
    | @core.Primitive::FlAsin
    | @core.Primitive::FlAcos
    | @core.Primitive::FlAtan
    | @core.Primitive::FlSqrt
    | @core.Primitive::FlExpt
    => Some(apply_numeric_primitive_core(prim, args))
    _ => None
  }
}

///|
/// @core.Primitive::Apply numeric primitives (core match).
fn apply_numeric_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    @core.Primitive::Add => {
      let acc = for i = 0, acc = @core.Datum::Int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_add(acc, value_as_number(args[i]))
      } else {
        acc
      }
      @core.Value::Datum(acc)
    }
    @core.Primitive::Sub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let number = value_as_number(value)
          @core.Value::Datum(num_sub(@core.Datum::Int(0), number))
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
          @core.Value::Datum(result)
        }
      }
    @core.Primitive::Mul => {
      let product = for i = 0, product = @core.Datum::Int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, num_mul(product, value_as_number(args[i]))
      } else {
        product
      }
      @core.Value::Datum(product)
    }
    @core.Primitive::Div =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let number = value_as_number(value)
          @core.Value::Datum(num_div(@core.Datum::Int(1), number))
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
          @core.Value::Datum(result)
        }
      }
    @core.Primitive::NumEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::Less =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
          let prev = value_as_number(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            if !num_less(prev, cur) {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    @core.Primitive::Greater =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
          let prev = value_as_number(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            if !num_greater(prev, cur) {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    @core.Primitive::LessEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
          let prev = value_as_number(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            if !num_less_eq(prev, cur) {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    @core.Primitive::GreaterEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
          let prev = value_as_number(first)
          let ok = for i = 0, prev = prev; i < rest.length(); {
            // invariant : i >= 0 && i <= rest.length()
            // decreases : rest.length() - i
            // assert : i <= rest.length()
            let cur = value_as_number(rest[i])
            if !num_greater_eq(prev, cur) {
              break false
            }
            continue i + 1, cur
          } else {
            true
          }
          bool_value(ok)
        }
      }
    @core.Primitive::ExactToInexact =>
      match args {
        [Datum(Float(f))] => @core.Value::Datum(@core.Datum::Float(f))
        [Datum(Complex(real, imag))] => {
          let real_f = number_to_float(real.val)
          let imag_f = number_to_float(imag.val)
          @core.Value::Datum(
            make_complex(@core.Datum::Float(real_f), @core.Datum::Float(imag_f)),
          )
        }
        [value] => {
          let value = value_as_number(value)
          @core.Value::Datum(@core.Datum::Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::InexactToExact =>
      match args {
        [Datum(Float(f))] =>
          @core.Value::Datum(float_to_exact_datum(f))
        [Datum(Complex(real, imag))] => {
          let real_exact = inexact_component_to_exact(real.val)
          let imag_exact = inexact_component_to_exact(imag.val)
          @core.Value::Datum(make_complex(real_exact, imag_exact))
        }
        [Datum(Int(n))] => @core.Value::Datum(@core.Datum::Int(n))
        [Datum(BigInt(n))] => @core.Value::Datum(@core.Datum::BigInt(n))
        [Datum(Rat(n, d))] => @core.Value::Datum(@core.Datum::Rat(n, d))
        [Datum(BigRat(n, d))] =>
          @core.Value::Datum(@core.Datum::BigRat(n, d))
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ExactIntegerSqrt =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          match value {
            Int(n) => {
              if n < 0 {
                raise @core.EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt(n)
              @core.Value::Values([
                @core.Value::Datum(@core.Datum::Int(s)),
                @core.Value::Datum(@core.Datum::Int(r)),
              ])
            }
            BigInt(n) => {
              if n.compare_int(0) < 0 {
                raise @core.EvalError(
                  "type error: exact nonnegative integer expected",
                )
              }
              let (s, r) = exact_integer_sqrt_bigint(n)
              @core.Value::Values([
                @core.Value::Datum(@core.Datum::BigInt(s)),
                @core.Value::Datum(@core.Datum::BigInt(r)),
              ])
            }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Rationalize =>
      match args {
        [x_value, tol_value] => {
          let x = value_as_number(x_value)
          let tol_value = value_as_number(tol_value)
          let tol = number_to_float(tol_value)
          if tol < 0.0 {
            raise @core.EvalError("type error: nonnegative real expected")
          }
          match x {
            Int(_)
            | BigInt(_)
            | Rat(_, _)
            | BigRat(_, _) => @core.Value::Datum(x)
            Float(f) => {
              let (n, d) = rationalize_float(f, tol)
              @core.Value::Datum(normalize_rat_or_error(n, d))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::NumberToString =>
      match args {
        [num_value] => {
          let num = value_as_number(num_value)
          let radix = 10
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          match num {
            Int(n) => {
              let text = int_to_string_radix(n, radix)
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            Rat(_, _)
            | BigRat(_, _)
            | Float(_)
            | Complex(_, _) => {
              if radix != 10 {
                raise @core.EvalError("type error: exact integer expected")
              }
              let text = @runtime.value_to_string(@core.Value::Datum(num))
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        [num_value, radix_value] => {
          let num = value_as_number(num_value)
          let radix = value_as_int_index(radix_value)
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          match num {
            Int(n) => {
              let text = int_to_string_radix(n, radix)
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            Rat(_, _)
            | BigRat(_, _)
            | Float(_)
            | Complex(_, _) => {
              if radix != 10 {
                raise @core.EvalError("type error: exact integer expected")
              }
              let text = @runtime.value_to_string(@core.Value::Datum(num))
              @core.Value::Datum(@core.Datum::String(Ref::new(text)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::StringToNumber =>
      match args {
        [string_value] => {
          let s = value_as_string(string_value)
          let radix = 10
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          let parsed = @parser.parse_number_token(s)
          match parsed {
            Some(num) => @core.Value::Datum(num)
            None => @core.Value::Datum(@core.Datum::Bool(false))
          }
        }
        [string_value, radix_value] => {
          let s = value_as_string(string_value)
          let radix = value_as_int_index(radix_value)
          if radix < 2 || radix > 36 {
            raise @core.EvalError("radix out of range")
          }
          let parsed = @parser.parse_number_token(s, radix=radix)
          match parsed {
            Some(num) => @core.Value::Datum(num)
            None => @core.Value::Datum(@core.Datum::Bool(false))
          }
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::MakeRectangular =>
      match args {
        [real_value, imag_value] => {
          let real = datum_to_real(value_as_number(real_value))
          let imag = datum_to_real(value_as_number(imag_value))
          @core.Value::Datum(make_complex(real, imag))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::MakePolar =>
      match args {
        [r_value, theta_value] => {
          let r = number_to_float(datum_to_real(value_as_number(r_value)))
          let theta = number_to_float(datum_to_real(value_as_number(theta_value)))
          let real = @core.Datum::Float(r * @math.cosf(theta))
          let imag = @core.Datum::Float(r * @math.sinf(theta))
          @core.Value::Datum(make_complex(real, imag))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::RealPart =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, _) => @core.Value::Datum(real.val)
            datum => @core.Value::Datum(datum)
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ImagPart =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(_, imag) => @core.Value::Datum(imag.val)
            Float(_) => @core.Value::Datum(@core.Datum::Float(0.0))
            Int(_)
            | BigInt(_)
            | Rat(_, _)
            | BigRat(_, _) => @core.Value::Datum(@core.Datum::Int(0))
            _ => raise @core.EvalError("type error: number expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Magnitude =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              @core.Value::Datum(@core.Datum::Float(mag))
            }
            Int(n) => @core.Value::Datum(@core.Datum::Int(int_abs(n)))
            Rat(n, d) =>
              @core.Value::Datum(normalize_rat_or_error(int_abs(n), d))
            BigInt(n) => @core.Value::Datum(@core.Datum::BigInt(bigint_abs(n)))
            BigRat(n, d) =>
              @core.Value::Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Float(f) =>
              if f < 0.0 {
                @core.Value::Datum(@core.Datum::Float(-f))
              } else {
                @core.Value::Datum(@core.Datum::Float(f))
              }
            _ => raise @core.EvalError("type error: number expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Angle =>
      match args {
        [num_value] =>
          match value_as_number(num_value) {
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              @core.Value::Datum(@core.Datum::Float(@math.atan2f(imag_f, real_f)))
            }
            datum => {
              let real_f = number_to_float(datum)
              @core.Value::Datum(@core.Datum::Float(@math.atan2f(0.0, real_f)))
            }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Sqrt =>
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
            Some(result) => @core.Value::Datum(result)
            None => {
              let (real_f, imag_f) = datum_to_complex_float(value)
              if imag_f == 0.0 && real_f >= 0.0 {
                @core.Value::Datum(@core.Datum::Float(real_f.sqrt()))
              } else {
                let (sr, si) = complex_sqrt_f(real_f, imag_f)
                @core.Value::Datum(complex_from_float(sr, si))
              }
            }
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Exp =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.expf(real_f)))
          } else {
            let (er, ei) = complex_exp_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(er, ei))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Log =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.lnf(real_f)))
          } else {
            let (lr, li) = complex_log_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(lr, li))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Expt =>
      match args {
        [base_value, exponent_value] => {
          let base = value_as_number(base_value)
          let exponent = value_as_number(exponent_value)
          match exponent {
            Int(exp_int) =>
              match base {
                Int(base_int) =>
                  if exp_int >= 0 {
                    @core.Value::Datum(@core.Datum::Int(pow_int(base_int, exp_int)))
                  } else {
                    @core.Value::Datum(
                      normalize_rat_or_error(1, pow_int(base_int, -exp_int)),
                    )
                  }
                BigInt(base_big) =>
                  if exp_int >= 0 {
                    @core.Value::Datum(
                      bigint_to_integer_datum(pow_bigint_int(base_big, exp_int)),
                    )
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_int(base_big, -exp_int)
                    @core.Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Rat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_int(n, exp_int)
                    let den = pow_int(d, exp_int)
                    @core.Value::Datum(normalize_rat_or_error(num, den))
                  } else {
                    let num = pow_int(d, -exp_int)
                    let den = pow_int(n, -exp_int)
                    @core.Value::Datum(normalize_rat_or_error(num, den))
                  }
                BigRat(n, d) =>
                  if exp_int >= 0 {
                    let num = pow_bigint_int(n, exp_int)
                    let den = pow_bigint_int(d, exp_int)
                    @core.Value::Datum(normalize_big_rat_or_error(num, den))
                  } else {
                    if bigint_is_zero(n) {
                      raise @core.EvalError("division by zero")
                    }
                    let num = pow_bigint_int(d, -exp_int)
                    let den = pow_bigint_int(n, -exp_int)
                    @core.Value::Datum(normalize_big_rat_or_error(num, den))
                  }
                Float(f) => {
                  let pow = if exp_int >= 0 {
                    pow_float_int(f, exp_int)
                  } else {
                    1.0 / pow_float_int(f, -exp_int)
                  }
                  @core.Value::Datum(@core.Datum::Float(pow))
                }
                Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(
                    br,
                    bi,
                    Float::from_int(exp_int),
                    0.0,
                  )
                  @core.Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise @core.EvalError("type error: number expected")
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
                    @core.Value::Datum(@core.Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    @core.Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    @core.Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                }
                BigInt(base_big) =>
                  if exp_sign == 0 {
                    @core.Value::Datum(@core.Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    @core.Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise @core.EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    @core.Value::Datum(
                      normalize_big_rat_or_error(bigint_from_int(1), denom),
                    )
                  }
                Rat(n, d) => {
                  let num = bigint_from_int(n)
                  let den = bigint_from_int(d)
                  if exp_sign == 0 {
                    @core.Value::Datum(@core.Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    @core.Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(num) {
                      raise @core.EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    @core.Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                }
                BigRat(n, d) =>
                  if exp_sign == 0 {
                    @core.Value::Datum(@core.Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    @core.Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(n) {
                      raise @core.EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    @core.Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                Float(f) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let abs_base = if f < 0.0 { -f } else { f }
                  let pow = @math.powf(abs_base, exp_f)
                  let pow = if exp_sign < 0 { Float::from_int(1) / pow } else { pow }
                  let pow = if f < 0.0 && exp_is_odd { -pow } else { pow }
                  @core.Value::Datum(@core.Datum::Float(pow))
                }
                Complex(_, _) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let (br, bi) = datum_to_complex_float(base)
                  let exp_real = if exp_sign < 0 { -exp_f } else { exp_f }
                  let (rr, ri) = complex_expt_f(br, bi, exp_real, 0.0)
                  @core.Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise @core.EvalError("type error: number expected")
              }
            }
            Float(exp_f) =>
              match base {
                Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  @core.Value::Datum(complex_from_float(rr, ri))
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
                      @core.Value::Datum(@core.Datum::Float(pow))
                    }
                    None =>
                      if base_f >= 0.0 {
                        @core.Value::Datum(@core.Datum::Float(@math.powf(base_f, exp_f)))
                      } else {
                        let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                        @core.Value::Datum(complex_from_float(rr, ri))
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
                  @core.Value::Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  if base_f >= 0.0 {
                    @core.Value::Datum(@core.Datum::Float(@math.powf(base_f, exp_f)))
                  } else {
                    let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                    @core.Value::Datum(complex_from_float(rr, ri))
                  }
                }
              }
            }
            Complex(_, _) => {
              let (br, bi) = datum_to_complex_float(base)
              let (er, ei) = datum_to_complex_float(exponent)
              let (rr, ri) = complex_expt_f(br, bi, er, ei)
              @core.Value::Datum(complex_from_float(rr, ri))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Sin =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.sinf(real_f)))
          } else {
            let (sr, si) = complex_sin_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(sr, si))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Cos =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.cosf(real_f)))
          } else {
            let (cr, ci) = complex_cos_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(cr, ci))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Tan =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.tanf(real_f)))
          } else {
            let (tr, ti) = complex_tan_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(tr, ti))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Asin =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            @core.Value::Datum(@core.Datum::Float(@math.asinf(real_f)))
          } else {
            let (ar, ai) = complex_asin_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(ar, ai))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Acos =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            @core.Value::Datum(@core.Datum::Float(@math.acosf(real_f)))
          } else {
            let (ar, ai) = complex_acos_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(ar, ai))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Atan =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            @core.Value::Datum(@core.Datum::Float(@math.atanf(real_f)))
          } else {
            let (ar, ai) = complex_atan_f(real_f, imag_f)
            @core.Value::Datum(complex_from_float(ar, ai))
          }
        }
        [y_value, x_value] => {
          let y = datum_to_real(value_as_number(y_value))
          let x = datum_to_real(value_as_number(x_value))
          let y_f = number_to_float(y)
          let x_f = number_to_float(x)
          @core.Value::Datum(@core.Datum::Float(@math.atan2f(y_f, x_f)))
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::Numerator =>
      match args {
        [value] =>
          match value {
            Datum(Int(n)) => @core.Value::Datum(@core.Datum::Int(n))
            Datum(Rat(n, _)) => @core.Value::Datum(@core.Datum::Int(n))
            Datum(BigInt(n)) => @core.Value::Datum(@core.Datum::BigInt(n))
            Datum(BigRat(n, _)) => @core.Value::Datum(@core.Datum::BigInt(n))
            _ => raise @core.EvalError("type error: rational expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Denominator =>
      match args {
        [value] =>
          match value {
            Datum(Int(_)) => @core.Value::Datum(@core.Datum::Int(1))
            Datum(Rat(_, d)) => @core.Value::Datum(@core.Datum::Int(d))
            Datum(BigInt(_)) => @core.Value::Datum(@core.Datum::Int(1))
            Datum(BigRat(_, d)) => @core.Value::Datum(@core.Datum::BigInt(d))
            _ => raise @core.EvalError("type error: rational expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Abs =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => @core.Value::Datum(@core.Datum::Int(int_abs(n)))
            Rat(n, d) =>
              @core.Value::Datum(normalize_rat_or_error(int_abs(n), d))
            BigInt(n) => @core.Value::Datum(@core.Datum::BigInt(bigint_abs(n)))
            BigRat(n, d) =>
              @core.Value::Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Float(f) =>
              if f < 0.0 {
                @core.Value::Datum(@core.Datum::Float(-f))
              } else {
                @core.Value::Datum(@core.Datum::Float(f))
              }
            Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              @core.Value::Datum(@core.Datum::Float(mag))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Quotient =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
              if right == 0 {
                raise @core.EvalError("division by zero")
              }
              @core.Value::Datum(@core.Datum::Int(left / right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise @core.EvalError("division by zero")
              }
              @core.Value::Datum(@core.Datum::BigInt(left / right))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Remainder =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
              if right == 0 {
                raise @core.EvalError("division by zero")
              }
              @core.Value::Datum(@core.Datum::Int(left % right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise @core.EvalError("division by zero")
              }
              @core.Value::Datum(@core.Datum::BigInt(left % right))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Modulo =>
      match args {
        [left_value, right_value] => {
          let left_val = value_as_exact_integer(left_value)
          let right_val = value_as_exact_integer(right_value)
          match (left_val, right_val) {
            (Int(left), Int(right)) => {
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
              @core.Value::Datum(@core.Datum::Int(result))
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
              @core.Value::Datum(@core.Datum::BigInt(result))
            }
          }
        }
        _ => raise arity_mismatch(2, args.length())
    }
    @core.Primitive::Gcd => {
      let (use_big, acc_int, acc_big) =
        for i = 0,
          use_big = false,
          acc_int = 0,
          acc_big = bigint_from_int(0);
          i < args.length(); {
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
        @core.Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => @core.Datum::Int(v)
            None => @core.Datum::BigInt(acc_big)
          },
        )
      } else {
        @core.Value::Datum(@core.Datum::Int(acc_int))
      }
    }
    @core.Primitive::Lcm => {
      let (use_big, acc_int, acc_big) =
        for i = 0,
          use_big = false,
          acc_int = 1,
          acc_big = bigint_from_int(1);
          i < args.length(); {
          // invariant : i >= 0 && i <= args.length()
          // decreases : args.length() - i
          // assert : use_big || acc_big == bigint_from_int(1)
          let value = value_as_exact_integer(args[i])
          match value {
            Int(n) =>
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
            BigInt(n) => {
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
        @core.Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => @core.Datum::Int(v)
            None => @core.Datum::BigInt(acc_big)
          },
        )
      } else {
        @core.Value::Datum(@core.Datum::Int(acc_int))
      }
    }
    @core.Primitive::Max => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let best = value_as_number(args[0])
      let has_inexact = best is Float(_)
      let (best, has_inexact) =
        for i = 1, best = best, has_inexact = has_inexact; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          let next_best = if num_less(best, cur) { cur } else { best }
          let next_has_inexact = has_inexact || cur is Float(_)
          continue i + 1, next_best, next_has_inexact
        } else {
          (best, has_inexact)
        }
      if has_inexact {
        @core.Value::Datum(@core.Datum::Float(number_to_float(best)))
      } else {
        @core.Value::Datum(best)
      }
    }
    @core.Primitive::Min => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let best = value_as_number(args[0])
      let has_inexact = best is Float(_)
      let (best, has_inexact) =
        for i = 1, best = best, has_inexact = has_inexact; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let cur = value_as_number(args[i])
          let next_best = if num_greater(best, cur) { cur } else { best }
          let next_has_inexact = has_inexact || cur is Float(_)
          continue i + 1, next_best, next_has_inexact
        } else {
          (best, has_inexact)
        }
      if has_inexact {
        @core.Value::Datum(@core.Datum::Float(number_to_float(best)))
      } else {
        @core.Value::Datum(best)
      }
    }
    @core.Primitive::Floor =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => @core.Value::Datum(@core.Datum::Int(n))
            Rat(n, d) => @core.Value::Datum(@core.Datum::Int(rat_floor(n, d)))
            BigInt(n) => @core.Value::Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_floor(n, d)
              @core.Value::Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_floor_int(f)
              @core.Value::Datum(@core.Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Ceiling =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => @core.Value::Datum(@core.Datum::Int(n))
            Rat(n, d) => @core.Value::Datum(@core.Datum::Int(rat_ceiling(n, d)))
            BigInt(n) => @core.Value::Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_ceiling(n, d)
              @core.Value::Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_ceiling_int(f)
              @core.Value::Datum(@core.Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Truncate =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => @core.Value::Datum(@core.Datum::Int(n))
            Rat(n, d) => @core.Value::Datum(@core.Datum::Int(n / d))
            BigInt(n) => @core.Value::Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = n / d
              @core.Value::Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = f.to_int()
              @core.Value::Datum(@core.Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Round =>
      match args {
        [num_value] => {
          let value = value_as_number(num_value)
          match value {
            Int(n) => @core.Value::Datum(@core.Datum::Int(n))
            Rat(n, d) => @core.Value::Datum(@core.Datum::Int(rat_round(n, d)))
            BigInt(n) => @core.Value::Datum(bigint_to_integer_datum(n))
            BigRat(n, d) => {
              let q = bigrat_round(n, d)
              @core.Value::Datum(bigint_to_integer_datum(q))
            }
            Float(f) => {
              let n = float_round_int(f)
              @core.Value::Datum(@core.Datum::Float(Float::from_int(n)))
            }
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BitwiseAnd =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(-1))
      } else {
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitAnd)
        } else {
          acc
        }
        @core.Value::Datum(acc)
      }
    @core.Primitive::BitwiseIor =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(0))
      } else {
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOr)
        } else {
          acc
        }
        @core.Value::Datum(acc)
      }
    @core.Primitive::BitwiseXor =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(0))
      } else {
        let acc = value_as_exact_integer(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_exact_integer(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitXor)
        } else {
          acc
        }
        @core.Value::Datum(acc)
      }
    @core.Primitive::BitwiseNot =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          @core.Value::Datum(datum_bitwise_not(value))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BitwiseIf =>
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
          @core.Value::Datum(datum_bitwise_binop(masked_yes, masked_no, BitOr))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::ArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_exact_integer(value)
          let shift = value_as_int_index(shift_value)
          @core.Value::Datum(datum_arithmetic_shift(value, shift))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::BitwiseBitCount =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_bit_count(value)
          @core.Value::Datum(@core.Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BitwiseLength =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let count = datum_bitwise_length(value)
          @core.Value::Datum(@core.Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BitwiseFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_exact_integer(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            @core.Value::Datum(@core.Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitAnd)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            @core.Value::Datum(@core.Datum::Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BitwiseBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_exact_integer(value)
          let index = value_as_nonnegative_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::BitwiseCopyBit =>
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
                raise @core.EvalError("type error: bit expected")
              }
            _ => raise @core.EvalError("type error: bit expected")
          }
          let mask = bigint_to_integer_datum(bigint_pow2(index))
          let result = if bit == 0 {
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitAnd)
          } else if bit == 1 {
            datum_bitwise_binop(value, mask, BitOr)
          } else {
            raise @core.EvalError("type error: bit expected")
          }
          @core.Value::Datum(result)
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::BitwiseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          @core.Value::Datum(datum_bitwise_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::BitwiseCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let from = value_as_exact_integer(from_value)
          @core.Value::Datum(datum_bitwise_copy_bit_field(value, start, end, from))
        }
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::BitwiseRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          let count = value_as_int_index(count_value)
          @core.Value::Datum(datum_bitwise_rotate_bit_field(value, start, end, count))
        }
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::BitwiseReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_exact_integer(value)
          let start = value_as_nonnegative_int(start_value)
          let end = value_as_nonnegative_int(end_value)
          @core.Value::Datum(datum_bitwise_reverse_bit_field(value, start, end))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FixnumP =>
      match args {
        [value] => bool_value(value_is_fixnum(value))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FixnumWidth =>
      match args {
        [] => @core.Value::Datum(@core.Datum::Int(fixnum_width_value()))
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::LeastFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          @core.Value::Datum(fixnum_from_bigint(-half))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::GreatestFixnum =>
      match args {
        [] => {
          let half = fixnum_half_bigint()
          @core.Value::Datum(fixnum_from_bigint(half - bigint_from_int(1)))
        }
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::FxEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FxLess =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FxGreater =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FxLessEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FxGreaterEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FxZeroP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxPositiveP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) > 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxNegativeP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) < 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxOddP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 != 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxEvenP =>
      match args {
        [value] => bool_value(value_as_fixnum_int(value) % 2 == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxMin => {
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
      @core.Value::Datum(@core.Datum::Int(current))
    }
    @core.Primitive::FxMax => {
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
      @core.Value::Datum(@core.Datum::Int(current))
    }
    @core.Primitive::FxAdd => {
      let acc = for i = 0, acc = bigint_from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc + value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
      }
      @core.Value::Datum(fixnum_from_bigint(acc))
    }
    @core.Primitive::FxSub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let result = value_as_fixnum_bigint(value)
          @core.Value::Datum(fixnum_from_bigint(-result))
        }
        _ => {
          let result = for i = 1, result = value_as_fixnum_bigint(args[0]);
            i < args.length(); {
              // invariant : i >= 1 && i <= args.length()
              // decreases : args.length() - i
              // assert : i <= args.length()
              continue i + 1, result - value_as_fixnum_bigint(args[i])
            } else {
              result
            }
          @core.Value::Datum(fixnum_from_bigint(result))
        }
      }
    @core.Primitive::FxMul => {
      let acc = for i = 0, acc = bigint_from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        let next = acc * value_as_fixnum_bigint(args[i])
        continue i + 1, next
      } else {
        acc
      }
      @core.Value::Datum(fixnum_from_bigint(acc))
    }
    @core.Primitive::FxDiv =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
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
          @core.Value::Datum(fixnum_from_bigint(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
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
          @core.Value::Datum(fixnum_from_bigint(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxDiv0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          @core.Value::Datum(fixnum_from_bigint(left / right))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_fixnum_bigint(left_value)
          let right = value_as_fixnum_bigint(right_value)
          if right.is_zero() {
            raise @core.EvalError("division by zero")
          }
          @core.Value::Datum(fixnum_from_bigint(left % right))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxAddCarry =>
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
          @core.Value::Values([
            @core.Value::Datum(fixnum_from_bigint(sum)),
            @core.Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxSubCarry =>
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
          @core.Value::Values([
            @core.Value::Datum(fixnum_from_bigint(sum)),
            @core.Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxMulCarry =>
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
          @core.Value::Values([
            @core.Value::Datum(fixnum_from_bigint(sum)),
            @core.Value::Datum(fixnum_from_bigint(carry_out)),
          ])
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxNot =>
      match args {
        [value] => {
          let datum = value_as_fixnum_datum(value)
          let result = datum_bitwise_not(datum)
          @core.Value::Datum(fixnum_from_datum(result))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxAnd =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(-1))
      } else {
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitAnd)
        } else {
          acc
        }
        @core.Value::Datum(fixnum_from_datum(acc))
      }
    @core.Primitive::FxIor =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(0))
      } else {
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitOr)
        } else {
          acc
        }
        @core.Value::Datum(fixnum_from_datum(acc))
      }
    @core.Primitive::FxXor =>
      if args.is_empty() {
        @core.Value::Datum(@core.Datum::Int(0))
      } else {
        let acc = value_as_fixnum_datum(args[0])
        let acc = for i = 1, acc = acc; i < args.length(); {
          // invariant : i >= 1 && i <= args.length()
          // decreases : args.length() - i
          // assert : i <= args.length()
          let next = value_as_fixnum_datum(args[i])
          continue i + 1, datum_bitwise_binop(acc, next, BitXor)
        } else {
          acc
        }
        @core.Value::Datum(fixnum_from_datum(acc))
      }
    @core.Primitive::FxIf =>
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
          @core.Value::Datum(
            fixnum_from_datum(
              datum_bitwise_binop(masked_yes, masked_no, BitOr),
            ),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxBitCount =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_bit_count(value)
          @core.Value::Datum(@core.Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxLength =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let count = datum_bitwise_length(value)
          @core.Value::Datum(@core.Datum::Int(count))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxFirstBitSet =>
      match args {
        [value] => {
          let value = value_as_fixnum_datum(value)
          let big = datum_to_bigint(value)
          if bigint_is_zero(big) {
            @core.Value::Datum(@core.Datum::Int(-1))
          } else {
            let lsb = bigint_bitwise_binop(big, -big, BitAnd)
            let idx = bigint_bit_length_nonneg(lsb) - 1
            @core.Value::Datum(@core.Datum::Int(idx))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FxBitSetP =>
      match args {
        [value, index_value] => {
          let value = value_as_fixnum_datum(value)
          let index = value_as_nonnegative_fixnum_int(index_value)
          bool_value(datum_bitwise_bit_set(value, index))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxCopyBit =>
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
            datum_bitwise_binop(value, datum_bitwise_not(mask), BitAnd)
          } else {
            datum_bitwise_binop(value, mask, BitOr)
          }
          @core.Value::Datum(fixnum_from_datum(result))
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          @core.Value::Datum(
            fixnum_from_datum(datum_bitwise_bit_field(value, start, end)),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxCopyBitField =>
      match args {
        [value, start_value, end_value, from_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let from = value_as_fixnum_datum(from_value)
          @core.Value::Datum(
            fixnum_from_datum(
              datum_bitwise_copy_bit_field(value, start, end, from),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::FxRotateBitField =>
      match args {
        [value, start_value, end_value, count_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          let count = value_as_fixnum_int(count_value)
          @core.Value::Datum(
            fixnum_from_datum(
              datum_bitwise_rotate_bit_field(value, start, end, count),
            ),
          )
        }
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::FxReverseBitField =>
      match args {
        [value, start_value, end_value] => {
          let value = value_as_fixnum_datum(value)
          let start = value_as_nonnegative_fixnum_int(start_value)
          let end = value_as_nonnegative_fixnum_int(end_value)
          @core.Value::Datum(
            fixnum_from_datum(
              datum_bitwise_reverse_bit_field(value, start, end),
            ),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::FxArithmeticShift =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_fixnum_int(shift_value)
          @core.Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxArithmeticShiftLeft =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          @core.Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FxArithmeticShiftRight =>
      match args {
        [value, shift_value] => {
          let value = value_as_fixnum_datum(value)
          let shift = value_as_nonnegative_fixnum_int(shift_value)
          @core.Value::Datum(fixnum_from_datum(datum_arithmetic_shift(value, -shift)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlonumP =>
      match args {
        [Datum(datum)] =>
          match @runtime.datum_unlabel(datum) {
            Float(_) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::RealToFlonum =>
      match args {
        [value] => {
          let value = value_as_number(value)
          @core.Value::Datum(@core.Datum::Float(number_to_float(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FixnumToFlonum =>
      match args {
        [value] => {
          let value = value_as_fixnum_int(value)
          @core.Value::Datum(@core.Datum::Float(Float::from_int(value)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FlLess =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FlGreater =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FlLessEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FlGreaterEq =>
      match args {
        [] | [_] => bool_value(true)
        [first, ..rest] => {
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
    @core.Primitive::FlIntegerP =>
      match args {
        [value] => {
          let f = value_as_flonum(value)
          let ok = float_is_finite(f) && float_to_int_exact(f) is Some(_)
          bool_value(ok)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlZeroP =>
      match args {
        [value] => bool_value(value_as_flonum(value) == 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlPositiveP =>
      match args {
        [value] => bool_value(value_as_flonum(value) > 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlNegativeP =>
      match args {
        [value] => bool_value(value_as_flonum(value) < 0.0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlOddP =>
      match args {
        [value] => bool_value(value_as_integer_flonum(value) % 2 != 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlEvenP =>
      match args {
        [value] => bool_value(value_as_integer_flonum(value) % 2 == 0)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlFiniteP =>
      match args {
        [value] => bool_value(float_is_finite(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlInfiniteP =>
      match args {
        [value] => bool_value(float_is_infinite(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlNanP =>
      match args {
        [value] => bool_value(float_is_nan(value_as_flonum(value)))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlMax => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return @core.Value::Datum(@core.Datum::Float(current))
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
      @core.Value::Datum(@core.Datum::Float(current))
    }
    @core.Primitive::FlMin => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let current = value_as_flonum(args[0])
      if float_is_nan(current) {
        return @core.Value::Datum(@core.Datum::Float(current))
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
      @core.Value::Datum(@core.Datum::Float(current))
    }
    @core.Primitive::FlAdd => {
      let acc = for i = 0, acc = Float::from_int(0); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc + value_as_flonum(args[i])
      } else {
        acc
      }
      @core.Value::Datum(@core.Datum::Float(acc))
    }
    @core.Primitive::FlMul => {
      let acc = for i = 0, acc = Float::from_int(1); i < args.length(); {
        // invariant : i >= 0 && i <= args.length()
        // decreases : args.length() - i
        // assert : i <= args.length()
        continue i + 1, acc * value_as_flonum(args[i])
      } else {
        acc
      }
      @core.Value::Datum(@core.Datum::Float(acc))
    }
    @core.Primitive::FlSub =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => @core.Value::Datum(@core.Datum::Float(-value_as_flonum(value)))
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
          @core.Value::Datum(@core.Datum::Float(result))
        }
      }
    @core.Primitive::FlDiv =>
      match args {
        [] => raise arity_mismatch(1, 0)
        [value] => {
          let denom = value_as_flonum(value)
          @core.Value::Datum(@core.Datum::Float(Float::from_int(1) / denom))
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
          @core.Value::Datum(@core.Datum::Float(result))
        }
      }
    @core.Primitive::FlAbs =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          @core.Value::Datum(@core.Datum::Float(if num < 0.0 { -num } else { num }))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlDivAndMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, r) = flonum_div_and_mod(left, right)
          @core.Value::Values([
            @core.Value::Datum(@core.Datum::Float(q)),
            @core.Value::Datum(@core.Datum::Float(r)),
          ])
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlDivInt =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, _) = flonum_div_and_mod(left, right)
          @core.Value::Datum(@core.Datum::Float(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlMod =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (_, r) = flonum_div_and_mod(left, right)
          @core.Value::Datum(@core.Datum::Float(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlDiv0AndMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, r) = flonum_div0_and_mod0(left, right)
          @core.Value::Values([
            @core.Value::Datum(@core.Datum::Float(q)),
            @core.Value::Datum(@core.Datum::Float(r)),
          ])
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlDiv0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (q, _) = flonum_div0_and_mod0(left, right)
          @core.Value::Datum(@core.Datum::Float(q))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlMod0 =>
      match args {
        [left_value, right_value] => {
          let left = value_as_flonum(left_value)
          let right = value_as_flonum(right_value)
          let (_, r) = flonum_div0_and_mod0(left, right)
          @core.Value::Datum(@core.Datum::Float(r))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlNumerator =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          @core.Value::Datum(@core.Datum::Float(flonum_numerator(num)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlDenominator =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          @core.Value::Datum(@core.Datum::Float(flonum_denominator(num)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlFloor =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            @core.Value::Datum(@core.Datum::Float(num))
          } else {
            @core.Value::Datum(@core.Datum::Float(Float::from_int(float_floor_int(num))))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlCeiling =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            @core.Value::Datum(@core.Datum::Float(num))
          } else {
            @core.Value::Datum(
              @core.Datum::Float(Float::from_int(float_ceiling_int(num))),
            )
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlTruncate =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            @core.Value::Datum(@core.Datum::Float(num))
          } else {
            @core.Value::Datum(@core.Datum::Float(Float::from_int(num.to_int())))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlRound =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          if !float_is_finite(num) {
            @core.Value::Datum(@core.Datum::Float(num))
          } else {
            @core.Value::Datum(@core.Datum::Float(Float::from_int(float_round_int(num))))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlExp =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.expf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlLog =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.lnf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlSin =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.sinf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlCos =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.cosf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlTan =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.tanf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlAsin =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.asinf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlAcos =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.acosf(value_as_flonum(value))))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlAtan =>
      match args {
        [value] =>
          @core.Value::Datum(@core.Datum::Float(@math.atanf(value_as_flonum(value))))
        [y_value, x_value] => {
          let y = value_as_flonum(y_value)
          let x = value_as_flonum(x_value)
          @core.Value::Datum(@core.Datum::Float(@math.atan2f(y, x)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::FlSqrt =>
      match args {
        [value] => {
          let num = value_as_flonum(value)
          @core.Value::Datum(@core.Datum::Float(num.sqrt()))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FlExpt =>
      match args {
        [base_value, exp_value] => {
          let base = value_as_flonum(base_value)
          let exp = value_as_flonum(exp_value)
          @core.Value::Datum(@core.Datum::Float(@math.powf(base, exp)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: numeric primitive dispatch")
  }
}

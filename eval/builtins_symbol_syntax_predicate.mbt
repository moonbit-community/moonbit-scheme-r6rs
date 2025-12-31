///|
/// Apply symbol, syntax, hash, and predicate primitives.
fn apply_symbol_syntax_predicate_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::Eq
    | Primitive::Eqv
    | Primitive::Equal
    | Primitive::SymbolP
    | Primitive::SymbolEq
    | Primitive::IdentifierP
    | Primitive::SyntaxP
    | Primitive::FreeIdentifierEq
    | Primitive::BoundIdentifierEq
    | Primitive::SymbolToString
    | Primitive::StringToSymbol
    | Primitive::StringHash
    | Primitive::StringCiHash
    | Primitive::SymbolHash
    | Primitive::EqualHash
    | Primitive::SyntaxToDatum
    | Primitive::DatumToSyntax
    | Primitive::BooleanP
    | Primitive::BooleanEq
    | Primitive::NumberP
    | Primitive::IntegerP
    | Primitive::ExactIntegerP
    | Primitive::RationalP
    | Primitive::RealP
    | Primitive::ComplexP
    | Primitive::ExactP
    | Primitive::InexactP
    | Primitive::ZeroP
    | Primitive::PositiveP
    | Primitive::NegativeP
    | Primitive::OddP
    | Primitive::EvenP
    | Primitive::FiniteP
    | Primitive::InfiniteP
    | Primitive::NanP
    | Primitive::ProcedureP
    => Some(apply_symbol_syntax_predicate_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply symbol, syntax, hash, and predicate primitives (core match).
fn apply_symbol_syntax_predicate_primitive_core(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
    Primitive::Eq =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Eqv =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Equal =>
      match args {
        [left, right] => bool_value(value_equal(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::SymbolP =>
      match args {
        [Value::Datum(Datum::Symbol(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::SymbolEq => bool_value(compare_chain_symbol(args))
    Primitive::IdentifierP =>
      match args {
        [value] => bool_value(is_identifier_value(value))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::SyntaxP =>
      match args {
        [Value::SyntaxObject(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FreeIdentifierEq =>
      match args {
        [left, right] => {
          let (a, a_scopes, a_binding) = identifier_info(left)
          let (b, b_scopes, b_binding) = identifier_info(right)
          if a != b {
            bool_value(false)
          } else if a_binding is Some(_) && b_binding is Some(_) {
            bool_value(a_binding == b_binding)
          } else {
            bool_value(scopes_equal(a_scopes, b_scopes))
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::BoundIdentifierEq =>
      match args {
        [left, right] => {
          let (a, a_scopes, a_binding) = identifier_info(left)
          let (b, b_scopes, b_binding) = identifier_info(right)
          if a != b {
            bool_value(false)
          } else if a_binding is Some(_) && b_binding is Some(_) {
            bool_value(a_binding == b_binding)
          } else {
            bool_value(scopes_equal(a_scopes, b_scopes))
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::SymbolToString =>
      match args {
        [Value::Datum(Datum::Symbol(name))] =>
          Value::Datum(Datum::String(Ref::new(name)))
        [_] => raise @core.EvalError("type error: symbol expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringToSymbol =>
      match args {
        [Value::Datum(Datum::String(name))] =>
          Value::Datum(Datum::Symbol(name.val))
        [_] => raise @core.EvalError("type error: string expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          Value::Datum(Datum::Int(hash_string_value(s)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringCiHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          let folded = unicode_string(s).foldcase().into_string()
          Value::Datum(Datum::Int(hash_string_value(folded)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::SymbolHash =>
      match args {
        [value] => {
          let name = value_as_symbol(value)
          Value::Datum(Datum::Int(hash_string_value(name)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EqualHash =>
      match args {
        [value] => {
          let repr = value_to_string(value)
          Value::Datum(Datum::Int(hash_string_value(repr)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::SyntaxToDatum =>
      match args {
        [Value::SyntaxObject(obj)] =>
          Value::Datum(strip_syntax_datum(obj.datum))
        [Value::Datum(d)] => Value::Datum(strip_syntax_datum(d))
        [_] => raise @core.EvalError("type error: syntax expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::DatumToSyntax =>
      match args {
        [context, base] | [context, base, _] => {
          let scopes = match context {
            Value::SyntaxObject(obj) => obj.scopes
            _ => []
          }
          let context_binding = match context {
            Value::SyntaxObject(obj) => obj.binding_id
            _ => None
          }
          let base = match base {
            Value::SyntaxObject(obj) => obj.datum
            Value::Datum(d) => d
            _ => raise @core.EvalError("type error: datum expected")
          }
          let template_name = match context {
            Value::SyntaxObject(obj) =>
              match datum_unlabel(obj.datum) {
                Datum::Symbol(name) => Some(name)
                _ => None
              }
            _ => None
          }
          let base = match (template_name, datum_unlabel(base)) {
            (Some(template), Datum::Symbol(base_name)) =>
              match strip_gensym_suffix(template) {
                Some(immediate) =>
                  if strip_gensym_suffix(immediate) is Some(_) {
                    let original = strip_all_gensym_suffixes(template)
                    if original == base_name {
                      Datum::Symbol(immediate)
                    } else {
                      base
                    }
                  } else {
                    base
                  }
                None => base
              }
            _ => base
          }
          let datum = syntax_wrap_root(base, scopes)
          let binding_id = match datum_unlabel(base) {
            Datum::Symbol(_) => context_binding
            _ => None
          }
          Value::SyntaxObject(SyntaxObject::{ datum, scopes, binding_id })
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 or 3 got \{args.length()}",
          )
      }
    Primitive::BooleanP =>
      match args {
        [Value::Datum(Datum::Bool(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::BooleanEq => bool_value(compare_chain_bool(args))
    Primitive::NumberP =>
      match args {
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))]
        | [Value::Datum(Datum::Float(_))]
        | [Value::Datum(Datum::Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::IntegerP =>
      match args {
        [Value::Datum(datum)] => bool_value(datum_is_integer_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ExactIntegerP =>
      match args {
        [Value::Datum(datum)] => bool_value(datum_is_exact_integer(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::RationalP =>
      match args {
        [Value::Datum(datum)] => bool_value(datum_is_rational_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::RealP =>
      match args {
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))]
        | [Value::Datum(Datum::Float(_))] => bool_value(true)
        [Value::Datum(Datum::Complex(_, imag))] =>
          bool_value(datum_is_zero(imag.val))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ComplexP =>
      match args {
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))]
        | [Value::Datum(Datum::Float(_))]
        | [Value::Datum(Datum::Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ExactP =>
      match args {
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))] => bool_value(true)
        [Value::Datum(Datum::Complex(real, imag))] =>
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
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::InexactP =>
      match args {
        [Value::Datum(Datum::Float(_))] => bool_value(true)
        [Value::Datum(Datum::Complex(real, imag))] =>
          match (real.val, imag.val) {
            (Datum::Float(_), _) => bool_value(true)
            (_, Datum::Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ZeroP =>
      match args {
        [Value::Datum(Datum::Int(n))] => bool_value(n == 0)
        [Value::Datum(Datum::Rat(n, _))] => bool_value(n == 0)
        [Value::Datum(Datum::BigInt(n))] => bool_value(n.is_zero())
        [Value::Datum(Datum::BigRat(n, _))] => bool_value(n.is_zero())
        [Value::Datum(Datum::Float(n))] => bool_value(n == 0.0)
        [Value::Datum(Datum::Complex(real, imag))] =>
          bool_value(datum_is_zero(real.val) && datum_is_zero(imag.val))
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::PositiveP =>
      match args {
        [value] => {
          let value = datum_to_real(value_as_number(value))
          match value {
            Datum::Int(n) => bool_value(n > 0)
            Datum::Rat(n, _) => bool_value(n > 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) > 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) > 0)
            Datum::Float(n) => bool_value(n > 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::NegativeP =>
      match args {
        [value] => {
          let value = datum_to_real(value_as_number(value))
          match value {
            Datum::Int(n) => bool_value(n < 0)
            Datum::Rat(n, _) => bool_value(n < 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) < 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) < 0)
            Datum::Float(n) => bool_value(n < 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::OddP =>
      match args {
        [value] => {
          let value = match value {
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EvenP =>
      match args {
        [value] => {
          let value = match value {
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::FiniteP =>
      match args {
        [Value::Datum(Datum::Float(f))] => bool_value(float_is_finite(f))
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))] => bool_value(true)
        [Value::Datum(Datum::Complex(real, imag))] =>
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
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::InfiniteP =>
      match args {
        [Value::Datum(Datum::Float(f))] => bool_value(float_is_infinite(f))
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))] => bool_value(false)
        [Value::Datum(Datum::Complex(real, imag))] =>
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
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::NanP =>
      match args {
        [Value::Datum(Datum::Float(f))] => bool_value(float_is_nan(f))
        [Value::Datum(Datum::Int(_))]
        | [Value::Datum(Datum::BigInt(_))]
        | [Value::Datum(Datum::Rat(_, _))]
        | [Value::Datum(Datum::BigRat(_, _))] => bool_value(false)
        [Value::Datum(Datum::Complex(real, imag))] =>
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
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ProcedureP =>
      match args {
        [Value::Primitive(_)]
        | [Value::Closure(_)]
        | [Value::CaseClosure(_)]
        | [Value::GuardHandler(_)]
        | [Value::Parameter(_)]
        | [Value::Continuation(_)]
        | [Value::RecordProc(_)]
        | [Value::ConditionProc(_)]
        | [Value::EnumSetProc(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_symbol_syntax_predicate_primitive dispatch")
  }
}

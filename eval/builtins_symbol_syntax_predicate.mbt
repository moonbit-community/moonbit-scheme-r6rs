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
          let folded = unicode_string(s).foldcase().into_string()
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
          let base = match args[1] {
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
    _ =>
      raise @core.EvalError("internal error: apply_symbol_syntax_predicate_primitive dispatch")
  }
}

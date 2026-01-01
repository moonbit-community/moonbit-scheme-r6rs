///|
/// @core.Primitive::Apply symbol, syntax, hash, and predicate primitives.
fn apply_symbol_syntax_predicate_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    @core.Primitive::Eq
    | @core.Primitive::Eqv
    | @core.Primitive::Equal
    | @core.Primitive::SymbolP
    | @core.Primitive::SymbolEq
    | @core.Primitive::IdentifierP
    | @core.Primitive::SyntaxP
    | @core.Primitive::FreeIdentifierEq
    | @core.Primitive::BoundIdentifierEq
    | @core.Primitive::SymbolToString
    | @core.Primitive::StringToSymbol
    | @core.Primitive::StringHash
    | @core.Primitive::StringCiHash
    | @core.Primitive::SymbolHash
    | @core.Primitive::EqualHash
    | @core.Primitive::SyntaxToDatum
    | @core.Primitive::DatumToSyntax
    | @core.Primitive::BooleanP
    | @core.Primitive::BooleanEq
    | @core.Primitive::NumberP
    | @core.Primitive::IntegerP
    | @core.Primitive::ExactIntegerP
    | @core.Primitive::RationalP
    | @core.Primitive::RealP
    | @core.Primitive::ComplexP
    | @core.Primitive::ExactP
    | @core.Primitive::InexactP
    | @core.Primitive::ZeroP
    | @core.Primitive::PositiveP
    | @core.Primitive::NegativeP
    | @core.Primitive::OddP
    | @core.Primitive::EvenP
    | @core.Primitive::FiniteP
    | @core.Primitive::InfiniteP
    | @core.Primitive::NanP
    | @core.Primitive::ProcedureP
    => Some(apply_symbol_syntax_predicate_primitive_core(prim, args))
    _ => None
  }
}

///|
/// @core.Primitive::Apply symbol, syntax, hash, and predicate primitives (core match).
fn apply_symbol_syntax_predicate_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    @core.Primitive::Eq =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Eqv =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Equal =>
      match args {
        [left, right] => bool_value(value_equal(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::SymbolP =>
      match args {
        [@core.Value::Datum(Symbol(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::SymbolEq => bool_value(compare_chain_symbol(args))
    @core.Primitive::IdentifierP =>
      match args {
        [value] => bool_value(is_identifier_value(value))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::SyntaxP =>
      match args {
        [@core.Value::SyntaxObject(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FreeIdentifierEq =>
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
    @core.Primitive::BoundIdentifierEq =>
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
    @core.Primitive::SymbolToString =>
      match args {
        [@core.Value::Datum(Symbol(name))] =>
          @core.Value::Datum(String(Ref::new(name)))
        [_] => raise @core.EvalError("type error: symbol expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::StringToSymbol =>
      match args {
        [@core.Value::Datum(String(name))] =>
          @core.Value::Datum(Symbol(name.val))
        [_] => raise @core.EvalError("type error: string expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::StringHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          @core.Value::Datum(Int(hash_string_value(s)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::StringCiHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          let folded = @core.UnicodeString::new(s).foldcase().into_string()
          @core.Value::Datum(Int(hash_string_value(folded)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::SymbolHash =>
      match args {
        [value] => {
          let name = value_as_symbol(value)
          @core.Value::Datum(Int(hash_string_value(name)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EqualHash =>
      match args {
        [value] => {
          let repr = @runtime.value_to_string(value)
          @core.Value::Datum(Int(hash_string_value(repr)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::SyntaxToDatum =>
      match args {
        [@core.Value::SyntaxObject(obj)] =>
          @core.Value::Datum(@runtime.strip_syntax_datum(obj.datum))
        [@core.Value::Datum(d)] => @core.Value::Datum(@runtime.strip_syntax_datum(d))
        [_] => raise @core.EvalError("type error: syntax expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::DatumToSyntax =>
      match args {
        [context, base] | [context, base, _] => {
          let scopes = match context {
            @core.Value::SyntaxObject(obj) => obj.scopes
            _ => []
          }
          let context_binding = match context {
            @core.Value::SyntaxObject(obj) => obj.binding_id
            _ => None
          }
          let base = match base {
            @core.Value::SyntaxObject(obj) => obj.datum
            @core.Value::Datum(d) => d
            _ => raise @core.EvalError("type error: datum expected")
          }
          let template_name = match context {
            @core.Value::SyntaxObject(obj) =>
              match @runtime.datum_unlabel(obj.datum) {
                Symbol(name) => Some(name)
                _ => None
              }
            _ => None
          }
          let base = match (template_name, @runtime.datum_unlabel(base)) {
            (Some(template), Symbol(base_name)) =>
              match strip_gensym_suffix(template) {
                Some(immediate) =>
                  if strip_gensym_suffix(immediate) is Some(_) {
                    let original = strip_all_gensym_suffixes(template)
                    if original == base_name {
                      @core.Datum::Symbol(immediate)
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
          let datum = @runtime.syntax_wrap_root(base, scopes)
          let binding_id = match @runtime.datum_unlabel(base) {
            Symbol(_) => context_binding
            _ => None
          }
          @core.Value::SyntaxObject(@core.SyntaxObject::{ datum, scopes, binding_id })
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 or 3 got \{args.length()}",
          )
      }
    @core.Primitive::BooleanP =>
      match args {
        [@core.Value::Datum(Bool(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::BooleanEq => bool_value(compare_chain_bool(args))
    @core.Primitive::NumberP =>
      match args {
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))]
        | [@core.Value::Datum(Float(_))]
        | [@core.Value::Datum(Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::IntegerP =>
      match args {
        [@core.Value::Datum(datum)] => bool_value(datum_is_integer_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ExactIntegerP =>
      match args {
        [@core.Value::Datum(datum)] => bool_value(datum_is_exact_integer(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::RationalP =>
      match args {
        [@core.Value::Datum(datum)] => bool_value(datum_is_rational_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::RealP =>
      match args {
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))]
        | [@core.Value::Datum(Float(_))] => bool_value(true)
        [@core.Value::Datum(Complex(_, imag))] =>
          bool_value(datum_is_zero(imag.val))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ComplexP =>
      match args {
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))]
        | [@core.Value::Datum(Float(_))]
        | [@core.Value::Datum(Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ExactP =>
      match args {
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))] => bool_value(true)
        [@core.Value::Datum(Complex(real, imag))] =>
          match (real.val, imag.val) {
            (Int(_), Int(_)) => bool_value(true)
            (Int(_), BigInt(_)) => bool_value(true)
            (Int(_), Rat(_, _)) => bool_value(true)
            (Int(_), BigRat(_, _)) => bool_value(true)
            (BigInt(_), Int(_)) => bool_value(true)
            (BigInt(_), BigInt(_)) => bool_value(true)
            (BigInt(_), Rat(_, _)) => bool_value(true)
            (BigInt(_), BigRat(_, _)) => bool_value(true)
            (Rat(_, _), Int(_)) => bool_value(true)
            (Rat(_, _), BigInt(_)) => bool_value(true)
            (Rat(_, _), Rat(_, _)) => bool_value(true)
            (Rat(_, _), BigRat(_, _)) => bool_value(true)
            (BigRat(_, _), Int(_)) => bool_value(true)
            (BigRat(_, _), BigInt(_)) => bool_value(true)
            (BigRat(_, _), Rat(_, _)) => bool_value(true)
            (BigRat(_, _), BigRat(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::InexactP =>
      match args {
        [@core.Value::Datum(Float(_))] => bool_value(true)
        [@core.Value::Datum(Complex(real, imag))] =>
          match (real.val, imag.val) {
            (Float(_), _) => bool_value(true)
            (_, Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ZeroP =>
      match args {
        [@core.Value::Datum(Int(n))] => bool_value(n == 0)
        [@core.Value::Datum(Rat(n, _))] => bool_value(n == 0)
        [@core.Value::Datum(BigInt(n))] => bool_value(n.is_zero())
        [@core.Value::Datum(BigRat(n, _))] => bool_value(n.is_zero())
        [@core.Value::Datum(Float(n))] => bool_value(n == 0.0)
        [@core.Value::Datum(Complex(real, imag))] =>
          bool_value(datum_is_zero(real.val) && datum_is_zero(imag.val))
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::PositiveP =>
      match args {
        [value] => {
          let value = datum_to_real(value_as_number(value))
          match value {
            Int(n) => bool_value(n > 0)
            Rat(n, _) => bool_value(n > 0)
            BigInt(n) => bool_value(n.compare_int(0) > 0)
            BigRat(n, _) => bool_value(n.compare_int(0) > 0)
            Float(n) => bool_value(n > 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::NegativeP =>
      match args {
        [value] => {
          let value = datum_to_real(value_as_number(value))
          match value {
            Int(n) => bool_value(n < 0)
            Rat(n, _) => bool_value(n < 0)
            BigInt(n) => bool_value(n.compare_int(0) < 0)
            BigRat(n, _) => bool_value(n.compare_int(0) < 0)
            Float(n) => bool_value(n < 0.0)
            _ => raise @core.EvalError("type error: number expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::OddP =>
      match args {
        [value] => {
          let value = match value {
            @core.Value::Datum(Int(n)) => @core.Datum::Int(n)
            @core.Value::Datum(BigInt(n)) => @core.Datum::BigInt(n)
            @core.Value::Datum(Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Int(n) => @core.Datum::Int(n)
                  BigInt(n) => @core.Datum::BigInt(n)
                  _ =>
                    raise @core.EvalError("type error: exact integer expected")
                }
              } else {
                raise @core.EvalError("type error: exact integer expected")
              }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
          match value {
            Int(n) => bool_value(n % 2 != 0)
            BigInt(n) =>
              bool_value(!bigint_is_zero(n % bigint_from_int(2)))
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EvenP =>
      match args {
        [value] => {
          let value = match value {
            @core.Value::Datum(Int(n)) => @core.Datum::Int(n)
            @core.Value::Datum(BigInt(n)) => @core.Datum::BigInt(n)
            @core.Value::Datum(Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Int(n) => @core.Datum::Int(n)
                  BigInt(n) => @core.Datum::BigInt(n)
                  _ =>
                    raise @core.EvalError("type error: exact integer expected")
                }
              } else {
                raise @core.EvalError("type error: exact integer expected")
              }
            _ => raise @core.EvalError("type error: exact integer expected")
          }
          match value {
            Int(n) => bool_value(n % 2 == 0)
            BigInt(n) =>
              bool_value(bigint_is_zero(n % bigint_from_int(2)))
            _ => raise @core.EvalError("type error: exact integer expected")
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::FiniteP =>
      match args {
        [@core.Value::Datum(Float(f))] => bool_value(float_is_finite(f))
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))] => bool_value(true)
        [@core.Value::Datum(Complex(real, imag))] =>
          if datum_is_zero(imag.val) {
            match real.val {
              Float(f) => bool_value(float_is_finite(f))
              Int(_)
              | BigInt(_)
              | Rat(_, _)
              | BigRat(_, _) => bool_value(true)
              _ => raise @core.EvalError("type error: number expected")
            }
          } else {
            raise @core.EvalError("type error: real expected")
          }
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::InfiniteP =>
      match args {
        [@core.Value::Datum(Float(f))] => bool_value(float_is_infinite(f))
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))] => bool_value(false)
        [@core.Value::Datum(Complex(real, imag))] =>
          if datum_is_zero(imag.val) {
            match real.val {
              Float(f) => bool_value(float_is_infinite(f))
              Int(_)
              | BigInt(_)
              | Rat(_, _)
              | BigRat(_, _) => bool_value(false)
              _ => raise @core.EvalError("type error: number expected")
            }
          } else {
            raise @core.EvalError("type error: real expected")
          }
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::NanP =>
      match args {
        [@core.Value::Datum(Float(f))] => bool_value(float_is_nan(f))
        [@core.Value::Datum(Int(_))]
        | [@core.Value::Datum(BigInt(_))]
        | [@core.Value::Datum(Rat(_, _))]
        | [@core.Value::Datum(BigRat(_, _))] => bool_value(false)
        [@core.Value::Datum(Complex(real, imag))] =>
          if datum_is_zero(imag.val) {
            match real.val {
              Float(f) => bool_value(float_is_nan(f))
              Int(_)
              | BigInt(_)
              | Rat(_, _)
              | BigRat(_, _) => bool_value(false)
              _ => raise @core.EvalError("type error: number expected")
            }
          } else {
            raise @core.EvalError("type error: real expected")
          }
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ProcedureP =>
      match args {
        [@core.Value::Primitive(_)]
        | [@core.Value::Closure(_)]
        | [@core.Value::CaseClosure(_)]
        | [@core.Value::GuardHandler(_)]
        | [@core.Value::Parameter(_)]
        | [@core.Value::Continuation(_)]
        | [@core.Value::RecordProc(_)]
        | [@core.Value::ConditionProc(_)]
        | [@core.Value::EnumSetProc(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_symbol_syntax_predicate_primitive dispatch")
  }
}

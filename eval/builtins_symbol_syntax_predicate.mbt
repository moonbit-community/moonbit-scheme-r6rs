///|
/// Apply symbol, syntax, hash, and predicate primitives.
fn apply_symbol_syntax_predicate_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    Eq
    | Eqv
    | Equal
    | SymbolP
    | SymbolEq
    | IdentifierP
    | SyntaxP
    | FreeIdentifierEq
    | BoundIdentifierEq
    | SymbolToString
    | StringToSymbol
    | StringHash
    | StringCiHash
    | SymbolHash
    | EqualHash
    | SyntaxToDatum
    | DatumToSyntax
    | BooleanP
    | BooleanEq
    | NumberP
    | IntegerP
    | ExactIntegerP
    | RationalP
    | RealP
    | ComplexP
    | ExactP
    | InexactP
    | ZeroP
    | PositiveP
    | NegativeP
    | OddP
    | EvenP
    | FiniteP
    | InfiniteP
    | NanP
    | ProcedureP
    => Some(apply_symbol_syntax_predicate_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply symbol, syntax, hash, and predicate primitives (core match).
fn apply_symbol_syntax_predicate_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    Eq =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    Eqv =>
      match args {
        [left, right] => bool_value(value_eqv(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    Equal =>
      match args {
        [left, right] => bool_value(value_equal(left, right))
        _ => raise arity_mismatch(2, args.length())
      }
    SymbolP =>
      match args {
        [Datum(Symbol(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    SymbolEq => bool_value(compare_chain_symbol(args))
    IdentifierP =>
      match args {
        [value] => bool_value(is_identifier_value(value))
        _ => raise arity_mismatch(1, args.length())
      }
    SyntaxP =>
      match args {
        [SyntaxObject(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    FreeIdentifierEq =>
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
    BoundIdentifierEq =>
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
    SymbolToString =>
      match args {
        [Datum(Symbol(name))] =>
          @core.Value::Datum(String(Ref::new(name)))
        [_] => raise @core.EvalError("type error: symbol expected")
        _ => raise arity_mismatch(1, args.length())
      }
    StringToSymbol =>
      match args {
        [Datum(String(name))] =>
          @core.Value::Datum(Symbol(name.val))
        [_] => raise @core.EvalError("type error: string expected")
        _ => raise arity_mismatch(1, args.length())
      }
    StringHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          @core.Value::Datum(Int(hash_string_value(s)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    StringCiHash =>
      match args {
        [value] => {
          let s = value_as_string(value)
          let folded = @core.UnicodeString::new(s).foldcase().into_string()
          @core.Value::Datum(Int(hash_string_value(folded)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    SymbolHash =>
      match args {
        [value] => {
          let name = value_as_symbol(value)
          @core.Value::Datum(Int(hash_string_value(name)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EqualHash =>
      match args {
        [value] => {
          let repr = @runtime.value_to_string(value)
          @core.Value::Datum(Int(hash_string_value(repr)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    SyntaxToDatum =>
      match args {
        [SyntaxObject(obj)] =>
          @core.Value::Datum(@runtime.strip_syntax_datum(obj.datum))
        [Datum(d)] => @core.Value::Datum(@runtime.strip_syntax_datum(d))
        [_] => raise @core.EvalError("type error: syntax expected")
        _ => raise arity_mismatch(1, args.length())
      }
    DatumToSyntax =>
      match args {
        [context, base] | [context, base, _] => {
          let scopes = match context {
            SyntaxObject(obj) => obj.scopes
            _ => []
          }
          let context_binding = match context {
            SyntaxObject(obj) => obj.binding_id
            _ => None
          }
          let base = match base {
            SyntaxObject(obj) => obj.datum
            Datum(d) => d
            _ => raise @core.EvalError("type error: datum expected")
          }
          let template_name = match context {
            SyntaxObject(obj) =>
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
          @core.Value::SyntaxObject(
            @core.SyntaxObject::new(datum, scopes, binding_id),
          )
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 or 3 got \{args.length()}",
          )
      }
    BooleanP =>
      match args {
        [Datum(Bool(_))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    BooleanEq => bool_value(compare_chain_bool(args))
    NumberP =>
      match args {
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))]
        | [Datum(Float(_))]
        | [Datum(Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    IntegerP =>
      match args {
        [Datum(datum)] => bool_value(datum_is_integer_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    ExactIntegerP =>
      match args {
        [Datum(datum)] => bool_value(datum_is_exact_integer(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RationalP =>
      match args {
        [Datum(datum)] => bool_value(datum_is_rational_valued(datum))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RealP =>
      match args {
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))]
        | [Datum(Float(_))] => bool_value(true)
        [Datum(Complex(_, imag))] =>
          bool_value(datum_is_zero(imag.val))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    ComplexP =>
      match args {
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))]
        | [Datum(Float(_))]
        | [Datum(Complex(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    ExactP =>
      match args {
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))] => bool_value(true)
        [Datum(Complex(real, imag))] =>
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
    InexactP =>
      match args {
        [Datum(Float(_))] => bool_value(true)
        [Datum(Complex(real, imag))] =>
          match (real.val, imag.val) {
            (Float(_), _) => bool_value(true)
            (_, Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    ZeroP =>
      match args {
        [Datum(Int(n))] => bool_value(n == 0)
        [Datum(Rat(n, _))] => bool_value(n == 0)
        [Datum(BigInt(n))] => bool_value(n.is_zero())
        [Datum(BigRat(n, _))] => bool_value(n.is_zero())
        [Datum(Float(n))] => bool_value(n == 0.0)
        [Datum(Complex(real, imag))] =>
          bool_value(datum_is_zero(real.val) && datum_is_zero(imag.val))
        [_] => raise @core.EvalError("type error: number expected")
        _ => raise arity_mismatch(1, args.length())
      }
    PositiveP =>
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
    NegativeP =>
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
    OddP =>
      match args {
        [value] => {
          let value = match value {
            Datum(Int(n)) => @core.Datum::Int(n)
            Datum(BigInt(n)) => @core.Datum::BigInt(n)
            Datum(Complex(real, imag)) =>
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
    EvenP =>
      match args {
        [value] => {
          let value = match value {
            Datum(Int(n)) => @core.Datum::Int(n)
            Datum(BigInt(n)) => @core.Datum::BigInt(n)
            Datum(Complex(real, imag)) =>
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
    FiniteP =>
      match args {
        [Datum(Float(f))] => bool_value(float_is_finite(f))
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))] => bool_value(true)
        [Datum(Complex(real, imag))] =>
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
    InfiniteP =>
      match args {
        [Datum(Float(f))] => bool_value(float_is_infinite(f))
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))] => bool_value(false)
        [Datum(Complex(real, imag))] =>
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
    NanP =>
      match args {
        [Datum(Float(f))] => bool_value(float_is_nan(f))
        [Datum(Int(_))]
        | [Datum(BigInt(_))]
        | [Datum(Rat(_, _))]
        | [Datum(BigRat(_, _))] => bool_value(false)
        [Datum(Complex(real, imag))] =>
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
    ProcedureP =>
      match args {
        [Primitive(_)]
        | [Closure(_)]
        | [CaseClosure(_)]
        | [GuardHandler(_)]
        | [Parameter(_)]
        | [Continuation(_)]
        | [RecordProc(_)]
        | [ConditionProc(_)]
        | [EnumSetProc(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_symbol_syntax_predicate_primitive dispatch")
  }
}

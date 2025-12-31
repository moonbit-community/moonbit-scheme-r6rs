///|
/// Apply a primitive to evaluated arguments.
fn apply_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match apply_numeric_primitive(prim, args) {
    Some(value) => value
    None =>
      match apply_record_condition_primitive(prim, args) {
        Some(value) => value
        None =>
          match apply_hashtable_enumset_primitive(prim, args) {
            Some(value) => value
            None =>
              match apply_pair_list_primitive(prim, args) {
                Some(value) => value
                None =>
                  match apply_char_string_primitive(prim, args) {
                    Some(value) => value
                    None =>
                      match apply_vector_primitive(prim, args) {
                        Some(value) => value
                        None =>
                          match apply_bytevector_primitive(prim, args) {
                            Some(value) => value
                            None => apply_primitive_core(prim, args)
                          }
                      }
                  }
              }
          }
      }
  }
}

///|
/// Apply primitives not handled by specialized dispatchers.
fn apply_primitive_core(
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
            // invariant : items.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : items.length() >= 0
            let (name, scopes, binding_id) = match datum_unlabel(item) {
              Datum::Symbol(name) => (name, [], None)
              Datum::Value(Value::SyntaxObject(obj)) =>
                match datum_unlabel(obj.datum) {
                  Datum::Symbol(name) => {
                    let copied : Array[Int] = []
                    for scope in obj.scopes {
                      // invariant : obj.scopes.length() >= 0
                      // TODO(decreases) : loop index not exposed; possible bug
                      // assert : obj.scopes.length() >= 0
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
    _ =>
      raise @core.EvalError(
        "internal error: primitive should be handled by specialized dispatcher",
      )
  }
}

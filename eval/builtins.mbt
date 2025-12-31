///|
/// Apply a primitive to evaluated arguments.
fn apply_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match apply_numeric_primitive(prim, args) {
    Some(value) => value
    None =>
      match apply_symbol_syntax_predicate_primitive(prim, args) {
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
}

///|
/// Apply primitives not handled by specialized dispatchers.
fn apply_primitive_core(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
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

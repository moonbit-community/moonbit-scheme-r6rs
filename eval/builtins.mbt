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
      match args {
        [value] => {
          let port = get_current_output_port()
          port_write(port, display_string(value))
          Value::Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          port_write(port, display_string(value))
          Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Primitive::Write =>
      match args {
        [value] => {
          let port = get_current_output_port()
          port_write(port, value_to_string(value))
          Value::Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          port_write(port, value_to_string(value))
          Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Primitive::Newline =>
      match args {
        [] => {
          let port = get_current_output_port()
          port_write(port, "\n")
          Value::Void
        }
        [port_value] => {
          let port = value_as_port(port_value)
          port_write(port, "\n")
          Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 0 or 1 got \{args.length()}",
          )
      }
    Primitive::OpenOutputString =>
      match args {
        [] => Value::Port(new_output_string_port())
        _ => raise arity_mismatch(0, args.length())
      }
    Primitive::GetOutputString =>
      match args {
        [port_value] => {
          let port = value_as_port(port_value)
          Value::Datum(Datum::String(Ref::new(port_get_output_string(port))))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CurrentOutputPort =>
      match args {
        [] => Value::Port(get_current_output_port())
        _ => raise arity_mismatch(0, args.length())
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
      match args {
        [value] => bool_value(is_false(value))
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [proc_value] => {
          if !is_procedure_value(proc_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          proc_value
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::GenerateTemporaries =>
      match args {
        [datum_value] => {
          let datum = value_as_datum(datum_value)
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
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [value] =>
          match value {
            Value::Promise(_) => bool_value(true)
            _ => bool_value(false)
          }
        _ => raise arity_mismatch(1, args.length())
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

///|
/// @core.Primitive::Apply a primitive to evaluated arguments.
fn apply_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
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
/// @core.Primitive::Apply primitives not handled by specialized dispatchers.
fn apply_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    @core.Primitive::Display =>
      match args {
        [value] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, display_string(value))
          @core.Value::Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, display_string(value))
          @core.Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::Write =>
      match args {
        [value] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, @runtime.value_to_string(value))
          @core.Value::Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, @runtime.value_to_string(value))
          @core.Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::Newline =>
      match args {
        [] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, "\n")
          @core.Value::Void
        }
        [port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, "\n")
          @core.Value::Void
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 0 or 1 got \{args.length()}",
          )
      }
    @core.Primitive::OpenOutputString =>
      match args {
        [] => @core.Value::Port(@runtime.new_output_string_port())
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::GetOutputString =>
      match args {
        [port_value] => {
          let port = value_as_port(port_value)
          @core.Value::Datum(String(Ref::new(@runtime.port_get_output_string(port))))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::CurrentOutputPort =>
      match args {
        [] => @core.Value::Port(@runtime.get_current_output_port())
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::WithExceptionHandler =>
      raise @core.EvalError(
        "internal error: with-exception-handler should be handled in apply_proc",
      )
    @core.Primitive::Raise =>
      raise @core.EvalError(
        "internal error: raise should be handled in apply_proc",
      )
    @core.Primitive::RaiseContinuable =>
      raise @core.EvalError(
        "internal error: raise-continuable should be handled in apply_proc",
      )
    @core.Primitive::Error =>
      raise @core.EvalError(
        "internal error: error should be handled in apply_proc",
      )
    @core.Primitive::AssertionViolation =>
      raise @core.EvalError(
        "internal error: assertion-violation should be handled in apply_proc",
      )
    @core.Primitive::ImplementationRestrictionViolation =>
      raise @core.EvalError(
        "internal error: implementation-restriction-violation should be handled in apply_proc",
      )
    @core.Primitive::UndefinedViolation =>
      raise @core.EvalError(
        "internal error: undefined-violation should be handled in apply_proc",
      )
    @core.Primitive::SyntaxViolation =>
      raise @core.EvalError(
        "internal error: syntax-violation should be handled in apply_proc",
      )
    @core.Primitive::Not =>
      match args {
        [value] => bool_value(is_false(value))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Apply =>
      // handled by apply_proc to allow proper argument flattening
      raise @core.EvalError(
        "internal error: apply should be handled in apply_proc",
      )
    @core.Primitive::CallCC =>
      raise @core.EvalError(
        "internal error: call/cc should be handled in apply_proc",
      )
    @core.Primitive::Map =>
      raise @core.EvalError(
        "internal error: map should be handled in apply_proc",
      )
    @core.Primitive::ForEach =>
      raise @core.EvalError(
        "internal error: for-each should be handled in apply_proc",
      )
    @core.Primitive::VectorMap =>
      raise @core.EvalError(
        "internal error: vector-map should be handled in apply_proc",
      )
    @core.Primitive::VectorForEach =>
      raise @core.EvalError(
        "internal error: vector-for-each should be handled in apply_proc",
      )
    @core.Primitive::StringMap =>
      raise @core.EvalError(
        "internal error: string-map should be handled in apply_proc",
      )
    @core.Primitive::StringForEach =>
      raise @core.EvalError(
        "internal error: string-for-each should be handled in apply_proc",
      )
    @core.Primitive::Values => @core.Value::Values(args)
    @core.Primitive::MakeVariableTransformer =>
      match args {
        [proc_value] => {
          if !@runtime.is_procedure_value(proc_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          proc_value
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::GenerateTemporaries =>
      match args {
        [datum_value] => {
          let datum = value_as_datum(datum_value)
          let items = datum_list_to_array(datum)
          let results : Array[@core.Datum] = []
          for item in items {
            let (name, scopes, binding_id) = match @runtime.datum_unlabel(item) {
              Symbol(name) => (name, [], None)
              Value(SyntaxObject(obj)) =>
                if @runtime.datum_unlabel(obj.datum) is Symbol(name) {
                  let copied : Array[Int] = []
                  for scope in obj.scopes {
                    copied.push(scope)
                  }
                  (name, copied, obj.binding_id)
                } else {
                  raise @core.EvalError("type error: identifier expected")
                }
              _ => raise @core.EvalError("type error: identifier expected")
            }
            let datum = @core.Datum::Symbol(@runtime.gensym(name))
            let temp = @core.Value::SyntaxObject(
              @core.SyntaxObject::new(datum, scopes, binding_id),
            )
            results.push(value_to_datum_element(temp))
          }
          @core.Value::Datum(list_from_array(results))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::CallWithValues =>
      raise @core.EvalError(
        "internal error: call-with-values should be handled in apply_proc",
      )
    @core.Primitive::MakeParameter =>
      raise @core.EvalError(
        "internal error: make-parameter should be handled in apply_proc",
      )
    @core.Primitive::DynamicWind =>
      raise @core.EvalError(
        "internal error: dynamic-wind should be handled in apply_proc",
      )
    @core.Primitive::Eval =>
      raise @core.EvalError(
        "internal error: eval should be handled in apply_proc",
      )
    @core.Primitive::Environment =>
      raise @core.EvalError(
        "internal error: environment should be handled in apply_proc",
      )
    @core.Primitive::PromiseP =>
      match args {
        [value] =>
          match value {
            Promise(_) => bool_value(true)
            _ => bool_value(false)
          }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::MakePromise =>
      raise @core.EvalError(
        "internal error: make-promise should be handled in apply_proc",
      )
    @core.Primitive::Force =>
      raise @core.EvalError(
        "internal error: force should be handled in apply_proc",
      )
    _ =>
      raise @core.EvalError(
        "internal error: primitive should be handled by specialized dispatcher",
      )
  }
}

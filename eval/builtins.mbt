///|
/// Apply a primitive to evaluated arguments.
fn apply_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  if apply_numeric_primitive(prim, args) is Some(value) {
    return value
  }
  if apply_symbol_syntax_predicate_primitive(prim, args) is Some(value) {
    return value
  }
  if apply_record_condition_primitive(prim, args) is Some(value) {
    return value
  }
  if apply_hashtable_enumset_primitive(prim, args) is Some(value) {
    return value
  }
  if is_pair_list_primitive(prim) {
    return apply_pair_list_primitive(prim, args)
  }
  if apply_char_string_primitive(prim, args) is Some(value) {
    return value
  }
  if apply_vector_primitive(prim, args) is Some(value) {
    return value
  }
  if apply_bytevector_primitive(prim, args) is Some(value) {
    return value
  }
  apply_primitive_core(prim, args)
}

///|
/// Apply primitives not handled by specialized dispatchers.
fn apply_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    Display =>
      match args {
        [value] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, display_string(value))
          Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, display_string(value))
          Void
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Write =>
      match args {
        [value] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, @runtime.value_to_string(value))
          Void
        }
        [value, port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, @runtime.value_to_string(value))
          Void
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Newline =>
      match args {
        [] => {
          let port = @runtime.get_current_output_port()
          @runtime.port_write(port, "\n")
          Void
        }
        [port_value] => {
          let port = value_as_port(port_value)
          @runtime.port_write(port, "\n")
          Void
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 0 or 1 got \{args.length()}",
          )
      }
    OpenOutputString =>
      match args {
        [] => Port(@runtime.new_output_string_port())
        _ => raise arity_mismatch(0, args.length())
      }
    GetOutputString =>
      match args {
        [port_value] => {
          let port = value_as_port(port_value)
          Datum(String(Ref::new(@runtime.port_get_output_string(port))))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    CurrentOutputPort =>
      match args {
        [] => Port(@runtime.get_current_output_port())
        _ => raise arity_mismatch(0, args.length())
      }
    WithExceptionHandler =>
      raise EvalError(
        "internal error: with-exception-handler should be handled in apply_proc",
      )
    Raise =>
      raise EvalError("internal error: raise should be handled in apply_proc")
    RaiseContinuable =>
      raise EvalError(
        "internal error: raise-continuable should be handled in apply_proc",
      )
    Error =>
      raise EvalError("internal error: error should be handled in apply_proc")
    AssertionViolation =>
      raise EvalError(
        "internal error: assertion-violation should be handled in apply_proc",
      )
    ImplementationRestrictionViolation =>
      raise EvalError(
        "internal error: implementation-restriction-violation should be handled in apply_proc",
      )
    UndefinedViolation =>
      raise EvalError(
        "internal error: undefined-violation should be handled in apply_proc",
      )
    SyntaxViolation =>
      raise EvalError(
        "internal error: syntax-violation should be handled in apply_proc",
      )
    Not =>
      match args {
        [value] => bool_value(is_false(value))
        _ => raise arity_mismatch(1, args.length())
      }
    Apply =>
      // handled by apply_proc to allow proper argument flattening
      raise EvalError("internal error: apply should be handled in apply_proc")
    CallCC =>
      raise EvalError("internal error: call/cc should be handled in apply_proc")
    Map =>
      raise EvalError("internal error: map should be handled in apply_proc")
    ForEach =>
      raise EvalError(
        "internal error: for-each should be handled in apply_proc",
      )
    VectorMap =>
      raise EvalError(
        "internal error: vector-map should be handled in apply_proc",
      )
    VectorForEach =>
      raise EvalError(
        "internal error: vector-for-each should be handled in apply_proc",
      )
    StringMap =>
      raise EvalError(
        "internal error: string-map should be handled in apply_proc",
      )
    StringForEach =>
      raise EvalError(
        "internal error: string-for-each should be handled in apply_proc",
      )
    Values => Values(args)
    MakeVariableTransformer =>
      match args {
        [proc_value] => {
          if !@runtime.is_procedure_value(proc_value) {
            raise EvalError("type error: procedure expected")
          }
          proc_value
        }
        _ => raise arity_mismatch(1, args.length())
      }
    GenerateTemporaries =>
      match args {
        [datum_value] => {
          let datum = value_as_datum(datum_value)
          let items = datum_list_to_array(datum)
          let results : Array[@core.Datum] = []
          for item in items {
            let (name, scopes, binding_id) = match
              @runtime.datum_unlabel(item) {
              Symbol(name) => (name, [], None)
              Value(SyntaxObject(obj)) =>
                if @runtime.datum_unlabel(obj.datum) is Symbol(name) {
                  let copied : Array[Int] = []
                  for scope in obj.scopes {
                    copied.push(scope)
                  }
                  (name, copied, obj.binding_id)
                } else {
                  raise EvalError("type error: identifier expected")
                }
              _ => raise EvalError("type error: identifier expected")
            }
            let datum = Datum::Symbol(@runtime.gensym(name))
            let temp = Value::SyntaxObject(
              @core.SyntaxObject::new(datum, scopes, binding_id),
            )
            results.push(value_to_datum_element(temp))
          }
          Datum(list_from_array(results))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    CallWithValues =>
      raise EvalError(
        "internal error: call-with-values should be handled in apply_proc",
      )
    MakeParameter =>
      raise EvalError(
        "internal error: make-parameter should be handled in apply_proc",
      )
    DynamicWind =>
      raise EvalError(
        "internal error: dynamic-wind should be handled in apply_proc",
      )
    Eval =>
      raise EvalError("internal error: eval should be handled in apply_proc")
    Environment =>
      raise EvalError(
        "internal error: environment should be handled in apply_proc",
      )
    PromiseP =>
      match args {
        [value] =>
          match value {
            Promise(_) => bool_value(true)
            _ => bool_value(false)
          }
        _ => raise arity_mismatch(1, args.length())
      }
    MakePromise =>
      raise EvalError(
        "internal error: make-promise should be handled in apply_proc",
      )
    Force =>
      raise EvalError("internal error: force should be handled in apply_proc")
    _ =>
      raise EvalError(
        "internal error: primitive should be handled by specialized dispatcher",
      )
  }
}

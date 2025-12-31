///|
/// Apply record and condition primitives.
fn apply_record_condition_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::RecordP
    | Primitive::RecordRtd
    | Primitive::RecordTypeDescriptorP
    | Primitive::RecordConstructorDescriptorP
    | Primitive::RecordTypeName
    | Primitive::RecordTypeParent
    | Primitive::RecordTypeUid
    | Primitive::RecordTypeGenerativeP
    | Primitive::RecordTypeSealedP
    | Primitive::RecordTypeOpaqueP
    | Primitive::RecordTypeFieldNames
    | Primitive::RecordConstructorDescriptor
    | Primitive::RecordConstructor
    | Primitive::RecordTypeFieldMutableP
    | Primitive::RecordPredicate
    | Primitive::RecordAccessor
    | Primitive::RecordMutator
    | Primitive::MakeRecordTypeDescriptor
    | Primitive::MakeRecordConstructorDescriptor
    | Primitive::Condition
    | Primitive::ConditionP
    | Primitive::SimpleConditions
    | Primitive::ConditionPredicate
    | Primitive::ConditionAccessor
    => Some(apply_record_condition_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply record and condition primitives (core match).
fn apply_record_condition_primitive_core(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
    Primitive::RecordP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Record(record) => bool_value(!record.record_type.is_opaque)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordRtd =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Record(record) =>
              if record.record_type.is_opaque {
                raise @core.EvalError("record type is opaque")
              } else {
                match
                  lookup_record_type_descriptor_by_id(record.record_type.id) {
                  Some(desc) => Value::RecordTypeDescriptor(desc)
                  None =>
                    raise @core.EvalError("invalid record type descriptor")
                }
              }
            _ => raise @core.EvalError("type error: record expected")
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeDescriptorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::RecordTypeDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructorDescriptorP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::RecordConstructorDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeName =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::Datum(Datum::Symbol(desc.record_type.name))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeParent =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.parent {
            Some(parent) =>
              match lookup_record_type_descriptor(parent.name) {
                Some(parent_desc) => Value::RecordTypeDescriptor(parent_desc)
                None => Value::Datum(Datum::Bool(false))
              }
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeUid =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.uid {
            Some(uid) => Value::Datum(Datum::Symbol(uid))
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeGenerativeP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.uid is None)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeSealedP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_sealed)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeOpaqueP =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_opaque)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordTypeFieldNames =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          let items : Array[Datum] = []
          for field in desc.record_type.fields {
            // invariant : desc.record_type.fields.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : desc.record_type.fields.length() >= 0
            items.push(Datum::Symbol(field.name))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructorDescriptor =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordConstructorDescriptor(desc.constructor_desc)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordConstructor =>
      raise @core.EvalError("record-constructor requires evaluation context")
    Primitive::RecordTypeFieldMutableP =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          bool_value(field.mutable)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::RecordPredicate =>
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordProc(
            make_record_proc(RecordProcKind::Predicate(desc.record_type)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::RecordAccessor =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          Value::RecordProc(
            make_record_proc(RecordProcKind::Accessor(desc.record_type, idx)),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::RecordMutator =>
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          if !field.mutable {
            raise @core.EvalError("record field is immutable: \{field.name}")
          }
          Value::RecordProc(
            make_record_proc(RecordProcKind::Mutator(desc.record_type, idx)),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::MakeRecordTypeDescriptor =>
      match args.length() {
        6 => {
          let name = value_as_symbol(args[0])
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) => None
            Value::RecordTypeDescriptor(desc) => Some(desc)
            _ =>
              raise @core.EvalError(
                "type error: record type descriptor expected",
              )
          }
          let parent_type = match parent_desc {
            Some(desc) => Some(desc.record_type)
            None => None
          }
          match parent_type {
            Some(parent) =>
              if parent.is_sealed {
                raise @core.EvalError("record type is sealed")
              }
            None => ()
          }
          let uid = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            Value::Datum(Datum::Symbol(uid_name)) => Some(uid_name)
            _ => raise @core.EvalError("type error: symbol expected")
          }
          let is_sealed = value_as_bool(args[3])
          let is_opaque = value_as_bool(args[4])
          let new_fields = parse_field_specs(value_as_vector(args[5]))
          let fields : Array[RecordField] = []
          match parent_type {
            Some(parent) =>
              for field in parent.fields {
                // invariant : parent.fields.length() >= 0
                // TODO(decreases) : loop index not exposed; possible bug
                // assert : parent.fields.length() >= 0
                fields.push(field)
              }
            None => ()
          }
          for field in new_fields {
            // invariant : new_fields.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : new_fields.length() >= 0
            fields.push(field)
          }
          let effective_opaque = match parent_type {
            Some(parent) => parent.is_opaque || is_opaque
            None => is_opaque
          }
          match uid {
            Some(uid_name) =>
              match lookup_record_type_descriptor_by_uid(uid_name) {
                Some(existing_desc) => {
                  if !record_type_matches(
                      existing_desc.record_type,
                      parent_type,
                      is_sealed,
                      is_opaque,
                      fields,
                    ) {
                    raise @core.EvalError("invalid record type descriptor")
                  }
                  register_record_type_alias(name, existing_desc)
                  Value::RecordTypeDescriptor(existing_desc)
                }
                None => {
                  let record_type = make_record_type(
                    name, parent_type, is_sealed, effective_opaque, uid, fields,
                  )
                  let ctor_desc = default_constructor_descriptor(record_type)
                  let rtd = make_record_type_descriptor(record_type, ctor_desc)
                  let _ = register_record_type(name, rtd)
                  Value::RecordTypeDescriptor(rtd)
                }
              }
            None => {
              let record_type = make_record_type(
                name, parent_type, is_sealed, effective_opaque, uid, fields,
              )
              let ctor_desc = default_constructor_descriptor(record_type)
              let rtd = make_record_type_descriptor(record_type, ctor_desc)
              let _ = register_record_type(name, rtd)
              Value::RecordTypeDescriptor(rtd)
            }
          }
        }
        n => raise arity_mismatch(6, n)
      }
    Primitive::MakeRecordConstructorDescriptor =>
      match args.length() {
        3 => {
          let rtd = value_as_record_type_descriptor(args[0])
          let record_type = rtd.record_type
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) =>
              match record_type.parent {
                Some(parent) => Some(default_constructor_descriptor(parent))
                None => None
              }
            _ => Some(value_as_record_constructor_descriptor(args[1]))
          }
          match record_type.parent {
            Some(parent) =>
              match parent_desc {
                Some(desc) =>
                  if desc.record_type.id != parent.id {
                    raise @core.EvalError(
                      "invalid record constructor descriptor",
                    )
                  }
                None =>
                  raise @core.EvalError("invalid record constructor descriptor")
              }
            None =>
              match parent_desc {
                Some(_) =>
                  raise @core.EvalError("invalid record constructor descriptor")
                None => ()
              }
          }
          let protocol = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            value => {
              if !is_procedure_value(value) {
                raise @core.EvalError("type error: procedure expected")
              }
              Some(value)
            }
          }
          match (protocol, parent_desc) {
            (None, Some(desc)) =>
              match desc.protocol {
                Some(_) =>
                  raise @core.EvalError("invalid record constructor descriptor")
                None => ()
              }
            _ => ()
          }
          Value::RecordConstructorDescriptor(
            make_record_constructor_descriptor(
              record_type, parent_desc, protocol,
            ),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    Primitive::Condition => {
      if args.length() == 0 {
        raise arity_mismatch(1, 0)
      }
      let base = condition_base_type()
      let components : Array[Record] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        let parts = condition_components(arg, base)
        for record in parts {
          // invariant : parts.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : parts.length() >= 0
          components.push(record)
        }
      }
      if components.length() == 1 {
        Value::Record(components[0])
      } else {
        Value::Datum(Datum::Condition(make_condition(components)))
      }
    }
    Primitive::ConditionP =>
      match args.length() {
        1 => {
          let base = condition_base_type()
          let ok = condition_components_opt(args[0], base) is Some(_)
          bool_value(ok)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::SimpleConditions =>
      match args.length() {
        1 => {
          let base = condition_base_type()
          let components = condition_components(args[0], base)
          let items : Array[Datum] = []
          for record in components {
            // invariant : components.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : components.length() >= 0
            items.push(Datum::Record(record))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ConditionPredicate =>
      match args.length() {
        1 => {
          let desc = value_as_condition_type_descriptor(args[0])
          Value::ConditionProc(
            make_condition_proc(ConditionProcKind::Predicate(desc.record_type)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::ConditionAccessor =>
      match args.length() {
        2 => {
          let desc = value_as_condition_type_descriptor(args[0])
          let (accessor_type, idx) = record_accessor_info(args[1])
          if accessor_type.id != desc.record_type.id {
            raise @core.EvalError("type error: record accessor expected")
          }
          Value::ConditionProc(
            make_condition_proc(
              ConditionProcKind::Accessor(desc.record_type, idx),
            ),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    _ =>
      raise @core.EvalError("internal error: apply_record_condition_primitive dispatch")
  }
}

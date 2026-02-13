///|
/// Apply record and condition primitives.
fn apply_record_condition_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    RecordP
    | RecordRtd
    | RecordTypeDescriptorP
    | RecordConstructorDescriptorP
    | RecordTypeName
    | RecordTypeParent
    | RecordTypeUid
    | RecordTypeGenerativeP
    | RecordTypeSealedP
    | RecordTypeOpaqueP
    | RecordTypeFieldNames
    | RecordConstructorDescriptor
    | RecordConstructor
    | RecordTypeFieldMutableP
    | RecordPredicate
    | RecordAccessor
    | RecordMutator
    | MakeRecordTypeDescriptor
    | MakeRecordConstructorDescriptor
    | Condition
    | ConditionP
    | SimpleConditions
    | ConditionPredicate
    | ConditionAccessor =>
      Some(apply_record_condition_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply record and condition primitives (core match).
fn apply_record_condition_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    RecordP =>
      match args {
        [Record(record)] => bool_value(!record.record_type.is_opaque)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RecordRtd =>
      match args {
        [Record(record)] =>
          if record.record_type.is_opaque {
            raise EvalError("record type is opaque")
          } else {
            match
              @runtime.lookup_record_type_descriptor_by_id(
                record.record_type.id,
              ) {
              Some(desc) => RecordTypeDescriptor(desc)
              None => raise EvalError("invalid record type descriptor")
            }
          }
        [_] => raise EvalError("type error: record expected")
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeDescriptorP =>
      match args {
        [RecordTypeDescriptor(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RecordConstructorDescriptorP =>
      match args {
        [RecordConstructorDescriptor(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeName =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          Datum(Symbol(desc.record_type.name))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeParent =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          match desc.record_type.parent {
            Some(parent) =>
              match @runtime.lookup_record_type_descriptor(parent.name) {
                Some(parent_desc) => RecordTypeDescriptor(parent_desc)
                None => Datum(Bool(false))
              }
            None => Datum(Bool(false))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeUid =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          match desc.record_type.uid {
            Some(uid) => Datum(Symbol(uid))
            None => Datum(Bool(false))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeGenerativeP =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          bool_value(desc.record_type.uid is None)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeSealedP =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          bool_value(desc.record_type.is_sealed)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeOpaqueP =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          bool_value(desc.record_type.is_opaque)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordTypeFieldNames =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          let items : Array[@core.Datum] = []
          for field in desc.record_type.fields {
            items.push(Symbol(field.name))
          }
          Datum(list_from_array(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordConstructorDescriptor =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          RecordConstructorDescriptor(desc.constructor_desc)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordConstructor =>
      raise EvalError("record-constructor requires evaluation context")
    RecordTypeFieldMutableP =>
      match args {
        [desc_value, idx_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          let idx = value_as_int_index(idx_value)
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          bool_value(field.mutable)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    RecordPredicate =>
      match args {
        [desc_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          RecordProc(@runtime.make_record_proc(Predicate(desc.record_type)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    RecordAccessor =>
      match args {
        [desc_value, idx_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          let idx = value_as_int_index(idx_value)
          check_index(idx, desc.record_type.fields.length())
          RecordProc(@runtime.make_record_proc(Accessor(desc.record_type, idx)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    RecordMutator =>
      match args {
        [desc_value, idx_value] => {
          let desc = value_as_record_type_descriptor(desc_value)
          let idx = value_as_int_index(idx_value)
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          if !field.mutable {
            raise EvalError("record field is immutable: \{field.name}")
          }
          RecordProc(@runtime.make_record_proc(Mutator(desc.record_type, idx)))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    MakeRecordTypeDescriptor =>
      match args {
        [
          name_value,
          parent_value,
          uid_value,
          sealed_value,
          opaque_value,
          fields_value,
        ] => {
          let name = value_as_symbol(name_value)
          let parent_desc = match parent_value {
            Datum(Bool(false)) => None
            RecordTypeDescriptor(desc) => Some(desc)
            _ => raise EvalError("type error: record type descriptor expected")
          }
          let parent_type = match parent_desc {
            Some(desc) => Some(desc.record_type)
            None => None
          }
          match parent_type {
            Some(parent) =>
              if parent.is_sealed {
                raise EvalError("record type is sealed")
              }
            None => ()
          }
          let uid = match uid_value {
            Datum(Bool(false)) => None
            Datum(Symbol(uid_name)) => Some(uid_name)
            _ => raise EvalError("type error: symbol expected")
          }
          let is_sealed = value_as_bool(sealed_value)
          let is_opaque = value_as_bool(opaque_value)
          let new_fields = parse_field_specs(value_as_vector(fields_value))
          let fields : Array[@core.RecordField] = []
          match parent_type {
            Some(parent) =>
              for field in parent.fields {
                fields.push(field)
              }
            None => ()
          }
          for field in new_fields {
            fields.push(field)
          }
          let effective_opaque = match parent_type {
            Some(parent) => parent.is_opaque || is_opaque
            None => is_opaque
          }
          match uid {
            Some(uid_name) =>
              match @runtime.lookup_record_type_descriptor_by_uid(uid_name) {
                Some(existing_desc) => {
                  if !record_type_matches(
                      existing_desc.record_type,
                      parent_type,
                      is_sealed,
                      is_opaque,
                      fields,
                    ) {
                    raise EvalError("invalid record type descriptor")
                  }
                  @runtime.register_record_type_alias(name, existing_desc)
                  RecordTypeDescriptor(existing_desc)
                }
                None => {
                  let record_type = @runtime.make_record_type(
                    name, parent_type, is_sealed, effective_opaque, uid, fields,
                  )
                  let ctor_desc = default_constructor_descriptor(record_type)
                  let rtd = @runtime.make_record_type_descriptor(
                    record_type, ctor_desc,
                  )
                  let _ = @runtime.register_record_type(name, rtd)
                  RecordTypeDescriptor(rtd)
                }
              }
            None => {
              let record_type = @runtime.make_record_type(
                name, parent_type, is_sealed, effective_opaque, uid, fields,
              )
              let ctor_desc = default_constructor_descriptor(record_type)
              let rtd = @runtime.make_record_type_descriptor(
                record_type, ctor_desc,
              )
              let _ = @runtime.register_record_type(name, rtd)
              RecordTypeDescriptor(rtd)
            }
          }
        }
        _ => raise arity_mismatch(6, args.length())
      }
    MakeRecordConstructorDescriptor =>
      match args {
        [rtd_value, parent_value, protocol_value] => {
          let rtd = value_as_record_type_descriptor(rtd_value)
          let record_type = rtd.record_type
          let parent_desc = match parent_value {
            Datum(Bool(false)) =>
              match record_type.parent {
                Some(parent) => Some(default_constructor_descriptor(parent))
                None => None
              }
            _ => Some(value_as_record_constructor_descriptor(parent_value))
          }
          match record_type.parent {
            Some(parent) =>
              match parent_desc {
                Some(desc) =>
                  if desc.record_type.id != parent.id {
                    raise EvalError("invalid record constructor descriptor")
                  }
                None => raise EvalError("invalid record constructor descriptor")
              }
            None =>
              match parent_desc {
                Some(_) =>
                  raise EvalError("invalid record constructor descriptor")
                None => ()
              }
          }
          let protocol = match protocol_value {
            Datum(Bool(false)) => None
            value => {
              if !@runtime.is_procedure_value(value) {
                raise EvalError("type error: procedure expected")
              }
              Some(value)
            }
          }
          match (protocol, parent_desc) {
            (None, Some(desc)) =>
              match desc.protocol {
                Some(_) =>
                  raise EvalError("invalid record constructor descriptor")
                None => ()
              }
            _ => ()
          }
          RecordConstructorDescriptor(
            @runtime.make_record_constructor_descriptor(
              record_type, parent_desc, protocol,
            ),
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Condition =>
      match args {
        [] => raise arity_mismatch(1, 0)
        _ => {
          let base = condition_base_type()
          let components : Array[@core.Record] = []
          for arg in args {
            let parts = condition_components(arg, base)
            components.push_iter(parts.iter())
          }
          if components.length() == 1 {
            Record(components[0])
          } else {
            Datum(Condition(@runtime.make_condition(components)))
          }
        }
      }
    ConditionP =>
      match args {
        [condition_value] => {
          let base = condition_base_type()
          let ok = condition_components_opt(condition_value, base) is Some(_)
          bool_value(ok)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    SimpleConditions =>
      match args {
        [condition_value] => {
          let base = condition_base_type()
          let components = condition_components(condition_value, base)
          let items : Array[@core.Datum] = components.map(record => {
            Record(record)
          })
          Datum(list_from_array(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    ConditionPredicate =>
      match args {
        [desc_value] => {
          let desc = value_as_condition_type_descriptor(desc_value)
          ConditionProc(
            @runtime.make_condition_proc(Predicate(desc.record_type)),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    ConditionAccessor =>
      match args {
        [desc_value, accessor_value] => {
          let desc = value_as_condition_type_descriptor(desc_value)
          let (accessor_type, idx) = record_accessor_info(accessor_value)
          if accessor_type.id != desc.record_type.id {
            raise EvalError("type error: record accessor expected")
          }
          ConditionProc(
            @runtime.make_condition_proc(Accessor(desc.record_type, idx)),
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    _ =>
      raise EvalError(
        "internal error: apply_record_condition_primitive dispatch",
      )
  }
}

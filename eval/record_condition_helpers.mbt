///|
/// Build a constructor descriptor chain for a record type.
fn default_constructor_descriptor(
  record_type : @core.RecordType,
) -> @core.RecordConstructorDescriptor {
  let parent_desc = record_type.parent.map(parent => {
    default_constructor_descriptor(parent)
  })
  @runtime.make_record_constructor_descriptor(record_type, parent_desc, None)
}

///|
/// Compare an existing record type against descriptor fields.
fn record_type_matches(
  existing : @core.RecordType,
  parent : @core.RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  fields : Array[@core.RecordField],
) -> Bool {
  if existing.is_sealed != is_sealed {
    return false
  }
  match (existing.parent, parent) {
    (None, None) => ()
    (Some(a), Some(b)) => if a.id != b.id { return false }
    _ => return false
  }
  let parent_is_opaque = match parent {
    Some(p) => p.is_opaque
    None => false
  }
  if !parent_is_opaque {
    if existing.is_opaque != is_opaque {
      return false
    }
  }
  if existing.fields.length() != fields.length() {
    return false
  }
  for i, expected in fields {
    let actual = existing.fields[i]
    if expected.name != actual.name || expected.mutable != actual.mutable {
      return false
    }
  }
  true
}

///|
/// Retrieve the base condition record type.
fn condition_base_type() -> @core.RecordType raise @core.EvalError {
  match @runtime.lookup_record_type_descriptor("&condition") {
    Some(desc) => desc.record_type
    None => raise EvalError("internal error: missing &condition")
  }
}

///|
/// Return the record component if the value is a condition record.
fn value_as_condition_record(
  value : @core.Value,
  base : @core.RecordType,
) -> @core.Record? {
  match value {
    Record(record) | Datum(Record(record)) =>
      if @runtime.record_type_is_a(record.record_type, base) {
        Some(record)
      } else {
        None
      }
    _ => None
  }
}

///|
/// Extract condition components if the value is a condition.
fn condition_components_opt(
  value : @core.Value,
  base : @core.RecordType,
) -> Array[@core.Record]? {
  match value {
    Datum(Condition(cond)) => Some(cond.components)
    _ => value_as_condition_record(value, base).map(record => [record])
  }
}

///|
/// Extract condition components or raise on type mismatch.
fn condition_components(
  value : @core.Value,
  base : @core.RecordType,
) -> Array[@core.Record] raise @core.EvalError {
  match condition_components_opt(value, base) {
    Some(items) => items
    None => raise EvalError("type error: condition expected")
  }
}

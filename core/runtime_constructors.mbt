///|
pub fn Port::new_output_string(id : Int, initial : String) -> Port {
  let buffer = Ref::Ref(initial)
  { id, kind: OutputString(buffer) }
}

///|
pub fn Winder::new_proc(id : Int, before : Value, after : Value) -> Winder {
  { id, kind: Proc(before, after) }
}

///|
pub fn Winder::new_params(id : Int, bindings : Array[ParamBinding]) -> Winder {
  { id, kind: Params(bindings) }
}

///|
pub fn RecordField::new(name : String, mutable : Bool) -> RecordField {
  { name, mutable }
}

///|
pub fn RecordType::new(
  id : Int,
  name : String,
  parent : RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  uid : String?,
  fields : Array[RecordField],
) -> RecordType {
  { id, name, parent, is_sealed, is_opaque, uid, fields }
}

///|
pub fn Record::new(
  id : Int,
  record_type : RecordType,
  values : Array[Value],
) -> Record {
  let fields = values.map(value => Ref::Ref(value))
  { id, record_type, fields }
}

///|
pub fn Condition::new(id : Int, components : Array[Record]) -> Condition {
  { id, components }
}

///|
pub fn HashtableEntry::new(key : Value, value : Value) -> HashtableEntry {
  { key, value: Ref(value) }
}

///|
pub fn Hashtable::new(
  id : Int,
  mutable : Bool,
  equiv : HashtableEquiv,
  hash : Value?,
  entries : Array[HashtableEntry],
) -> Hashtable {
  { id, mutable, equiv, hash, entries: Ref(entries) }
}

///|
pub fn EnumSet::new(
  id : Int,
  universe : Array[String],
  members : Array[Bool],
) -> EnumSet {
  { id, universe, members }
}

///|
pub fn EnumSetProc::new(id : Int, kind : EnumSetProcKind) -> EnumSetProc {
  { id, kind }
}

///|
pub fn RecordProc::new(id : Int, kind : RecordProcKind) -> RecordProc {
  { id, kind }
}

///|
pub fn ConditionProc::new(id : Int, kind : ConditionProcKind) -> ConditionProc {
  { id, kind }
}

///|
pub fn RecordConstructorDescriptor::new(
  id : Int,
  record_type : RecordType,
  parent_desc : RecordConstructorDescriptor?,
  protocol : Value?,
) -> RecordConstructorDescriptor {
  { id, record_type, parent_desc, protocol }
}

///|
pub fn RecordTypeDescriptor::new(
  id : Int,
  record_type : RecordType,
  constructor_desc : RecordConstructorDescriptor,
) -> RecordTypeDescriptor {
  { id, record_type, constructor_desc }
}

///|
pub fn Formals::new(params : Array[String], rest : String?) -> Formals {
  { params, rest }
}

///|
pub fn ParamBinding::new(
  param : Parameter,
  old_value : Value,
  new_value : Value,
) -> ParamBinding {
  { param, old_value, new_value }
}

///|
pub fn GuardInfo::new(
  condition : Value,
  resume_value : Value,
  handlers : Array[Value],
  raise_kont : Kont,
  continuable : Bool,
) -> GuardInfo {
  { condition, resume_value, handlers, raise_kont, continuable }
}

///|
pub fn Continuation::new(
  id : Int,
  kont : Kont,
  handlers : Array[Value],
  winds : Array[Winder],
) -> Continuation {
  { id, kont, handlers, winds }
}

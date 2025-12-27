///|
pub fn bool_value(value : Bool) -> Value {
  Value::Datum(Datum::Bool(value))
}

///|
pub(all) struct Library {
  exports : Map[String, Value]
}

///|
let port_counter : Ref[Int] = Ref::new(0)
let gensym_counter : Ref[Int] = Ref::new(0)
let syntax_context_counter : Ref[Int] = Ref::new(0)
let record_type_counter : Ref[Int] = Ref::new(0)
let record_counter : Ref[Int] = Ref::new(0)
let record_proc_counter : Ref[Int] = Ref::new(0)
let record_type_desc_counter : Ref[Int] = Ref::new(0)
let record_ctor_desc_counter : Ref[Int] = Ref::new(0)
let condition_counter : Ref[Int] = Ref::new(0)
let condition_proc_counter : Ref[Int] = Ref::new(0)
let hashtable_counter : Ref[Int] = Ref::new(0)
let enum_set_counter : Ref[Int] = Ref::new(0)
let enum_set_proc_counter : Ref[Int] = Ref::new(0)
let library_registry : Ref[Map[String, Library]] = Ref::new({})
let record_type_registry : Ref[Map[String, RecordTypeDescriptor]] = Ref::new({})
let record_type_uid_registry : Ref[Map[String, RecordTypeDescriptor]] = Ref::new({})
let record_type_id_registry : Ref[Map[Int, RecordTypeDescriptor]] = Ref::new({})

///|
pub fn gensym(base : String) -> String {
  let idx = gensym_counter.val
  gensym_counter.val = idx + 1
  "\{base}__gs\{idx}"
}

///|
pub fn next_syntax_context() -> Int {
  let id = syntax_context_counter.val
  syntax_context_counter.val = id + 1
  id
}

///|
fn next_port_id() -> Int {
  let id = port_counter.val
  port_counter.val = id + 1
  id
}

///|
fn next_record_type_id() -> Int {
  let id = record_type_counter.val
  record_type_counter.val = id + 1
  id
}

///|
fn next_record_id() -> Int {
  let id = record_counter.val
  record_counter.val = id + 1
  id
}

///|
fn next_record_proc_id() -> Int {
  let id = record_proc_counter.val
  record_proc_counter.val = id + 1
  id
}

///|
fn next_record_type_desc_id() -> Int {
  let id = record_type_desc_counter.val
  record_type_desc_counter.val = id + 1
  id
}

///|
fn next_record_ctor_desc_id() -> Int {
  let id = record_ctor_desc_counter.val
  record_ctor_desc_counter.val = id + 1
  id
}

///|
fn next_condition_id() -> Int {
  let id = condition_counter.val
  condition_counter.val = id + 1
  id
}

///|
fn next_condition_proc_id() -> Int {
  let id = condition_proc_counter.val
  condition_proc_counter.val = id + 1
  id
}

///|
pub fn next_hashtable_id() -> Int {
  let id = hashtable_counter.val
  hashtable_counter.val = id + 1
  id
}

///|
fn next_enum_set_id() -> Int {
  let id = enum_set_counter.val
  enum_set_counter.val = id + 1
  id
}

///|
fn next_enum_set_proc_id() -> Int {
  let id = enum_set_proc_counter.val
  enum_set_proc_counter.val = id + 1
  id
}

///|
pub fn make_record_type(
  name : String,
  parent : RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  uid : String?,
  fields : Array[RecordField]
) -> RecordType {
  RecordType::{ id: next_record_type_id(), name, parent, is_sealed, is_opaque, uid, fields }
}

///|
pub fn make_record_proc(kind : RecordProcKind) -> RecordProc {
  RecordProc::{ id: next_record_proc_id(), kind }
}

///|
pub fn make_condition_proc(kind : ConditionProcKind) -> ConditionProc {
  ConditionProc::{ id: next_condition_proc_id(), kind }
}

///|
pub fn make_record_constructor_descriptor(
  record_type : RecordType,
  parent_desc : RecordConstructorDescriptor?,
  protocol : Value?
) -> RecordConstructorDescriptor {
  RecordConstructorDescriptor::{
    id: next_record_ctor_desc_id(),
    record_type,
    parent_desc,
    protocol,
  }
}

///|
pub fn make_record_type_descriptor(
  record_type : RecordType,
  constructor_desc : RecordConstructorDescriptor
) -> RecordTypeDescriptor {
  RecordTypeDescriptor::{
    id: next_record_type_desc_id(),
    record_type,
    constructor_desc,
  }
}

///|
pub fn make_record_instance(record_type : RecordType, values : Array[Value]) -> Record {
  let fields : Array[Ref[Value]] = []
  for value in values {
    fields.push(Ref::new(value))
  }
  Record::{ id: next_record_id(), record_type, fields }
}

///|
pub fn make_condition(components : Array[Record]) -> Condition {
  Condition::{ id: next_condition_id(), components }
}

///|
pub fn make_hashtable(
  equiv : HashtableEquiv,
  hash : Value?,
  mutable : Bool,
) -> Hashtable {
  Hashtable::{
    id: next_hashtable_id(),
    mutable,
    equiv,
    hash,
    entries: Ref::new([]),
  }
}

///|
pub fn make_enum_set(universe : Array[String], members : Array[Bool]) -> EnumSet {
  EnumSet::{ id: next_enum_set_id(), universe, members }
}

///|
pub fn make_enum_set_proc(kind : EnumSetProcKind) -> EnumSetProc {
  EnumSetProc::{ id: next_enum_set_proc_id(), kind }
}

///|
pub fn enum_set_members_all_true(universe : Array[String]) -> Array[Bool] {
  let members : Array[Bool] = []
  for _ in universe {
    members.push(true)
  }
  members
}

///|
pub fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] {
  let members : Array[Bool] = []
  for _ in universe {
    members.push(false)
  }
  members
}

///|
pub fn enum_set_universe_equal(a : EnumSet, b : EnumSet) -> Bool {
  if a.universe.length() != b.universe.length() {
    return false
  }
  let mut i = 0
  while i < a.universe.length() {
    if a.universe[i] != b.universe[i] {
      return false
    }
    i = i + 1
  }
  true
}

///|
pub fn enum_set_index_of(universe : Array[String], name : String) -> Int? {
  let mut i = 0
  while i < universe.length() {
    if universe[i] == name {
      return Some(i)
    }
    i = i + 1
  }
  None
}

///|
pub fn enum_set_member_by_name(set : EnumSet, name : String) -> Bool {
  match enum_set_index_of(set.universe, name) {
    Some(idx) => set.members[idx]
    None => false
  }
}

///|
pub fn enum_set_from_names(
  universe : Array[String],
  names : Array[String],
) -> EnumSet raise EvalError {
  let members = enum_set_members_all_false(universe)
  for name in names {
    match enum_set_index_of(universe, name) {
      Some(idx) => members[idx] = true
      None => raise @core.EvalError("enum-set: unknown symbol")
    }
  }
  make_enum_set(universe, members)
}
///|
pub fn reset_library_registry() -> Unit {
  library_registry.val = {}
}

///|
pub fn reset_record_type_registry() -> Unit {
  record_type_registry.val = {}
  record_type_uid_registry.val = {}
  record_type_id_registry.val = {}
}

///|
pub fn register_record_type(name : String, desc : RecordTypeDescriptor) -> RecordTypeDescriptor {
  record_type_registry.val[name] = desc
  record_type_id_registry.val[desc.record_type.id] = desc
  match desc.record_type.uid {
    Some(uid) => record_type_uid_registry.val[uid] = desc
    None => ()
  }
  desc
}

///|
pub fn register_record_type_alias(name : String, desc : RecordTypeDescriptor) -> Unit {
  record_type_registry.val[name] = desc
  record_type_id_registry.val[desc.record_type.id] = desc
  match desc.record_type.uid {
    Some(uid) => record_type_uid_registry.val[uid] = desc
    None => ()
  }
}

///|
///|
pub fn lookup_record_type_descriptor(name : String) -> RecordTypeDescriptor? {
  record_type_registry.val.get(name)
}

///|
pub fn lookup_record_type_descriptor_by_uid(uid : String) -> RecordTypeDescriptor? {
  record_type_uid_registry.val.get(uid)
}

///|
pub fn lookup_record_type_descriptor_by_id(id : Int) -> RecordTypeDescriptor? {
  record_type_id_registry.val.get(id)
}

///|
pub fn default_constructor_descriptor(record_type : RecordType) -> RecordConstructorDescriptor {
  let parent_desc = match record_type.parent {
    Some(parent) => Some(default_constructor_descriptor(parent))
    None => None
  }
  make_record_constructor_descriptor(record_type, parent_desc, None)
}

///|
pub fn is_procedure_value(value : Value) -> Bool {
  match value {
    Value::Primitive(_)
    | Value::Closure(_)
    | Value::CaseClosure(_)
    | Value::Parameter(_)
    | Value::Continuation(_)
    | Value::RecordProc(_)
    | Value::ConditionProc(_)
    | Value::EnumSetProc(_) => true
    _ => false
  }
}

///|
pub fn record_type_matches(
  existing : RecordType,
  parent : RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  fields : Array[RecordField],
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
  let mut idx = 0
  while idx < fields.length() {
    let expected = fields[idx]
    let actual = existing.fields[idx]
    if expected.name != actual.name || expected.mutable != actual.mutable {
      return false
    }
    idx = idx + 1
  }
  true
}

///|
pub fn record_type_is_a(actual : RecordType, target : RecordType) -> Bool {
  let mut cur : RecordType? = Some(actual)
  while true {
    match cur {
      Some(rt) => {
        if rt.id == target.id {
          return true
        }
        cur = rt.parent
      }
      None => return false
    }
  }
  false
}

///|
pub fn register_library(name : String, exports : Map[String, Value]) -> Unit {
  library_registry.val[name] = Library::{ exports }
}

///|
pub fn lookup_library(name : String) -> Library? {
  library_registry.val.get(name)
}

///|
pub fn new_output_string_port() -> Port {
  let buffer = Ref::new("")
  Port::{ id: next_port_id(), kind: PortKind::OutputString(buffer) }
}

///|
let current_output_port : Ref[Port] = Ref::new(new_output_string_port())

///|
pub fn get_current_output_port() -> Port {
  current_output_port.val
}

///|
pub fn set_current_output_port(port : Port) -> Unit {
  current_output_port.val = port
}

///|
pub fn port_write(port : Port, text : String) -> Unit {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val = buffer.val + text
  }
}

///|
pub fn port_get_output_string(port : Port) -> String {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val
  }
}

///|
pub fn is_false(value : Value) -> Bool {
  match value {
    Value::Datum(Datum::Bool(false)) => true
    _ => false
  }
}

///|
pub fn value_as_datum(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => d
    Value::Record(record) => Datum::Record(record)
    _ => raise @core.EvalError("type error: datum expected")
  }
}

///|
pub fn value_to_datum_element(value : Value) -> Datum {
  match value {
    Value::Datum(d) => d
    Value::Record(record) => Datum::Record(record)
    _ => Datum::Value(value)
  }
}

///|
pub fn value_from_datum(datum : Datum) -> Value {
  match datum {
    Datum::Record(record) => Value::Record(record)
    Datum::Value(value) => value
    _ => Value::Datum(datum)
  }
}

///|
pub fn condition_base_type() -> RecordType raise EvalError {
  match lookup_record_type_descriptor("&condition") {
    Some(desc) => desc.record_type
    None => raise @core.EvalError("internal error: missing &condition")
  }
}

///|
fn value_as_condition_record(value : Value, base : RecordType) -> Record? {
  match value {
    Value::Record(record) =>
      if record_type_is_a(record.record_type, base) { Some(record) } else { None }
    Value::Datum(Datum::Record(record)) =>
      if record_type_is_a(record.record_type, base) { Some(record) } else { None }
    _ => None
  }
}

///|
pub fn condition_components_opt(value : Value, base : RecordType) -> Array[Record]? {
  match value {
    Value::Datum(Datum::Condition(cond)) => Some(cond.components)
    _ =>
      match value_as_condition_record(value, base) {
        Some(record) => Some([record])
        None => None
      }
  }
}

///|
pub fn condition_components(value : Value, base : RecordType) -> Array[Record] raise EvalError {
  match condition_components_opt(value, base) {
    Some(items) => items
    None => raise @core.EvalError("type error: condition expected")
  }
}

///|
///|
pub fn int_abs(value : Int) -> Int {
  if value < 0 { -value } else { value }
}

///|
pub fn gcd(a : Int, b : Int) -> Int {
  let mut x = int_abs(a)
  let mut y = int_abs(b)
  while y != 0 {
    let temp = x % y
    x = y
    y = temp
  }
  x
}

///|
pub fn normalize_rat(num : Int, den : Int) -> Datum? {
  if den == 0 {
    None
  } else if num == 0 {
    Some(Datum::Int(0))
  } else {
    let mut n = num
    let mut d = den
    if d < 0 {
      n = -n
      d = -d
    }
    let g = gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd == 1 {
      Some(Datum::Int(nn))
    } else {
      Some(Datum::Rat(nn, dd))
    }
  }
}

///|
pub fn bigint_from_int(value : Int) -> @bigint.BigInt {
  @bigint.BigInt::from_int(value)
}

///|
pub fn bigint_is_zero(value : @bigint.BigInt) -> Bool {
  value.is_zero()
}

///|
pub fn bigint_abs(value : @bigint.BigInt) -> @bigint.BigInt {
  if value.compare_int(0) < 0 { -value } else { value }
}

///|
pub fn bigint_to_int_option(value : @bigint.BigInt) -> Int? {
  let int_max = 2147483647
  let int_min = -2147483648
  if value.compare_int(int_max) > 0 || value.compare_int(int_min) < 0 {
    None
  } else {
    Some(value.to_int())
  }
}

///|
pub fn bigint_gcd(a : @bigint.BigInt, b : @bigint.BigInt) -> @bigint.BigInt {
  let mut x = bigint_abs(a)
  let mut y = bigint_abs(b)
  while !bigint_is_zero(y) {
    let temp = x % y
    x = y
    y = temp
  }
  x
}

///|
pub fn normalize_big_rat(num : @bigint.BigInt, den : @bigint.BigInt) -> Datum? {
  if bigint_is_zero(den) {
    None
  } else if bigint_is_zero(num) {
    Some(Datum::Int(0))
  } else {
    let mut n = num
    let mut d = den
    if d.compare_int(0) < 0 {
      n = -n
      d = -d
    }
    let g = bigint_gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd.compare_int(1) == 0 {
      Some(match bigint_to_int_option(nn) {
        Some(v) => Datum::Int(v)
        None => Datum::BigInt(nn)
      })
    } else {
      match (bigint_to_int_option(nn), bigint_to_int_option(dd)) {
        (Some(nv), Some(dv)) => Some(Datum::Rat(nv, dv))
        _ => Some(Datum::BigRat(nn, dd))
      }
    }
  }
}

///|
pub fn datum_unlabel(value : Datum) -> Datum {
  let mut cur = value
  let seen : Array[Int] = []
  while true {
    match cur {
      Datum::Label(id, cell) => {
        let mut dup = false
        for item in seen {
          if item == id {
            dup = true
            break
          }
        }
        if dup {
          return cur
        }
        seen.push(id)
        cur = cell.val
      }
      _ => return cur
    }
  }
  cur
}

///|
pub fn value_as_pair(value : Value) -> (Datum, Datum) raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Pair(a, b) => (a.val, b.val)
        _ => raise @core.EvalError("type error: pair expected")
      }
    _ => raise @core.EvalError("type error: pair expected")
  }
}

///|
pub fn value_as_pair_cells(value : Value) -> (Ref[Datum], Ref[Datum]) raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Pair(a, b) => (a, b)
        _ => raise @core.EvalError("type error: pair expected")
      }
    _ => raise @core.EvalError("type error: pair expected")
  }
}

///|
pub fn pair_new(car : Datum, cdr : Datum) -> Datum {
  Datum::Pair(Ref::new(car), Ref::new(cdr))
}

///|
pub fn datum_list_to_array(list : Datum) -> Array[Datum] raise EvalError {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match datum_unlabel(cur) {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a.val)
        cur = b.val
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  }
  items
}

///|
pub fn datum_list_to_value_array(list : Datum) -> Array[Value] raise EvalError {
  let items : Array[Value] = []
  let mut cur = list
  while true {
    match datum_unlabel(cur) {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(value_from_datum(a.val))
        cur = b.val
      }
      _ => raise @core.EvalError("type error: list expected")
    }
  }
  items
}

///|
pub fn list_from_array(items : Array[Datum]) -> Datum {
  let mut tail = Datum::Nil
  for item in items.rev_iter() {
    tail = pair_new(item, tail)
  }
  tail
}

///|
pub fn symbol_name(expr : Datum) -> String? {
  match expr {
    Datum::Symbol(name) => Some(name)
    Datum::Value(Value::SyntaxObject(obj)) => symbol_name(obj.datum)
    _ => None
  }
}

fn scopes_contains(scopes : Array[Int], scope : Int) -> Bool {
  for item in scopes {
    if item == scope {
      return true
    }
  }
  false
}

fn scopes_clone(scopes : Array[Int]) -> Array[Int] {
  let next : Array[Int] = []
  for item in scopes {
    next.push(item)
  }
  next
}

fn scopes_with_added(scopes : Array[Int], scope : Int) -> Array[Int] {
  let next = scopes_clone(scopes)
  if !scopes_contains(scopes, scope) {
    next.push(scope)
  }
  next
}

///|
pub fn strip_syntax_datum(datum : Datum) -> Datum {
  match datum {
    Datum::Value(Value::SyntaxObject(obj)) => strip_syntax_datum(obj.datum)
    Datum::Pair(a, b) => pair_new(strip_syntax_datum(a.val), strip_syntax_datum(b.val))
    Datum::Vector(items) => {
      let stripped : Array[Datum] = []
      for item in items {
        stripped.push(strip_syntax_datum(item))
      }
      Datum::Vector(stripped)
    }
    Datum::Complex(real, imag) =>
      Datum::Complex(Ref::new(strip_syntax_datum(real.val)), Ref::new(strip_syntax_datum(imag.val)))
    _ => datum
  }
}

///|
pub fn syntax_wrap_datum(datum : Datum, scopes : Array[Int]) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      Datum::Value(Value::SyntaxObject(SyntaxObject::{ datum: Datum::Symbol(name), scopes }))
    Datum::Pair(a, b) =>
      pair_new(syntax_wrap_datum(a.val, scopes), syntax_wrap_datum(b.val, scopes))
    Datum::Vector(items) => {
      let wrapped : Array[Datum] = []
      for item in items {
        wrapped.push(syntax_wrap_datum(item, scopes))
      }
      Datum::Vector(wrapped)
    }
    Datum::Complex(real, imag) =>
      Datum::Complex(Ref::new(syntax_wrap_datum(real.val, scopes)), Ref::new(syntax_wrap_datum(imag.val, scopes)))
    Datum::Value(Value::SyntaxObject(_)) => datum
    _ => datum
  }
}

///|
pub fn syntax_wrap_root(datum : Datum, scopes : Array[Int]) -> Datum {
  match datum {
    Datum::Symbol(_) => datum
    Datum::Value(Value::SyntaxObject(_)) => datum
    _ => syntax_wrap_datum(datum, scopes)
  }
}

///|
pub fn syntax_add_scope(datum : Datum, scope : Int) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{ datum: Datum::Symbol(name), scopes: [scope] }),
      )
    Datum::Value(Value::SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        Datum::Symbol(_) => obj.datum
        _ => syntax_add_scope(obj.datum, scope)
      }
      Datum::Value(Value::SyntaxObject(SyntaxObject::{ datum: inner, scopes: next_scopes }))
    }
    Datum::Pair(a, b) => pair_new(syntax_add_scope(a.val, scope), syntax_add_scope(b.val, scope))
    Datum::Vector(items) => {
      let wrapped : Array[Datum] = []
      for item in items {
        wrapped.push(syntax_add_scope(item, scope))
      }
      Datum::Vector(wrapped)
    }
    Datum::Complex(real, imag) =>
      Datum::Complex(Ref::new(syntax_add_scope(real.val, scope)), Ref::new(syntax_add_scope(imag.val, scope)))
    _ => datum
  }
}

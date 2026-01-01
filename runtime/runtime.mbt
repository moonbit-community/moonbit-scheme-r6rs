priv struct Counter {
  cell : Ref[Int]
}

///|
fn Counter::new() -> Counter {
  { cell: Ref::new(0) }
}

///|
fn Counter::next(self : Counter) -> Int {
  let id = self.cell.val
  self.cell.val = id + 1
  id
}

///|
let port_counter : Counter = Counter::new()

///|
let gensym_counter : Counter = Counter::new()

///|
let syntax_context_counter : Counter = Counter::new()

///|
let binding_counter : Counter = Counter::new()

///|
let record_type_counter : Counter = Counter::new()

///|
let record_counter : Counter = Counter::new()

///|
let record_proc_counter : Counter = Counter::new()

///|
let record_type_desc_counter : Counter = Counter::new()

///|
let record_ctor_desc_counter : Counter = Counter::new()

///|
let condition_counter : Counter = Counter::new()

///|
let condition_proc_counter : Counter = Counter::new()

///|
let hashtable_counter : Counter = Counter::new()

///|
let enum_set_counter : Counter = Counter::new()

///|
let enum_set_proc_counter : Counter = Counter::new()

///|
let library_registry : Ref[Map[String, Map[String, @core.Binding]]] = Ref::new({})

///|
let record_type_registry : Ref[Map[String, @core.RecordTypeDescriptor]] = Ref::new({})

///|
let record_type_uid_registry : Ref[Map[String, @core.RecordTypeDescriptor]] = Ref::new({},
)

///|
let record_type_id_registry : Ref[Map[Int, @core.RecordTypeDescriptor]] = Ref::new({})

///|
/// Generate a fresh symbol name with a gensym suffix.
///
/// # Example
/// ```mbt check
/// test "gensym prefix" {
///   let a = gensym("x")
///   let b = gensym("x")
///   inspect(a != b, content="true")
///   guard a is [.."x__gs", .._rest] else { fail("expected gensym prefix") }
/// }
/// ```
pub fn gensym(base : String) -> String {
  let idx = gensym_counter.next()
  "\{base}__gs\{idx}"
}

///|
/// Allocate a new syntax context id.
pub fn next_syntax_context() -> Int {
  syntax_context_counter.next()
}

///|
/// Allocate a new binding id.
fn next_binding_id() -> Int {
  binding_counter.next()
}

///|
fn next_port_id() -> Int {
  port_counter.next()
}

///|
fn next_record_type_id() -> Int {
  record_type_counter.next()
}

///|
fn next_record_id() -> Int {
  record_counter.next()
}

///|
fn next_record_proc_id() -> Int {
  record_proc_counter.next()
}

///|
fn next_record_type_desc_id() -> Int {
  record_type_desc_counter.next()
}

///|
fn next_record_ctor_desc_id() -> Int {
  record_ctor_desc_counter.next()
}

///|
fn next_condition_id() -> Int {
  condition_counter.next()
}

///|
fn next_condition_proc_id() -> Int {
  condition_proc_counter.next()
}

///|
/// Allocate a new hashtable id.
fn next_hashtable_id() -> Int {
  hashtable_counter.next()
}

///|
fn next_enum_set_id() -> Int {
  enum_set_counter.next()
}

///|
fn next_enum_set_proc_id() -> Int {
  enum_set_proc_counter.next()
}

///|
/// Construct a new record type descriptor.
pub fn make_record_type(
  name : String,
  parent : @core.RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  uid : String?,
  fields : Array[@core.RecordField],
) -> @core.RecordType {
  @core.RecordType::{
    id: next_record_type_id(),
    name,
    parent,
    is_sealed,
    is_opaque,
    uid,
    fields,
  }
}

///|
/// Allocate a record procedure wrapper.
pub fn make_record_proc(kind : @core.RecordProcKind) -> @core.RecordProc {
  @core.RecordProc::{ id: next_record_proc_id(), kind }
}

///|
/// Allocate a condition procedure wrapper.
pub fn make_condition_proc(kind : @core.ConditionProcKind) -> @core.ConditionProc {
  @core.ConditionProc::{ id: next_condition_proc_id(), kind }
}

///|
/// Construct a record constructor descriptor.
pub fn make_record_constructor_descriptor(
  record_type : @core.RecordType,
  parent_desc : @core.RecordConstructorDescriptor?,
  protocol : @core.Value?,
) -> @core.RecordConstructorDescriptor {
  @core.RecordConstructorDescriptor::{
    id: next_record_ctor_desc_id(),
    record_type,
    parent_desc,
    protocol,
  }
}

///|
/// Construct a record type descriptor wrapper.
pub fn make_record_type_descriptor(
  record_type : @core.RecordType,
  constructor_desc : @core.RecordConstructorDescriptor,
) -> @core.RecordTypeDescriptor {
  @core.RecordTypeDescriptor::{
    id: next_record_type_desc_id(),
    record_type,
    constructor_desc,
  }
}

///|
/// Create a record instance with boxed fields.
pub fn make_record_instance(
  record_type : @core.RecordType,
  values : Array[@core.Value],
) -> @core.Record {
  let fields = values.map((value) => Ref::new(value))
  @core.Record::{ id: next_record_id(), record_type, fields }
}

///|
/// Construct a condition with the given record components.
pub fn make_condition(components : Array[@core.Record]) -> @core.Condition {
  @core.Condition::{ id: next_condition_id(), components }
}

///|
/// Construct a new hashtable with equivalence and hash settings.
pub fn make_hashtable(
  equiv : @core.HashtableEquiv,
  hash : @core.Value?,
  mutable : Bool,
) -> @core.Hashtable {
  @core.Hashtable::{
    id: next_hashtable_id(),
    mutable,
    equiv,
    hash,
    entries: Ref::new([]),
  }
}

///|
/// Construct an enum set with a universe and membership bitmap.
pub fn make_enum_set(
  universe : Array[String],
  members : Array[Bool],
) -> @core.EnumSet {
  @core.EnumSet::{ id: next_enum_set_id(), universe, members }
}

///|
/// Allocate an enum-set procedure wrapper.
pub fn make_enum_set_proc(kind : @core.EnumSetProcKind) -> @core.EnumSetProc {
  @core.EnumSetProc::{ id: next_enum_set_proc_id(), kind }
}

///|
/// Create a membership bitmap with all values set to false.
fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] {
  enum_set_members_fill(universe, false)
}

///|
fn enum_set_members_fill(universe : Array[String], value : Bool) -> Array[Bool] {
  universe.map((_name) => value)
}

///|
/// Compare enum-set universes for equal ordering.
pub fn enum_set_universe_equal(a : @core.EnumSet, b : @core.EnumSet) -> Bool {
  if a.universe.length() != b.universe.length() {
    return false
  }
  for i, name in a.universe {
    if b.universe[i] != name {
      return false
    }
  }
  true
}

///|
/// Find the index of a symbol in an enum-set universe.
pub fn enum_set_index_of(universe : Array[String], name : String) -> Int? {
  for i, item in universe {
    if item == name {
      return Some(i)
    }
  }
  None
}

///|
/// Check membership by symbol name.
pub fn enum_set_member_by_name(set : @core.EnumSet, name : String) -> Bool {
  match enum_set_index_of(set.universe, name) {
    Some(idx) => set.members[idx]
    None => false
  }
}

///|
/// Build an enum set from a list of names, erroring on unknown names.
///
/// # Example
/// ```mbt check
/// test "enum set from names" {
///   let set = enum_set_from_names(["a", "b"], ["b"])
///   inspect(enum_set_member_by_name(set, "b"), content="true")
///   inspect(enum_set_member_by_name(set, "a"), content="false")
/// }
/// ```
pub fn enum_set_from_names(
  universe : Array[String],
  names : Array[String],
) -> @core.EnumSet raise @core.EvalError {
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
/// Clear the library registry.
pub fn reset_library_registry() -> Unit {
  library_registry.val = {}
}

///|
/// Clear record type registries.
pub fn reset_record_type_registry() -> Unit {
  record_type_registry.val = {}
  record_type_uid_registry.val = {}
  record_type_id_registry.val = {}
}

///|
/// Register a record type descriptor by name and id.
pub fn register_record_type(
  name : String,
  desc : @core.RecordTypeDescriptor,
) -> @core.RecordTypeDescriptor {
  record_type_registry.val[name] = desc
  record_type_id_registry.val[desc.record_type.id] = desc
  match desc.record_type.uid {
    Some(uid) => record_type_uid_registry.val[uid] = desc
    None => ()
  }
  desc
}

///|
/// Register a record type alias without changing descriptor identity.
pub fn register_record_type_alias(
  name : String,
  desc : @core.RecordTypeDescriptor,
) -> Unit {
  record_type_registry.val[name] = desc
  record_type_id_registry.val[desc.record_type.id] = desc
  match desc.record_type.uid {
    Some(uid) => record_type_uid_registry.val[uid] = desc
    None => ()
  }
}

///|
/// Look up a record type descriptor by name.
pub fn lookup_record_type_descriptor(name : String) -> @core.RecordTypeDescriptor? {
  record_type_registry.val.get(name)
}

///|
/// Look up a record type descriptor by uid.
pub fn lookup_record_type_descriptor_by_uid(
  uid : String,
) -> @core.RecordTypeDescriptor? {
  record_type_uid_registry.val.get(uid)
}

///|
/// Look up a record type descriptor by internal id.
pub fn lookup_record_type_descriptor_by_id(id : Int) -> @core.RecordTypeDescriptor? {
  record_type_id_registry.val.get(id)
}

///|
/// Return true if the value is a callable procedure.
pub fn is_procedure_value(value : @core.Value) -> Bool {
  match value {
    Primitive(_)
    | Closure(_)
    | CaseClosure(_)
    | Parameter(_)
    | Continuation(_)
    | @core.RecordProc(_)
    | @core.ConditionProc(_)
    | @core.EnumSetProc(_) => true
    _ => false
  }
}

///|
/// Return true if actual is target or derives from target.
///
/// # Example
/// ```mbt check
/// test "record type ancestry" {
///   let base = make_record_type("base", None, false, false, None, [])
///   let child = make_record_type("child", Some(base), false, false, None, [])
///   inspect(record_type_is_a(child, base), content="true")
///   inspect(record_type_is_a(base, child), content="false")
/// }
/// ```
pub fn record_type_is_a(actual : @core.RecordType, target : @core.RecordType) -> Bool {
  if actual.id == target.id {
    true
  } else {
    match actual.parent {
      Some(parent) => record_type_is_a(parent, target)
      None => false
    }
  }
}

///|
/// Register a library export table.
pub fn register_library(name : String, exports : Map[String, @core.Binding]) -> Unit {
  library_registry.val[name] = exports
}

///|
/// Look up a library by name.
pub fn lookup_library(name : String) -> Map[String, @core.Binding]? {
  library_registry.val.get(name)
}

///|
/// Create a fresh output string port.
///
/// # Example
/// ```mbt check
/// test "output string port" {
///   let port = new_output_string_port()
///   port_write(port, "hi")
///   inspect(port_get_output_string(port), content="hi")
/// }
/// ```
pub fn new_output_string_port() -> @core.Port {
  let buffer = Ref::new("")
  @core.Port::{ id: next_port_id(), kind: OutputString(buffer) }
}

///|
let current_output_port : Ref[@core.Port] = Ref::new(new_output_string_port())

///|
/// Fetch the current output port.
pub fn get_current_output_port() -> @core.Port {
  current_output_port.val
}

///|
/// Replace the current output port.
pub fn set_current_output_port(port : @core.Port) -> Unit {
  current_output_port.val = port
}

///|
/// Write text to a port.
///
/// # Example
/// ```mbt check
/// test "port_write" {
///   let port = new_output_string_port()
///   port_write(port, "ok")
///   inspect(port_get_output_string(port), content="ok")
/// }
/// ```
pub fn port_write(port : @core.Port, text : String) -> Unit {
  match port.kind {
    OutputString(buffer) => buffer.val = buffer.val + text
  }
}

///|
/// Extract the accumulated output from a string port.
///
/// # Example
/// ```mbt check
/// test "port_get_output_string empty" {
///   let port = new_output_string_port()
///   inspect(port_get_output_string(port), content="")
/// }
/// ```
pub fn port_get_output_string(port : @core.Port) -> String {
  match port.kind {
    OutputString(buffer) => buffer.val
  }
}

///|
/// Strip a datum label wrapper if present.
pub fn datum_unlabel(value : @core.Datum) -> @core.Datum {
  let seen : Array[Int] = []
  for cur = value; true; {
    // invariant : seen.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert : seen.length() >= 0
    match cur {
      Label(id, cell) => {
        if seen.contains(id) {
          break cur
        }
        seen.push(id)
        continue cell.val
      }
      _ => break cur
    }
  } else {
    value
  }
}

///|
/// Allocate a new pair datum.
fn pair_new(car : @core.Datum, cdr : @core.Datum) -> @core.Datum {
  Pair(Ref::new(car), Ref::new(cdr))
}

///|
/// Extract a symbol name from a datum when possible.
pub fn symbol_name(expr : @core.Datum) -> String? {
  match expr {
    Symbol(name) => Some(name)
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => symbol_name(obj.datum)
    _ => None
  }
}

///|
///|
fn scopes_with_added(scopes : Array[Int], scope : Int) -> Array[Int] {
  if scopes.contains(scope) {
    scopes.copy()
  } else {
    let next = scopes.copy()
    next.push(scope)
    next
  }
}

///|
/// Remove syntax wrappers from a datum tree.
pub fn strip_syntax_datum(datum : @core.Datum) -> @core.Datum {
  match datum {
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => strip_syntax_datum(obj.datum)
    Pair(a, b) =>
      pair_new(strip_syntax_datum(a.val), strip_syntax_datum(b.val))
    Vector(items) =>
      Vector(items.map((item) => strip_syntax_datum(item)))
    Complex(real, imag) =>
      Complex(
        Ref::new(strip_syntax_datum(real.val)),
        Ref::new(strip_syntax_datum(imag.val)),
      )
    _ => datum
  }
}

///|
/// Wrap datum leaves in syntax objects with given scopes.
fn syntax_wrap_datum(datum : @core.Datum, scopes : Array[Int]) -> @core.Datum {
  match datum {
    Symbol(name) =>
      @core.Datum::Value(
        @core.Value::SyntaxObject(@core.SyntaxObject::{
          datum: Symbol(name),
          scopes,
          binding_id: None,
        }),
      )
    Pair(a, b) =>
      pair_new(
        syntax_wrap_datum(a.val, scopes),
        syntax_wrap_datum(b.val, scopes),
      )
    Vector(items) =>
      Vector(items.map((item) => syntax_wrap_datum(item, scopes)))
    Complex(real, imag) =>
      Complex(
        Ref::new(syntax_wrap_datum(real.val, scopes)),
        Ref::new(syntax_wrap_datum(imag.val, scopes)),
      )
    @core.Datum::Value(@core.Value::SyntaxObject(_)) => datum
    _ => datum
  }
}

///|
/// Wrap a datum in syntax objects unless already wrapped.
pub fn syntax_wrap_root(datum : @core.Datum, scopes : Array[Int]) -> @core.Datum {
  match datum {
    Symbol(_) => datum
    @core.Datum::Value(@core.Value::SyntaxObject(_)) => datum
    _ => syntax_wrap_datum(datum, scopes)
  }
}

///|
/// Add a scope to every syntax object within a datum.
pub fn syntax_add_scope(datum : @core.Datum, scope : Int) -> @core.Datum {
  match datum {
    Symbol(name) =>
      @core.Datum::Value(
        @core.Value::SyntaxObject(@core.SyntaxObject::{
          datum: Symbol(name),
          scopes: [scope],
          binding_id: None,
        }),
      )
    @core.Datum::Value(@core.Value::SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        Symbol(_) => obj.datum
        _ => syntax_add_scope(obj.datum, scope)
      }
      @core.Datum::Value(
        @core.Value::SyntaxObject(@core.SyntaxObject::{
          datum: inner,
          scopes: next_scopes,
          binding_id: obj.binding_id,
        }),
      )
    }
    Pair(a, b) =>
      pair_new(syntax_add_scope(a.val, scope), syntax_add_scope(b.val, scope))
    Vector(items) =>
      Vector(items.map((item) => syntax_add_scope(item, scope)))
    Complex(real, imag) =>
      Complex(
        Ref::new(syntax_add_scope(real.val, scope)),
        Ref::new(syntax_add_scope(imag.val, scope)),
      )
    _ => datum
  }
}

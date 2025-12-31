///|
/// Wrap a Bool into a Datum Value.
pub fn bool_value(value : Bool) -> Value {
  Value::Datum(Datum::Bool(value))
}

///|
pub(all) struct Library {
  exports : Map[String, Binding]
}

///|
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
let library_registry : Ref[Map[String, Library]] = Ref::new({})

///|
let record_type_registry : Ref[Map[String, RecordTypeDescriptor]] = Ref::new({})

///|
let record_type_uid_registry : Ref[Map[String, RecordTypeDescriptor]] = Ref::new({},
)

///|
let record_type_id_registry : Ref[Map[Int, RecordTypeDescriptor]] = Ref::new({})

///|
/// Generate a fresh symbol name with a gensym suffix.
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
pub fn next_binding_id() -> Int {
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
pub fn next_hashtable_id() -> Int {
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
  parent : RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  uid : String?,
  fields : Array[RecordField],
) -> RecordType {
  RecordType::{
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
pub fn make_record_proc(kind : RecordProcKind) -> RecordProc {
  RecordProc::{ id: next_record_proc_id(), kind }
}

///|
/// Allocate a condition procedure wrapper.
pub fn make_condition_proc(kind : ConditionProcKind) -> ConditionProc {
  ConditionProc::{ id: next_condition_proc_id(), kind }
}

///|
/// Construct a record constructor descriptor.
pub fn make_record_constructor_descriptor(
  record_type : RecordType,
  parent_desc : RecordConstructorDescriptor?,
  protocol : Value?,
) -> RecordConstructorDescriptor {
  RecordConstructorDescriptor::{
    id: next_record_ctor_desc_id(),
    record_type,
    parent_desc,
    protocol,
  }
}

///|
/// Construct a record type descriptor wrapper.
pub fn make_record_type_descriptor(
  record_type : RecordType,
  constructor_desc : RecordConstructorDescriptor,
) -> RecordTypeDescriptor {
  RecordTypeDescriptor::{
    id: next_record_type_desc_id(),
    record_type,
    constructor_desc,
  }
}

///|
/// Create a record instance with boxed fields.
pub fn make_record_instance(
  record_type : RecordType,
  values : Array[Value],
) -> Record {
  let fields = values.map((value) => Ref::new(value))
  Record::{ id: next_record_id(), record_type, fields }
}

///|
/// Construct a condition with the given record components.
pub fn make_condition(components : Array[Record]) -> Condition {
  Condition::{ id: next_condition_id(), components }
}

///|
/// Construct a new hashtable with equivalence and hash settings.
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
/// Construct an enum set with a universe and membership bitmap.
pub fn make_enum_set(
  universe : Array[String],
  members : Array[Bool],
) -> EnumSet {
  EnumSet::{ id: next_enum_set_id(), universe, members }
}

///|
/// Allocate an enum-set procedure wrapper.
pub fn make_enum_set_proc(kind : EnumSetProcKind) -> EnumSetProc {
  EnumSetProc::{ id: next_enum_set_proc_id(), kind }
}

///|
/// Create a membership bitmap with all values set to true.
pub fn enum_set_members_all_true(universe : Array[String]) -> Array[Bool] {
  enum_set_members_fill(universe, true)
}

///|
/// Create a membership bitmap with all values set to false.
pub fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] {
  enum_set_members_fill(universe, false)
}

///|
fn enum_set_members_fill(universe : Array[String], value : Bool) -> Array[Bool] {
  universe.map((_name) => value)
}

///|
/// Compare enum-set universes for equal ordering.
pub fn enum_set_universe_equal(a : EnumSet, b : EnumSet) -> Bool {
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
pub fn enum_set_member_by_name(set : EnumSet, name : String) -> Bool {
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
  desc : RecordTypeDescriptor,
) -> RecordTypeDescriptor {
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
  desc : RecordTypeDescriptor,
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
pub fn lookup_record_type_descriptor(name : String) -> RecordTypeDescriptor? {
  record_type_registry.val.get(name)
}

///|
/// Look up a record type descriptor by uid.
pub fn lookup_record_type_descriptor_by_uid(
  uid : String,
) -> RecordTypeDescriptor? {
  record_type_uid_registry.val.get(uid)
}

///|
/// Look up a record type descriptor by internal id.
pub fn lookup_record_type_descriptor_by_id(id : Int) -> RecordTypeDescriptor? {
  record_type_id_registry.val.get(id)
}

///|
/// Build a constructor descriptor chain for a record type.
pub fn default_constructor_descriptor(
  record_type : RecordType,
) -> RecordConstructorDescriptor {
  let parent_desc = match record_type.parent {
    Some(parent) => Some(default_constructor_descriptor(parent))
    None => None
  }
  make_record_constructor_descriptor(record_type, parent_desc, None)
}

///|
/// Return true if the value is a callable procedure.
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
/// Compare an existing record type against descriptor fields.
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
  for i, expected in fields {
    let actual = existing.fields[i]
    if expected.name != actual.name || expected.mutable != actual.mutable {
      return false
    }
  }
  true
}

///|
/// Return true if actual is target or derives from target.
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
/// Register a library export table.
pub fn register_library(name : String, exports : Map[String, Binding]) -> Unit {
  library_registry.val[name] = Library::{ exports, }
}

///|
/// Look up a library by name.
pub fn lookup_library(name : String) -> Library? {
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
pub fn new_output_string_port() -> Port {
  let buffer = Ref::new("")
  Port::{ id: next_port_id(), kind: PortKind::OutputString(buffer) }
}

///|
let current_output_port : Ref[Port] = Ref::new(new_output_string_port())

///|
/// Fetch the current output port.
pub fn get_current_output_port() -> Port {
  current_output_port.val
}

///|
/// Replace the current output port.
pub fn set_current_output_port(port : Port) -> Unit {
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
pub fn port_write(port : Port, text : String) -> Unit {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val = buffer.val + text
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
pub fn port_get_output_string(port : Port) -> String {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val
  }
}

///|
/// Scheme false predicate for values.
pub fn is_false(value : Value) -> Bool {
  match value {
    Value::Datum(Datum::Bool(false)) => true
    _ => false
  }
}

///|
/// Convert a Value into a Datum or raise on type mismatch.
pub fn value_as_datum(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => d
    Value::Record(record) => Datum::Record(record)
    _ => raise @core.EvalError("type error: datum expected")
  }
}

///|
/// Convert a Value into a Datum suitable for list/vector elements.
pub fn value_to_datum_element(value : Value) -> Datum {
  match value {
    Value::Datum(d) => d
    Value::Record(record) => Datum::Record(record)
    _ => Datum::Value(value)
  }
}

///|
/// Convert a Datum back to a Value.
pub fn value_from_datum(datum : Datum) -> Value {
  match datum {
    Datum::Record(record) => Value::Record(record)
    Datum::Value(value) => value
    _ => Value::Datum(datum)
  }
}

///|
/// Retrieve the base condition record type.
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
      if record_type_is_a(record.record_type, base) {
        Some(record)
      } else {
        None
      }
    Value::Datum(Datum::Record(record)) =>
      if record_type_is_a(record.record_type, base) {
        Some(record)
      } else {
        None
      }
    _ => None
  }
}

///|
/// Extract condition components if the value is a condition.
pub fn condition_components_opt(
  value : Value,
  base : RecordType,
) -> Array[Record]? {
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
/// Extract condition components or raise on type mismatch.
pub fn condition_components(
  value : Value,
  base : RecordType,
) -> Array[Record] raise EvalError {
  match condition_components_opt(value, base) {
    Some(items) => items
    None => raise @core.EvalError("type error: condition expected")
  }
}

///|
/// Absolute value for Int.
pub fn int_abs(value : Int) -> Int {
  if value < 0 {
    -value
  } else {
    value
  }
}

///|
/// Compute the greatest common divisor.
///
/// # Example
/// ```mbt check
/// test "gcd" {
///   inspect(gcd(48, 18), content="6")
/// }
/// ```
pub fn gcd(a : Int, b : Int) -> Int {
  for x = int_abs(a), y = int_abs(b); y != 0; {
    continue y, x % y
  } else {
    x
  }
}

///|
/// Normalize an Int rational into a Datum.
///
/// # Example
/// ```mbt check
/// test "normalize rat" {
///   match normalize_rat(2, 4) {
///     Some(@core.Datum::Rat(1, 2)) => ()
///     _ => fail("expected reduced ratio")
///   }
/// }
/// ```
pub fn normalize_rat(num : Int, den : Int) -> Datum? {
  if den == 0 {
    None
  } else if num == 0 {
    Some(Datum::Int(0))
  } else {
    let (n, d) = if den < 0 { (-num, -den) } else { (num, den) }
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
/// Convert an Int into a BigInt.
pub fn bigint_from_int(value : Int) -> @bigint.BigInt {
  @bigint.BigInt::from_int(value)
}

///|
/// Return true if the BigInt is zero.
pub fn bigint_is_zero(value : @bigint.BigInt) -> Bool {
  value.is_zero()
}

///|
/// Absolute value for BigInt.
pub fn bigint_abs(value : @bigint.BigInt) -> @bigint.BigInt {
  if value.compare_int(0) < 0 {
    -value
  } else {
    value
  }
}

///|
/// Convert a BigInt into Int if it fits.
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
/// Compute the greatest common divisor for BigInt.
pub fn bigint_gcd(a : @bigint.BigInt, b : @bigint.BigInt) -> @bigint.BigInt {
  for x = bigint_abs(a), y = bigint_abs(b); !bigint_is_zero(y); {
    continue y, x % y
  } else {
    x
  }
}

///|
/// Normalize a BigInt rational into a Datum.
pub fn normalize_big_rat(num : @bigint.BigInt, den : @bigint.BigInt) -> Datum? {
  if bigint_is_zero(den) {
    None
  } else if bigint_is_zero(num) {
    Some(Datum::Int(0))
  } else {
    let (n, d) = if den.compare_int(0) < 0 {
      (-num, -den)
    } else {
      (num, den)
    }
    let g = bigint_gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd.compare_int(1) == 0 {
      Some(
        match bigint_to_int_option(nn) {
          Some(v) => Datum::Int(v)
          None => Datum::BigInt(nn)
        },
      )
    } else {
      match (bigint_to_int_option(nn), bigint_to_int_option(dd)) {
        (Some(nv), Some(dv)) => Some(Datum::Rat(nv, dv))
        _ => Some(Datum::BigRat(nn, dd))
      }
    }
  }
}

///|
/// Strip a datum label wrapper if present.
pub fn datum_unlabel(value : Datum) -> Datum {
  let mut cur = value
  let seen : Array[Int] = []
  while true {
    match cur {
      Datum::Label(id, cell) => {
        if seen.contains(id) {
          return cur
        }
        seen.push(id)
        cur = cell.val
      }
      _ => return cur
    }
  }
  else {
    cur
  }
}

///|
/// Extract pair elements as datums or raise on type mismatch.
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
/// Extract pair cells as mutable references or raise on mismatch.
pub fn value_as_pair_cells(
  value : Value,
) -> (Ref[Datum], Ref[Datum]) raise EvalError {
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
/// Allocate a new pair datum.
pub fn pair_new(car : Datum, cdr : Datum) -> Datum {
  Datum::Pair(Ref::new(car), Ref::new(cdr))
}

///|
/// Convert a proper list datum into an array.
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
/// Convert a list datum into an array of values.
pub fn datum_list_to_value_array(list : Datum) -> Array[Value] raise EvalError {
  datum_list_to_array(list).map((item) => value_from_datum(item))
}

///|
/// Build a list datum from an array.
pub fn list_from_array(items : Array[Datum]) -> Datum {
  let mut tail = Datum::Nil
  for item in items.rev_iter() {
    tail = pair_new(item, tail)
  }
  tail
}

///|
/// Extract a symbol name from a datum when possible.
pub fn symbol_name(expr : Datum) -> String? {
  match expr {
    Datum::Symbol(name) => Some(name)
    Datum::Value(Value::SyntaxObject(obj)) => symbol_name(obj.datum)
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
pub fn strip_syntax_datum(datum : Datum) -> Datum {
  match datum {
    Datum::Value(Value::SyntaxObject(obj)) => strip_syntax_datum(obj.datum)
    Datum::Pair(a, b) =>
      pair_new(strip_syntax_datum(a.val), strip_syntax_datum(b.val))
    Datum::Vector(items) =>
      Datum::Vector(items.map((item) => strip_syntax_datum(item)))
    Datum::Complex(real, imag) =>
      Datum::Complex(
        Ref::new(strip_syntax_datum(real.val)),
        Ref::new(strip_syntax_datum(imag.val)),
      )
    _ => datum
  }
}

///|
/// Wrap datum leaves in syntax objects with given scopes.
pub fn syntax_wrap_datum(datum : Datum, scopes : Array[Int]) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{
          datum: Datum::Symbol(name),
          scopes,
          binding_id: None,
        }),
      )
    Datum::Pair(a, b) =>
      pair_new(
        syntax_wrap_datum(a.val, scopes),
        syntax_wrap_datum(b.val, scopes),
      )
    Datum::Vector(items) =>
      Datum::Vector(items.map((item) => syntax_wrap_datum(item, scopes)))
    Datum::Complex(real, imag) =>
      Datum::Complex(
        Ref::new(syntax_wrap_datum(real.val, scopes)),
        Ref::new(syntax_wrap_datum(imag.val, scopes)),
      )
    Datum::Value(Value::SyntaxObject(_)) => datum
    _ => datum
  }
}

///|
/// Wrap a datum in syntax objects unless already wrapped.
pub fn syntax_wrap_root(datum : Datum, scopes : Array[Int]) -> Datum {
  match datum {
    Datum::Symbol(_) => datum
    Datum::Value(Value::SyntaxObject(_)) => datum
    _ => syntax_wrap_datum(datum, scopes)
  }
}

///|
/// Add a scope to every syntax object within a datum.
pub fn syntax_add_scope(datum : Datum, scope : Int) -> Datum {
  match datum {
    Datum::Symbol(name) =>
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{
          datum: Datum::Symbol(name),
          scopes: [scope],
          binding_id: None,
        }),
      )
    Datum::Value(Value::SyntaxObject(obj)) => {
      let next_scopes = scopes_with_added(obj.scopes, scope)
      let inner = match obj.datum {
        Datum::Symbol(_) => obj.datum
        _ => syntax_add_scope(obj.datum, scope)
      }
      Datum::Value(
        Value::SyntaxObject(SyntaxObject::{
          datum: inner,
          scopes: next_scopes,
          binding_id: obj.binding_id,
        }),
      )
    }
    Datum::Pair(a, b) =>
      pair_new(syntax_add_scope(a.val, scope), syntax_add_scope(b.val, scope))
    Datum::Vector(items) =>
      Datum::Vector(items.map((item) => syntax_add_scope(item, scope)))
    Datum::Complex(real, imag) =>
      Datum::Complex(
        Ref::new(syntax_add_scope(real.val, scope)),
        Ref::new(syntax_add_scope(imag.val, scope)),
      )
    _ => datum
  }
}

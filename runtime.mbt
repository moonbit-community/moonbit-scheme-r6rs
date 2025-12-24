///|
fn bool_value(value : Bool) -> Value {
  Value::Datum(Datum::Bool(value))
}

///|
priv enum PortKind {
  OutputString(Ref[String])
}

///|
struct Port {
  id : Int
  kind : PortKind
}

///|
priv struct RecordField {
  name : String
  mutable : Bool
}

///|
priv struct RecordType {
  id : Int
  name : String
  parent : RecordType?
  is_sealed : Bool
  is_opaque : Bool
  fields : Array[RecordField]
}

///|
struct Record {
  id : Int
  record_type : RecordType
  fields : Array[Ref[Value]]
}

///|
priv enum RecordProcKind {
  Constructor(RecordType)
  Predicate(RecordType)
  Accessor(RecordType, Int)
  Mutator(RecordType, Int)
}

///|
struct RecordProc {
  id : Int
  kind : RecordProcKind
}

///|
struct RecordConstructorDescriptor {
  id : Int
  ctor : Value
}

///|
struct RecordTypeDescriptor {
  id : Int
  record_type : RecordType
  constructor_desc : RecordConstructorDescriptor
}

///|
priv struct Library {
  exports : Map[String, Value]
}

///|
let port_counter : Ref[Int] = Ref::new(0)
let record_type_counter : Ref[Int] = Ref::new(0)
let record_counter : Ref[Int] = Ref::new(0)
let record_proc_counter : Ref[Int] = Ref::new(0)
let record_type_desc_counter : Ref[Int] = Ref::new(0)
let record_ctor_desc_counter : Ref[Int] = Ref::new(0)
let library_registry : Ref[Map[String, Library]] = Ref::new({})
let record_type_registry : Ref[Map[String, RecordTypeDescriptor]] = Ref::new({})

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
fn make_record_type(
  name : String,
  parent : RecordType?,
  is_sealed : Bool,
  is_opaque : Bool,
  fields : Array[RecordField]
) -> RecordType {
  RecordType::{ id: next_record_type_id(), name, parent, is_sealed, is_opaque, fields }
}

///|
fn make_record_proc(kind : RecordProcKind) -> RecordProc {
  RecordProc::{ id: next_record_proc_id(), kind }
}

///|
fn make_record_constructor_descriptor(
  ctor : Value
) -> RecordConstructorDescriptor {
  RecordConstructorDescriptor::{ id: next_record_ctor_desc_id(), ctor }
}

///|
fn make_record_type_descriptor(
  record_type : RecordType,
  ctor : Value
) -> RecordTypeDescriptor {
  let ctor_desc = make_record_constructor_descriptor(ctor)
  RecordTypeDescriptor::{ id: next_record_type_desc_id(), record_type, constructor_desc: ctor_desc }
}

///|
fn make_record_instance(record_type : RecordType, values : Array[Value]) -> Record {
  let fields : Array[Ref[Value]] = []
  for value in values {
    fields.push(Ref::new(value))
  }
  Record::{ id: next_record_id(), record_type, fields }
}

///|
fn reset_library_registry() -> Unit {
  library_registry.val = {}
}

///|
fn reset_record_type_registry() -> Unit {
  record_type_registry.val = {}
}

///|
fn register_record_type(name : String, record_type : RecordType, ctor : Value) -> Unit {
  record_type_registry.val[name] = make_record_type_descriptor(record_type, ctor)
}

///|
fn lookup_record_type(name : String) -> RecordType? {
  match record_type_registry.val.get(name) {
    Some(desc) => Some(desc.record_type)
    None => None
  }
}

///|
fn lookup_record_type_descriptor(name : String) -> RecordTypeDescriptor? {
  record_type_registry.val.get(name)
}

///|
fn register_library(name : String, exports : Map[String, Value]) -> Unit {
  library_registry.val[name] = Library::{ exports }
}

///|
fn lookup_library(name : String) -> Library? {
  library_registry.val.get(name)
}

///|
fn new_output_string_port() -> Port {
  let buffer = Ref::new("")
  Port::{ id: next_port_id(), kind: PortKind::OutputString(buffer) }
}

///|
let current_output_port : Ref[Port] = Ref::new(new_output_string_port())

///|
fn get_current_output_port() -> Port {
  current_output_port.val
}

///|
fn set_current_output_port(port : Port) -> Unit {
  current_output_port.val = port
}

///|
fn port_write(port : Port, text : String) -> Unit {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val = buffer.val + text
  }
}

///|
fn port_get_output_string(port : Port) -> String {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val
  }
}

///|
fn is_false(value : Value) -> Bool {
  match value {
    Value::Datum(Datum::Bool(false)) => true
    _ => false
  }
}

///|
fn value_as_datum(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => d
    _ => raise EvalError("type error: datum expected")
  }
}

///|
///|
fn int_abs(value : Int) -> Int {
  if value < 0 { -value } else { value }
}

///|
fn gcd(a : Int, b : Int) -> Int {
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
fn normalize_rat(num : Int, den : Int) -> Datum? {
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
fn value_as_pair(value : Value) -> (Datum, Datum) raise EvalError {
  match value {
    Value::Datum(Datum::Pair(a, b)) => (a.val, b.val)
    _ => raise EvalError("type error: pair expected")
  }
}

///|
fn value_as_pair_cells(value : Value) -> (Ref[Datum], Ref[Datum]) raise EvalError {
  match value {
    Value::Datum(Datum::Pair(a, b)) => (a, b)
    _ => raise EvalError("type error: pair expected")
  }
}

///|
fn pair_new(car : Datum, cdr : Datum) -> Datum {
  Datum::Pair(Ref::new(car), Ref::new(cdr))
}

///|
fn datum_list_to_array(list : Datum) -> Array[Datum] raise EvalError {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a.val)
        cur = b.val
      }
      _ => raise EvalError("type error: proper list expected")
    }
  }
  items
}

///|
fn datum_list_to_value_array(list : Datum) -> Array[Value] raise EvalError {
  let items : Array[Value] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(Value::Datum(a.val))
        cur = b.val
      }
      _ => raise EvalError("type error: list expected")
    }
  }
  items
}

///|
fn list_from_array(items : Array[Datum]) -> Datum {
  let mut tail = Datum::Nil
  for item in items.rev_iter() {
    tail = pair_new(item, tail)
  }
  tail
}

///|
fn symbol_name(expr : Datum) -> String? {
  match expr {
    Datum::Symbol(name) => Some(name)
    _ => None
  }
}

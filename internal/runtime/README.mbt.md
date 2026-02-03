# runtime

Runtime helpers for environments, records, conditions, ports, and value
conversion.

## Entry points

- env_new and env_lookup for lexical environments
- value_to_string for rendering values
- make_record_type and make_record_instance for records
- new_output_string_port and port_get_output_string for ports

## Example

```mbt nocheck
///|
let env = @runtime.env_new()

///|
let port = @runtime.new_output_string_port()
```

```mbt check
///|
test "string port" {
  let port = new_output_string_port()
  port_write(port, "hi")
  inspect(port_get_output_string(port), content="hi")
}

///|
test "env basics" {
  let env = env_new()
  env_define(env, "x", Datum(Int(1)))
  match env_lookup_optional(env, "x") {
    Some(Datum(Int(1))) => ()
    _ => fail("expected bound value")
  }
}

///|
test "env set" {
  let env = env_new()
  env_define(env, "x", Datum(Int(1)))
  env_set(env, "x", Datum(Int(2)))
  match env_lookup_optional(env, "x") {
    Some(Datum(Int(2))) => ()
    _ => fail("expected updated value")
  }
}

///|
test "env clone" {
  let env = env_new()
  env_define(env, "x", Datum(Int(1)))
  let cloned = env_clone(env)
  env_set(env, "x", Datum(Int(2)))
  match env_lookup_optional(cloned, "x") {
    Some(Datum(Int(1))) => ()
    _ => fail("expected cloned value to stay 1")
  }
}

///|
test "gensym unique suffix" {
  let a = gensym("x")
  let b = gensym("x")
  inspect(a != b, content="true")
  guard a is [.. "x__gs", .. _rest] else { fail("expected gensym prefix") }
}

///|
test "symbol name" {
  match symbol_name(Symbol("x")) {
    Some("x") => ()
    _ => fail("expected symbol name")
  }
}

///|
test "enum set from names" {
  let set = enum_set_from_names(["a", "b"], ["b"])
  inspect(enum_set_member_by_name(set, "b"), content="true")
  inspect(enum_set_member_by_name(set, "a"), content="false")
}

///|
test "enum set index" {
  inspect(enum_set_index_of(["a", "b"], "b"), content="Some(1)")
}

///|
test "enum set universe equal" {
  let base = ["a", "b"]
  let same = enum_set_from_names(base, [])
  let other = enum_set_from_names(["b", "a"], [])
  inspect(enum_set_universe_equal(same, same), content="true")
  inspect(enum_set_universe_equal(same, other), content="false")
}

///|
test "value to string" {
  let value = Value::Datum(Int(5))
  inspect(value_to_string(value), content="5")
}

///|
test "printer datum rendering" {
  let record_type = @core.RecordType::new(1, "r", None, false, false, None, [])
  let record = @core.Record::new(1, record_type, [])
  let record_for_condition = @core.Record::new(2, record_type, [])
  let condition = @core.Condition::new(1, [record_for_condition])
  let label_cell = Ref::new(Datum::Nil)
  let label = Datum::Label(1, label_cell)
  label_cell.val = label
  let proper_list = Datum::Pair(
    Ref::new(Int(1)),
    Ref::new(
      Pair(
        Ref::new(Int(2)),
        Ref::new(Nil),
      ),
    ),
  )
  let dotted_list = Datum::Pair(
    Ref::new(Int(1)),
    Ref::new(Int(2)),
  )
  let entries : Array[(@core.Datum, String)] = [
    (Nil, "()"),
    (Bool(true), "#t"),
    (Int(3), "3"),
    (BigInt(@bigint.BigInt::from_int(-12)), "-12"),
    (Rat(1, 2), "1/2"),
    (
      BigRat(
        @bigint.BigInt::from_int(-1),
        @bigint.BigInt::from_int(3),
      ),
      "-1/3",
    ),
    (Float(1.5), "1.5"),
    (
      Complex(
        Ref::new(Int(1)),
        Ref::new(BigInt(@bigint.BigInt::from_int(-2))),
      ),
      "1-2i",
    ),
    (
      Complex(
        Ref::new(Int(1)),
        Ref::new(Rat(-1, 2)),
      ),
      "1-1/2i",
    ),
    (
      Complex(
        Ref::new(Int(1)),
        Ref::new(
          BigRat(
            @bigint.BigInt::from_int(-1),
            @bigint.BigInt::from_int(3),
          ),
        ),
      ),
      "1-1/3i",
    ),
    (
      Complex(
        Ref::new(Int(1)),
        Ref::new(Float(-1.5)),
      ),
      "1-1.5i",
    ),
    (Char(' '), "#\\space"),
    (Char('\n'), "#\\newline"),
    (Char('\t'), "#\\tab"),
    (Char('a'), "#\\a"),
    (String(Ref::new("a\n\t\r\"\\b")), "\"a\\n\\t\\r\\\"\\\\b\""),
    (Symbol("foo"), "foo"),
    (proper_list, "(1 2)"),
    (dotted_list, "(1 . 2)"),
    (
      Vector([Int(1), Bool(true)]),
      "#(1 #t)",
    ),
    (ByteVector([1, 2]), "#vu8(1 2)"),
    (Record(record), "#<record>"),
    (Condition(condition), "#<condition>"),
    (Value(Void), "#<void>"),
    (label, "#1=#1#"),
  ]
  for entry in entries {
    let (datum, expected) = entry
    inspect(value_to_string(Datum(datum)), content=expected)
  }
}

///|
test "value to string variants" {
  let env = env_new()
  let record_type = @core.RecordType::new(1, "r", None, false, false, None, [])
  let record = @core.Record::new(1, record_type, [])
  let ctor_desc_for_type = @core.RecordConstructorDescriptor::new(
    1,
    record_type,
    None,
    None,
  )
  let ctor_desc = @core.RecordConstructorDescriptor::new(
    2,
    record_type,
    None,
    None,
  )
  let record_type_desc = @core.RecordTypeDescriptor::new(
    1, record_type, ctor_desc_for_type,
  )
  let enum_set = @core.EnumSet::new(1, ["a"], [true])
  let table = @core.Hashtable::new(1, true, Eq, None, [])
  let port = new_output_string_port()
  let promise = @core.Promise::new(
    1,
    Value(Void),
  )
  let eval_env = @core.EvalEnv::new(1, env)
  let syntax_obj = @core.SyntaxObject::new(Symbol("x"), [], None)
  let values : Array[(@core.Value, String)] = [
    (Primitive(Add), "#<procedure>"),
    (Values([Void]), "#<values>"),
    (Promise(promise), "#<promise>"),
    (EvalEnv(eval_env), "#<environment>"),
    (Port(port), "#<port>"),
    (Record(record), "#<record>"),
    (Hashtable(table), "#<hashtable>"),
    (EnumSet(enum_set), "#<enum-set>"),
    (
      RecordTypeDescriptor(record_type_desc),
      "#<record-type-descriptor>",
    ),
    (
      RecordConstructorDescriptor(ctor_desc),
      "#<record-constructor-descriptor>",
    ),
    (SyntaxObject(syntax_obj), "#<syntax>"),
    (SyntaxKeyword("syntax"), "#<syntax-keyword>"),
  ]
  for entry in values {
    let (value, expected) = entry
    inspect(value_to_string(value), content=expected)
  }
}

///|
test "datum unlabel" {
  match datum_unlabel(Int(3)) {
    Int(3) => ()
    _ => fail("expected same datum")
  }
}

///|
test "strip syntax datum" {
  let wrapped = Datum::Value(
    SyntaxObject(
      @core.SyntaxObject::new(Symbol("x"), [], None),
    ),
  )
  match strip_syntax_datum(wrapped) {
    Symbol("x") => ()
    _ => fail("expected symbol")
  }
}

///|
test "env error paths" {
  let empty : @core.Env = []
  let set_empty = try? env_set(empty, "x", Void)
  inspect(set_empty is Err(_), content="true")
  inspect(env_lookup_optional(empty, "x") is None, content="true")
  let env = env_new()
  let set_missing = try? env_set(env, "x", Void)
  inspect(set_missing is Err(_), content="true")
  inspect(is_procedure_value(Void), content="false")
}

///|
test "enum set error paths" {
  let short = @core.EnumSet::new(1, ["a"], [true])
  let long = @core.EnumSet::new(2, ["a", "b"], [true, false])
  inspect(enum_set_universe_equal(short, long), content="false")
  inspect(enum_set_member_by_name(short, "missing"), content="false")
  let bad = try? enum_set_from_names(["a"], ["b"])
  inspect(bad is Err(_), content="true")
}

///|
test "syntax helpers extra" {
  let syntax = Datum::Value(
    SyntaxObject(
      @core.SyntaxObject::new(Symbol("x"), [1], None),
    ),
  )
  match symbol_name(syntax) {
    Some("x") => ()
    _ => fail("expected symbol name")
  }
  let complex = Datum::Complex(
    Ref::new(Int(1)),
    Ref::new(Int(2)),
  )
  match syntax_wrap_root(complex, [1]) {
    Complex(_, _) => ()
    _ => fail("expected complex")
  }
  match syntax_add_scope(complex, 2) {
    Complex(_, _) => ()
    _ => fail("expected complex")
  }
  match syntax_add_scope(syntax, 1) {
    Value(SyntaxObject(obj)) => inspect(obj.scopes.length(), content="1")
    _ => fail("expected syntax object")
  }
  match syntax_add_scope(syntax, 2) {
    Value(SyntaxObject(obj)) => inspect(obj.scopes.length(), content="2")
    _ => fail("expected syntax object")
  }
  let vector = Datum::Vector([Symbol("y")])
  match syntax_add_scope(vector, 2) {
    Vector(items) =>
      match items[0] {
        Value(SyntaxObject(_)) => ()
        _ => fail("expected syntax object")
      }
    _ => fail("expected vector")
  }
  let pair = Datum::Pair(
    Ref::new(Symbol("p")),
    Ref::new(Nil),
  )
  let wrapped_pair = Datum::Value(
    SyntaxObject(@core.SyntaxObject::new(pair, [3], None)),
  )
  match syntax_add_scope(wrapped_pair, 4) {
    Value(SyntaxObject(obj)) =>
      match obj.datum {
        Pair(_, _) => ()
        _ => fail("expected pair")
      }
    _ => fail("expected syntax object")
  }
  let cell = Ref::new(Datum::Nil)
  let label = Datum::Label(1, cell)
  cell.val = label
  match datum_unlabel(label) {
    Label(_, _) => ()
    _ => fail("expected label")
  }
}

///|
test "record type alias without uid" {
  reset_record_type_registry()
  let record_type = @core.RecordType::new(
    50,
    "doc/alias",
    None,
    false,
    false,
    None,
    [],
  )
  let ctor_desc = @core.RecordConstructorDescriptor::new(
    50,
    record_type,
    None,
    None,
  )
  let desc = @core.RecordTypeDescriptor::new(50, record_type, ctor_desc)
  register_record_type_alias("doc/alias", desc)
  match lookup_record_type_descriptor("doc/alias") {
    Some(_) => ()
    _ => fail("expected record type alias")
  }
}

///|
test "library exports" {
  let binding = make_binding(Datum(Int(1)))
  register_library("doc/runtime-lib", { "x": binding })
  match lookup_library("doc/runtime-lib") {
    Some(exports) =>
      match exports.get("x") {
        Some(exported) =>
          match exported.value() {
            Datum(Int(1)) => ()
            _ => fail("expected datum export")
          }
        None => fail("expected export")
      }
    None => fail("expected library")
  }
}
```

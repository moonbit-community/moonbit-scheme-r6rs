///|
fn init_env() -> Env {
  let env = env_new()
  env_define(env, "+", Value::Primitive(Primitive::Add))
  env_define(env, "-", Value::Primitive(Primitive::Sub))
  env_define(env, "*", Value::Primitive(Primitive::Mul))
  env_define(env, "/", Value::Primitive(Primitive::Div))
  env_define(env, "=", Value::Primitive(Primitive::NumEq))
  env_define(env, "<", Value::Primitive(Primitive::Less))
  env_define(env, ">", Value::Primitive(Primitive::Greater))
  env_define(env, "<=", Value::Primitive(Primitive::LessEq))
  env_define(env, ">=", Value::Primitive(Primitive::GreaterEq))
  env_define(env, "cons", Value::Primitive(Primitive::Cons))
  env_define(env, "car", Value::Primitive(Primitive::Car))
  env_define(env, "cdr", Value::Primitive(Primitive::Cdr))
  env_define(env, "list", Value::Primitive(Primitive::List))
  env_define(env, "make-list", Value::Primitive(Primitive::MakeList))
  env_define(env, "null?", Value::Primitive(Primitive::NullP))
  env_define(env, "pair?", Value::Primitive(Primitive::PairP))
  env_define(env, "symbol?", Value::Primitive(Primitive::SymbolP))
  env_define(env, "identifier?", Value::Primitive(Primitive::IdentifierP))
  env_define(env, "syntax?", Value::Primitive(Primitive::SyntaxP))
  env_define(env, "free-identifier=?", Value::Primitive(Primitive::FreeIdentifierEq))
  env_define(env, "bound-identifier=?", Value::Primitive(Primitive::BoundIdentifierEq))
  env_define(env, "symbol->string", Value::Primitive(Primitive::SymbolToString))
  env_define(env, "string->symbol", Value::Primitive(Primitive::StringToSymbol))
  env_define(env, "syntax->datum", Value::Primitive(Primitive::SyntaxToDatum))
  env_define(env, "datum->syntax", Value::Primitive(Primitive::DatumToSyntax))
  env_define(env, "boolean?", Value::Primitive(Primitive::BooleanP))
  env_define(env, "number?", Value::Primitive(Primitive::NumberP))
  env_define(env, "integer?", Value::Primitive(Primitive::IntegerP))
  env_define(env, "rational?", Value::Primitive(Primitive::RationalP))
  env_define(env, "real?", Value::Primitive(Primitive::RealP))
  env_define(env, "complex?", Value::Primitive(Primitive::ComplexP))
  env_define(env, "exact?", Value::Primitive(Primitive::ExactP))
  env_define(env, "inexact?", Value::Primitive(Primitive::InexactP))
  env_define(env, "zero?", Value::Primitive(Primitive::ZeroP))
  env_define(env, "positive?", Value::Primitive(Primitive::PositiveP))
  env_define(env, "negative?", Value::Primitive(Primitive::NegativeP))
  env_define(env, "odd?", Value::Primitive(Primitive::OddP))
  env_define(env, "even?", Value::Primitive(Primitive::EvenP))
  env_define(env, "finite?", Value::Primitive(Primitive::FiniteP))
  env_define(env, "infinite?", Value::Primitive(Primitive::InfiniteP))
  env_define(env, "nan?", Value::Primitive(Primitive::NanP))
  env_define(env, "procedure?", Value::Primitive(Primitive::ProcedureP))
  env_define(env, "record?", Value::Primitive(Primitive::RecordP))
  env_define(env, "record-rtd", Value::Primitive(Primitive::RecordRtd))
  env_define(env, "record-type-descriptor?", Value::Primitive(Primitive::RecordTypeDescriptorP))
  env_define(env, "record-constructor-descriptor?", Value::Primitive(Primitive::RecordConstructorDescriptorP))
  env_define(env, "record-type-name", Value::Primitive(Primitive::RecordTypeName))
  env_define(env, "record-type-parent", Value::Primitive(Primitive::RecordTypeParent))
  env_define(env, "record-type-uid", Value::Primitive(Primitive::RecordTypeUid))
  env_define(env, "record-type-generative?", Value::Primitive(Primitive::RecordTypeGenerativeP))
  env_define(env, "record-type-sealed?", Value::Primitive(Primitive::RecordTypeSealedP))
  env_define(env, "record-type-opaque?", Value::Primitive(Primitive::RecordTypeOpaqueP))
  env_define(env, "record-type-field-names", Value::Primitive(Primitive::RecordTypeFieldNames))
  env_define(env, "record-type-field-mutable?", Value::Primitive(Primitive::RecordTypeFieldMutableP))
  env_define(env, "record-constructor-descriptor", Value::Primitive(Primitive::RecordConstructorDescriptor))
  env_define(env, "record-constructor", Value::Primitive(Primitive::RecordConstructor))
  env_define(env, "record-predicate", Value::Primitive(Primitive::RecordPredicate))
  env_define(env, "record-accessor", Value::Primitive(Primitive::RecordAccessor))
  env_define(env, "record-mutator", Value::Primitive(Primitive::RecordMutator))
  env_define(env, "make-record-type-descriptor", Value::Primitive(Primitive::MakeRecordTypeDescriptor))
  env_define(
    env,
    "make-record-constructor-descriptor",
    Value::Primitive(Primitive::MakeRecordConstructorDescriptor),
  )
  env_define(env, "not", Value::Primitive(Primitive::Not))
  env_define(env, "apply", Value::Primitive(Primitive::Apply))
  env_define(env, "values", Value::Primitive(Primitive::Values))
  env_define(env, "call-with-values", Value::Primitive(Primitive::CallWithValues))
  env_define(env, "make-parameter", Value::Primitive(Primitive::MakeParameter))
  env_define(env, "dynamic-wind", Value::Primitive(Primitive::DynamicWind))
  env_define(env, "exact->inexact", Value::Primitive(Primitive::ExactToInexact))
  env_define(env, "inexact->exact", Value::Primitive(Primitive::InexactToExact))
  env_define(env, "exact-integer-sqrt", Value::Primitive(Primitive::ExactIntegerSqrt))
  env_define(env, "rationalize", Value::Primitive(Primitive::Rationalize))
  env_define(env, "number->string", Value::Primitive(Primitive::NumberToString))
  env_define(env, "string->number", Value::Primitive(Primitive::StringToNumber))
  env_define(env, "make-rectangular", Value::Primitive(Primitive::MakeRectangular))
  env_define(env, "make-polar", Value::Primitive(Primitive::MakePolar))
  env_define(env, "real-part", Value::Primitive(Primitive::RealPart))
  env_define(env, "imag-part", Value::Primitive(Primitive::ImagPart))
  env_define(env, "magnitude", Value::Primitive(Primitive::Magnitude))
  env_define(env, "angle", Value::Primitive(Primitive::Angle))
  env_define(env, "sqrt", Value::Primitive(Primitive::Sqrt))
  env_define(env, "exp", Value::Primitive(Primitive::Exp))
  env_define(env, "log", Value::Primitive(Primitive::Log))
  env_define(env, "expt", Value::Primitive(Primitive::Expt))
  env_define(env, "sin", Value::Primitive(Primitive::Sin))
  env_define(env, "cos", Value::Primitive(Primitive::Cos))
  env_define(env, "tan", Value::Primitive(Primitive::Tan))
  env_define(env, "asin", Value::Primitive(Primitive::Asin))
  env_define(env, "acos", Value::Primitive(Primitive::Acos))
  env_define(env, "atan", Value::Primitive(Primitive::Atan))
  env_define(env, "numerator", Value::Primitive(Primitive::Numerator))
  env_define(env, "denominator", Value::Primitive(Primitive::Denominator))
  env_define(env, "abs", Value::Primitive(Primitive::Abs))
  env_define(env, "quotient", Value::Primitive(Primitive::Quotient))
  env_define(env, "remainder", Value::Primitive(Primitive::Remainder))
  env_define(env, "modulo", Value::Primitive(Primitive::Modulo))
  env_define(env, "gcd", Value::Primitive(Primitive::Gcd))
  env_define(env, "lcm", Value::Primitive(Primitive::Lcm))
  env_define(env, "max", Value::Primitive(Primitive::Max))
  env_define(env, "min", Value::Primitive(Primitive::Min))
  env_define(env, "floor", Value::Primitive(Primitive::Floor))
  env_define(env, "ceiling", Value::Primitive(Primitive::Ceiling))
  env_define(env, "truncate", Value::Primitive(Primitive::Truncate))
  env_define(env, "round", Value::Primitive(Primitive::Round))
  env_define(env, "eq?", Value::Primitive(Primitive::Eq))
  env_define(env, "eqv?", Value::Primitive(Primitive::Eqv))
  env_define(env, "equal?", Value::Primitive(Primitive::Equal))
  env_define(env, "list?", Value::Primitive(Primitive::ListP))
  env_define(env, "list-ref", Value::Primitive(Primitive::ListRef))
  env_define(env, "list-tail", Value::Primitive(Primitive::ListTail))
  env_define(env, "member", Value::Primitive(Primitive::Member))
  env_define(env, "memq", Value::Primitive(Primitive::Memq))
  env_define(env, "memv", Value::Primitive(Primitive::Memv))
  env_define(env, "assoc", Value::Primitive(Primitive::Assoc))
  env_define(env, "assq", Value::Primitive(Primitive::Assq))
  env_define(env, "assv", Value::Primitive(Primitive::Assv))
  env_define(env, "map", Value::Primitive(Primitive::Map))
  env_define(env, "for-each", Value::Primitive(Primitive::ForEach))
  env_define(env, "set-car!", Value::Primitive(Primitive::SetCar))
  env_define(env, "set-cdr!", Value::Primitive(Primitive::SetCdr))
  env_define(env, "list-copy", Value::Primitive(Primitive::ListCopy))
  let cxr_chains : Array[String] = [
    "aa",
    "ad",
    "da",
    "dd",
    "aaa",
    "aad",
    "ada",
    "add",
    "daa",
    "dad",
    "dda",
    "ddd",
    "aaaa",
    "aaad",
    "aada",
    "aadd",
    "adaa",
    "adad",
    "adda",
    "addd",
    "daaa",
    "daad",
    "dada",
    "dadd",
    "ddaa",
    "ddad",
    "ddda",
    "dddd",
  ]
  for chain in cxr_chains {
    env_define(env, "c\{chain}r", Value::Primitive(Primitive::Cxr(chain)))
  }
  env_define(env, "length", Value::Primitive(Primitive::Length))
  env_define(env, "append", Value::Primitive(Primitive::Append))
  env_define(env, "reverse", Value::Primitive(Primitive::Reverse))
  env_define(env, "char=?", Value::Primitive(Primitive::CharEq))
  env_define(env, "char<?", Value::Primitive(Primitive::CharLess))
  env_define(env, "char>?", Value::Primitive(Primitive::CharGreater))
  env_define(env, "char<=?", Value::Primitive(Primitive::CharLessEq))
  env_define(env, "char>=?", Value::Primitive(Primitive::CharGreaterEq))
  env_define(env, "char-ci=?", Value::Primitive(Primitive::CharCiEq))
  env_define(env, "char-ci<?", Value::Primitive(Primitive::CharCiLess))
  env_define(env, "char-ci>?", Value::Primitive(Primitive::CharCiGreater))
  env_define(env, "char-ci<=?", Value::Primitive(Primitive::CharCiLessEq))
  env_define(env, "char-ci>=?", Value::Primitive(Primitive::CharCiGreaterEq))
  env_define(env, "char?", Value::Primitive(Primitive::CharP))
  env_define(env, "char->integer", Value::Primitive(Primitive::CharToInteger))
  env_define(env, "integer->char", Value::Primitive(Primitive::IntegerToChar))
  env_define(env, "char-alphabetic?", Value::Primitive(Primitive::CharAlphabeticP))
  env_define(env, "char-numeric?", Value::Primitive(Primitive::CharNumericP))
  env_define(env, "char-whitespace?", Value::Primitive(Primitive::CharWhitespaceP))
  env_define(env, "char-upper-case?", Value::Primitive(Primitive::CharUpperCaseP))
  env_define(env, "char-lower-case?", Value::Primitive(Primitive::CharLowerCaseP))
  env_define(env, "char-upcase", Value::Primitive(Primitive::CharUpcase))
  env_define(env, "char-downcase", Value::Primitive(Primitive::CharDowncase))
  env_define(env, "char-foldcase", Value::Primitive(Primitive::CharFoldcase))
  env_define(env, "string=?", Value::Primitive(Primitive::StringEq))
  env_define(env, "string<?", Value::Primitive(Primitive::StringLess))
  env_define(env, "string>?", Value::Primitive(Primitive::StringGreater))
  env_define(env, "string<=?", Value::Primitive(Primitive::StringLessEq))
  env_define(env, "string>=?", Value::Primitive(Primitive::StringGreaterEq))
  env_define(env, "string-ci=?", Value::Primitive(Primitive::StringCiEq))
  env_define(env, "string-ci<?", Value::Primitive(Primitive::StringCiLess))
  env_define(env, "string-ci>?", Value::Primitive(Primitive::StringCiGreater))
  env_define(env, "string-ci<=?", Value::Primitive(Primitive::StringCiLessEq))
  env_define(env, "string-ci>=?", Value::Primitive(Primitive::StringCiGreaterEq))
  env_define(env, "string", Value::Primitive(Primitive::String))
  env_define(env, "make-string", Value::Primitive(Primitive::MakeString))
  env_define(env, "string?", Value::Primitive(Primitive::StringP))
  env_define(env, "string-length", Value::Primitive(Primitive::StringLength))
  env_define(env, "string-append", Value::Primitive(Primitive::StringAppend))
  env_define(env, "string-ref", Value::Primitive(Primitive::StringRef))
  env_define(env, "string-set!", Value::Primitive(Primitive::StringSet))
  env_define(env, "string-copy", Value::Primitive(Primitive::StringCopy))
  env_define(env, "substring", Value::Primitive(Primitive::Substring))
  env_define(env, "string-copy!", Value::Primitive(Primitive::StringCopyBang))
  env_define(env, "string-fill!", Value::Primitive(Primitive::StringFill))
  env_define(env, "string->list", Value::Primitive(Primitive::StringToList))
  env_define(env, "list->string", Value::Primitive(Primitive::ListToString))
  env_define(env, "string-map", Value::Primitive(Primitive::StringMap))
  env_define(env, "string-for-each", Value::Primitive(Primitive::StringForEach))
  env_define(env, "string-upcase", Value::Primitive(Primitive::StringUpcase))
  env_define(env, "string-downcase", Value::Primitive(Primitive::StringDowncase))
  env_define(env, "string-foldcase", Value::Primitive(Primitive::StringFoldcase))
  env_define(env, "vector", Value::Primitive(Primitive::Vector))
  env_define(env, "make-vector", Value::Primitive(Primitive::MakeVector))
  env_define(env, "vector?", Value::Primitive(Primitive::VectorP))
  env_define(env, "vector-length", Value::Primitive(Primitive::VectorLength))
  env_define(env, "vector-ref", Value::Primitive(Primitive::VectorRef))
  env_define(env, "vector-set!", Value::Primitive(Primitive::VectorSet))
  env_define(env, "vector-fill!", Value::Primitive(Primitive::VectorFill))
  env_define(env, "vector-copy", Value::Primitive(Primitive::VectorCopy))
  env_define(env, "vector-copy!", Value::Primitive(Primitive::VectorCopyBang))
  env_define(env, "vector-append", Value::Primitive(Primitive::VectorAppend))
  env_define(env, "vector-map", Value::Primitive(Primitive::VectorMap))
  env_define(env, "vector-for-each", Value::Primitive(Primitive::VectorForEach))
  env_define(env, "vector->list", Value::Primitive(Primitive::VectorToList))
  env_define(env, "list->vector", Value::Primitive(Primitive::ListToVector))
  env_define(env, "bytevector", Value::Primitive(Primitive::ByteVector))
  env_define(env, "make-bytevector", Value::Primitive(Primitive::MakeByteVector))
  env_define(env, "bytevector?", Value::Primitive(Primitive::ByteVectorP))
  env_define(env, "bytevector-length", Value::Primitive(Primitive::ByteVectorLength))
  env_define(env, "bytevector-u8-ref", Value::Primitive(Primitive::ByteVectorU8Ref))
  env_define(env, "bytevector-u8-set!", Value::Primitive(Primitive::ByteVectorU8Set))
  env_define(env, "bytevector-copy", Value::Primitive(Primitive::ByteVectorCopy))
  env_define(env, "bytevector-copy!", Value::Primitive(Primitive::ByteVectorCopyBang))
  env_define(env, "bytevector-append", Value::Primitive(Primitive::ByteVectorAppend))
  env_define(env, "bytevector-fill!", Value::Primitive(Primitive::ByteVectorFill))
  env_define(env, "bytevector->u8-list", Value::Primitive(Primitive::ByteVectorToU8List))
  env_define(env, "u8-list->bytevector", Value::Primitive(Primitive::U8ListToByteVector))
  env_define(env, "display", Value::Primitive(Primitive::Display))
  env_define(env, "write", Value::Primitive(Primitive::Write))
  env_define(env, "newline", Value::Primitive(Primitive::Newline))
  env_define(env, "open-output-string", Value::Primitive(Primitive::OpenOutputString))
  env_define(env, "get-output-string", Value::Primitive(Primitive::GetOutputString))
  env_define(env, "current-output-port", Value::Primitive(Primitive::CurrentOutputPort))
  env_define(
    env,
    "with-exception-handler",
    Value::Primitive(Primitive::WithExceptionHandler),
  )
  env_define(env, "raise", Value::Primitive(Primitive::Raise))
  env_define(env, "raise-continuable", Value::Primitive(Primitive::RaiseContinuable))
  env_define(env, "call/cc", Value::Primitive(Primitive::CallCC))
  env_define(
    env,
    "call-with-current-continuation",
    Value::Primitive(Primitive::CallCC),
  )
  env
}

///|
let closure_counter : Ref[Int] = Ref::new(0)

///|
fn next_closure_id() -> Int {
  let id = closure_counter.val
  closure_counter.val = id + 1
  id
}

///|
fn make_closure(params : Array[String], body : Array[Datum], env : Env) -> Closure {
  { id: next_closure_id(), params, body, env }
}

///|
let parameter_counter : Ref[Int] = Ref::new(0)

///|
fn next_parameter_id() -> Int {
  let id = parameter_counter.val
  parameter_counter.val = id + 1
  id
}

///|
fn make_parameter(value : Value) -> Parameter {
  { id: next_parameter_id(), value: Ref::new(value), converter: None }
}

///|
fn value_as_parameter(value : Value) -> Parameter raise EvalError {
  match value {
    Value::Parameter(param) => param
    _ => raise EvalError("type error: parameter expected")
  }
}

///|
fn parameter_get(param : Parameter) -> Value {
  param.value.val
}

///|
fn parameter_set(param : Parameter, value : Value) -> Unit {
  param.value.val = value
}

///|
fn parameter_with_converter(value : Value, converter : Value) -> Parameter {
  { id: next_parameter_id(), value: Ref::new(value), converter: Some(converter) }
}

///|
fn make_case_closure(clauses : Array[CaseClause], env : Env) -> CaseClosure {
  { id: next_closure_id(), clauses, env }
}

///|
fn make_guard_handler(
  name : String,
  clauses : Array[Datum],
  env : Env,
  resume_value : Value,
  handlers : Array[Value],
) -> GuardHandler {
  GuardHandler::{ id: next_guard_handler_id(), name, clauses, env, resume_value, handlers }
}

///|
fn capture_continuation(kont : Kont, handlers : Array[Value]) -> Continuation {
  let winds_copy : Array[Winder] = []
  for item in wind_stack.val {
    winds_copy.push(item)
  }
  Continuation::{ id: next_continuation_id(), kont, handlers, winds: winds_copy }
}

///|
priv enum Kont {
  Halt
  If(Datum, Datum, Env, Kont)
  Begin(Array[Datum], Env, Kont)
  Set(String, Env, Kont)
  Define(String, Env, Kont)
  AppFun(Array[Datum], Env, Kont)
  AppArgs(Value, Array[Value], Array[Datum], Env, Kont)
  And(Array[Datum], Env, Kont)
  Or(Array[Datum], Env, Kont)
  Cond(Array[Datum], Array[Datum], Env, Kont)
  CondArrow(Value, Kont)
  Let(Array[(String, Datum)], Int, Array[Value], Env, Array[Datum], Kont)
  LetStar(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
  LetRec(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
  LetValues(
    Array[(Array[String], Datum)],
    Int,
    Array[Array[Value]],
    Env,
    Array[Datum],
    Kont,
  )
  LetStarValues(Array[(Array[String], Datum)], Int, Env, Array[Datum], Kont)
  DefineValues(Array[String], Env, Kont)
  ParameterizeParam(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont,
  )
  ParameterizeValue(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont,
  )
  ParameterizeConvert(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont,
  )
  MakeParameter(Value, Kont)
  SetParameter(Parameter, Kont)
  GuardResult(GuardInfo)
  GuardCond(Array[Datum], Array[Datum], Env, GuardInfo)
  GuardCondArrow(Value, GuardInfo)
  Case(Array[Datum], Env, Kont)
  CaseArrow(Value, Kont)
  CallWithValues(Value, Kont)
  RecordConstructorDone(Kont)
  RecordProtocolNApply(RecordType, Array[Value], Kont)
  RecordProtocolNResult(RecordType, Kont)
  DefineRecordType(String, String, RecordType, Array[RecordFieldBinding], Env, Kont)
  MapStep(Value, Array[Datum], Array[Datum], Bool, Kont)
  VectorMapFinalize(Kont)
  StringMapFinalize(Kont)
  WindEnter(Value, Winder, Kont)
  WindExit(Winder, Kont)
  WindActions(Array[WindAction], Value, Kont, Array[Value])
  WindPush(Winder, Array[WindAction], Value, Kont, Array[Value])
  RaiseNonCont
  RaiseCont(Kont)
  RestoreHandlers(Array[Value], Kont)
}

///|
struct Continuation {
  id : Int
  kont : Kont
  handlers : Array[Value]
  winds : Array[Winder]
}

///|
priv struct ParamBinding {
  param : Parameter
  old_value : Value
  new_value : Value
}

///|
priv struct RecordFieldBinding {
  accessor : String
  index : Int
  mutator : String?
}

///|
priv struct GuardInfo {
  condition : Value
  resume_value : Value
  handlers : Array[Value]
  raise_kont : Kont
  continuable : Bool
}

///|
priv enum WinderKind {
  Proc(Value, Value)
  Params(Array[ParamBinding])
}

///|
priv struct Winder {
  id : Int
  kind : WinderKind
}

///|
priv enum WindAction {
  After(Winder)
  Before(Winder)
  SwitchHandlers(Array[Value])
}

///|
priv enum MachineState {
  Eval(Datum, Env, Kont, Array[Value])
  Apply(Value, Kont, Array[Value])
}

///|
let winder_counter : Ref[Int] = Ref::new(0)

///|
fn next_winder_id() -> Int {
  let id = winder_counter.val
  winder_counter.val = id + 1
  id
}

///|
fn make_proc_winder(before : Value, after : Value) -> Winder {
  Winder::{ id: next_winder_id(), kind: WinderKind::Proc(before, after) }
}

///|
fn make_param_winder(bindings : Array[ParamBinding]) -> Winder {
  Winder::{ id: next_winder_id(), kind: WinderKind::Params(bindings) }
}

///|
let wind_stack : Ref[Array[Winder]] = Ref::new([])

///|
let continuation_counter : Ref[Int] = Ref::new(0)
let guard_handler_counter : Ref[Int] = Ref::new(0)

///|
fn next_continuation_id() -> Int {
  let id = continuation_counter.val
  continuation_counter.val = id + 1
  id
}

///|
fn next_guard_handler_id() -> Int {
  let id = guard_handler_counter.val
  guard_handler_counter.val = id + 1
  id
}

///|
let do_counter : Ref[Int] = Ref::new(0)

///|
fn next_do_name() -> String {
  let id = do_counter.val
  do_counter.val = id + 1
  "__do_loop__\{id}"
}

///|
///|
pub fn eval_program(src : String) -> Value raise {
  let results = eval_program_all(src)
  if results.is_empty() {
    Value::Void
  } else {
    results[results.length() - 1]
  }
}

///|
pub fn eval_program_all(src : String) -> Array[Value] raise {
  let forms = parse_program(src)
  reset_library_registry()
  reset_record_type_registry()
  set_current_output_port(new_output_string_port())
  wind_stack.val = []
  let env = init_env()
  load_stdlib(env)
  let results : Array[Value] = []
  for form in forms {
    let value = eval_expr(form, env)
    results.push(value)
  }
  results
}

///|
fn stdlib_source() -> String {
  let lines : Array[String] = [
    "(define (fold-left f acc lst)",
    "  (let loop ((acc acc) (lst lst))",
    "    (if (null? lst) acc",
    "        (loop (f acc (car lst)) (cdr lst)))))",
    "(define (fold-right f acc lst)",
    "  (if (null? lst) acc",
    "      (f (car lst) (fold-right f acc (cdr lst)))))",
    "(define (filter pred lst)",
    "  (cond ((null? lst) '())",
    "        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))",
    "        (else (filter pred (cdr lst)))))",
    "(define (remove pred lst)",
    "  (filter (lambda (x) (not (pred x))) lst))",
    "(define (find pred lst)",
    "  (cond ((null? lst) #f)",
    "        ((pred (car lst)) (car lst))",
    "        (else (find pred (cdr lst)))))",
    "(define (any pred lst)",
    "  (cond ((null? lst) #f)",
    "        ((pred (car lst)) #t)",
    "        (else (any pred (cdr lst)))))",
    "(define (every pred lst)",
    "  (cond ((null? lst) #t)",
    "        ((pred (car lst)) (every pred (cdr lst)))",
    "        (else #f)))",
    "(define (take lst n)",
    "  (if (<= n 0) '()",
    "      (cons (car lst) (take (cdr lst) (- n 1)))))",
    "(define (drop lst n)",
    "  (if (<= n 0) lst",
    "      (drop (cdr lst) (- n 1))))",
    "(define (take-while pred lst)",
    "  (cond ((null? lst) '())",
    "        ((pred (car lst)) (cons (car lst) (take-while pred (cdr lst))))",
    "        (else '())))",
    "(define (drop-while pred lst)",
    "  (cond ((null? lst) '())",
    "        ((pred (car lst)) (drop-while pred (cdr lst)))",
    "        (else lst)))",
    "(define (append-map f lst)",
    "  (if (null? lst) '()",
    "      (append (f (car lst)) (append-map f (cdr lst)))))",
    "(define (count pred lst)",
    "  (fold-left (lambda (acc x) (if (pred x) (+ acc 1) acc)) 0 lst))",
    "(define (list-index pred lst)",
    "  (let loop ((lst lst) (idx 0))",
    "    (cond ((null? lst) #f)",
    "          ((pred (car lst)) idx)",
    "          (else (loop (cdr lst) (+ idx 1))))))",
    "(define (partition pred lst)",
    "  (let loop ((lst lst) (yes '()) (no '()))",
    "    (if (null? lst)",
    "        (list (reverse yes) (reverse no))",
    "        (if (pred (car lst))",
    "            (loop (cdr lst) (cons (car lst) yes) no)",
    "            (loop (cdr lst) yes (cons (car lst) no))))))",
    "(define (zip xs ys)",
    "  (if (or (null? xs) (null? ys))",
    "      '()",
    "      (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))",
    "(define (unzip pairs)",
    "  (let loop ((pairs pairs) (xs '()) (ys '()))",
    "    (if (null? pairs)",
    "        (list (reverse xs) (reverse ys))",
    "        (let ((p (car pairs)))",
    "          (loop (cdr pairs) (cons (car p) xs) (cons (cadr p) ys))))))",
    "(define (last lst)",
    "  (if (null? (cdr lst)) (car lst) (last (cdr lst))))",
    "(define (string-join strs sep)",
    "  (if (null? strs) \"\"",
    "      (let loop ((rest (cdr strs)) (acc (car strs)))",
    "        (if (null? rest) acc",
    "            (loop (cdr rest) (string-append acc sep (car rest)))))))",
    "(define (list-tabulate n f)",
    "  (let loop ((idx 0) (acc '()))",
    "    (if (>= idx n)",
    "        (reverse acc)",
    "        (loop (+ idx 1) (cons (f idx) acc)))))",
  ]
  lines.join("\n")
}

///|
fn load_stdlib(env : Env) -> Unit raise {
  let forms = parse_program(stdlib_source())
  for form in forms {
    let _ = eval_expr(form, env)
  }
}

///|
fn eval_expr(expr : Datum, env : Env) -> Value raise EvalError {
  let handlers : Array[Value] = []
  let mut state = MachineState::Eval(expr, env, Kont::Halt, handlers)
  while true {
    match state {
      MachineState::Eval(current_expr, current_env, kont, handlers) => {
        match current_expr {
          Datum::Nil
          | Datum::Bool(_)
          | Datum::Int(_)
          | Datum::Rat(_, _)
          | Datum::Float(_)
          | Datum::Complex(_, _)
          | Datum::Char(_)
          | Datum::String(_)
          | Datum::Vector(_)
          | Datum::ByteVector(_) =>
            state = MachineState::Apply(Value::Datum(current_expr), kont, handlers)
          Datum::Symbol(name) => {
            match env_get_macro(current_env, name) {
              Some(rules) => {
                let expanded = expand_macro(rules, Datum::Symbol(name), current_env)
                state = MachineState::Eval(expanded, current_env, kont, handlers)
              }
              None => {
                let value = env_lookup(current_env, name)
                state = MachineState::Apply(value, kont, handlers)
              }
            }
          }
          Datum::Pair(_, _) => state = eval_pair(current_expr, current_env, kont, handlers)
        }
      }
      MachineState::Apply(value, kont, handlers) => {
        match kont {
          Kont::Halt => return value
          Kont::If(then_expr, else_expr, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = MachineState::Eval(else_expr, env, next, handlers)
            } else {
              state = MachineState::Eval(then_expr, env, next, handlers)
            }
          }
          Kont::Begin(rest, env, next) => {
            if rest.is_empty() {
              state = MachineState::Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Begin(tail, env, next), handlers)
            }
          }
          Kont::Set(name, env, next) => {
            let single = expect_single(value)
            env_set(env, name, single)
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::Define(name, env, next) => {
            let single = expect_single(value)
            env_define(env, name, single)
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::AppFun(args, env, next) => {
            let proc_value = expect_single(value)
            if args.is_empty() {
              state = apply_proc(proc_value, [], next, handlers)
            } else {
              let rest = args.sub(start=1).to_array()
              state = MachineState::Eval(
                args[0],
                env,
                Kont::AppArgs(proc_value, [], rest, env, next),
                handlers,
              )
            }
          }
          Kont::AppArgs(proc, evaluated, remaining, env, next) => {
            let arg_value = expect_single(value)
            let next_values : Array[Value] = []
            for item in evaluated {
              next_values.push(item)
            }
            next_values.push(arg_value)
            if remaining.is_empty() {
              state = apply_proc(proc, next_values, next, handlers)
            } else {
              let rest = remaining.sub(start=1).to_array()
              state = MachineState::Eval(
                remaining[0],
                env,
                Kont::AppArgs(proc, next_values, rest, env, next),
                handlers,
              )
            }
          }
          Kont::And(rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = MachineState::Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::And(tail, env, next), handlers)
            }
          }
          Kont::Or(rest, env, next) => {
            let test_value = expect_single(value)
            if !is_false(test_value) {
              state = MachineState::Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = MachineState::Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Or(tail, env, next), handlers)
            }
          }
          Kont::Cond(parts, rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = cond_start(rest, env, next, handlers)
            } else if parts.length() == 1 {
              state = MachineState::Apply(test_value, next, handlers)
            } else if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
              if parts.length() != 3 {
                raise EvalError("invalid cond")
              }
              state = MachineState::Eval(parts[2], env, Kont::CondArrow(test_value, next), handlers)
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          Kont::CondArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          Kont::Let(bindings, index, values, env, body, next) => {
            let single = expect_single(value)
            let next_values : Array[Value] = []
            for item in values {
              next_values.push(item)
            }
            next_values.push(single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::Let(bindings, next_index, next_values, env, body, next),
                handlers,
              )
            } else {
              let new_env = env_extend(env)
              let mut i = 0
              while i < bindings.length() {
                let (name, _) = bindings[i]
                env_define(new_env, name, next_values[i])
                i = i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetStar(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            let new_env = env_extend(env)
            env_define(new_env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                Kont::LetStar(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetRec(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            env_set(env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::LetRec(bindings, next_index, env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          Kont::LetValues(bindings, index, collected, env, body, next) => {
            let values = value_to_values(value)
            let (names, _) = bindings[index]
            if values.length() != names.length() {
              raise arity_mismatch(names.length(), values.length())
            }
            let next_collected : Array[Array[Value]] = []
            for item in collected {
              next_collected.push(item)
            }
            next_collected.push(values)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::LetValues(bindings, next_index, next_collected, env, body, next),
                handlers,
              )
            } else {
              let new_env = env_extend(env)
              let mut i = 0
              while i < bindings.length() {
                let (names, _) = bindings[i]
                let bound_values = next_collected[i]
                let mut j = 0
                while j < names.length() {
                  env_define(new_env, names[j], bound_values[j])
                  j = j + 1
                }
                i = i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetStarValues(bindings, index, env, body, next) => {
            let values = value_to_values(value)
            let (names, _) = bindings[index]
            if values.length() != names.length() {
              raise arity_mismatch(names.length(), values.length())
            }
            let new_env = env_extend(env)
            let mut i = 0
            while i < names.length() {
              env_define(new_env, names[i], values[i])
              i = i + 1
            }
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                Kont::LetStarValues(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::DefineValues(names, env, next) => {
            let values = value_to_values(value)
            if values.length() != names.length() {
              raise arity_mismatch(names.length(), values.length())
            }
            let mut i = 0
            while i < names.length() {
              env_define(env, names[i], values[i])
              i = i + 1
            }
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::ParameterizeParam(bindings, index, params, values, env, body, next) => {
            let param_value = expect_single(value)
            let param = value_as_parameter(param_value)
            let next_params : Array[Parameter] = []
            for item in params {
              next_params.push(item)
            }
            next_params.push(param)
            let (_, expr) = bindings[index]
            state = MachineState::Eval(
              expr,
              env,
              Kont::ParameterizeValue(bindings, index, next_params, values, env, body, next),
              handlers,
            )
          }
          Kont::ParameterizeValue(bindings, index, params, values, env, body, next) => {
            let new_value = expect_single(value)
            let param = params[index]
            match param.converter {
              Some(converter) =>
                state = apply_proc(
                  converter,
                  [new_value],
                  Kont::ParameterizeConvert(bindings, index, params, values, env, body, next),
                  handlers,
                )
              None => {
                let next_values : Array[Value] = []
                for item in values {
                  next_values.push(item)
                }
                next_values.push(new_value)
                let next_index = index + 1
                if next_index < bindings.length() {
                  let (param_expr, _) = bindings[next_index]
                  state = MachineState::Eval(
                    param_expr,
                    env,
                    Kont::ParameterizeParam(
                      bindings,
                      next_index,
                      params,
                      next_values,
                      env,
                      body,
                      next,
                    ),
                    handlers,
                  )
                } else {
                  let bindings_info : Array[ParamBinding] = []
                  let mut i = 0
                  while i < params.length() {
                    let current = params[i]
                    let old_value = parameter_get(current)
                    let new_val = next_values[i]
                    bindings_info.push(
                      ParamBinding::{ param: current, old_value: old_value, new_value: new_val },
                    )
                    parameter_set(current, new_val)
                    i = i + 1
                  }
                  let winder = make_param_winder(bindings_info)
                  wind_push(winder)
                  state = eval_sequence_state(body, env, Kont::WindExit(winder, next), handlers)
                }
              }
            }
          }
          Kont::ParameterizeConvert(bindings, index, params, values, env, body, next) => {
            let converted = expect_single(value)
            let next_values : Array[Value] = []
            for item in values {
              next_values.push(item)
            }
            next_values.push(converted)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (param_expr, _) = bindings[next_index]
              state = MachineState::Eval(
                param_expr,
                env,
                Kont::ParameterizeParam(
                  bindings,
                  next_index,
                  params,
                  next_values,
                  env,
                  body,
                  next,
                ),
                handlers,
              )
            } else {
              let bindings_info : Array[ParamBinding] = []
              let mut i = 0
              while i < params.length() {
                let current = params[i]
                let old_value = parameter_get(current)
                let new_val = next_values[i]
                bindings_info.push(
                  ParamBinding::{ param: current, old_value: old_value, new_value: new_val },
                )
                parameter_set(current, new_val)
                i = i + 1
              }
              let winder = make_param_winder(bindings_info)
              wind_push(winder)
              state = eval_sequence_state(body, env, Kont::WindExit(winder, next), handlers)
            }
          }
          Kont::MakeParameter(converter, next) => {
            let converted = expect_single(value)
            let param = parameter_with_converter(converted, converter)
            state = MachineState::Apply(Value::Parameter(param), next, handlers)
          }
          Kont::SetParameter(param, next) => {
            let converted = expect_single(value)
            parameter_set(param, converted)
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::GuardResult(info) => {
            let guard_value = expect_single(value)
            state = apply_proc(info.resume_value, [guard_value], kont, info.handlers)
          }
          Kont::GuardCond(parts, rest, env, info) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = guard_start(rest, env, info, handlers)
            } else if parts.length() == 1 {
              state = apply_proc(info.resume_value, [test_value], kont, info.handlers)
            } else if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
              if parts.length() != 3 {
                raise EvalError("invalid guard")
              }
              state = MachineState::Eval(parts[2], env, Kont::GuardCondArrow(test_value, info), handlers)
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(body, env, Kont::GuardResult(info), handlers)
            }
          }
          Kont::GuardCondArrow(test_value, info) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], Kont::GuardResult(info), handlers)
          }
          Kont::Case(clauses, env, next) => {
            let key_value = expect_single(value)
            state = case_dispatch(key_value, clauses, env, next, handlers)
          }
          Kont::CaseArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          Kont::CallWithValues(consumer, next) => {
            let values = value_to_values(value)
            state = apply_proc(consumer, values, next, handlers)
          }
          Kont::RecordConstructorDone(next) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise EvalError("type error: procedure expected")
            }
            state = MachineState::Apply(ctor_value, next, handlers)
          }
          Kont::RecordProtocolNApply(record_type, parent_args, next) => {
            let ctor_value = expect_single(value)
            state = apply_proc(
              ctor_value,
              parent_args,
              Kont::RecordProtocolNResult(record_type, next),
              handlers,
            )
          }
          Kont::RecordProtocolNResult(record_type, next) => {
            let record_value = expect_single(value)
            let record = value_as_record(record_value)
            let parent_type = match record_type.parent {
              Some(parent) => parent
              None => raise EvalError("invalid record constructor descriptor")
            }
            if !record_type_is_a(record.record_type, parent_type) {
              raise EvalError("type error: record type mismatch: expected \{parent_type.name}")
            }
            let parent_values : Array[Value] = []
            for field in record.fields {
              parent_values.push(field.val)
            }
            let proc = Value::RecordProc(
              make_record_proc(RecordProcKind::ProtocolP(record_type, parent_values)),
            )
            state = MachineState::Apply(proc, next, handlers)
          }
          Kont::DefineRecordType(ctor_name, pred_name, record_type, bindings, env, next) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise EvalError("type error: procedure expected")
            }
            env_define(env, ctor_name, ctor_value)
            env_define(
              env,
              pred_name,
              Value::RecordProc(make_record_proc(RecordProcKind::Predicate(record_type))),
            )
            for binding in bindings {
              env_define(
                env,
                binding.accessor,
                Value::RecordProc(make_record_proc(RecordProcKind::Accessor(record_type, binding.index))),
              )
              match binding.mutator {
                Some(mut_name) => {
                  env_define(
                    env,
                    mut_name,
                    Value::RecordProc(
                      make_record_proc(RecordProcKind::Mutator(record_type, binding.index)),
                    ),
                  )
                }
                None => ()
              }
            }
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::MapStep(proc, lists, results, for_each, next) => {
            let single = expect_single(value)
            let next_results = results
            if !for_each {
              next_results.push(value_as_datum(single))
            }
            state = map_step(proc, lists, next_results, for_each, next, handlers)
          }
          Kont::VectorMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let items = datum_list_to_array(result_datum)
            state = MachineState::Apply(Value::Datum(Datum::Vector(items)), next, handlers)
          }
          Kont::StringMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let string_value =
              apply_primitive(Primitive::ListToString, [Value::Datum(result_datum)])
            state = MachineState::Apply(string_value, next, handlers)
          }
          Kont::WindEnter(thunk, winder, next) => {
            wind_push(winder)
            state = apply_proc(thunk, [], Kont::WindExit(winder, next), handlers)
          }
          Kont::WindExit(winder, next) => {
            let actions = [WindAction::After(winder)]
            state = run_wind_actions(actions, value, next, handlers, handlers)
          }
          Kont::WindActions(actions, resume_value, final_kont, final_handlers) => {
            let _ = value
            state = run_wind_actions(actions, resume_value, final_kont, final_handlers, handlers)
          }
          Kont::WindPush(winder, actions, resume_value, final_kont, final_handlers) => {
            let _ = value
            wind_push(winder)
            state = run_wind_actions(actions, resume_value, final_kont, final_handlers, handlers)
          }
          Kont::RaiseNonCont => raise EvalError("non-continuable exception")
          Kont::RaiseCont(next) => state = MachineState::Apply(value, next, handlers)
          Kont::RestoreHandlers(prev_handlers, next) =>
            state = MachineState::Apply(value, next, prev_handlers)
        }
      }
    }
  }
  Value::Void
}

///|
///|
fn eval_quote(args : Array[Datum]) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(args[0])
    n => raise arity_mismatch(1, n)
  }
}

///|
fn eval_quasiquote(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(quasiquote_expand(args[0], env, 1))
    n => raise arity_mismatch(1, n)
  }
}

///|
fn unwrap_form(expr : Datum, name : String) -> Datum? {
  match expr {
    Datum::Pair(head, tail) =>
      match head.val {
        Datum::Symbol(head_name) if head_name == name =>
          match tail.val {
            Datum::Pair(arg, rest) =>
              match rest.val {
                Datum::Nil => Some(arg.val)
                _ => None
              }
            _ => None
          }
        _ => None
      }
    _ => None
  }
}

///|
fn quasiquote_expand(expr : Datum, env : Env, depth : Int) -> Datum raise EvalError {
  match unwrap_form(expr, "quasiquote") {
    Some(arg) =>
      return list_from_array([Datum::Symbol("quasiquote"), quasiquote_expand(arg, env, depth + 1)])
    None => ()
  }
  match unwrap_form(expr, "unquote") {
    Some(arg) => {
      if depth == 1 {
        return value_as_datum(eval_expr(arg, env))
      } else {
        return list_from_array([Datum::Symbol("unquote"), quasiquote_expand(arg, env, depth - 1)])
      }
    }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) => {
      if depth == 1 {
        raise EvalError("unquote-splicing not in list context")
      } else {
        return list_from_array([
          Datum::Symbol("unquote-splicing"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    }
    None => ()
  }
  match expr {
    Datum::Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unquote-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasiquote_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasiquote_expand(car.val, env, depth),
        quasiquote_expand(cdr.val, env, depth),
      )
    }
    _ => expr
  }
}

///|
fn list_with_tail(items : Array[Datum], tail : Datum) -> Datum {
  let mut result = tail
  for item in items.rev_iter() {
    result = pair_new(item, result)
  }
  result
}
///|
fn parse_params(expr : Datum) -> Array[String] raise EvalError {
  let params = datum_list_to_array(expr)
  let names : Array[String] = []
  for param in params {
    match param {
      Datum::Symbol(name) => names.push(name)
      _ => raise EvalError("type error: symbol expected")
    }
  }
  names
}

///|
fn eval_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let params = parse_params(args[0])
    let body = args.sub(start=1).to_array()
    Value::Closure(make_closure(params, body, env))
  }
}

///|
fn eval_case_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.is_empty() {
    raise arity_mismatch(1, 0)
  }
  let clauses : Array[CaseClause] = []
  for clause in args {
    let parts = datum_list_to_array(clause)
    if parts.length() < 2 {
      raise EvalError("invalid case-lambda")
    }
    let params = parse_params(parts[0])
    let body = parts.sub(start=1).to_array()
    clauses.push(CaseClause::{ params, body })
  }
  Value::CaseClosure(make_case_closure(clauses, env))
}

///|
fn eval_define_syntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => {
          let rules = parse_transformer(args[1], name)
          env_define(env, name, Value::Macro(rules))
          Value::Void
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn parse_syntax_bindings(expr : Datum) -> Array[(String, SyntaxRules)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, SyntaxRules)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: syntax binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => {
          let rules = parse_transformer(parts[1], name)
          parsed.push((name, rules))
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_bindings(expr : Datum) -> Array[(String, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => parsed.push((name, parts[1]))
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_value_bindings(expr : Datum) -> Array[(Array[String], Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(Array[String], Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("invalid let-values")
    } else {
      let names = parse_params(parts[0])
      parsed.push((names, parts[1]))
    }
  }
  parsed
}

///|
fn parse_parameterize_bindings(expr : Datum) -> Array[(Datum, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(Datum, Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("invalid parameterize")
    } else {
      parsed.push((parts[0], parts[1]))
    }
  }
  parsed
}

///|
fn parse_symbol(expr : Datum) -> String raise EvalError {
  match expr {
    Datum::Symbol(name) => name
    _ => raise EvalError("type error: symbol expected")
  }
}

///|
fn parse_bool(expr : Datum) -> Bool raise EvalError {
  match expr {
    Datum::Bool(value) => value
    _ => raise EvalError("invalid define-record-type")
  }
}

///|
fn parse_symbol_list(items : Array[Datum]) -> Array[String] raise EvalError {
  let names : Array[String] = []
  for item in items {
    names.push(parse_symbol(item))
  }
  names
}

///|
fn expect_single(value : Value) -> Value raise EvalError {
  match value {
    Value::Values(values) => {
      if values.length() == 1 {
        values[0]
      } else {
        raise EvalError("multiple values in single-value context")
      }
    }
    _ => value
  }
}

///|
fn value_to_values(value : Value) -> Array[Value] {
  match value {
    Value::Values(values) => values
    _ => {
      let values : Array[Value] = []
      values.push(value)
      values
    }
  }
}

///|
fn raise_to_handlers(
  condition : Value,
  handlers : Array[Value],
  kont : Kont,
  continuable : Bool,
) -> MachineState raise EvalError {
  if handlers.is_empty() {
    raise EvalError("uncaught exception")
  } else {
    let handler = handlers[handlers.length() - 1]
    if continuable {
      apply_proc(handler, [condition], Kont::RaiseCont(kont), handlers)
    } else {
      apply_proc(handler, [condition], Kont::RaiseNonCont, handlers)
    }
  }
}

///|
fn guard_start(
  clauses : Array[Datum],
  env : Env,
  info : GuardInfo,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, Kont::GuardResult(info), handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(parts[0], env, Kont::GuardCond(parts, rest, env, info), handlers)
      }
    }
  }
  raise_to_handlers(info.condition, info.handlers, info.raise_kont, info.continuable)
}

///|
fn handle_guard(
  handler : GuardHandler,
  condition : Value,
  raise_kont : Kont,
  continuable : Bool,
) -> MachineState raise EvalError {
  let guard_env = env_extend(handler.env)
  env_define(guard_env, handler.name, condition)
  let info = GuardInfo::{
    condition,
    resume_value: handler.resume_value,
    handlers: handler.handlers,
    raise_kont,
    continuable,
  }
  guard_start(handler.clauses, guard_env, info, handler.handlers)
}

///|
fn wind_push(winder : Winder) -> Unit {
  let next : Array[Winder] = []
  for item in wind_stack.val {
    next.push(item)
  }
  next.push(winder)
  wind_stack.val = next
}

///|
fn wind_pop() -> Winder {
  let current = wind_stack.val
  let last_index = current.length() - 1
  let winder = current[last_index]
  let next = current.sub(start=0, end=last_index).to_array()
  wind_stack.val = next
  winder
}

///|
fn apply_winder_before(
  winder : Winder,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match winder.kind {
    WinderKind::Proc(before, _) => apply_proc(before, [], kont, handlers)
    WinderKind::Params(bindings) => {
      for binding in bindings {
        parameter_set(binding.param, binding.new_value)
      }
      MachineState::Apply(Value::Void, kont, handlers)
    }
  }
}

///|
fn apply_winder_after(
  winder : Winder,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match winder.kind {
    WinderKind::Proc(_, after) => apply_proc(after, [], kont, handlers)
    WinderKind::Params(bindings) => {
      for binding in bindings {
        parameter_set(binding.param, binding.old_value)
      }
      MachineState::Apply(Value::Void, kont, handlers)
    }
  }
}

///|
fn common_wind_prefix(current : Array[Winder], target : Array[Winder]) -> Int {
  let mut i = 0
  while i < current.length() && i < target.length() {
    if current[i].id != target[i].id {
      break
    }
    i = i + 1
  }
  i
}

///|
///|
fn run_wind_actions(
  actions : Array[WindAction],
  resume_value : Value,
  final_kont : Kont,
  final_handlers : Array[Value],
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if actions.is_empty() {
    MachineState::Apply(resume_value, final_kont, final_handlers)
  } else {
    let rest = actions.sub(start=1).to_array()
    match actions[0] {
      WindAction::After(winder) => {
        let _ = wind_pop()
        apply_winder_after(
          winder,
          Kont::WindActions(rest, resume_value, final_kont, final_handlers),
          handlers,
        )
      }
      WindAction::Before(winder) => apply_winder_before(
        winder,
        Kont::WindPush(winder, rest, resume_value, final_kont, final_handlers),
        handlers,
      )
      WindAction::SwitchHandlers(next_handlers) =>
        run_wind_actions(rest, resume_value, final_kont, final_handlers, next_handlers)
    }
  }
}

///|
fn eval_sequence_state(
  exprs : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if exprs.is_empty() {
    MachineState::Apply(Value::Void, kont, handlers)
  } else {
    let rest = exprs.sub(start=1).to_array()
    MachineState::Eval(exprs[0], env, Kont::Begin(rest, env, kont), handlers)
  }
}

///|
fn eval_if_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  } else {
    MachineState::Eval(args[0], env, Kont::If(args[1], args[2], env, kont), handlers)
  }
}

///|
fn eval_define_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
        match args.length() {
          2 => MachineState::Eval(args[1], env, Kont::Define(name, env, kont), handlers)
          n => raise arity_mismatch(2, n)
        }
      }
      Datum::Pair(_, _) => {
        let sig = datum_list_to_array(args[0])
        if sig.is_empty() {
          raise EvalError("type error: symbol expected")
        } else {
          match sig[0] {
            Datum::Symbol(name) => {
              let params = parse_params(list_from_array(sig.sub(start=1).to_array()))
              let body = args.sub(start=1).to_array()
              env_define(env, name, Value::Closure(make_closure(params, body, env)))
              MachineState::Apply(Value::Void, kont, handlers)
            }
            _ => raise EvalError("type error: symbol expected")
          }
        }
      }
      _ => raise EvalError("type error: symbol expected")
    }
  }
}

///|
fn eval_define_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args.length() {
    2 => {
      let names = parse_params(args[0])
      MachineState::Eval(args[1], env, Kont::DefineValues(names, env, kont), handlers)
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn eval_guard_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let spec = datum_list_to_array(args[0])
    if spec.is_empty() {
      raise EvalError("invalid guard")
    }
    let name = parse_symbol(spec[0])
    let clauses = spec.sub(start=1).to_array()
    let body = args.sub(start=1).to_array()
    let resume_value = Value::Continuation(capture_continuation(kont, handlers))
    let guard_handlers : Array[Value] = []
    for item in handlers {
      guard_handlers.push(item)
    }
    let handler = Value::GuardHandler(
      make_guard_handler(name, clauses, env, resume_value, guard_handlers),
    )
    let next_handlers : Array[Value] = []
    for item in handlers {
      next_handlers.push(item)
    }
    next_handlers.push(handler)
    eval_sequence_state(body, env, Kont::RestoreHandlers(handlers, kont), next_handlers)
  }
}

///|
fn eval_parameterize_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_parameterize_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (param_expr, _) = bindings[0]
      MachineState::Eval(
        param_expr,
        env,
        Kont::ParameterizeParam(bindings, 0, [], [], env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_set_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => {
          match env_get_macro(env, name) {
            Some(rules) => {
              let expanded = expand_macro(
                rules,
                list_from_array([Datum::Symbol("set!"), Datum::Symbol(name), args[1]]),
                env,
              )
              MachineState::Eval(expanded, env, kont, handlers)
            }
            None => MachineState::Eval(args[1], env, Kont::Set(name, env, kont), handlers)
          }
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn eval_let_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
        if args.length() < 3 {
          raise arity_mismatch(3, args.length())
        }
        let bindings = parse_bindings(args[1])
        let body = args.sub(start=2).to_array()
        let expanded = expand_named_let(name, bindings, body)
        MachineState::Eval(expanded, env, kont, handlers)
      }
      _ => {
        let bindings = parse_bindings(args[0])
        let body = args.sub(start=1).to_array()
        if bindings.is_empty() {
          eval_sequence_state(body, env, kont, handlers)
        } else {
          let (_, expr) = bindings[0]
          MachineState::Eval(expr, env, Kont::Let(bindings, 0, [], env, body, kont), handlers)
        }
      }
    }
  }
}

///|
fn eval_let_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::LetValues(bindings, 0, [], env, body, kont), handlers)
    }
  }
}

///|
fn eval_let_star_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::LetStarValues(bindings, 0, env, body, kont), handlers)
    }
  }
}

///|
fn eval_let_star_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::LetStar(bindings, 0, env, body, kont), handlers)
    }
  }
}

///|
fn eval_letrec_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, _) = binding
      env_define(new_env, name, Value::Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(
        expr,
        new_env,
        Kont::LetRec(bindings, 0, new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn expand_named_let(name : String, bindings : Array[(String, Datum)], body : Array[Datum]) -> Datum {
  let param_items : Array[Datum] = []
  let arg_items : Array[Datum] = []
  for binding in bindings {
    let (param, expr) = binding
    param_items.push(Datum::Symbol(param))
    arg_items.push(expr)
  }
  let lambda_items : Array[Datum] = []
  lambda_items.push(Datum::Symbol("lambda"))
  lambda_items.push(list_from_array(param_items))
  for expr in body {
    lambda_items.push(expr)
  }
  let lambda_expr = list_from_array(lambda_items)
  let binding_expr = list_from_array([Datum::Symbol(name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let call_items : Array[Datum] = []
  call_items.push(Datum::Symbol(name))
  for arg in arg_items {
    call_items.push(arg)
  }
  let call_expr = list_from_array(call_items)
  list_from_array([Datum::Symbol("letrec"), bindings_expr, call_expr])
}

///|
fn eval_let_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn eval_letrec_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn build_begin(exprs : Array[Datum]) -> Datum {
  let items : Array[Datum] = []
  items.push(Datum::Symbol("begin"))
  for expr in exprs {
    items.push(expr)
  }
  list_from_array(items)
}

///|
fn parse_do_bindings(expr : Datum) -> (Array[String], Array[Datum], Array[Datum]) raise EvalError {
  let bindings = datum_list_to_array(expr)
  let names : Array[String] = []
  let inits : Array[Datum] = []
  let steps : Array[Datum] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() < 2 || parts.length() > 3 {
      raise EvalError("invalid do")
    }
    let name = parse_symbol(parts[0])
    names.push(name)
    inits.push(parts[1])
    if parts.length() == 3 {
      steps.push(parts[2])
    } else {
      steps.push(Datum::Symbol(name))
    }
  }
  (names, inits, steps)
}

///|
fn eval_do_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let (names, inits, steps) = parse_do_bindings(args[0])
  let test_parts = datum_list_to_array(args[1])
  if test_parts.is_empty() {
    raise EvalError("invalid do")
  }
  let test_expr = test_parts[0]
  let result_exprs = test_parts.sub(start=1).to_array()
  let command_exprs = args.sub(start=2).to_array()
  let loop_name = next_do_name()
  let params : Array[Datum] = []
  for name in names {
    params.push(Datum::Symbol(name))
  }
  let call_items : Array[Datum] = []
  call_items.push(Datum::Symbol(loop_name))
  for step in steps {
    call_items.push(step)
  }
  let call_expr = list_from_array(call_items)
  let else_body : Array[Datum] = []
  for expr in command_exprs {
    else_body.push(expr)
  }
  else_body.push(call_expr)
  let then_expr = build_begin(result_exprs)
  let else_expr = build_begin(else_body)
  let if_expr = list_from_array([
    Datum::Symbol("if"),
    test_expr,
    then_expr,
    else_expr,
  ])
  let lambda_expr = list_from_array([
    Datum::Symbol("lambda"),
    list_from_array(params),
    if_expr,
  ])
  let binding_expr = list_from_array([Datum::Symbol(loop_name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let init_call_items : Array[Datum] = []
  init_call_items.push(Datum::Symbol(loop_name))
  for init in inits {
    init_call_items.push(init)
  }
  let init_call_expr = list_from_array(init_call_items)
  let expanded = list_from_array([
    Datum::Symbol("letrec"),
    bindings_expr,
    init_call_expr,
  ])
  MachineState::Eval(expanded, env, kont, handlers)
}

///|
fn eval_define_record_type_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise EvalError("invalid define-record-type")
  }
  let record_name = parse_symbol(args[0])
  let ctor_parts = datum_list_to_array(args[1])
  if ctor_parts.is_empty() {
    raise EvalError("invalid define-record-type")
  }
  let ctor_name = parse_symbol(ctor_parts[0])
  let ctor_fields = parse_symbol_list(ctor_parts.sub(start=1).to_array())
  let pred_name = parse_symbol(args[2])
  let raw_specs = args.sub(start=3).to_array()
  let mut parent_name : String? = None
  let mut protocol_expr : Datum? = None
  let mut is_sealed = false
  let mut is_opaque = false
  let mut uid : String? = None
  let field_specs : Array[Datum] = []
  for spec in raw_specs {
    let parts = datum_list_to_array(spec)
    if parts.length() >= 1 && parts.length() <= 2 {
      match symbol_name(parts[0]) {
        Some("parent") if parts.length() == 2 => {
          if parent_name is Some(_) {
            raise EvalError("invalid define-record-type")
          }
          parent_name = Some(parse_symbol(parts[1]))
          continue
        }
        Some("protocol") if parts.length() == 2 => {
          if protocol_expr is Some(_) {
            raise EvalError("invalid define-record-type")
          }
          protocol_expr = Some(parts[1])
          continue
        }
        Some("sealed") => {
          is_sealed = if parts.length() == 1 { true } else { parse_bool(parts[1]) }
          continue
        }
        Some("opaque") => {
          is_opaque = if parts.length() == 1 { true } else { parse_bool(parts[1]) }
          continue
        }
        Some("nongenerative") => {
          if uid is Some(_) {
            raise EvalError("invalid define-record-type")
          }
          if parts.length() == 1 {
            uid = Some(record_name)
          } else {
            uid = Some(parse_symbol(parts[1]))
          }
          continue
        }
        _ => ()
      }
    }
    field_specs.push(spec)
  }
  let parent_desc = match parent_name {
    Some(name) =>
      match lookup_record_type_descriptor(name) {
        Some(desc) => Some(desc)
        None => raise EvalError("invalid define-record-type")
      }
    None => None
  }
  let parent_type = match parent_desc {
    Some(desc) => Some(desc.record_type)
    None => None
  }
  match parent_type {
    Some(parent) => {
      if parent.is_sealed {
        raise EvalError("invalid define-record-type")
      }
    }
    None => ()
  }
  let parent_fields = match parent_type {
    Some(record_type) => record_type.fields
    None => []
  }
  let parent_count = parent_fields.length()
  let parent_field_names : Array[String] = []
  for field in parent_fields {
    parent_field_names.push(field.name)
  }
  let accessors : Map[String, String] = {}
  let mutators : Map[String, String] = {}
  let mutable : Map[String, Bool] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    let parts = datum_list_to_array(spec)
    if parts.length() < 2 || parts.length() > 3 {
      raise EvalError("invalid define-record-type")
    }
    let field_name = parse_symbol(parts[0])
    if array_contains_name(parent_field_names, field_name) {
      raise EvalError("invalid define-record-type")
    }
    if accessors.contains(field_name) {
      raise EvalError("invalid define-record-type")
    }
    let accessor = parse_symbol(parts[1])
    accessors[field_name] = accessor
    if parts.length() == 3 {
      let mutator = parse_symbol(parts[2])
      mutators[field_name] = mutator
      mutable[field_name] = true
    } else {
      mutable[field_name] = false
    }
    new_field_names.push(field_name)
  }
  if parent_count == 0 {
    if ctor_fields.length() != new_field_names.length() {
      raise EvalError("invalid define-record-type")
    }
  } else {
    if ctor_fields.length() != parent_count + new_field_names.length() {
      raise EvalError("invalid define-record-type")
    }
  }
  let fields : Array[RecordField] = []
  let field_index : Map[String, Int] = {}
  let mut i = 0
  while i < ctor_fields.length() {
    let field_name = ctor_fields[i]
    if field_index.contains(field_name) {
      raise EvalError("invalid define-record-type")
    }
    if i < parent_count {
      let parent_field = parent_fields[i]
      if parent_field.name != field_name {
        raise EvalError("invalid define-record-type")
      }
      fields.push(parent_field)
    } else {
      let is_mutable = match mutable.get(field_name) {
        Some(flag) => flag
        None => raise EvalError("invalid define-record-type")
      }
      fields.push(RecordField::{ name: field_name, mutable: is_mutable })
    }
    field_index[field_name] = i
    i = i + 1
  }
  let bindings : Array[RecordFieldBinding] = []
  for field_name in new_field_names {
    let idx = match field_index.get(field_name) {
      Some(value) => value
      None => raise EvalError("invalid define-record-type")
    }
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise EvalError("invalid define-record-type")
    }
    let mutator_name = mutators.get(field_name)
    bindings.push(
      RecordFieldBinding::{ accessor: accessor_name, index: idx, mutator: mutator_name },
    )
  }
  let existing_desc = match uid {
    Some(uid_name) => lookup_record_type_descriptor_by_uid(uid_name)
    None => None
  }
  let (record_type, ctor_desc) = match existing_desc {
    Some(desc) => {
      if !record_type_matches(desc.record_type, parent_type, is_sealed, is_opaque, fields) {
        raise EvalError("invalid define-record-type")
      }
      if protocol_expr is Some(_) {
        raise EvalError("invalid define-record-type")
      }
      register_record_type_alias(record_name, desc)
      (desc.record_type, desc.constructor_desc)
    }
    None => {
      let effective_opaque = match parent_type {
        Some(parent) => parent.is_opaque || is_opaque
        None => is_opaque
      }
      let record_type = make_record_type(
        record_name,
        parent_type,
        is_sealed,
        effective_opaque,
        uid,
        fields,
      )
      let protocol_value = match protocol_expr {
        Some(expr) => {
          let proc = eval_expr(expr, env)
          if !is_procedure_value(proc) {
            raise EvalError("type error: procedure expected")
          }
          Some(proc)
        }
        None => None
      }
      let parent_ctor_desc = match parent_desc {
        Some(desc) => Some(desc.constructor_desc)
        None => None
      }
      let ctor_desc = make_record_constructor_descriptor(record_type, parent_ctor_desc, protocol_value)
      let rtd = make_record_type_descriptor(record_type, ctor_desc)
      let _ = register_record_type(record_name, rtd)
      (record_type, ctor_desc)
    }
  }
  let next_kont =
    Kont::DefineRecordType(ctor_name, pred_name, record_type, bindings, env, kont)
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn record_type_is_a(actual : RecordType, target : RecordType) -> Bool {
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
///|
fn array_contains_name(items : Array[String], name : String) -> Bool {
  for item in items {
    if item == name {
      return true
    }
  }
  false
}

///|
fn value_as_record(value : Value) -> Record raise EvalError {
  match value {
    Value::Record(record) => record
    _ => raise EvalError("type error: record expected")
  }
}

///|
fn record_constructor_from_descriptor(
  desc : RecordConstructorDescriptor,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match desc.protocol {
    None => {
      let ctor = Value::RecordProc(make_record_proc(RecordProcKind::Constructor(desc.record_type)))
      MachineState::Apply(ctor, kont, handlers)
    }
    Some(protocol) => {
      if !is_procedure_value(protocol) {
        raise EvalError("type error: procedure expected")
      }
      let arg = match desc.record_type.parent {
        None => Value::RecordProc(make_record_proc(RecordProcKind::Constructor(desc.record_type)))
        Some(parent) => {
          let parent_desc = match desc.parent_desc {
            Some(p) => p
            None => default_constructor_descriptor(parent)
          }
          Value::RecordProc(make_record_proc(RecordProcKind::ProtocolN(desc.record_type, parent_desc)))
        }
      }
      apply_proc(protocol, [arg], Kont::RecordConstructorDone(kont), handlers)
    }
  }
}

///|
fn apply_record_proc(
  proc : RecordProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    RecordProcKind::Constructor(record_type) => {
      let field_count = record_type.fields.length()
      if args.length() != field_count {
        raise arity_mismatch(field_count, args.length())
      } else {
        let record = make_record_instance(record_type, args)
        MachineState::Apply(Value::Record(record), kont, handlers)
      }
    }
    RecordProcKind::Predicate(record_type) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let result = match args[0] {
          Value::Record(record) => record_type_is_a(record.record_type, record_type)
          _ => false
        }
        MachineState::Apply(bool_value(result), kont, handlers)
      }
    }
    RecordProcKind::Accessor(record_type, idx) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let record = value_as_record(args[0])
        if !record_type_is_a(record.record_type, record_type) {
          raise EvalError("type error: record type mismatch: expected \{record_type.name}")
        }
        MachineState::Apply(record.fields[idx].val, kont, handlers)
      }
    }
    RecordProcKind::Mutator(record_type, idx) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let record = value_as_record(args[0])
        if !record_type_is_a(record.record_type, record_type) {
          raise EvalError("type error: record type mismatch: expected \{record_type.name}")
        }
        let field = record.record_type.fields[idx]
        if !field.mutable {
          raise EvalError("record field is immutable: \{field.name}")
        }
        record.fields[idx].val = args[1]
        MachineState::Apply(Value::Void, kont, handlers)
      }
    }
    RecordProcKind::ProtocolN(record_type, parent_desc) => {
      let parent_type = match record_type.parent {
        Some(parent) => parent
        None => raise EvalError("invalid record constructor descriptor")
      }
      match parent_desc.protocol {
        None => {
          if args.length() != parent_type.fields.length() {
            raise arity_mismatch(parent_type.fields.length(), args.length())
          }
          let parent_values : Array[Value] = []
          for arg in args {
            parent_values.push(arg)
          }
          let proc = Value::RecordProc(
            make_record_proc(RecordProcKind::ProtocolP(record_type, parent_values)),
          )
          MachineState::Apply(proc, kont, handlers)
        }
        Some(_) => {
          let parent_args : Array[Value] = []
          for arg in args {
            parent_args.push(arg)
          }
          record_constructor_from_descriptor(
            parent_desc,
            Kont::RecordProtocolNApply(record_type, parent_args, kont),
            handlers,
          )
        }
      }
    }
    RecordProcKind::ProtocolP(record_type, parent_values) => {
      let total_fields = record_type.fields.length()
      if parent_values.length() > total_fields {
        raise EvalError("invalid record constructor descriptor")
      }
      let child_count = total_fields - parent_values.length()
      if args.length() != child_count {
        raise arity_mismatch(child_count, args.length())
      }
      let values : Array[Value] = []
      for value in parent_values {
        values.push(value)
      }
      for arg in args {
        values.push(arg)
      }
      let record = make_record_instance(record_type, values)
      MachineState::Apply(Value::Record(record), kont, handlers)
    }
  }
}

///|
fn parse_library_name(expr : Datum) -> String raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise EvalError("invalid library name")
  }
  let names = parse_symbol_list(parts)
  names.join(".")
}

///|
fn parse_export_clause(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("export") => parse_symbol_list(items.sub(start=1).to_array())
    _ => raise EvalError("invalid library")
  }
}

///|
fn parse_import_clause(expr : Datum) -> Array[String] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("import") => {
      let names : Array[String] = []
      let mut i = 1
      while i < items.length() {
        names.push(parse_library_name(items[i]))
        i = i + 1
      }
      names
    }
    _ => raise EvalError("invalid library")
  }
}

///|
fn import_library_into_env(name : String, env : Env) -> Unit raise EvalError {
  match lookup_library(name) {
    Some(lib) => {
      for key in lib.exports.keys() {
        match lib.exports.get(key) {
          Some(value) => env_define(env, key, value)
          None => ()
        }
      }
    }
    None => raise EvalError("unknown library: \{name}")
  }
}

///|
fn eval_library_state(
  args : Array[Datum],
  _env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise EvalError("invalid library")
  }
  let name = parse_library_name(args[0])
  let exports = parse_export_clause(args[1])
  let imports = parse_import_clause(args[2])
  let body = args.sub(start=3).to_array()
  let lib_env = init_env()
  for lib_name in imports {
    import_library_into_env(lib_name, lib_env)
  }
  for form in body {
    let _ = eval_expr(form, lib_env)
  }
  let export_map : Map[String, Value] = {}
  for export_name in exports {
    export_map[export_name] = env_lookup(lib_env, export_name)
  }
  register_library(name, export_map)
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_import_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for spec in args {
    let name = parse_library_name(spec)
    import_library_into_env(name, env)
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_and_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(true), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::And(rest, env, kont), handlers)
  }
}

///|
fn eval_or_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(false), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::Or(rest, env, kont), handlers)
  }
}

///|
fn eval_case_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let key_expr = args[0]
  let clauses = args.sub(start=1).to_array()
  MachineState::Eval(key_expr, env, Kont::Case(clauses, env, kont), handlers)
}

///|
fn case_dispatch(
  key : Value,
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      match symbol_name(parts[0]) {
        Some("else") => {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
        _ => ()
      }
      let datums = datum_list_to_array(parts[0])
      let mut matched = false
      for datum in datums {
        if value_eqv(key, Value::Datum(datum)) {
          matched = true
        }
      }
      if matched {
        if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
          if parts.length() != 3 {
            raise EvalError("invalid case")
          }
          return MachineState::Eval(parts[2], env, Kont::CaseArrow(key, kont), handlers)
        }
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      }
      i = i + 1
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn cond_start(
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(parts[0], env, Kont::Cond(parts, rest, env, kont), handlers)
      }
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
fn eval_pair(
  expr : Datum,
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    MachineState::Apply(Value::Datum(Datum::Nil), kont, handlers)
  } else {
    let head = items[0]
    let args = items.sub(start=1).to_array()
    match symbol_name(head) {
      Some("define-syntax") => MachineState::Apply(eval_define_syntax(args, env), kont, handlers)
      Some("library") => eval_library_state(args, env, kont, handlers)
      Some("import") => eval_import_state(args, env, kont, handlers)
      Some("define-record-type") => eval_define_record_type_state(args, env, kont, handlers)
      Some("quote") => MachineState::Apply(eval_quote(args), kont, handlers)
      Some("quasiquote") => MachineState::Apply(eval_quasiquote(args, env), kont, handlers)
      Some("syntax") => MachineState::Apply(eval_quote(args), kont, handlers)
      Some("if") => eval_if_state(args, env, kont, handlers)
      Some("begin") => eval_sequence_state(args, env, kont, handlers)
      Some("lambda") => MachineState::Apply(eval_lambda(args, env), kont, handlers)
      Some("case-lambda") => MachineState::Apply(eval_case_lambda(args, env), kont, handlers)
      Some("define") => eval_define_state(args, env, kont, handlers)
      Some("define-values") => eval_define_values_state(args, env, kont, handlers)
      Some("set!") => eval_set_state(args, env, kont, handlers)
      Some("let") => eval_let_state(args, env, kont, handlers)
      Some("let-values") => eval_let_values_state(args, env, kont, handlers)
      Some("let*-values") => eval_let_star_values_state(args, env, kont, handlers)
      Some("let*") => eval_let_star_state(args, env, kont, handlers)
      Some("letrec") => eval_letrec_state(args, env, kont, handlers)
      Some("letrec*") => eval_letrec_state(args, env, kont, handlers)
      Some("parameterize") => eval_parameterize_state(args, env, kont, handlers)
      Some("let-syntax") => eval_let_syntax_state(args, env, kont, handlers)
      Some("letrec-syntax") => eval_letrec_syntax_state(args, env, kont, handlers)
      Some("and") => eval_and_state(args, env, kont, handlers)
      Some("or") => eval_or_state(args, env, kont, handlers)
      Some("cond") => cond_start(args, env, kont, handlers)
      Some("guard") => eval_guard_state(args, env, kont, handlers)
      Some("do") => eval_do_state(args, env, kont, handlers)
      Some("case") => eval_case_state(args, env, kont, handlers)
      Some(name) => {
        match env_get_macro(env, name) {
          Some(rules) => {
            let expanded = expand_macro(rules, expr, env)
            MachineState::Eval(expanded, env, kont, handlers)
          }
          None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont), handlers)
        }
      }
      None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont), handlers)
    }
  }
}

///|
fn map_step(
  proc : Value,
  lists : Array[Datum],
  results : Array[Datum],
  for_each : Bool,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if lists.is_empty() {
    raise EvalError("arity mismatch: expected at least 1 list")
  }
  let args : Array[Value] = []
  let next_lists : Array[Datum] = []
  for list in lists {
    match list {
      Datum::Nil => {
        if for_each {
          return MachineState::Apply(Value::Void, kont, handlers)
        }
        return MachineState::Apply(Value::Datum(list_from_array(results)), kont, handlers)
      }
      Datum::Pair(car, cdr) => {
        args.push(Value::Datum(car.val))
        next_lists.push(cdr.val)
      }
      _ => raise EvalError("type error: proper list expected")
    }
  }
  apply_proc(proc, args, Kont::MapStep(proc, next_lists, results, for_each, kont), handlers)
}

///|
fn string_to_char_list(s : String) -> Datum raise EvalError {
  let items : Array[Datum] = []
  let mut i = 0
  while i < s.length() {
    match s.get_char(i) {
      Some(ch) => items.push(Datum::Char(ch))
      None => raise EvalError("index out of range")
    }
    i = i + 1
  }
  list_from_array(items)
}

///|
fn apply_proc(
  proc : Value,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc {
    Value::Primitive(Primitive::Apply) => apply_apply(args, kont, handlers)
    Value::Primitive(Primitive::CallWithValues) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let producer = args[0]
        let consumer = args[1]
        apply_proc(producer, [], Kont::CallWithValues(consumer, kont), handlers)
      }
    }
    Value::Primitive(Primitive::RecordConstructor) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let desc = value_as_record_constructor_descriptor(args[0])
        record_constructor_from_descriptor(desc, kont, handlers)
      }
    }
    Value::Primitive(Primitive::MakeParameter) => {
      match args.length() {
        1 => MachineState::Apply(Value::Parameter(make_parameter(args[0])), kont, handlers)
        2 => {
          let converter = args[1]
          apply_proc(converter, [args[0]], Kont::MakeParameter(converter, kont), handlers)
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Value::Primitive(Primitive::DynamicWind) => {
      if args.length() != 3 {
        raise arity_mismatch(3, args.length())
      } else {
        let before = args[0]
        let thunk = args[1]
        let after = args[2]
        let winder = make_proc_winder(before, after)
        apply_winder_before(winder, Kont::WindEnter(thunk, winder, kont), handlers)
      }
    }
    Value::Primitive(Primitive::WithExceptionHandler) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let handler = args[0]
        let thunk = args[1]
        let next_handlers : Array[Value] = []
        for item in handlers {
          next_handlers.push(item)
        }
        next_handlers.push(handler)
        let restore = Kont::RestoreHandlers(handlers, kont)
        apply_proc(thunk, [], restore, next_handlers)
      }
    }
    Value::Primitive(Primitive::Raise) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else if handlers.is_empty() {
        raise EvalError("uncaught exception")
      } else {
        let handler = handlers[handlers.length() - 1]
        match handler {
          Value::GuardHandler(guard_handler) => handle_guard(guard_handler, args[0], kont, false)
          _ => apply_proc(handler, [args[0]], Kont::RaiseNonCont, handlers)
        }
      }
    }
    Value::Primitive(Primitive::RaiseContinuable) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else if handlers.is_empty() {
        raise EvalError("uncaught exception")
      } else {
        let handler = handlers[handlers.length() - 1]
        match handler {
          Value::GuardHandler(guard_handler) => handle_guard(guard_handler, args[0], kont, true)
          _ => apply_proc(handler, [args[0]], Kont::RaiseCont(kont), handlers)
        }
      }
    }
    Value::Primitive(Primitive::CallCC) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let cont = capture_continuation(kont, handlers)
        apply_proc(args[0], [Value::Continuation(cont)], kont, handlers)
      }
    }
    Value::Primitive(Primitive::Map) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let lists : Array[Datum] = []
        let mut i = 1
        while i < args.length() {
          lists.push(value_as_datum(args[i]))
          i = i + 1
        }
        map_step(proc_value, lists, [], false, kont, handlers)
      }
    }
    Value::Primitive(Primitive::ForEach) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let lists : Array[Datum] = []
        let mut i = 1
        while i < args.length() {
          lists.push(value_as_datum(args[i]))
          i = i + 1
        }
        map_step(proc_value, lists, [], true, kont, handlers)
      }
    }
    Value::Primitive(Primitive::VectorMap) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let vectors : Array[Array[Datum]] = []
        let mut i = 1
        while i < args.length() {
          vectors.push(value_as_vector(args[i]))
          i = i + 1
        }
        let len = vectors[0].length()
        let mut j = 0
        while j < vectors.length() {
          if vectors[j].length() != len {
            raise EvalError("vector length mismatch")
          }
          j = j + 1
        }
        let lists : Array[Datum] = []
        let mut k = 0
        while k < vectors.length() {
          lists.push(list_from_array(vectors[k]))
          k = k + 1
        }
        map_step(proc_value, lists, [], false, Kont::VectorMapFinalize(kont), handlers)
      }
    }
    Value::Primitive(Primitive::VectorForEach) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let vectors : Array[Array[Datum]] = []
        let mut i = 1
        while i < args.length() {
          vectors.push(value_as_vector(args[i]))
          i = i + 1
        }
        let len = vectors[0].length()
        let mut j = 0
        while j < vectors.length() {
          if vectors[j].length() != len {
            raise EvalError("vector length mismatch")
          }
          j = j + 1
        }
        let lists : Array[Datum] = []
        let mut k = 0
        while k < vectors.length() {
          lists.push(list_from_array(vectors[k]))
          k = k + 1
        }
        map_step(proc_value, lists, [], true, kont, handlers)
      }
    }
    Value::Primitive(Primitive::StringMap) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let first = value_as_string(args[1])
        let len = first.length()
        let lists : Array[Datum] = []
        lists.push(string_to_char_list(first))
        let mut i = 2
        while i < args.length() {
          let s = value_as_string(args[i])
          if s.length() != len {
            raise EvalError("string length mismatch")
          }
          lists.push(string_to_char_list(s))
          i = i + 1
        }
        map_step(proc_value, lists, [], false, Kont::StringMapFinalize(kont), handlers)
      }
    }
    Value::Primitive(Primitive::StringForEach) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let proc_value = args[0]
        let first = value_as_string(args[1])
        let len = first.length()
        let lists : Array[Datum] = []
        lists.push(string_to_char_list(first))
        let mut i = 2
        while i < args.length() {
          let s = value_as_string(args[i])
          if s.length() != len {
            raise EvalError("string length mismatch")
          }
          lists.push(string_to_char_list(s))
          i = i + 1
        }
        map_step(proc_value, lists, [], true, kont, handlers)
      }
    }
    Value::Primitive(prim) => MachineState::Apply(apply_primitive(prim, args), kont, handlers)
    Value::Closure(cl) => {
      if args.length() != cl.params.length() {
        raise arity_mismatch(cl.params.length(), args.length())
      } else {
        let new_env = env_extend(cl.env)
        let mut i = 0
        while i < cl.params.length() {
          env_define(new_env, cl.params[i], args[i])
          i = i + 1
        }
        eval_sequence_state(cl.body, new_env, kont, handlers)
      }
    }
    Value::CaseClosure(cl) => {
      let mut matched = false
      let mut matched_params : Array[String] = []
      let mut matched_body : Array[Datum] = []
      for clause in cl.clauses {
        if clause.params.length() == args.length() {
          matched = true
          matched_params = clause.params
          matched_body = clause.body
          break
        }
      }
      if !matched {
        let lengths : Array[Int] = []
        for clause in cl.clauses {
          lengths.push(clause.params.length())
        }
        if lengths.length() == 1 {
          raise arity_mismatch(lengths[0], args.length())
        } else {
          let expected_parts : Array[String] = []
          for len in lengths {
            expected_parts.push(len.to_string())
          }
          let expected = expected_parts.join(", ")
          raise EvalError("arity mismatch: expected one of \{expected} got \{args.length()}")
        }
      }
      let new_env = env_extend(cl.env)
      let mut i = 0
      while i < matched_params.length() {
        env_define(new_env, matched_params[i], args[i])
        i = i + 1
      }
      eval_sequence_state(matched_body, new_env, kont, handlers)
    }
    Value::Parameter(param) => {
      match args.length() {
        0 => MachineState::Apply(parameter_get(param), kont, handlers)
        1 =>
          match param.converter {
            Some(converter) =>
              apply_proc(converter, [args[0]], Kont::SetParameter(param, kont), handlers)
            None => {
              parameter_set(param, args[0])
              MachineState::Apply(Value::Void, kont, handlers)
            }
          }
        n => raise EvalError("arity mismatch: expected 0 or 1 got \{n}")
      }
    }
    Value::Continuation(cont) => {
      let resume_value = if args.length() == 1 {
        args[0]
      } else {
        Value::Values(args)
      }
      let current_winds = wind_stack.val
      let target_winds = cont.winds
      let actions : Array[WindAction] = []
      let prefix = common_wind_prefix(current_winds, target_winds)
      let mut i = current_winds.length()
      while i > prefix {
        i = i - 1
        actions.push(WindAction::After(current_winds[i]))
      }
      actions.push(WindAction::SwitchHandlers(cont.handlers))
      i = prefix
      while i < target_winds.length() {
        actions.push(WindAction::Before(target_winds[i]))
        i = i + 1
      }
      run_wind_actions(actions, resume_value, cont.kont, cont.handlers, handlers)
    }
    Value::RecordProc(proc) => apply_record_proc(proc, args, kont, handlers)
    _ => raise EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let proc = args[0]
    let flat : Array[Value] = []
    let mut i = 1
    while i < args.length() - 1 {
      flat.push(args[i])
      i = i + 1
    }
    let list_value = value_as_datum(args[args.length() - 1])
    let tail = datum_list_to_value_array(list_value)
    flat.append(tail[:])
    apply_proc(proc, flat, kont, handlers)
  }
}

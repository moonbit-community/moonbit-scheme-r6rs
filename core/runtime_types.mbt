///|
/// Kinds of runtime ports.
pub(all) enum PortKind {
  OutputString(Ref[String])
}

///|
/// Runtime port descriptor.
pub(all) struct Port {
  id : Int
  kind : PortKind
}

///|
pub(all) struct RecordField {
  name : String
  mutable : Bool
}

///|
/// Record type metadata used by the runtime.
pub(all) struct RecordType {
  id : Int
  name : String
  parent : RecordType?
  is_sealed : Bool
  is_opaque : Bool
  uid : String?
  fields : Array[RecordField]
}

///|
/// Record instance with boxed fields.
pub(all) struct Record {
  id : Int
  record_type : RecordType
  fields : Array[Ref[Value]]
}

///|
/// Condition value containing record components.
pub(all) struct Condition {
  id : Int
  components : Array[Record]
}

///|
pub(all) enum HashtableEquiv {
  Eq
  Eqv
  Equal
  Proc(Value)
}

///|
pub(all) struct HashtableEntry {
  key : Value
  value : Ref[Value]
}

///|
/// Hashtable with configured equivalence and hash function.
pub(all) struct Hashtable {
  id : Int
  mutable : Bool
  equiv : HashtableEquiv
  hash : Value?
  entries : Ref[Array[HashtableEntry]]
}

///|
/// Enum set value with a fixed universe ordering.
pub(all) struct EnumSet {
  id : Int
  universe : Array[String]
  members : Array[Bool]
}

///|
pub(all) enum EnumSetProcKind {
  Constructor(EnumSet)
  Indexer(EnumSet)
}

///|
pub(all) struct EnumSetProc {
  id : Int
  kind : EnumSetProcKind
}

///|
pub(all) enum RecordProcKind {
  Constructor(RecordType)
  Predicate(RecordType)
  Accessor(RecordType, Int)
  Mutator(RecordType, Int)
  ProtocolN(RecordType, RecordConstructorDescriptor)
  ProtocolP(RecordType, Array[Value])
}

///|
/// Record-related procedure wrapper.
pub(all) struct RecordProc {
  id : Int
  kind : RecordProcKind
}

///|
pub(all) enum ConditionProcKind {
  Predicate(RecordType)
  Accessor(RecordType, Int)
}

///|
/// Condition-related procedure wrapper.
pub(all) struct ConditionProc {
  id : Int
  kind : ConditionProcKind
}

///|
pub(all) struct RecordConstructorDescriptor {
  id : Int
  record_type : RecordType
  parent_desc : RecordConstructorDescriptor?
  protocol : Value?
}

///|
pub(all) struct RecordTypeDescriptor {
  id : Int
  record_type : RecordType
  constructor_desc : RecordConstructorDescriptor
}

///|
pub(all) struct Formals {
  params : Array[String]
  rest : String?
}

///|
pub(all) struct ParamBinding {
  param : Parameter
  old_value : Value
  new_value : Value
}

///|
/// Record field binding details for generated accessors.
pub struct RecordFieldBinding {
  accessor : String
  index : Int
  mutator : String?
}

///|
/// Create a record field binding.
///
/// # Example
/// ```mbt check
/// test "record field binding" {
///   let binding = RecordFieldBinding::new("get", 0, None)
///   inspect(binding.accessor(), content="get")
///   inspect(binding.index(), content="0")
///   inspect(binding.mutator(), content="None")
/// }
/// ```
pub fn RecordFieldBinding::new(
  accessor : String,
  index : Int,
  mutator : String?,
) -> RecordFieldBinding {
  { accessor, index, mutator }
}

///|
/// Return the accessor name.
pub fn RecordFieldBinding::accessor(self : RecordFieldBinding) -> String {
  self.accessor
}

///|
/// Return the field index.
pub fn RecordFieldBinding::index(self : RecordFieldBinding) -> Int {
  self.index
}

///|
/// Return the mutator name, if any.
pub fn RecordFieldBinding::mutator(self : RecordFieldBinding) -> String? {
  self.mutator
}

///|
pub(all) enum HashtableOp {
  Ref(Value)
  Contains
  Set(Value)
  Delete
  Update(Value, Value)
}

///|
pub(all) struct GuardInfo {
  condition : Value
  resume_value : Value
  handlers : Array[Value]
  raise_kont : Kont
  continuable : Bool
}

///|
pub(all) enum WinderKind {
  Proc(Value, Value)
  Params(Array[ParamBinding])
}

///|
pub(all) struct Winder {
  id : Int
  kind : WinderKind
}

///|
pub(all) enum WindAction {
  After(Winder)
  Before(Winder)
  SwitchHandlers(Array[Value])
}

///|
/// Continuation frames for the evaluator.
pub(all) enum Kont {
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
  LetRecInit(Array[(String, Datum)], Int, Array[Value], Env, Array[Datum], Kont)
  LetValues(
    Array[(Formals, Datum)],
    Int,
    Array[Array[(String, Value)]],
    Env,
    Array[Datum],
    Kont
  )
  LetStarValues(Array[(Formals, Datum)], Int, Env, Array[Datum], Kont)
  DefineValues(Formals, Env, Kont)
  SyntaxCase(Array[Datum], Env, Kont)
  ParameterizeParam(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont
  )
  ParameterizeValue(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont
  )
  ParameterizeConvert(
    Array[(Datum, Datum)],
    Int,
    Array[Parameter],
    Array[Value],
    Env,
    Array[Datum],
    Kont
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
  DefineRecordType(
    String,
    String,
    RecordType,
    Array[RecordFieldBinding],
    Env,
    Kont
  )
  DefineConditionType(
    String,
    String,
    RecordType,
    Array[RecordFieldBinding],
    Env,
    Kont
  )
  HashtableFindResult(Hashtable, Value, Int, HashtableOp, Kont)
  HashtableUpdateApply(Hashtable, Value, Int, Bool, Kont)
  MapStep(Value, Array[Datum], Array[Datum], Bool, Kont)
  VectorMapFinalize(Kont)
  StringMapFinalize(Kont)
  WindEnter(Value, Winder, Kont)
  WindExit(Winder, Kont)
  WindActions(Array[WindAction], Value, Kont, Array[Value])
  WindPush(Winder, Array[WindAction], Value, Kont, Array[Value])
  ForcePromise(Promise, Kont)
  RaiseNonCont
  RaiseCont(Kont)
  RestoreHandlers(Array[Value], Kont)
}

///|
/// First-class continuation value.
pub(all) struct Continuation {
  id : Int
  kont : Kont
  handlers : Array[Value]
  winds : Array[Winder]
}

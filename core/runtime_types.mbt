///|
pub(all) enum PortKind {
  OutputString(Ref[String])
}

///|
pub(all) struct Port {
  pub id : Int
  pub kind : PortKind
}

///|
pub(all) struct RecordField {
  pub name : String
  pub mutable : Bool
}

///|
pub(all) struct RecordType {
  pub id : Int
  pub name : String
  pub parent : RecordType?
  pub is_sealed : Bool
  pub is_opaque : Bool
  pub uid : String?
  pub fields : Array[RecordField]
}

///|
pub(all) struct Record {
  pub id : Int
  pub record_type : RecordType
  pub fields : Array[Ref[Value]]
}

///|
pub(all) struct Condition {
  pub id : Int
  pub components : Array[Record]
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
  pub key : Value
  pub value : Ref[Value]
}

///|
pub(all) struct Hashtable {
  pub id : Int
  pub mutable : Bool
  pub equiv : HashtableEquiv
  pub hash : Value?
  pub entries : Ref[Array[HashtableEntry]]
}

///|
pub(all) struct EnumSet {
  pub id : Int
  pub universe : Array[String]
  pub members : Array[Bool]
}

///|
pub(all) enum EnumSetProcKind {
  Constructor(EnumSet)
  Indexer(EnumSet)
}

///|
pub(all) struct EnumSetProc {
  pub id : Int
  pub kind : EnumSetProcKind
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
pub(all) struct RecordProc {
  pub id : Int
  pub kind : RecordProcKind
}

///|
pub(all) enum ConditionProcKind {
  Predicate(RecordType)
  Accessor(RecordType, Int)
}

///|
pub(all) struct ConditionProc {
  pub id : Int
  pub kind : ConditionProcKind
}

///|
pub(all) struct RecordConstructorDescriptor {
  pub id : Int
  pub record_type : RecordType
  pub parent_desc : RecordConstructorDescriptor?
  pub protocol : Value?
}

///|
pub(all) struct RecordTypeDescriptor {
  pub id : Int
  pub record_type : RecordType
  pub constructor_desc : RecordConstructorDescriptor
}

///|
pub(all) struct Formals {
  pub params : Array[String]
  pub rest : String?
}

///|
pub(all) struct ParamBinding {
  pub param : Parameter
  pub old_value : Value
  pub new_value : Value
}

///|
pub(all) struct RecordFieldBinding {
  pub accessor : String
  pub index : Int
  pub mutator : String?
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
  pub condition : Value
  pub resume_value : Value
  pub handlers : Array[Value]
  pub raise_kont : Kont
  pub continuable : Bool
}

///|
pub(all) enum WinderKind {
  Proc(Value, Value)
  Params(Array[ParamBinding])
}

///|
pub(all) struct Winder {
  pub id : Int
  pub kind : WinderKind
}

///|
pub(all) enum WindAction {
  After(Winder)
  Before(Winder)
  SwitchHandlers(Array[Value])
}

///|
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
    Kont,
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
  DefineConditionType(String, String, RecordType, Array[RecordFieldBinding], Env, Kont)
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
pub(all) struct Continuation {
  pub id : Int
  pub kont : Kont
  pub handlers : Array[Value]
  pub winds : Array[Winder]
}

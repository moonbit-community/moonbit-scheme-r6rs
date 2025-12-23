///|
/// Spec for a small, R6RS-compatible Scheme interpreter.
///
/// Phase 1 scope (implemented by tests in this module):
/// - exact integers, exact rationals, inexact floats, booleans, symbols,
///   characters, strings, pairs/lists
/// - quote, quasiquote, unquote, unquote-splicing, if, begin, lambda, define,
///   set!, let (including named let), let*, letrec, letrec*, let-syntax,
///   letrec-syntax, and, or, cond (including =>), case, do, case-lambda,
///   let-values, let*-values, define-values, parameterize, guard,
///   syntax-case (limited), identifier-syntax
/// - dynamic-wind
/// - define-syntax + syntax-rules (hygienic identifiers), including "..." repetition
/// - application with left-to-right evaluation
/// - proper tail calls for procedure application
/// - call/cc (call-with-current-continuation)
/// - define-record-type (simple record types, no inheritance)
/// - library and import (minimal library support)
/// - builtins: + - * / = < > <= >= cons car cdr list null? pair? symbol?
///   boolean? number? integer? rational? real? exact? inexact? exact->inexact
///   inexact->exact numerator denominator procedure? not apply call/cc eq? eqv?
///   equal? list? make-list list-ref list-tail length append
///   reverse c[ad]+r member memq memv assoc assq assv map for-each
///   char? char=? char<? char>? char<=? char>=?
///   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
///   char->integer integer->char char-alphabetic? char-numeric? char-whitespace?
///   char-upper-case? char-lower-case? char-upcase char-downcase char-foldcase
///   string? string=? string<? string>? string<=? string>=?
///   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
///   string-length string-append string-ref string->list list->string
///   string-upcase string-downcase string-foldcase
///   vector make-vector vector? vector-length vector-ref vector-set!
///   vector->list list->vector
///   bytevector bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
///   display write newline open-output-string get-output-string current-output-port
///   with-exception-handler raise raise-continuable
///   values call-with-values make-parameter dynamic-wind parameterize
///   guard
///   (equality is structural for datums in this subset)
/// - printing via value_to_string (booleans as #t/#f, empty list as ())
///
/// Out of scope for now: full numeric tower.
///|
pub(all) suberror ParseError String derive(Show, Eq)

///|
pub(all) suberror EvalError String derive(Show, Eq)

///|
/// Datum is the reader-level data representation used by quote.
/// It contains only pure data (no procedures).
///|
pub enum Datum {
  Nil
  Bool(Bool)
  Int(Int)
  Rat(Int, Int)
  Float(Float)
  Char(Char)
  String(String)
  Symbol(String)
  Pair(Datum, Datum)
  Vector(Array[Datum])
  ByteVector(Array[Int])
}

///|
/// Primitive procedures provided by the base environment.
///|
pub enum Primitive {
  Add
  Sub
  Mul
  Div
  NumEq
  Less
  Greater
  LessEq
  GreaterEq
  Eq
  Eqv
  Equal
  Cons
  Car
  Cdr
  List
  NullP
  PairP
  SymbolP
  BooleanP
  NumberP
  IntegerP
  RationalP
  RealP
  ExactP
  InexactP
  ProcedureP
  Not
  Apply
  CallCC
  Values
  CallWithValues
  MakeParameter
  DynamicWind
  ExactToInexact
  InexactToExact
  Numerator
  Denominator
  ListP
  MakeList
  Length
  Append
  Reverse
  ListRef
  ListTail
  Cxr(String)
  Member
  Memq
  Memv
  Assoc
  Assq
  Assv
  Map
  ForEach
  CharEq
  CharLess
  CharGreater
  CharLessEq
  CharGreaterEq
  CharCiEq
  CharCiLess
  CharCiGreater
  CharCiLessEq
  CharCiGreaterEq
  CharP
  CharToInteger
  IntegerToChar
  CharAlphabeticP
  CharNumericP
  CharWhitespaceP
  CharUpperCaseP
  CharLowerCaseP
  CharUpcase
  CharDowncase
  CharFoldcase
  StringEq
  StringLess
  StringGreater
  StringLessEq
  StringGreaterEq
  StringCiEq
  StringCiLess
  StringCiGreater
  StringCiLessEq
  StringCiGreaterEq
  StringP
  StringLength
  StringAppend
  StringRef
  StringToList
  ListToString
  StringUpcase
  StringDowncase
  StringFoldcase
  Vector
  MakeVector
  VectorP
  VectorLength
  VectorRef
  VectorSet
  VectorToList
  ListToVector
  ByteVector
  ByteVectorP
  ByteVectorLength
  ByteVectorU8Ref
  ByteVectorU8Set
  Display
  Write
  Newline
  OpenOutputString
  GetOutputString
  CurrentOutputPort
  WithExceptionHandler
  Raise
  RaiseContinuable
}

///|
/// Lexical environment (stack of frames) captured by closures.
/// The implementation treats this as an internal detail.
///|
pub type Env = Array[Map[String, Value]]

///|
/// A syntax-rules transformer.
///|
pub struct SyntaxRule {
  pattern : Datum
  template : Datum
}

///|
/// A collection of syntax-rules with literal identifiers.
///|
pub struct SyntaxRules {
  literals : Array[String]
  rules : Array[SyntaxRule]
}

///|
/// Closure is an opaque procedure value produced by lambda.
/// The interpreter may store extra data internally; tests treat it as opaque.
///|
pub struct Closure {
  id : Int
  params : Array[String]
  body : Array[Datum]
  env : Env
}

///|
pub struct CaseClause {
  params : Array[String]
  body : Array[Datum]
}

///|
pub struct CaseClosure {
  id : Int
  clauses : Array[CaseClause]
  env : Env
}

///|
pub struct GuardHandler {
  id : Int
  name : String
  clauses : Array[Datum]
  env : Env
  resume_value : Value
  handlers : Array[Value]
}

///|
pub struct Parameter {
  id : Int
  value : Ref[Value]
  converter : Value?
}

///|
/// Runtime values produced by evaluation.
///|
pub enum Value {
  Void
  Datum(Datum)
  Primitive(Primitive)
  Closure(Closure)
  CaseClosure(CaseClosure)
  Values(Array[Value])
  GuardHandler(GuardHandler)
  Parameter(Parameter)
  Continuation(Continuation)
  Port(Port)
  Record(Record)
  RecordProc(RecordProc)
  Macro(SyntaxRules)
}

///|
/// Evaluate a Scheme program consisting of one or more forms.
/// Returns the last value produced.
///
/// Parse errors raise ParseError, evaluation errors raise EvalError.
///|
#declaration_only
pub fn eval_program(src : String) -> Value raise {
  ...
}

///|
/// Evaluate all top-level forms and return each value in order.
///
/// Parse errors raise ParseError, evaluation errors raise EvalError.
///|
#declaration_only
pub fn eval_program_all(src : String) -> Array[Value] raise {
  ...
}

///|
/// Render a runtime value using Scheme surface syntax.
///
/// - Void prints as "#<void>"
/// - Booleans print as "#t" or "#f"
/// - Integers print in base-10 without prefixes
/// - Symbols print as their identifier names
/// - Strings print with double quotes and backslash escapes
/// - Proper lists print as "(a b c)"
/// - Improper lists print as "(a b . c)"
/// - Macros print as "#<syntax>"
/// - Continuations print as "#<procedure>"
/// - Ports print as "#<port>"
/// - Multiple values print as "#<values>"
///|
#declaration_only
pub fn value_to_string(value : Value) -> String {
  ...
}

///|
fn arity_mismatch(expected : Int, got : Int) -> EvalError {
  EvalError("arity mismatch: expected \{expected} got \{got}")
}

///|
fn is_proper_list(value : Datum) -> Bool {
  let mut cur = value
  while true {
    match datum_unlabel(cur) {
      Datum::Nil => return true
      Datum::Pair(_, cdr) => cur = cdr.val
      _ => return false
    }
  }
  false
}

///|
fn primitive_eq(left : Primitive, right : Primitive) -> Bool {
  match (left, right) {
    (Primitive::Cxr(a), Primitive::Cxr(b)) => a == b
    (Primitive::Add, Primitive::Add)
    | (Primitive::Sub, Primitive::Sub)
    | (Primitive::Mul, Primitive::Mul)
    | (Primitive::Div, Primitive::Div)
    | (Primitive::NumEq, Primitive::NumEq)
    | (Primitive::Less, Primitive::Less)
    | (Primitive::Greater, Primitive::Greater)
    | (Primitive::LessEq, Primitive::LessEq)
    | (Primitive::GreaterEq, Primitive::GreaterEq)
    | (Primitive::Eq, Primitive::Eq)
    | (Primitive::Eqv, Primitive::Eqv)
    | (Primitive::Equal, Primitive::Equal)
    | (Primitive::Cons, Primitive::Cons)
    | (Primitive::Car, Primitive::Car)
    | (Primitive::Cdr, Primitive::Cdr)
    | (Primitive::List, Primitive::List)
    | (Primitive::NullP, Primitive::NullP)
    | (Primitive::PairP, Primitive::PairP)
    | (Primitive::SymbolP, Primitive::SymbolP)
    | (Primitive::IdentifierP, Primitive::IdentifierP)
    | (Primitive::SyntaxP, Primitive::SyntaxP)
    | (Primitive::FreeIdentifierEq, Primitive::FreeIdentifierEq)
    | (Primitive::BoundIdentifierEq, Primitive::BoundIdentifierEq)
    | (Primitive::SymbolToString, Primitive::SymbolToString)
    | (Primitive::StringToSymbol, Primitive::StringToSymbol)
    | (Primitive::SyntaxToDatum, Primitive::SyntaxToDatum)
    | (Primitive::DatumToSyntax, Primitive::DatumToSyntax)
    | (Primitive::BooleanP, Primitive::BooleanP)
    | (Primitive::NumberP, Primitive::NumberP)
    | (Primitive::IntegerP, Primitive::IntegerP)
    | (Primitive::RationalP, Primitive::RationalP)
    | (Primitive::RealP, Primitive::RealP)
    | (Primitive::ComplexP, Primitive::ComplexP)
    | (Primitive::ExactP, Primitive::ExactP)
    | (Primitive::InexactP, Primitive::InexactP)
    | (Primitive::ZeroP, Primitive::ZeroP)
    | (Primitive::PositiveP, Primitive::PositiveP)
    | (Primitive::NegativeP, Primitive::NegativeP)
    | (Primitive::OddP, Primitive::OddP)
    | (Primitive::EvenP, Primitive::EvenP)
    | (Primitive::FiniteP, Primitive::FiniteP)
    | (Primitive::InfiniteP, Primitive::InfiniteP)
    | (Primitive::NanP, Primitive::NanP)
    | (Primitive::ProcedureP, Primitive::ProcedureP)
    | (Primitive::RecordP, Primitive::RecordP)
    | (Primitive::RecordRtd, Primitive::RecordRtd)
    | (Primitive::RecordTypeDescriptorP, Primitive::RecordTypeDescriptorP)
    | (Primitive::RecordConstructorDescriptorP, Primitive::RecordConstructorDescriptorP)
    | (Primitive::RecordTypeName, Primitive::RecordTypeName)
    | (Primitive::RecordTypeParent, Primitive::RecordTypeParent)
    | (Primitive::RecordTypeUid, Primitive::RecordTypeUid)
    | (Primitive::RecordTypeGenerativeP, Primitive::RecordTypeGenerativeP)
    | (Primitive::RecordTypeSealedP, Primitive::RecordTypeSealedP)
    | (Primitive::RecordTypeOpaqueP, Primitive::RecordTypeOpaqueP)
    | (Primitive::RecordTypeFieldNames, Primitive::RecordTypeFieldNames)
    | (Primitive::RecordTypeFieldMutableP, Primitive::RecordTypeFieldMutableP)
    | (Primitive::RecordConstructorDescriptor, Primitive::RecordConstructorDescriptor)
    | (Primitive::RecordConstructor, Primitive::RecordConstructor)
    | (Primitive::RecordPredicate, Primitive::RecordPredicate)
    | (Primitive::RecordAccessor, Primitive::RecordAccessor)
    | (Primitive::RecordMutator, Primitive::RecordMutator)
    | (Primitive::MakeRecordTypeDescriptor, Primitive::MakeRecordTypeDescriptor)
    | (Primitive::MakeRecordConstructorDescriptor, Primitive::MakeRecordConstructorDescriptor)
    | (Primitive::Condition, Primitive::Condition)
    | (Primitive::ConditionP, Primitive::ConditionP)
    | (Primitive::SimpleConditions, Primitive::SimpleConditions)
    | (Primitive::ConditionPredicate, Primitive::ConditionPredicate)
    | (Primitive::ConditionAccessor, Primitive::ConditionAccessor)
    | (Primitive::MakeEqHashtable, Primitive::MakeEqHashtable)
    | (Primitive::MakeEqvHashtable, Primitive::MakeEqvHashtable)
    | (Primitive::MakeHashtable, Primitive::MakeHashtable)
    | (Primitive::HashtableP, Primitive::HashtableP)
    | (Primitive::HashtableSize, Primitive::HashtableSize)
    | (Primitive::HashtableRef, Primitive::HashtableRef)
    | (Primitive::HashtableSet, Primitive::HashtableSet)
    | (Primitive::HashtableDelete, Primitive::HashtableDelete)
    | (Primitive::HashtableContainsP, Primitive::HashtableContainsP)
    | (Primitive::HashtableUpdate, Primitive::HashtableUpdate)
    | (Primitive::HashtableCopy, Primitive::HashtableCopy)
    | (Primitive::HashtableClear, Primitive::HashtableClear)
    | (Primitive::HashtableKeys, Primitive::HashtableKeys)
    | (Primitive::HashtableEntries, Primitive::HashtableEntries)
    | (Primitive::HashtableEquivalenceFunction, Primitive::HashtableEquivalenceFunction)
    | (Primitive::HashtableHashFunction, Primitive::HashtableHashFunction)
    | (Primitive::HashtableMutableP, Primitive::HashtableMutableP)
    | (Primitive::MakeEnumeration, Primitive::MakeEnumeration)
    | (Primitive::EnumSetUniverse, Primitive::EnumSetUniverse)
    | (Primitive::EnumSetIndexer, Primitive::EnumSetIndexer)
    | (Primitive::EnumSetConstructor, Primitive::EnumSetConstructor)
    | (Primitive::EnumSetP, Primitive::EnumSetP)
    | (Primitive::EnumSetMemberP, Primitive::EnumSetMemberP)
    | (Primitive::EnumSetSubsetP, Primitive::EnumSetSubsetP)
    | (Primitive::EnumSetEq, Primitive::EnumSetEq)
    | (Primitive::EnumSetUnion, Primitive::EnumSetUnion)
    | (Primitive::EnumSetIntersection, Primitive::EnumSetIntersection)
    | (Primitive::EnumSetDifference, Primitive::EnumSetDifference)
    | (Primitive::EnumSetComplement, Primitive::EnumSetComplement)
    | (Primitive::EnumSetProjection, Primitive::EnumSetProjection)
    | (Primitive::EnumSetToList, Primitive::EnumSetToList)
    | (Primitive::Not, Primitive::Not)
    | (Primitive::Apply, Primitive::Apply)
    | (Primitive::CallCC, Primitive::CallCC)
    | (Primitive::Values, Primitive::Values)
    | (Primitive::CallWithValues, Primitive::CallWithValues)
    | (Primitive::MakeParameter, Primitive::MakeParameter)
    | (Primitive::DynamicWind, Primitive::DynamicWind)
    | (Primitive::Eval, Primitive::Eval)
    | (Primitive::Environment, Primitive::Environment)
    | (Primitive::PromiseP, Primitive::PromiseP)
    | (Primitive::MakePromise, Primitive::MakePromise)
    | (Primitive::Force, Primitive::Force)
    | (Primitive::ExactToInexact, Primitive::ExactToInexact)
    | (Primitive::InexactToExact, Primitive::InexactToExact)
    | (Primitive::ExactIntegerSqrt, Primitive::ExactIntegerSqrt)
    | (Primitive::Rationalize, Primitive::Rationalize)
    | (Primitive::NumberToString, Primitive::NumberToString)
    | (Primitive::StringToNumber, Primitive::StringToNumber)
    | (Primitive::MakeRectangular, Primitive::MakeRectangular)
    | (Primitive::MakePolar, Primitive::MakePolar)
    | (Primitive::RealPart, Primitive::RealPart)
    | (Primitive::ImagPart, Primitive::ImagPart)
    | (Primitive::Magnitude, Primitive::Magnitude)
    | (Primitive::Angle, Primitive::Angle)
    | (Primitive::Sqrt, Primitive::Sqrt)
    | (Primitive::Exp, Primitive::Exp)
    | (Primitive::Log, Primitive::Log)
    | (Primitive::Expt, Primitive::Expt)
    | (Primitive::Sin, Primitive::Sin)
    | (Primitive::Cos, Primitive::Cos)
    | (Primitive::Tan, Primitive::Tan)
    | (Primitive::Asin, Primitive::Asin)
    | (Primitive::Acos, Primitive::Acos)
    | (Primitive::Atan, Primitive::Atan)
    | (Primitive::Numerator, Primitive::Numerator)
    | (Primitive::Denominator, Primitive::Denominator)
    | (Primitive::Abs, Primitive::Abs)
    | (Primitive::Quotient, Primitive::Quotient)
    | (Primitive::Remainder, Primitive::Remainder)
    | (Primitive::Modulo, Primitive::Modulo)
    | (Primitive::Gcd, Primitive::Gcd)
    | (Primitive::Lcm, Primitive::Lcm)
    | (Primitive::Max, Primitive::Max)
    | (Primitive::Min, Primitive::Min)
    | (Primitive::Floor, Primitive::Floor)
    | (Primitive::Ceiling, Primitive::Ceiling)
    | (Primitive::Truncate, Primitive::Truncate)
    | (Primitive::Round, Primitive::Round)
    | (Primitive::ListP, Primitive::ListP)
    | (Primitive::MakeList, Primitive::MakeList)
    | (Primitive::Length, Primitive::Length)
    | (Primitive::Append, Primitive::Append)
    | (Primitive::Reverse, Primitive::Reverse)
    | (Primitive::ListRef, Primitive::ListRef)
    | (Primitive::ListTail, Primitive::ListTail)
    | (Primitive::Member, Primitive::Member)
    | (Primitive::Memq, Primitive::Memq)
    | (Primitive::Memv, Primitive::Memv)
    | (Primitive::Assoc, Primitive::Assoc)
    | (Primitive::Assq, Primitive::Assq)
    | (Primitive::Assv, Primitive::Assv)
    | (Primitive::Map, Primitive::Map)
    | (Primitive::ForEach, Primitive::ForEach)
    | (Primitive::SetCar, Primitive::SetCar)
    | (Primitive::SetCdr, Primitive::SetCdr)
    | (Primitive::ListCopy, Primitive::ListCopy)
    | (Primitive::CharEq, Primitive::CharEq)
    | (Primitive::CharLess, Primitive::CharLess)
    | (Primitive::CharGreater, Primitive::CharGreater)
    | (Primitive::CharLessEq, Primitive::CharLessEq)
    | (Primitive::CharGreaterEq, Primitive::CharGreaterEq)
    | (Primitive::CharCiEq, Primitive::CharCiEq)
    | (Primitive::CharCiLess, Primitive::CharCiLess)
    | (Primitive::CharCiGreater, Primitive::CharCiGreater)
    | (Primitive::CharCiLessEq, Primitive::CharCiLessEq)
    | (Primitive::CharCiGreaterEq, Primitive::CharCiGreaterEq)
    | (Primitive::CharP, Primitive::CharP)
    | (Primitive::CharToInteger, Primitive::CharToInteger)
    | (Primitive::IntegerToChar, Primitive::IntegerToChar)
    | (Primitive::CharAlphabeticP, Primitive::CharAlphabeticP)
    | (Primitive::CharNumericP, Primitive::CharNumericP)
    | (Primitive::CharWhitespaceP, Primitive::CharWhitespaceP)
    | (Primitive::CharUpperCaseP, Primitive::CharUpperCaseP)
    | (Primitive::CharLowerCaseP, Primitive::CharLowerCaseP)
    | (Primitive::CharUpcase, Primitive::CharUpcase)
    | (Primitive::CharDowncase, Primitive::CharDowncase)
    | (Primitive::CharFoldcase, Primitive::CharFoldcase)
    | (Primitive::StringEq, Primitive::StringEq)
    | (Primitive::StringLess, Primitive::StringLess)
    | (Primitive::StringGreater, Primitive::StringGreater)
    | (Primitive::StringLessEq, Primitive::StringLessEq)
    | (Primitive::StringGreaterEq, Primitive::StringGreaterEq)
    | (Primitive::StringCiEq, Primitive::StringCiEq)
    | (Primitive::StringCiLess, Primitive::StringCiLess)
    | (Primitive::StringCiGreater, Primitive::StringCiGreater)
    | (Primitive::StringCiLessEq, Primitive::StringCiLessEq)
    | (Primitive::StringCiGreaterEq, Primitive::StringCiGreaterEq)
    | (Primitive::String, Primitive::String)
    | (Primitive::MakeString, Primitive::MakeString)
    | (Primitive::StringP, Primitive::StringP)
    | (Primitive::StringLength, Primitive::StringLength)
    | (Primitive::StringAppend, Primitive::StringAppend)
    | (Primitive::StringRef, Primitive::StringRef)
    | (Primitive::StringSet, Primitive::StringSet)
    | (Primitive::StringCopy, Primitive::StringCopy)
    | (Primitive::Substring, Primitive::Substring)
    | (Primitive::StringCopyBang, Primitive::StringCopyBang)
    | (Primitive::StringFill, Primitive::StringFill)
    | (Primitive::StringToList, Primitive::StringToList)
    | (Primitive::ListToString, Primitive::ListToString)
    | (Primitive::StringMap, Primitive::StringMap)
    | (Primitive::StringForEach, Primitive::StringForEach)
    | (Primitive::StringUpcase, Primitive::StringUpcase)
    | (Primitive::StringDowncase, Primitive::StringDowncase)
    | (Primitive::StringFoldcase, Primitive::StringFoldcase)
    | (Primitive::Vector, Primitive::Vector)
    | (Primitive::MakeVector, Primitive::MakeVector)
    | (Primitive::VectorP, Primitive::VectorP)
    | (Primitive::VectorLength, Primitive::VectorLength)
    | (Primitive::VectorRef, Primitive::VectorRef)
    | (Primitive::VectorSet, Primitive::VectorSet)
    | (Primitive::VectorFill, Primitive::VectorFill)
    | (Primitive::VectorCopy, Primitive::VectorCopy)
    | (Primitive::VectorCopyBang, Primitive::VectorCopyBang)
    | (Primitive::VectorAppend, Primitive::VectorAppend)
    | (Primitive::VectorMap, Primitive::VectorMap)
    | (Primitive::VectorForEach, Primitive::VectorForEach)
    | (Primitive::VectorToList, Primitive::VectorToList)
    | (Primitive::ListToVector, Primitive::ListToVector)
    | (Primitive::ByteVector, Primitive::ByteVector)
    | (Primitive::MakeByteVector, Primitive::MakeByteVector)
    | (Primitive::ByteVectorP, Primitive::ByteVectorP)
    | (Primitive::ByteVectorLength, Primitive::ByteVectorLength)
    | (Primitive::ByteVectorU8Ref, Primitive::ByteVectorU8Ref)
    | (Primitive::ByteVectorU8Set, Primitive::ByteVectorU8Set)
    | (Primitive::ByteVectorCopy, Primitive::ByteVectorCopy)
    | (Primitive::ByteVectorCopyBang, Primitive::ByteVectorCopyBang)
    | (Primitive::ByteVectorAppend, Primitive::ByteVectorAppend)
    | (Primitive::ByteVectorFill, Primitive::ByteVectorFill)
    | (Primitive::ByteVectorToU8List, Primitive::ByteVectorToU8List)
    | (Primitive::U8ListToByteVector, Primitive::U8ListToByteVector)
    | (Primitive::Display, Primitive::Display)
    | (Primitive::Write, Primitive::Write)
    | (Primitive::Newline, Primitive::Newline)
    | (Primitive::OpenOutputString, Primitive::OpenOutputString)
    | (Primitive::GetOutputString, Primitive::GetOutputString)
    | (Primitive::CurrentOutputPort, Primitive::CurrentOutputPort)
    | (Primitive::WithExceptionHandler, Primitive::WithExceptionHandler)
    | (Primitive::Raise, Primitive::Raise)
    | (Primitive::RaiseContinuable, Primitive::RaiseContinuable)
    | (Primitive::Error, Primitive::Error)
    | (Primitive::AssertionViolation, Primitive::AssertionViolation)
    | (Primitive::ImplementationRestrictionViolation, Primitive::ImplementationRestrictionViolation)
    | (Primitive::UndefinedViolation, Primitive::UndefinedViolation)
    | (Primitive::SyntaxViolation, Primitive::SyntaxViolation) => true
    _ => false
  }
}

///|
priv enum CompareMode {
  Eq
  Lt
  Gt
  Le
  Ge
}

///|
fn compare_ok(mode : CompareMode, cmp : Int) -> Bool {
  match mode {
    CompareMode::Eq => cmp == 0
    CompareMode::Lt => cmp < 0
    CompareMode::Gt => cmp > 0
    CompareMode::Le => cmp <= 0
    CompareMode::Ge => cmp >= 0
  }
}

///|
fn value_eqv(left : Value, right : Value) -> Bool {
  match (left, right) {
    (Value::Datum(a), Value::Datum(b)) => datum_equal(a, b)
    (Value::Primitive(a), Value::Primitive(b)) => primitive_eq(a, b)
    (Value::Closure(a), Value::Closure(b)) => a.id == b.id
    (Value::CaseClosure(a), Value::CaseClosure(b)) => a.id == b.id
    (Value::GuardHandler(a), Value::GuardHandler(b)) => a.id == b.id
    (Value::Parameter(a), Value::Parameter(b)) => a.id == b.id
    (Value::Promise(a), Value::Promise(b)) => a.id == b.id
    (Value::EvalEnv(a), Value::EvalEnv(b)) => a.id == b.id
    (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id
    (Value::Port(a), Value::Port(b)) => a.id == b.id
    (Value::Record(a), Value::Record(b)) => a.id == b.id
    (Value::RecordProc(a), Value::RecordProc(b)) => a.id == b.id
    (Value::ConditionProc(a), Value::ConditionProc(b)) => a.id == b.id
    (Value::Hashtable(a), Value::Hashtable(b)) => a.id == b.id
    (Value::EnumSet(a), Value::EnumSet(b)) => a.id == b.id
    (Value::EnumSetProc(a), Value::EnumSetProc(b)) => a.id == b.id
    (Value::RecordTypeDescriptor(a), Value::RecordTypeDescriptor(b)) => a.id == b.id
    (Value::RecordConstructorDescriptor(a), Value::RecordConstructorDescriptor(b)) => a.id == b.id
    (Value::Void, Value::Void) => true
    _ => false
  }
}

///|
fn value_equal(left : Value, right : Value) -> Bool {
  value_eqv(left, right)
}

///|
fn value_as_number(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => {
      let unlabel = datum_unlabel(d)
      match unlabel {
        Datum::Int(_)
        | Datum::BigInt(_)
        | Datum::Rat(_, _)
        | Datum::BigRat(_, _)
        | Datum::Float(_)
        | Datum::Complex(_, _) => unlabel
        _ => raise EvalError("type error: number expected")
      }
    }
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn bigint_to_float(value : @bigint.BigInt) -> Float raise EvalError {
  try @strconv.parse_double(value.to_string()) catch {
    _ => raise EvalError("number too large")
  } noraise {
    v => Float::from_double(v)
  }
}

///|
fn bigint_to_float_unchecked(value : @bigint.BigInt) -> Float {
  try @strconv.parse_double(value.to_string()) catch {
    _ => Float::from_int(0)
  } noraise {
    v => Float::from_double(v)
  }
}

///|
fn number_to_float(value : Datum) -> Float raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => Float::from_int(n)
    Datum::BigInt(n) => bigint_to_float(n)
    Datum::Rat(n, d) => Float::from_int(n) / Float::from_int(d)
    Datum::BigRat(n, d) => bigint_to_float(n) / bigint_to_float(d)
    Datum::Float(f) => f
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn number_to_rat(value : Datum) -> (Int, Int) raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => (n, 1)
    Datum::Rat(n, d) => (n, d)
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn number_to_bigrat(value : Datum) -> (@bigint.BigInt, @bigint.BigInt) raise EvalError {
  let real_value = datum_to_real(value)
  match real_value {
    Datum::Int(n) => (bigint_from_int(n), bigint_from_int(1))
    Datum::BigInt(n) => (n, bigint_from_int(1))
    Datum::Rat(n, d) => (bigint_from_int(n), bigint_from_int(d))
    Datum::BigRat(n, d) => (n, d)
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn float_to_exact_datum(value : Float) -> Datum raise EvalError {
  if !float_is_finite(value) {
    raise EvalError("inexact->exact: unsupported")
  }
  let text = value.to_string()
  match parse_exact_decimal(text) {
    Some(datum) => datum
    None => raise EvalError("inexact->exact: unsupported")
  }
}

///|
fn inexact_component_to_exact(value : Datum) -> Datum raise EvalError {
  match value {
    Datum::Float(f) => float_to_exact_datum(f)
    Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) => value
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn normalize_rat_or_error(num : Int, den : Int) -> Datum raise EvalError {
  match normalize_rat(num, den) {
    Some(result) => result
    None => raise EvalError("division by zero")
  }
}

///|
fn normalize_big_rat_or_error(num : @bigint.BigInt, den : @bigint.BigInt) -> Datum raise EvalError {
  match normalize_big_rat(num, den) {
    Some(result) => result
    None => raise EvalError("division by zero")
  }
}

///|
fn datum_is_zero(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(n) => n == 0
    Datum::BigInt(n) => n.is_zero()
    Datum::Rat(n, _) => n == 0
    Datum::BigRat(n, _) => n.is_zero()
    Datum::Float(f) => f == 0.0
    _ => false
  }
}

///|
fn datum_is_inexact(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Float(_) => true
    _ => false
  }
}

///|
fn datum_is_big_exact(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::BigInt(_) | Datum::BigRat(_, _) => true
    _ => false
  }
}

///|
fn datum_to_real(value : Datum) -> Datum raise EvalError {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) => {
      if datum_is_zero(imag.val) {
        real.val
      } else {
        raise EvalError("type error: real expected")
      }
    }
    _ => value
  }
}

///|
fn datum_to_complex_parts(value : Datum) -> (Datum, Datum) {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) => (real.val, imag.val)
    _ => (value, Datum::Int(0))
  }
}

///|
fn datum_to_complex_float(value : Datum) -> (Float, Float) raise EvalError {
  match datum_unlabel(value) {
    Datum::Complex(real, imag) => (number_to_float(real.val), number_to_float(imag.val))
    _ => (number_to_float(value), 0.0)
  }
}

///|
fn make_complex(real : Datum, imag : Datum) -> Datum {
  if datum_is_zero(imag) {
    if datum_is_inexact(imag) && !datum_is_inexact(real) {
      match real {
        Datum::Int(n) => Datum::Float(Float::from_int(n))
        Datum::Rat(n, d) => Datum::Float(Float::from_int(n) / Float::from_int(d))
        Datum::BigInt(n) => Datum::Float(bigint_to_float_unchecked(n))
        Datum::BigRat(n, d) => Datum::Float(bigint_to_float_unchecked(n) / bigint_to_float_unchecked(d))
        Datum::Float(f) => Datum::Float(f)
        _ => real
      }
    } else {
      real
    }
  } else {
    Datum::Complex(Ref::new(real), Ref::new(imag))
  }
}

///|
fn complex_from_float(real : Float, imag : Float) -> Datum {
  make_complex(Datum::Float(real), Datum::Float(imag))
}

///|
fn float_to_int_exact(value : Float) -> Int? {
  let n = value.to_int()
  if value == Float::from_int(n) { Some(n) } else { None }
}

///|
fn pow_int(base : Int, exp : Int) -> Int {
  let mut result = 1
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn pow_bigint_int(base : @bigint.BigInt, exp : Int) -> @bigint.BigInt {
  let mut result = bigint_from_int(1)
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn pow_bigint_bigint(base : @bigint.BigInt, exp : @bigint.BigInt) -> @bigint.BigInt {
  let mut result = bigint_from_int(1)
  let mut b = base
  let mut e = exp
  let two = bigint_from_int(2)
  while e.compare_int(0) > 0 {
    if !bigint_is_zero(e % two) {
      result = result * b
    }
    b = b * b
    e = e / two
  }
  result
}

///|
fn pow_float_int(base : Float, exp : Int) -> Float {
  let mut result = Float::from_int(1)
  let mut b = base
  let mut e = exp
  while e > 0 {
    if e % 2 != 0 {
      result = result * b
    }
    b = b * b
    e = e / 2
  }
  result
}

///|
fn complex_add_f(ar : Float, ai : Float, br : Float, bi : Float) -> (Float, Float) {
  (ar + br, ai + bi)
}

///|
fn complex_sub_f(ar : Float, ai : Float, br : Float, bi : Float) -> (Float, Float) {
  (ar - br, ai - bi)
}

///|
fn complex_mul_f(ar : Float, ai : Float, br : Float, bi : Float) -> (Float, Float) {
  (ar * br - ai * bi, ar * bi + ai * br)
}

///|
fn complex_div_f(ar : Float, ai : Float, br : Float, bi : Float) -> (Float, Float) raise EvalError {
  let denom = br * br + bi * bi
  if denom == 0.0 {
    raise EvalError("division by zero")
  }
  ((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)
}

///|
fn complex_exp_f(ar : Float, ai : Float) -> (Float, Float) {
  let exp_a = @math.expf(ar)
  (exp_a * @math.cosf(ai), exp_a * @math.sinf(ai))
}

///|
fn complex_log_f(ar : Float, ai : Float) -> (Float, Float) {
  let r = (ar * ar + ai * ai).sqrt()
  (@math.lnf(r), @math.atan2f(ai, ar))
}

///|
fn complex_sqrt_f(ar : Float, ai : Float) -> (Float, Float) {
  let r = (ar * ar + ai * ai).sqrt()
  let real = ((r + ar) / 2.0).sqrt()
  let imag_mag = ((r - ar) / 2.0).sqrt()
  let imag = if ai < 0.0 { -imag_mag } else { imag_mag }
  (real, imag)
}

///|
fn complex_sin_f(ar : Float, ai : Float) -> (Float, Float) {
  let sin_a = @math.sinf(ar)
  let cos_a = @math.cosf(ar)
  let exp_b = @math.expf(ai)
  let exp_neg_b = @math.expf(-ai)
  let cosh_b = (exp_b + exp_neg_b) / 2.0
  let sinh_b = (exp_b - exp_neg_b) / 2.0
  (sin_a * cosh_b, cos_a * sinh_b)
}

///|
fn complex_cos_f(ar : Float, ai : Float) -> (Float, Float) {
  let sin_a = @math.sinf(ar)
  let cos_a = @math.cosf(ar)
  let exp_b = @math.expf(ai)
  let exp_neg_b = @math.expf(-ai)
  let cosh_b = (exp_b + exp_neg_b) / 2.0
  let sinh_b = (exp_b - exp_neg_b) / 2.0
  (cos_a * cosh_b, -sin_a * sinh_b)
}

///|
fn complex_tan_f(ar : Float, ai : Float) -> (Float, Float) raise EvalError {
  let (sr, si) = complex_sin_f(ar, ai)
  let (cr, ci) = complex_cos_f(ar, ai)
  complex_div_f(sr, si, cr, ci)
}

///|
fn complex_asin_f(ar : Float, ai : Float) -> (Float, Float) {
  let (zr, zi) = (ar, ai)
  let (z2r, z2i) = complex_mul_f(zr, zi, zr, zi)
  let (one_minus_r, one_minus_i) = complex_sub_f(1.0, 0.0, z2r, z2i)
  let (sqrt_r, sqrt_i) = complex_sqrt_f(one_minus_r, one_minus_i)
  let (iz_r, iz_i) = (-zi, zr)
  let (inside_r, inside_i) = complex_add_f(iz_r, iz_i, sqrt_r, sqrt_i)
  let (log_r, log_i) = complex_log_f(inside_r, inside_i)
  (log_i, -log_r)
}

///|
fn complex_acos_f(ar : Float, ai : Float) -> (Float, Float) {
  let (asin_r, asin_i) = complex_asin_f(ar, ai)
  let pi_over_2 = Float::from_double(@math.PI / 2.0)
  (pi_over_2 - asin_r, -asin_i)
}

///|
fn complex_atan_f(ar : Float, ai : Float) -> (Float, Float) {
  let (iz_r, iz_i) = (-ai, ar)
  let (one_minus_r, one_minus_i) = complex_sub_f(1.0, 0.0, iz_r, iz_i)
  let (one_plus_r, one_plus_i) = complex_add_f(1.0, 0.0, iz_r, iz_i)
  let (log1_r, log1_i) = complex_log_f(one_minus_r, one_minus_i)
  let (log2_r, log2_i) = complex_log_f(one_plus_r, one_plus_i)
  let (diff_r, diff_i) = complex_sub_f(log1_r, log1_i, log2_r, log2_i)
  (-diff_i / 2.0, diff_r / 2.0)
}

///|
fn complex_expt_f(base_r : Float, base_i : Float, exp_r : Float, exp_i : Float) -> (Float, Float) {
  let (log_r, log_i) = complex_log_f(base_r, base_i)
  let (mul_r, mul_i) = complex_mul_f(exp_r, exp_i, log_r, log_i)
  complex_exp_f(mul_r, mul_i)
}

///|
fn num_add(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_add(lr, rr)
    let imag = num_add(li, ri)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) + number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * d2 + n2 * d1, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 + n2 * d1, d1 * d2)
  }
}

///|
fn num_sub(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_sub(lr, rr)
    let imag = num_sub(li, ri)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) - number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * d2 - n2 * d1, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 - n2 * d1, d1 * d2)
  }
}

///|
fn num_mul(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let real = num_sub(num_mul(lr, rr), num_mul(li, ri))
    let imag = num_add(num_mul(lr, ri), num_mul(li, rr))
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) * number_to_float(right))
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    normalize_big_rat_or_error(n1 * n2, d1 * d2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * n2, d1 * d2)
  }
}

///|
fn num_div(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    let denom = num_add(num_mul(rr, rr), num_mul(ri, ri))
    let real_num = num_add(num_mul(lr, rr), num_mul(li, ri))
    let imag_num = num_sub(num_mul(li, rr), num_mul(lr, ri))
    let real = num_div(real_num, denom)
    let imag = num_div(imag_num, denom)
    return make_complex(real, imag)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    let denom = number_to_float(right)
    if denom == 0.0 {
      raise EvalError("division by zero")
    }
    Datum::Float(number_to_float(left) / denom)
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    if bigint_is_zero(n2) {
      raise EvalError("division by zero")
    }
    normalize_big_rat_or_error(n1 * d2, d1 * n2)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    if n2 == 0 {
      raise EvalError("division by zero")
    }
    normalize_rat_or_error(n1 * d2, d1 * n2)
  }
}

///|
fn num_equal(left : Datum, right : Datum) -> Bool raise EvalError {
  if left is Datum::Complex(_, _) || right is Datum::Complex(_, _) {
    let (lr, li) = datum_to_complex_parts(left)
    let (rr, ri) = datum_to_complex_parts(right)
    return num_equal(lr, rr) && num_equal(li, ri)
  }
  if left is Datum::Float(_) || right is Datum::Float(_) {
    number_to_float(left) == number_to_float(right)
  } else if datum_is_big_exact(left) || datum_is_big_exact(right) {
    let (n1, d1) = number_to_bigrat(left)
    let (n2, d2) = number_to_bigrat(right)
    n1 * d2 == n2 * d1
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    n1 * d2 == n2 * d1
  }
}

///|
fn num_less(left : Datum, right : Datum) -> Bool raise EvalError {
  let lreal = datum_to_real(left)
  let rreal = datum_to_real(right)
  if lreal is Datum::Float(_) || rreal is Datum::Float(_) {
    number_to_float(lreal) < number_to_float(rreal)
  } else if datum_is_big_exact(lreal) || datum_is_big_exact(rreal) {
    let (n1, d1) = number_to_bigrat(lreal)
    let (n2, d2) = number_to_bigrat(rreal)
    n1 * d2 < n2 * d1
  } else {
    let (n1, d1) = number_to_rat(lreal)
    let (n2, d2) = number_to_rat(rreal)
    n1 * d2 < n2 * d1
  }
}

///|
fn num_greater(left : Datum, right : Datum) -> Bool raise EvalError {
  let lreal = datum_to_real(left)
  let rreal = datum_to_real(right)
  if lreal is Datum::Float(_) || rreal is Datum::Float(_) {
    number_to_float(lreal) > number_to_float(rreal)
  } else if datum_is_big_exact(lreal) || datum_is_big_exact(rreal) {
    let (n1, d1) = number_to_bigrat(lreal)
    let (n2, d2) = number_to_bigrat(rreal)
    n1 * d2 > n2 * d1
  } else {
    let (n1, d1) = number_to_rat(lreal)
    let (n2, d2) = number_to_rat(rreal)
    n1 * d2 > n2 * d1
  }
}

///|
fn num_less_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_less(left, right) || num_equal(left, right)
}

///|
fn num_greater_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_greater(left, right) || num_equal(left, right)
}

///|
fn rat_floor(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else if num < 0 {
    q - 1
  } else {
    q
  }
}

///|
fn rat_ceiling(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else if num > 0 {
    q + 1
  } else {
    q
  }
}

///|
fn rat_round(num : Int, den : Int) -> Int {
  let q = num / den
  let r = num % den
  if r == 0 {
    q
  } else {
    let abs_r = int_abs(r)
    let twice = abs_r * 2
    if twice < den {
      q
    } else if twice > den {
      if num < 0 { q - 1 } else { q + 1 }
    } else {
      if q % 2 == 0 {
        q
      } else if num < 0 {
        q - 1
      } else {
        q + 1
      }
    }
  }
}

///|
fn bigrat_floor(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else if num.compare_int(0) < 0 {
    q - bigint_from_int(1)
  } else {
    q
  }
}

///|
fn bigrat_ceiling(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else if num.compare_int(0) > 0 {
    q + bigint_from_int(1)
  } else {
    q
  }
}

///|
fn bigrat_round(num : @bigint.BigInt, den : @bigint.BigInt) -> @bigint.BigInt {
  let q = num / den
  let r = num % den
  if bigint_is_zero(r) {
    q
  } else {
    let abs_r = bigint_abs(r)
    let twice = abs_r * bigint_from_int(2)
    if twice < den {
      q
    } else if twice > den {
      if num.compare_int(0) < 0 { q - bigint_from_int(1) } else { q + bigint_from_int(1) }
    } else {
      if bigint_is_zero(q % bigint_from_int(2)) {
        q
      } else if num.compare_int(0) < 0 {
        q - bigint_from_int(1)
      } else {
        q + bigint_from_int(1)
      }
    }
  }
}

///|
fn float_floor_int(value : Float) -> Int {
  let n = value.to_int()
  if value == Float::from_int(n) {
    n
  } else if value > 0.0 {
    n
  } else {
    n - 1
  }
}

///|
fn float_ceiling_int(value : Float) -> Int {
  let n = value.to_int()
  if value == Float::from_int(n) {
    n
  } else if value > 0.0 {
    n + 1
  } else {
    n
  }
}

///|
fn float_round_int(value : Float) -> Int {
  let floor_n = float_floor_int(value)
  let diff = value - Float::from_int(floor_n)
  if diff < 0.5 {
    floor_n
  } else if diff > 0.5 {
    floor_n + 1
  } else if floor_n % 2 == 0 {
    floor_n
  } else {
    floor_n + 1
  }
}

///|
fn float_is_nan(value : Float) -> Bool {
  value != value
}

///|
fn float_is_infinite(value : Float) -> Bool {
  if float_is_nan(value) {
    false
  } else {
    let diff = value - value
    diff != diff
  }
}

///|
fn float_is_finite(value : Float) -> Bool {
  !float_is_nan(value) && !float_is_infinite(value)
}

///|
fn int_to_string_radix(value : Int, radix : Int) -> String {
  if value == 0 {
    return "0"
  }
  let digits : Array[Char] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z',
  ]
  let mut n = if value < 0 { -value } else { value }
  let chars : Array[Char] = []
  while n > 0 {
    let rem = n % radix
    chars.push(digits[rem])
    n = n / radix
  }
  if value < 0 {
    chars.push('-')
  }
  let out : Array[Char] = []
  for ch in chars.rev_iter() {
    out.push(ch)
  }
  String::from_array(out)
}

///|
fn bigint_to_string_radix(value : @bigint.BigInt, radix : Int) -> String {
  if value.is_zero() {
    return "0"
  }
  let digits : Array[Char] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
    'u', 'v', 'w', 'x', 'y', 'z',
  ]
  let mut n = if value.compare_int(0) < 0 { -value } else { value }
  let radix_big = bigint_from_int(radix)
  let chars : Array[Char] = []
  while n.compare_int(0) > 0 {
    let rem = n % radix_big
    chars.push(digits[rem.to_int()])
    n = n / radix_big
  }
  if value.compare_int(0) < 0 {
    chars.push('-')
  }
  let out : Array[Char] = []
  for ch in chars.rev_iter() {
    out.push(ch)
  }
  String::from_array(out)
}

///|
fn value_as_string(value : Value) -> String raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::String(s) => s.val
        _ => raise EvalError("type error: string expected")
      }
    _ => raise EvalError("type error: string expected")
  }
}

///|
fn value_as_string_ref(value : Value) -> Ref[String] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::String(s) => s
        _ => raise EvalError("type error: string expected")
      }
    _ => raise EvalError("type error: string expected")
  }
}

///|
fn string_replace_range(s : String, start : Int, end : Int, fill : Char) -> String raise EvalError {
  let len = s.length()
  if start < 0 || end < start || end > len {
    raise EvalError("index out of range")
  }
  let buf = @buffer.new()
  let mut i = 0
  while i < len {
    match s.get_char(i) {
      Some(ch) => {
        if i >= start && i < end {
          buf.write_char(fill)
        } else {
          buf.write_char(ch)
        }
      }
      None => raise EvalError("index out of range")
    }
    i = i + 1
  }
  buf.to_string()
}

///|
fn value_as_char(value : Value) -> Char raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Char(ch) => ch
        _ => raise EvalError("type error: char expected")
      }
    _ => raise EvalError("type error: char expected")
  }
}

///|
fn char_from_int_value(value : Value) -> Char raise EvalError {
  let n = value_as_int_index(value)
  match n.to_char() {
    Some(ch) => ch
    None => raise EvalError("type error: valid scalar value expected")
  }
}

///|
fn exact_integer_sqrt(n : Int) -> (Int, Int) {
  if n <= 0 {
    if n == 0 {
      return (0, 0)
    }
    // Caller ensures nonnegative; keep a safe fallback.
    return (0, n)
  }
  let mut low = 0
  let mut high = n
  let mut ans = 0
  while low <= high {
    let mid = (low + high) / 2
    if mid == 0 {
      ans = 0
      low = 1
    } else {
      let div = n / mid
      if mid <= div {
        ans = mid
        low = mid + 1
      } else {
        high = mid - 1
      }
    }
  }
  let rem = n - (ans * ans)
  (ans, rem)
}

///|
fn exact_integer_sqrt_bigint(n : @bigint.BigInt) -> (@bigint.BigInt, @bigint.BigInt) {
  if n.compare_int(0) <= 0 {
    if n.is_zero() {
      return (bigint_from_int(0), bigint_from_int(0))
    }
    return (bigint_from_int(0), n)
  }
  let mut low = bigint_from_int(0)
  let mut high = n
  let mut ans = bigint_from_int(0)
  let one = bigint_from_int(1)
  let two = bigint_from_int(2)
  while low <= high {
    let mid = (low + high) / two
    if mid.is_zero() {
      ans = bigint_from_int(0)
      low = one
    } else {
      let div = n / mid
      if mid <= div {
        ans = mid
        low = mid + one
      } else {
        high = mid - one
      }
    }
  }
  let rem = n - (ans * ans)
  (ans, rem)
}

///|
fn rationalize_float(value : Float, tol : Float) -> (Int, Int) {
  if value == 0.0 {
    return (0, 1)
  }
  let mut sign = 1
  let mut x = value
  if x < 0.0 {
    sign = -1
    x = -x
  }
  let mut a = float_floor_int(x)
  let mut p0 = 1
  let mut q0 = 0
  let mut p1 = a
  let mut q1 = 1
  let mut frac = x - Float::from_int(a)
  let mut iter = 0
  while iter < 64 {
    let approx = Float::from_int(p1) / Float::from_int(q1)
    let mut diff = x - approx
    if diff < 0.0 {
      diff = -diff
    }
    if diff <= tol || frac == 0.0 {
      break
    }
    frac = 1.0 / frac
    a = float_floor_int(frac)
    let p2 = a * p1 + p0
    let q2 = a * q1 + q0
    if q2 == 0 {
      break
    }
    p0 = p1
    q0 = q1
    p1 = p2
    q1 = q2
    frac = frac - Float::from_int(a)
    iter = iter + 1
  }
  (sign * p1, q1)
}

///|
fn value_as_int_index(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Int(n) => n
        Datum::BigInt(n) =>
          match bigint_to_int_option(n) {
            Some(v) => v
            None => raise EvalError("index out of range")
          }
        Datum::Complex(real, imag) => {
          if datum_is_zero(imag.val) {
            match datum_unlabel(real.val) {
              Datum::Int(n) => n
              Datum::BigInt(n) =>
                match bigint_to_int_option(n) {
                  Some(v) => v
                  None => raise EvalError("index out of range")
                }
              _ => raise EvalError("type error: exact integer expected")
            }
          } else {
            raise EvalError("type error: exact integer expected")
          }
        }
        _ => raise EvalError("type error: exact integer expected")
      }
    _ => raise EvalError("type error: exact integer expected")
  }
}

///|
fn value_as_exact_integer(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Int(n) => Datum::Int(n)
        Datum::BigInt(n) => Datum::BigInt(n)
        Datum::Complex(real, imag) =>
          if datum_is_zero(imag.val) {
            match datum_unlabel(real.val) {
              Datum::Int(n) => Datum::Int(n)
              Datum::BigInt(n) => Datum::BigInt(n)
              _ => raise EvalError("type error: integer expected")
            }
          } else {
            raise EvalError("type error: integer expected")
          }
        _ => raise EvalError("type error: integer expected")
      }
    _ => raise EvalError("type error: integer expected")
  }
}

///|
fn datum_to_bigint(value : Datum) -> @bigint.BigInt raise EvalError {
  match datum_unlabel(value) {
    Datum::Int(n) => bigint_from_int(n)
    Datum::BigInt(n) => n
    _ => raise EvalError("type error: integer expected")
  }
}

///|
fn bigint_to_integer_datum(value : @bigint.BigInt) -> Datum {
  match bigint_to_int_option(value) {
    Some(v) => Datum::Int(v)
    None => Datum::BigInt(value)
  }
}

///|
fn value_as_nonnegative_int(value : Value) -> Int raise EvalError {
  let n = value_as_int_index(value)
  if n < 0 {
    raise EvalError("type error: exact nonnegative integer expected")
  }
  n
}

///|
fn check_index(idx : Int, len : Int) -> Unit raise EvalError {
  if idx < 0 || idx >= len {
    raise EvalError("index out of range")
  }
}

///|
fn compare_chain_char(
  args : Array[Value],
  mode : CompareMode,
  case_insensitive : Bool,
) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let mut prev = value_as_char(args[0])
  if case_insensitive {
    prev = prev.to_ascii_lowercase()
  }
  let mut i = 1
  while i < args.length() {
    let mut cur = value_as_char(args[i])
    if case_insensitive {
      cur = cur.to_ascii_lowercase()
    }
    let cmp = prev.to_int().compare(cur.to_int())
    if !compare_ok(mode, cmp) {
      return false
    }
    prev = cur
    i = i + 1
  }
  true
}

///|
fn compare_chain_string(
  args : Array[Value],
  mode : CompareMode,
  case_insensitive : Bool,
) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let mut prev = value_as_string(args[0])
  if case_insensitive {
    prev = prev.to_lower()
  }
  let mut i = 1
  while i < args.length() {
    let mut cur = value_as_string(args[i])
    if case_insensitive {
      cur = cur.to_lower()
    }
    let cmp = prev.lexical_compare(cur)
    if !compare_ok(mode, cmp) {
      return false
    }
    prev = cur
    i = i + 1
  }
  true
}

///|
priv enum EqualityMode {
  Eqv
  Equal
}

///|
fn equality_match(mode : EqualityMode, left : Value, right : Value) -> Bool {
  match mode {
    EqualityMode::Eqv => value_eqv(left, right)
    EqualityMode::Equal => value_equal(left, right)
  }
}

///|
fn list_member(
  mode : EqualityMode,
  item : Value,
  list : Datum,
) -> Value raise EvalError {
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => return bool_value(false)
      Datum::Pair(car, cdr) => {
        if equality_match(mode, item, value_from_datum(car.val)) {
          return Value::Datum(cur)
        }
        cur = cdr.val
      }
      _ => raise EvalError("type error: proper list expected")
    }
  }
  bool_value(false)
}

///|
fn list_assoc(
  mode : EqualityMode,
  key : Value,
  list : Datum,
) -> Value raise EvalError {
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => return bool_value(false)
      Datum::Pair(entry, rest) => {
        let entry_val = entry.val
        match entry_val {
          Datum::Pair(car, _) => {
            if equality_match(mode, key, value_from_datum(car.val)) {
              return Value::Datum(entry_val)
            }
          }
          _ => raise EvalError("type error: pair expected")
        }
        cur = rest.val
      }
      _ => raise EvalError("type error: proper list expected")
    }
  }
  bool_value(false)
}

///|
fn value_as_vector(value : Value) -> Array[Datum] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Vector(items) => items
        _ => raise EvalError("type error: vector expected")
      }
    _ => raise EvalError("type error: vector expected")
  }
}

///|
fn value_as_bool(value : Value) -> Bool raise EvalError {
  match value {
    Value::Datum(Datum::Bool(flag)) => flag
    _ => raise EvalError("type error: boolean expected")
  }
}

///|
fn value_as_symbol(value : Value) -> String raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::Symbol(name) => name
        _ => raise EvalError("type error: symbol expected")
      }
    _ => raise EvalError("type error: symbol expected")
  }
}

///|
fn parse_field_spec(item : Datum) -> RecordField raise EvalError {
  let parts = datum_list_to_array(item)
  if parts.length() != 2 {
    raise EvalError("invalid record field spec")
  }
  let mut_flag = match symbol_name(parts[0]) {
    Some("mutable") => true
    Some("immutable") => false
    _ => raise EvalError("invalid record field spec")
  }
  let name = match symbol_name(parts[1]) {
    Some(field_name) => field_name
    None => raise EvalError("invalid record field spec")
  }
  RecordField::{ name, mutable: mut_flag }
}

///|
fn parse_field_specs(items : Array[Datum]) -> Array[RecordField] raise EvalError {
  let fields : Array[RecordField] = []
  for item in items {
    fields.push(parse_field_spec(item))
  }
  fields
}

///|
fn value_as_bytevector(value : Value) -> Array[Int] raise EvalError {
  match value {
    Value::Datum(datum) =>
      match datum_unlabel(datum) {
        Datum::ByteVector(items) => items
        _ => raise EvalError("type error: bytevector expected")
      }
    _ => raise EvalError("type error: bytevector expected")
  }
}

///|
fn value_as_byte(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(Datum::Int(n)) => {
      if n < 0 || n > 255 {
        raise EvalError("type error: byte expected")
      }
      n
    }
    _ => raise EvalError("type error: byte expected")
  }
}

///|
fn value_as_port(value : Value) -> Port raise EvalError {
  match value {
    Value::Port(port) => port
    _ => raise EvalError("type error: port expected")
  }
}

///|
fn value_as_record_type_descriptor(value : Value) -> RecordTypeDescriptor raise EvalError {
  match value {
    Value::RecordTypeDescriptor(desc) => desc
    _ => raise EvalError("type error: record type descriptor expected")
  }
}

///|
fn value_as_record_constructor_descriptor(value : Value) -> RecordConstructorDescriptor raise EvalError {
  match value {
    Value::RecordConstructorDescriptor(desc) => desc
    _ => raise EvalError("type error: record constructor descriptor expected")
  }
}

///|
fn value_as_hashtable(value : Value) -> Hashtable raise EvalError {
  match value {
    Value::Hashtable(table) => table
    _ => raise EvalError("type error: hashtable expected")
  }
}

///|
fn value_as_enum_set(value : Value) -> EnumSet raise EvalError {
  match value {
    Value::EnumSet(set) => set
    _ => raise EvalError("type error: enum-set expected")
  }
}

///|
fn hashtable_equiv_from_value(value : Value) -> HashtableEquiv raise EvalError {
  match value {
    Value::Primitive(Primitive::Eq) => HashtableEquiv::Eq
    Value::Primitive(Primitive::Eqv) => HashtableEquiv::Eqv
    Value::Primitive(Primitive::Equal) => HashtableEquiv::Equal
    _ => {
      if !is_procedure_value(value) {
        raise EvalError("type error: procedure expected")
      }
      HashtableEquiv::Proc(value)
    }
  }
}

///|
fn hashtable_equiv_to_value(equiv : HashtableEquiv) -> Value {
  match equiv {
    HashtableEquiv::Eq => Value::Primitive(Primitive::Eq)
    HashtableEquiv::Eqv => Value::Primitive(Primitive::Eqv)
    HashtableEquiv::Equal => Value::Primitive(Primitive::Equal)
    HashtableEquiv::Proc(proc) => proc
  }
}

///|
fn validate_hashtable_size_arg(value : Value) -> Unit raise EvalError {
  let n = value_as_int_index(value)
  if n < 0 {
    raise EvalError("type error: non-negative integer expected")
  }
}

///|
fn enum_set_require_same_universe(a : EnumSet, b : EnumSet) -> Unit raise EvalError {
  if !enum_set_universe_equal(a, b) {
    raise EvalError("type error: enum-set universe mismatch")
  }
}

///|
fn enum_set_symbol_list(value : Value) -> Array[String] raise EvalError {
  let datum = value_as_datum(value)
  let items = datum_list_to_array(datum)
  parse_symbol_list(items)
}

///|
fn ensure_unique_symbols(names : Array[String]) -> Unit raise EvalError {
  let seen : Map[String, Bool] = {}
  for name in names {
    if seen.contains(name) {
      raise EvalError("type error: duplicate symbol")
    }
    seen[name] = true
  }
}

///|
fn value_as_condition_type_descriptor(value : Value) -> RecordTypeDescriptor raise EvalError {
  let desc = value_as_record_type_descriptor(value)
  let base = condition_base_type()
  if !record_type_is_a(desc.record_type, base) {
    raise EvalError("type error: condition type expected")
  }
  desc
}

///|
fn record_accessor_info(value : Value) -> (RecordType, Int) raise EvalError {
  match value {
    Value::RecordProc(proc) =>
      match proc.kind {
        RecordProcKind::Accessor(record_type, idx) => (record_type, idx)
        _ => raise EvalError("type error: record accessor expected")
      }
    _ => raise EvalError("type error: record accessor expected")
  }
}

///|
fn char_to_raw_string(ch : Char) -> String {
  let buf = @buffer.new()
  buf.write_char(ch)
  buf.to_string()
}

///|
fn display_string(value : Value) -> String {
  match value {
    Value::Datum(Datum::String(s)) => s.val
    Value::Datum(Datum::Char(ch)) => char_to_raw_string(ch)
    Value::Datum(Datum::Symbol(name)) => name
    _ => value_to_string(value)
  }
}

///|
fn apply_primitive(prim : Primitive, args : Array[Value]) -> Value raise EvalError {
  match prim {
    Primitive::Add => {
      let mut acc = Datum::Int(0)
      for arg in args {
        acc = num_add(acc, value_as_number(arg))
      }
      Value::Datum(acc)
    }
    Primitive::Sub => {
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_sub(Datum::Int(0), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_sub(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    }
    Primitive::Mul => {
      let mut product = Datum::Int(1)
      for arg in args {
        product = num_mul(product, value_as_number(arg))
      }
      Value::Datum(product)
    }
    Primitive::Div => {
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_div(Datum::Int(1), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_div(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    }
    Primitive::NumEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_number(args[0])
        let mut ok = true
        let mut i = 1
        while i < args.length() {
          if !num_equal(value_as_number(args[i]), first) {
            ok = false
            break
          } else {
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::Less => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::Greater => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::LessEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::GreaterEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::ExactToInexact => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(_)) => args[0]
            Value::Datum(Datum::Complex(real, imag)) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              Value::Datum(make_complex(Datum::Float(real_f), Datum::Float(imag_f)))
            }
            _ => {
              let value = value_as_number(args[0])
              Value::Datum(Datum::Float(number_to_float(value)))
            }
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::InexactToExact => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(f)) => Value::Datum(float_to_exact_datum(f))
            Value::Datum(Datum::Complex(real, imag)) => {
              let real_exact = inexact_component_to_exact(real.val)
              let imag_exact = inexact_component_to_exact(imag.val)
              Value::Datum(make_complex(real_exact, imag_exact))
            }
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => args[0]
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ExactIntegerSqrt => {
      match args.length() {
        1 => {
          let value = value_as_exact_integer(args[0])
          match value {
            Datum::Int(n) => {
              if n < 0 {
                raise EvalError("type error: exact nonnegative integer expected")
              }
              let (s, r) = exact_integer_sqrt(n)
              Value::Values([
                Value::Datum(Datum::Int(s)),
                Value::Datum(Datum::Int(r)),
              ])
            }
            Datum::BigInt(n) => {
              if n.compare_int(0) < 0 {
                raise EvalError("type error: exact nonnegative integer expected")
              }
              let (s, r) = exact_integer_sqrt_bigint(n)
              Value::Values([
                Value::Datum(Datum::BigInt(s)),
                Value::Datum(Datum::BigInt(r)),
              ])
            }
            _ => raise EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Rationalize => {
      match args.length() {
        2 => {
          let x = value_as_number(args[0])
          let tol_value = value_as_number(args[1])
          let tol = number_to_float(tol_value)
          if tol < 0.0 {
            raise EvalError("type error: nonnegative real expected")
          }
          match x {
            Datum::Int(_)
            | Datum::BigInt(_)
            | Datum::Rat(_, _)
            | Datum::BigRat(_, _) => Value::Datum(x)
            Datum::Float(f) => {
              let (n, d) = rationalize_float(f, tol)
              Value::Datum(normalize_rat_or_error(n, d))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::NumberToString => {
      match args.length() {
        1 | 2 => {
          let num = value_as_number(args[0])
          let radix = if args.length() == 2 {
            value_as_int_index(args[1])
          } else {
            10
          }
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          match num {
            Datum::Int(n) => {
              let text = int_to_string_radix(n, radix)
              Value::Datum(Datum::String(Ref::new(text)))
            }
            Datum::BigInt(n) => {
              let text = bigint_to_string_radix(n, radix)
              Value::Datum(Datum::String(Ref::new(text)))
            }
            Datum::Rat(_, _)
            | Datum::BigRat(_, _)
            | Datum::Float(_)
            | Datum::Complex(_, _) => {
              if radix != 10 {
                raise EvalError("type error: exact integer expected")
              }
              let text = value_to_string(Value::Datum(num))
              Value::Datum(Datum::String(Ref::new(text)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::StringToNumber => {
      match args.length() {
        1 | 2 => {
          let s = value_as_string(args[0])
          let radix = if args.length() == 2 {
            value_as_int_index(args[1])
          } else {
            10
          }
          if radix < 2 || radix > 36 {
            raise EvalError("radix out of range")
          }
          let parsed = if args.length() == 2 {
            parse_number_token_with_radix(s, radix)
          } else {
            parse_number_token(s)
          }
          match parsed {
            Some(num) => Value::Datum(num)
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::MakeRectangular => {
      match args.length() {
        2 => {
          let real = datum_to_real(value_as_number(args[0]))
          let imag = datum_to_real(value_as_number(args[1]))
          Value::Datum(make_complex(real, imag))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::MakePolar => {
      match args.length() {
        2 => {
          let r = number_to_float(datum_to_real(value_as_number(args[0])))
          let theta = number_to_float(datum_to_real(value_as_number(args[1])))
          let real = Datum::Float(r * @math.cosf(theta))
          let imag = Datum::Float(r * @math.sinf(theta))
          Value::Datum(make_complex(real, imag))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::RealPart => {
      match args.length() {
        1 => {
          match value_as_number(args[0]) {
            Datum::Complex(real, _) => Value::Datum(real.val)
            datum => Value::Datum(datum)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ImagPart => {
      match args.length() {
        1 => {
          match value_as_number(args[0]) {
            Datum::Complex(_, imag) => Value::Datum(imag.val)
            Datum::Float(_) => Value::Datum(Datum::Float(0.0))
            Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
              Value::Datum(Datum::Int(0))
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Magnitude => {
      match args.length() {
        1 => {
          match value_as_number(args[0]) {
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Value::Datum(Datum::Float(mag))
            }
            Datum::Int(n) => Value::Datum(Datum::Int(int_abs(n)))
            Datum::Rat(n, d) => Value::Datum(normalize_rat_or_error(int_abs(n), d))
            Datum::BigInt(n) => Value::Datum(Datum::BigInt(bigint_abs(n)))
            Datum::BigRat(n, d) =>
              Value::Datum(normalize_big_rat_or_error(bigint_abs(n), d))
            Datum::Float(f) =>
              if f < 0.0 {
                Value::Datum(Datum::Float(-f))
              } else {
                Value::Datum(Datum::Float(f))
              }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Angle => {
      match args.length() {
        1 => {
          match value_as_number(args[0]) {
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              Value::Datum(Datum::Float(@math.atan2f(imag_f, real_f)))
            }
            datum => {
              let real_f = number_to_float(datum)
              Value::Datum(Datum::Float(@math.atan2f(0.0, real_f)))
            }
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Sqrt => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= 0.0 {
            Value::Datum(Datum::Float(real_f.sqrt()))
          } else {
            let (sr, si) = complex_sqrt_f(real_f, imag_f)
            Value::Datum(complex_from_float(sr, si))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Exp => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.expf(real_f)))
          } else {
            let (er, ei) = complex_exp_f(real_f, imag_f)
            Value::Datum(complex_from_float(er, ei))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Log => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= 0.0 {
            Value::Datum(Datum::Float(@math.lnf(real_f)))
          } else {
            let (lr, li) = complex_log_f(real_f, imag_f)
            Value::Datum(complex_from_float(lr, li))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Expt => {
      match args.length() {
        2 => {
          let base = value_as_number(args[0])
          let exponent = value_as_number(args[1])
          match exponent {
            Datum::Int(exp_int) => {
              match base {
                Datum::Int(base_int) => {
                  if exp_int >= 0 {
                    Value::Datum(Datum::Int(pow_int(base_int, exp_int)))
                  } else {
                    Value::Datum(normalize_rat_or_error(1, pow_int(base_int, -exp_int)))
                  }
                }
                Datum::BigInt(base_big) => {
                  if exp_int >= 0 {
                    Value::Datum(bigint_to_integer_datum(pow_bigint_int(base_big, exp_int)))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_int(base_big, -exp_int)
                    Value::Datum(normalize_big_rat_or_error(bigint_from_int(1), denom))
                  }
                }
                Datum::Rat(n, d) => {
                  if exp_int >= 0 {
                    let num = pow_int(n, exp_int)
                    let den = pow_int(d, exp_int)
                    Value::Datum(normalize_rat_or_error(num, den))
                  } else {
                    let num = pow_int(d, -exp_int)
                    let den = pow_int(n, -exp_int)
                    Value::Datum(normalize_rat_or_error(num, den))
                  }
                }
                Datum::BigRat(n, d) => {
                  if exp_int >= 0 {
                    let num = pow_bigint_int(n, exp_int)
                    let den = pow_bigint_int(d, exp_int)
                    Value::Datum(normalize_big_rat_or_error(num, den))
                  } else {
                    if bigint_is_zero(n) {
                      raise EvalError("division by zero")
                    }
                    let num = pow_bigint_int(d, -exp_int)
                    let den = pow_bigint_int(n, -exp_int)
                    Value::Datum(normalize_big_rat_or_error(num, den))
                  }
                }
                Datum::Float(f) => {
                  let pow = if exp_int >= 0 {
                    pow_float_int(f, exp_int)
                  } else {
                    1.0 / pow_float_int(f, -exp_int)
                  }
                  Value::Datum(Datum::Float(pow))
                }
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, Float::from_int(exp_int), 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise EvalError("type error: number expected")
              }
            }
            Datum::BigInt(exp_big) => {
              let exp_sign = exp_big.compare_int(0)
              let abs_exp = if exp_sign < 0 { bigint_abs(exp_big) } else { exp_big }
              let exp_is_odd = !bigint_is_zero(abs_exp % bigint_from_int(2))
              match base {
                Datum::Int(base_int) => {
                  let base_big = bigint_from_int(base_int)
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(bigint_from_int(1), denom))
                  }
                }
                Datum::BigInt(base_big) => {
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let result = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(bigint_to_integer_datum(result))
                  } else {
                    if bigint_is_zero(base_big) {
                      raise EvalError("division by zero")
                    }
                    let denom = pow_bigint_bigint(base_big, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(bigint_from_int(1), denom))
                  }
                }
                Datum::Rat(n, d) => {
                  let num = bigint_from_int(n)
                  let den = bigint_from_int(d)
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(num) {
                      raise EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(num, abs_exp)
                    let den_pow = pow_bigint_bigint(den, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                }
                Datum::BigRat(n, d) => {
                  if exp_sign == 0 {
                    Value::Datum(Datum::Int(1))
                  } else if exp_sign > 0 {
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(num_pow, den_pow))
                  } else {
                    if bigint_is_zero(n) {
                      raise EvalError("division by zero")
                    }
                    let num_pow = pow_bigint_bigint(n, abs_exp)
                    let den_pow = pow_bigint_bigint(d, abs_exp)
                    Value::Datum(normalize_big_rat_or_error(den_pow, num_pow))
                  }
                }
                Datum::Float(f) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let abs_base = if f < 0.0 { -f } else { f }
                  let mut pow = @math.powf(abs_base, exp_f)
                  if exp_sign < 0 {
                    pow = 1.0 / pow
                  }
                  if f < 0.0 && exp_is_odd {
                    pow = -pow
                  }
                  Value::Datum(Datum::Float(pow))
                }
                Datum::Complex(_, _) => {
                  let exp_f = bigint_to_float(abs_exp)
                  let (br, bi) = datum_to_complex_float(base)
                  let exp_real = if exp_sign < 0 { -exp_f } else { exp_f }
                  let (rr, ri) = complex_expt_f(br, bi, exp_real, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => raise EvalError("type error: number expected")
              }
            }
            Datum::Float(exp_f) => {
              match base {
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  match float_to_int_exact(exp_f) {
                    Some(exp_int) => {
                      let pow = if exp_int >= 0 {
                        pow_float_int(base_f, exp_int)
                      } else {
                        1.0 / pow_float_int(base_f, -exp_int)
                      }
                      Value::Datum(Datum::Float(pow))
                    }
                    None => {
                      if base_f >= 0.0 {
                        Value::Datum(Datum::Float(@math.powf(base_f, exp_f)))
                      } else {
                        let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                        Value::Datum(complex_from_float(rr, ri))
                      }
                    }
                  }
                }
              }
            }
            Datum::Rat(_, _) | Datum::BigRat(_, _) => {
              let exp_f = number_to_float(exponent)
              match base {
                Datum::Complex(_, _) => {
                  let (br, bi) = datum_to_complex_float(base)
                  let (rr, ri) = complex_expt_f(br, bi, exp_f, 0.0)
                  Value::Datum(complex_from_float(rr, ri))
                }
                _ => {
                  let base_f = number_to_float(base)
                  if base_f >= 0.0 {
                    Value::Datum(Datum::Float(@math.powf(base_f, exp_f)))
                  } else {
                    let (rr, ri) = complex_expt_f(base_f, 0.0, exp_f, 0.0)
                    Value::Datum(complex_from_float(rr, ri))
                  }
                }
              }
            }
            Datum::Complex(_, _) => {
              let (br, bi) = datum_to_complex_float(base)
              let (er, ei) = datum_to_complex_float(exponent)
              let (rr, ri) = complex_expt_f(br, bi, er, ei)
              Value::Datum(complex_from_float(rr, ri))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Sin => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.sinf(real_f)))
          } else {
            let (sr, si) = complex_sin_f(real_f, imag_f)
            Value::Datum(complex_from_float(sr, si))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Cos => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.cosf(real_f)))
          } else {
            let (cr, ci) = complex_cos_f(real_f, imag_f)
            Value::Datum(complex_from_float(cr, ci))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Tan => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.tanf(real_f)))
          } else {
            let (tr, ti) = complex_tan_f(real_f, imag_f)
            Value::Datum(complex_from_float(tr, ti))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Asin => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Value::Datum(Datum::Float(@math.asinf(real_f)))
          } else {
            let (ar, ai) = complex_asin_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Acos => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 && real_f >= -1.0 && real_f <= 1.0 {
            Value::Datum(Datum::Float(@math.acosf(real_f)))
          } else {
            let (ar, ai) = complex_acos_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Atan => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          let (real_f, imag_f) = datum_to_complex_float(value)
          if imag_f == 0.0 {
            Value::Datum(Datum::Float(@math.atanf(real_f)))
          } else {
            let (ar, ai) = complex_atan_f(real_f, imag_f)
            Value::Datum(complex_from_float(ar, ai))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Numerator => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(n)) => Value::Datum(Datum::Int(n))
            Value::Datum(Datum::Rat(n, _)) => Value::Datum(Datum::Int(n))
            Value::Datum(Datum::BigInt(n)) => Value::Datum(Datum::BigInt(n))
            Value::Datum(Datum::BigRat(n, _)) => Value::Datum(Datum::BigInt(n))
            _ => raise EvalError("type error: rational expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Denominator => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) => Value::Datum(Datum::Int(1))
            Value::Datum(Datum::Rat(_, d)) => Value::Datum(Datum::Int(d))
            Value::Datum(Datum::BigInt(_)) => Value::Datum(Datum::Int(1))
            Value::Datum(Datum::BigRat(_, d)) => Value::Datum(Datum::BigInt(d))
            _ => raise EvalError("type error: rational expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Abs => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(int_abs(n)))
            Datum::Rat(n, d) => Value::Datum(normalize_rat_or_error(int_abs(n), d))
            Datum::BigInt(n) => Value::Datum(Datum::BigInt(bigint_abs(n)))
            Datum::BigRat(n, d) => {
              Value::Datum(
                normalize_big_rat_or_error(bigint_abs(n), d),
              )
            }
            Datum::Float(f) =>
              if f < 0.0 {
                Value::Datum(Datum::Float(-f))
              } else {
                Value::Datum(Datum::Float(f))
              }
            Datum::Complex(real, imag) => {
              let real_f = number_to_float(real.val)
              let imag_f = number_to_float(imag.val)
              let mag = (real_f * real_f + imag_f * imag_f).sqrt()
              Value::Datum(Datum::Float(mag))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Quotient => {
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              Value::Datum(Datum::Int(left / right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
              }
              Value::Datum(Datum::BigInt(left / right))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Remainder => {
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              Value::Datum(Datum::Int(left % right))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
              }
              Value::Datum(Datum::BigInt(left % right))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Modulo => {
      match args.length() {
        2 => {
          let left_val = value_as_exact_integer(args[0])
          let right_val = value_as_exact_integer(args[1])
          match (left_val, right_val) {
            (Datum::Int(left), Datum::Int(right)) => {
              if right == 0 {
                raise EvalError("division by zero")
              }
              let rem = left % right
              let result =
                if rem == 0 || (rem > 0 && right > 0) || (rem < 0 && right < 0) {
                  rem
                } else {
                  rem + right
                }
              Value::Datum(Datum::Int(result))
            }
            _ => {
              let left = datum_to_bigint(left_val)
              let right = datum_to_bigint(right_val)
              if right.is_zero() {
                raise EvalError("division by zero")
              }
              let rem = left % right
              let rem_sign = rem.compare_int(0)
              let right_sign = right.compare_int(0)
              let result =
                if rem.is_zero() || (rem_sign > 0 && right_sign > 0) || (rem_sign < 0 && right_sign < 0) {
                  rem
                } else {
                  rem + right
                }
              Value::Datum(Datum::BigInt(result))
            }
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Gcd => {
      let mut acc_int = 0
      let mut acc_big = bigint_from_int(0)
      let mut use_big = false
      for arg in args {
        let value = value_as_exact_integer(arg)
        match value {
          Datum::Int(n) => {
            if use_big {
              acc_big = bigint_gcd(acc_big, bigint_from_int(n))
            } else {
              acc_int = gcd(acc_int, n)
            }
          }
          Datum::BigInt(n) => {
            if !use_big {
              use_big = true
              acc_big = bigint_from_int(acc_int)
            }
            acc_big = bigint_gcd(acc_big, n)
          }
          _ => raise EvalError("type error: integer expected")
        }
      }
      if use_big {
        Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Datum::Int(v)
            None => Datum::BigInt(acc_big)
          },
        )
      } else {
        Value::Datum(Datum::Int(acc_int))
      }
    }
    Primitive::Lcm => {
      let mut acc_int = 1
      let mut acc_big = bigint_from_int(1)
      let mut use_big = false
      for arg in args {
        let value = value_as_exact_integer(arg)
        match value {
          Datum::Int(n) => {
            if use_big {
              let n_big = bigint_from_int(n)
              if acc_big.is_zero() || n_big.is_zero() {
                acc_big = bigint_from_int(0)
              } else {
                let g = bigint_gcd(acc_big, n_big)
                acc_big = bigint_abs((acc_big / g) * n_big)
              }
            } else {
              if acc_int == 0 || n == 0 {
                acc_int = 0
              } else {
                let g = gcd(acc_int, n)
                acc_int = int_abs((acc_int / g) * n)
              }
            }
          }
          Datum::BigInt(n) => {
            if !use_big {
              use_big = true
              acc_big = bigint_from_int(acc_int)
            }
            if acc_big.is_zero() || n.is_zero() {
              acc_big = bigint_from_int(0)
            } else {
              let g = bigint_gcd(acc_big, n)
              acc_big = bigint_abs((acc_big / g) * n)
            }
          }
          _ => raise EvalError("type error: integer expected")
        }
      }
      if use_big {
        Value::Datum(
          match bigint_to_int_option(acc_big) {
            Some(v) => Datum::Int(v)
            None => Datum::BigInt(acc_big)
          },
        )
      } else {
        Value::Datum(Datum::Int(acc_int))
      }
    }
    Primitive::Max => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut best = value_as_number(args[0])
      let mut has_inexact = best is Datum::Float(_)
      let mut i = 1
      while i < args.length() {
        let cur = value_as_number(args[i])
        if cur is Datum::Float(_) {
          has_inexact = true
        }
        if num_less(best, cur) {
          best = cur
        }
        i = i + 1
      }
      if has_inexact {
        Value::Datum(Datum::Float(number_to_float(best)))
      } else {
        Value::Datum(best)
      }
    }
    Primitive::Min => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let mut best = value_as_number(args[0])
      let mut has_inexact = best is Datum::Float(_)
      let mut i = 1
      while i < args.length() {
        let cur = value_as_number(args[i])
        if cur is Datum::Float(_) {
          has_inexact = true
        }
        if num_greater(best, cur) {
          best = cur
        }
        i = i + 1
      }
      if has_inexact {
        Value::Datum(Datum::Float(number_to_float(best)))
      } else {
        Value::Datum(best)
      }
    }
    Primitive::Floor => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_floor(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_floor(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_floor_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Ceiling => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_ceiling(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_ceiling(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_ceiling_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Truncate => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(n / d))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = n / d
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = f.to_int()
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Round => {
      match args.length() {
        1 => {
          let value = value_as_number(args[0])
          match value {
            Datum::Int(n) => Value::Datum(Datum::Int(n))
            Datum::Rat(n, d) => Value::Datum(Datum::Int(rat_round(n, d)))
            Datum::BigInt(n) => Value::Datum(bigint_to_integer_datum(n))
            Datum::BigRat(n, d) => {
              let q = bigrat_round(n, d)
              Value::Datum(bigint_to_integer_datum(q))
            }
            Datum::Float(f) => {
              let n = float_round_int(f)
              Value::Datum(Datum::Float(Float::from_int(n)))
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Eq => {
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Eqv => {
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Equal => {
      match args.length() {
        2 => bool_value(value_equal(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Cons => {
      match args.length() {
        2 => {
          let car = value_to_datum_element(args[0])
          let cdr = value_to_datum_element(args[1])
          Value::Datum(pair_new(car, cdr))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::SetCar => {
      match args.length() {
        2 => {
          let (car_cell, _) = value_as_pair_cells(args[0])
          car_cell.val = value_to_datum_element(args[1])
          Value::Void
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::SetCdr => {
      match args.length() {
        2 => {
          let (_, cdr_cell) = value_as_pair_cells(args[0])
          cdr_cell.val = value_to_datum_element(args[1])
          Value::Void
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Car => {
      match args.length() {
        1 => {
          let (car, _) = value_as_pair(args[0])
          value_from_datum(car)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Cdr => {
      match args.length() {
        1 => {
          let (_, cdr) = value_as_pair(args[0])
          value_from_datum(cdr)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Cxr(chain) => {
      match args.length() {
        1 => {
          let mut cur = value_as_datum(args[0])
          let mut i = chain.length()
          while i > 0 {
            i = i - 1
            let op = match chain.get_char(i) {
              Some(ch) => ch
              None => raise EvalError("internal error: invalid cxr")
            }
            match cur {
              Datum::Pair(car, cdr) => {
                match op {
                  'a' => cur = car.val
                  'd' => cur = cdr.val
                  _ => raise EvalError("internal error: invalid cxr")
                }
              }
              _ => raise EvalError("type error: pair expected")
            }
          }
          value_from_datum(cur)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::List => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_to_datum_element(arg))
      }
      Value::Datum(list_from_array(items))
    }
    Primitive::MakeList => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_to_datum_element(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::NullP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Nil) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::PairP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Pair(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::SymbolP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Symbol(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::IdentifierP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Symbol(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::SyntaxP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(_) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::FreeIdentifierEq => {
      match args.length() {
        2 => {
          match (args[0], args[1]) {
            (Value::Datum(Datum::Symbol(a)), Value::Datum(Datum::Symbol(b))) => bool_value(a == b)
            _ => raise EvalError("type error: identifier expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::BoundIdentifierEq => {
      match args.length() {
        2 => {
          match (args[0], args[1]) {
            (Value::Datum(Datum::Symbol(a)), Value::Datum(Datum::Symbol(b))) => bool_value(a == b)
            _ => raise EvalError("type error: identifier expected")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::SymbolToString => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Symbol(name)) => Value::Datum(Datum::String(Ref::new(name)))
            _ => raise EvalError("type error: symbol expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringToSymbol => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::String(name)) => Value::Datum(Datum::Symbol(name.val))
            _ => raise EvalError("type error: string expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::SyntaxToDatum => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          Value::Datum(datum)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::DatumToSyntax => {
      match args.length() {
        2 => {
          let _ = value_as_datum(args[0])
          let datum = value_as_datum(args[1])
          Value::Datum(datum)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::BooleanP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Bool(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::NumberP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_))
            | Value::Datum(Datum::Complex(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::IntegerP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) | Value::Datum(Datum::BigInt(_)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(_) | Datum::BigInt(_) => bool_value(true)
                  _ => bool_value(false)
                }
              } else {
                bool_value(false)
              }
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RationalP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(_)
                  | Datum::BigInt(_)
                  | Datum::Rat(_, _)
                  | Datum::BigRat(_, _) => bool_value(true)
                  _ => bool_value(false)
                }
              } else {
                bool_value(false)
              }
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RealP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_)) => bool_value(true)
            Value::Datum(Datum::Complex(_, imag)) => bool_value(datum_is_zero(imag.val))
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ComplexP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _))
            | Value::Datum(Datum::Float(_))
            | Value::Datum(Datum::Complex(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ExactP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) => {
              match (real.val, imag.val) {
                (Datum::Int(_), Datum::Int(_)) => bool_value(true)
                (Datum::Int(_), Datum::BigInt(_)) => bool_value(true)
                (Datum::Int(_), Datum::Rat(_, _)) => bool_value(true)
                (Datum::Int(_), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::BigInt(_), Datum::Int(_)) => bool_value(true)
                (Datum::BigInt(_), Datum::BigInt(_)) => bool_value(true)
                (Datum::BigInt(_), Datum::Rat(_, _)) => bool_value(true)
                (Datum::BigInt(_), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::Rat(_, _), Datum::Int(_)) => bool_value(true)
                (Datum::Rat(_, _), Datum::BigInt(_)) => bool_value(true)
                (Datum::Rat(_, _), Datum::Rat(_, _)) => bool_value(true)
                (Datum::Rat(_, _), Datum::BigRat(_, _)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::Int(_)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::BigInt(_)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::Rat(_, _)) => bool_value(true)
                (Datum::BigRat(_, _), Datum::BigRat(_, _)) => bool_value(true)
                _ => bool_value(false)
              }
            }
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::InexactP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(_)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) => {
              match (real.val, imag.val) {
                (Datum::Float(_), _) => bool_value(true)
                (_, Datum::Float(_)) => bool_value(true)
                _ => bool_value(false)
              }
            }
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ZeroP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(n)) => bool_value(n == 0)
            Value::Datum(Datum::Rat(n, _)) => bool_value(n == 0)
            Value::Datum(Datum::BigInt(n)) => bool_value(n.is_zero())
            Value::Datum(Datum::BigRat(n, _)) => bool_value(n.is_zero())
            Value::Datum(Datum::Float(n)) => bool_value(n == 0.0)
            Value::Datum(Datum::Complex(real, imag)) =>
              bool_value(datum_is_zero(real.val) && datum_is_zero(imag.val))
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::PositiveP => {
      match args.length() {
        1 => {
          let value = datum_to_real(value_as_number(args[0]))
          match value {
            Datum::Int(n) => bool_value(n > 0)
            Datum::Rat(n, _) => bool_value(n > 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) > 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) > 0)
            Datum::Float(n) => bool_value(n > 0.0)
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::NegativeP => {
      match args.length() {
        1 => {
          let value = datum_to_real(value_as_number(args[0]))
          match value {
            Datum::Int(n) => bool_value(n < 0)
            Datum::Rat(n, _) => bool_value(n < 0)
            Datum::BigInt(n) => bool_value(n.compare_int(0) < 0)
            Datum::BigRat(n, _) => bool_value(n.compare_int(0) < 0)
            Datum::Float(n) => bool_value(n < 0.0)
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::OddP => {
      match args.length() {
        1 => {
          let value = match args[0] {
            Value::Datum(Datum::Int(n)) => Datum::Int(n)
            Value::Datum(Datum::BigInt(n)) => Datum::BigInt(n)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(n) => Datum::Int(n)
                  Datum::BigInt(n) => Datum::BigInt(n)
                  _ => raise EvalError("type error: exact integer expected")
                }
              } else {
                raise EvalError("type error: exact integer expected")
              }
            _ => raise EvalError("type error: exact integer expected")
          }
          match value {
            Datum::Int(n) => bool_value(n % 2 != 0)
            Datum::BigInt(n) => bool_value(!bigint_is_zero(n % bigint_from_int(2)))
            _ => raise EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EvenP => {
      match args.length() {
        1 => {
          let value = match args[0] {
            Value::Datum(Datum::Int(n)) => Datum::Int(n)
            Value::Datum(Datum::BigInt(n)) => Datum::BigInt(n)
            Value::Datum(Datum::Complex(real, imag)) =>
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Int(n) => Datum::Int(n)
                  Datum::BigInt(n) => Datum::BigInt(n)
                  _ => raise EvalError("type error: exact integer expected")
                }
              } else {
                raise EvalError("type error: exact integer expected")
              }
            _ => raise EvalError("type error: exact integer expected")
          }
          match value {
            Datum::Int(n) => bool_value(n % 2 == 0)
            Datum::BigInt(n) => bool_value(bigint_is_zero(n % bigint_from_int(2)))
            _ => raise EvalError("type error: exact integer expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::FiniteP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_finite(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(true)
            Value::Datum(Datum::Complex(real, imag)) => {
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_finite(f))
                  Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
                    bool_value(true)
                  _ => raise EvalError("type error: number expected")
                }
              } else {
                raise EvalError("type error: real expected")
              }
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::InfiniteP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_infinite(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(false)
            Value::Datum(Datum::Complex(real, imag)) => {
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_infinite(f))
                  Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
                    bool_value(false)
                  _ => raise EvalError("type error: number expected")
                }
              } else {
                raise EvalError("type error: real expected")
              }
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::NanP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(f)) => bool_value(float_is_nan(f))
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::BigInt(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::BigRat(_, _)) => bool_value(false)
            Value::Datum(Datum::Complex(real, imag)) => {
              if datum_is_zero(imag.val) {
                match real.val {
                  Datum::Float(f) => bool_value(float_is_nan(f))
                  Datum::Int(_) | Datum::BigInt(_) | Datum::Rat(_, _) | Datum::BigRat(_, _) =>
                    bool_value(false)
                  _ => raise EvalError("type error: number expected")
                }
              } else {
                raise EvalError("type error: real expected")
              }
            }
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ProcedureP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Primitive(_)
            | Value::Closure(_)
            | Value::CaseClosure(_)
            | Value::GuardHandler(_)
            | Value::Parameter(_)
            | Value::Continuation(_)
            | Value::RecordProc(_)
            | Value::ConditionProc(_)
            | Value::EnumSetProc(_) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Record(record) => bool_value(!record.record_type.is_opaque)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordRtd => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Record(record) =>
              if record.record_type.is_opaque {
                raise EvalError("record type is opaque")
              } else {
                match lookup_record_type_descriptor_by_id(record.record_type.id) {
                  Some(desc) => Value::RecordTypeDescriptor(desc)
                  None => raise EvalError("invalid record type descriptor")
                }
              }
            _ => raise EvalError("type error: record expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeDescriptorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::RecordTypeDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordConstructorDescriptorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::RecordConstructorDescriptor(_) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeName => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::Datum(Datum::Symbol(desc.record_type.name))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeParent => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.parent {
            Some(parent) => {
              match lookup_record_type_descriptor(parent.name) {
                Some(parent_desc) => Value::RecordTypeDescriptor(parent_desc)
                None => Value::Datum(Datum::Bool(false))
              }
            }
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeUid => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          match desc.record_type.uid {
            Some(uid) => Value::Datum(Datum::Symbol(uid))
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeGenerativeP => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.uid is None)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeSealedP => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_sealed)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeOpaqueP => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          bool_value(desc.record_type.is_opaque)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordTypeFieldNames => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          let items : Array[Datum] = []
          for field in desc.record_type.fields {
            items.push(Datum::Symbol(field.name))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordConstructorDescriptor => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordConstructorDescriptor(desc.constructor_desc)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordConstructor => {
      raise EvalError("record-constructor requires evaluation context")
    }
    Primitive::RecordTypeFieldMutableP => {
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          bool_value(field.mutable)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::RecordPredicate => {
      match args.length() {
        1 => {
          let desc = value_as_record_type_descriptor(args[0])
          Value::RecordProc(make_record_proc(RecordProcKind::Predicate(desc.record_type)))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RecordAccessor => {
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          Value::RecordProc(make_record_proc(RecordProcKind::Accessor(desc.record_type, idx)))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::RecordMutator => {
      match args.length() {
        2 => {
          let desc = value_as_record_type_descriptor(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, desc.record_type.fields.length())
          let field = desc.record_type.fields[idx]
          if !field.mutable {
            raise EvalError("record field is immutable: \{field.name}")
          }
          Value::RecordProc(make_record_proc(RecordProcKind::Mutator(desc.record_type, idx)))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::MakeRecordTypeDescriptor => {
      match args.length() {
        6 => {
          let name = value_as_symbol(args[0])
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) => None
            Value::RecordTypeDescriptor(desc) => Some(desc)
            _ => raise EvalError("type error: record type descriptor expected")
          }
          let parent_type = match parent_desc {
            Some(desc) => Some(desc.record_type)
            None => None
          }
          match parent_type {
            Some(parent) => {
              if parent.is_sealed {
                raise EvalError("record type is sealed")
              }
            }
            None => ()
          }
          let uid = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            Value::Datum(Datum::Symbol(uid_name)) => Some(uid_name)
            _ => raise EvalError("type error: symbol expected")
          }
          let is_sealed = value_as_bool(args[3])
          let is_opaque = value_as_bool(args[4])
          let new_fields = parse_field_specs(value_as_vector(args[5]))
          let fields : Array[RecordField] = []
          match parent_type {
            Some(parent) => {
              for field in parent.fields {
                fields.push(field)
              }
            }
            None => ()
          }
          for field in new_fields {
            fields.push(field)
          }
          let effective_opaque = match parent_type {
            Some(parent) => parent.is_opaque || is_opaque
            None => is_opaque
          }
          match uid {
            Some(uid_name) => {
              match lookup_record_type_descriptor_by_uid(uid_name) {
                Some(existing_desc) => {
                  if !record_type_matches(
                    existing_desc.record_type,
                    parent_type,
                    is_sealed,
                    is_opaque,
                    fields,
                  ) {
                    raise EvalError("invalid record type descriptor")
                  }
                  register_record_type_alias(name, existing_desc)
                  Value::RecordTypeDescriptor(existing_desc)
                }
                None => {
                  let record_type = make_record_type(
                    name,
                    parent_type,
                    is_sealed,
                    effective_opaque,
                    uid,
                    fields,
                  )
                  let ctor_desc = default_constructor_descriptor(record_type)
                  let rtd = make_record_type_descriptor(record_type, ctor_desc)
                  let _ = register_record_type(name, rtd)
                  Value::RecordTypeDescriptor(rtd)
                }
              }
            }
            None => {
              let record_type = make_record_type(
                name,
                parent_type,
                is_sealed,
                effective_opaque,
                uid,
                fields,
              )
              let ctor_desc = default_constructor_descriptor(record_type)
              let rtd = make_record_type_descriptor(record_type, ctor_desc)
              let _ = register_record_type(name, rtd)
              Value::RecordTypeDescriptor(rtd)
            }
          }
        }
        n => raise arity_mismatch(6, n)
      }
    }
    Primitive::MakeRecordConstructorDescriptor => {
      match args.length() {
        3 => {
          let rtd = value_as_record_type_descriptor(args[0])
          let record_type = rtd.record_type
          let parent_desc = match args[1] {
            Value::Datum(Datum::Bool(false)) => {
              match record_type.parent {
                Some(parent) => Some(default_constructor_descriptor(parent))
                None => None
              }
            }
            _ => Some(value_as_record_constructor_descriptor(args[1]))
          }
          match record_type.parent {
            Some(parent) =>
              match parent_desc {
                Some(desc) => {
                  if desc.record_type.id != parent.id {
                    raise EvalError("invalid record constructor descriptor")
                  }
                }
                None => raise EvalError("invalid record constructor descriptor")
              }
            None =>
              match parent_desc {
                Some(_) => raise EvalError("invalid record constructor descriptor")
                None => ()
              }
          }
          let protocol = match args[2] {
            Value::Datum(Datum::Bool(false)) => None
            value => {
              if !is_procedure_value(value) {
                raise EvalError("type error: procedure expected")
              }
              Some(value)
            }
          }
          match (protocol, parent_desc) {
            (None, Some(desc)) =>
              match desc.protocol {
                Some(_) => raise EvalError("invalid record constructor descriptor")
                None => ()
              }
            _ => ()
          }
          Value::RecordConstructorDescriptor(
            make_record_constructor_descriptor(record_type, parent_desc, protocol),
          )
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::Condition => {
      if args.length() == 0 {
        raise arity_mismatch(1, 0)
      }
      let base = condition_base_type()
      let components : Array[Record] = []
      for arg in args {
        let parts = condition_components(arg, base)
        for record in parts {
          components.push(record)
        }
      }
      if components.length() == 1 {
        Value::Record(components[0])
      } else {
        Value::Datum(Datum::Condition(make_condition(components)))
      }
    }
    Primitive::ConditionP => {
      match args.length() {
        1 => {
          let base = condition_base_type()
          let ok = condition_components_opt(args[0], base) is Some(_)
          bool_value(ok)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::SimpleConditions => {
      match args.length() {
        1 => {
          let base = condition_base_type()
          let components = condition_components(args[0], base)
          let items : Array[Datum] = []
          for record in components {
            items.push(Datum::Record(record))
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ConditionPredicate => {
      match args.length() {
        1 => {
          let desc = value_as_condition_type_descriptor(args[0])
          Value::ConditionProc(
            make_condition_proc(ConditionProcKind::Predicate(desc.record_type)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ConditionAccessor => {
      match args.length() {
        2 => {
          let desc = value_as_condition_type_descriptor(args[0])
          let (accessor_type, idx) = record_accessor_info(args[1])
          if accessor_type.id != desc.record_type.id {
            raise EvalError("type error: record accessor expected")
          }
          Value::ConditionProc(
            make_condition_proc(ConditionProcKind::Accessor(desc.record_type, idx)),
          )
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::MakeEqHashtable => {
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::MakeEqvHashtable => {
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::MakeHashtable => {
      match args.length() {
        2 | 3 => {
          let hash = args[0]
          if !is_procedure_value(hash) {
            raise EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(args[1])
          if args.length() == 3 {
            validate_hashtable_size_arg(args[2])
          }
          Value::Hashtable(make_hashtable(equiv, Some(hash), true))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::HashtableP => {
      match args.length() {
        1 =>
          match args[0] {
            Value::Hashtable(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableSize => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          Value::Datum(Datum::Int(table.entries.val.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableRef => {
      raise EvalError("internal error: hashtable-ref should be handled in apply_proc")
    }
    Primitive::HashtableSet => {
      raise EvalError("internal error: hashtable-set! should be handled in apply_proc")
    }
    Primitive::HashtableDelete => {
      raise EvalError("internal error: hashtable-delete! should be handled in apply_proc")
    }
    Primitive::HashtableContainsP => {
      raise EvalError("internal error: hashtable-contains? should be handled in apply_proc")
    }
    Primitive::HashtableUpdate => {
      raise EvalError("internal error: hashtable-update! should be handled in apply_proc")
    }
    Primitive::HashtableCopy => {
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          let mutable = if args.length() == 2 { value_as_bool(args[1]) } else { table.mutable }
          let entries : Array[HashtableEntry] = []
          for entry in table.entries.val {
            entries.push(HashtableEntry::{ key: entry.key, value: Ref::new(entry.value.val) })
          }
          Value::Hashtable(
            Hashtable::{
              id: next_hashtable_id(),
              mutable,
              equiv: table.equiv,
              hash: table.hash,
              entries: Ref::new(entries),
            },
          )
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableClear => {
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          if !table.mutable {
            raise EvalError("hashtable is immutable")
          }
          if args.length() == 2 {
            validate_hashtable_size_arg(args[1])
          }
          table.entries.val = []
          Value::Void
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableKeys => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let items : Array[Datum] = []
          for entry in table.entries.val {
            items.push(value_to_datum_element(entry.key))
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableEntries => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let key_items : Array[Datum] = []
          let value_items : Array[Datum] = []
          for entry in table.entries.val {
            key_items.push(value_to_datum_element(entry.key))
            value_items.push(value_to_datum_element(entry.value.val))
          }
          Value::Values([
            Value::Datum(Datum::Vector(key_items)),
            Value::Datum(Datum::Vector(value_items)),
          ])
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableEquivalenceFunction => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          hashtable_equiv_to_value(table.equiv)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableHashFunction => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          match table.hash {
            Some(hash) => hash
            None => Value::Datum(Datum::Bool(false))
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::HashtableMutableP => {
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          bool_value(table.mutable)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::MakeEnumeration => {
      match args.length() {
        1 => {
          let names = enum_set_symbol_list(args[0])
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          Value::EnumSet(make_enum_set(names, members))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetUniverse => {
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_true(set.universe)
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetIndexer => {
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Indexer(set)))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetConstructor => {
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Constructor(set)))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetP => {
      match args.length() {
        1 =>
          match args[0] {
            Value::EnumSet(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetMemberP => {
      match args.length() {
        2 => {
          let name = parse_symbol(value_as_datum(args[0]))
          let set = value_as_enum_set(args[1])
          bool_value(enum_set_member_by_name(set, name))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetSubsetP => {
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let mut i = 0
          while i < left.members.length() {
            if left.members[i] && !right.members[i] {
              return bool_value(false)
            }
            i = i + 1
          }
          bool_value(true)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetEq => {
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let mut i = 0
          while i < left.members.length() {
            if left.members[i] != right.members[i] {
              return bool_value(false)
            }
            i = i + 1
          }
          bool_value(true)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetUnion => {
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] || right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetIntersection => {
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] && right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetDifference => {
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = left.members[i] && !right.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetComplement => {
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_false(set.universe)
          let mut i = 0
          while i < members.length() {
            members[i] = !set.members[i]
            i = i + 1
          }
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::EnumSetProjection => {
      match args.length() {
        2 => {
          let source = value_as_enum_set(args[0])
          let target = value_as_enum_set(args[1])
          let members = enum_set_members_all_false(target.universe)
          let mut i = 0
          while i < target.universe.length() {
            let name = target.universe[i]
            members[i] = enum_set_member_by_name(source, name)
            i = i + 1
          }
          Value::EnumSet(make_enum_set(target.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::EnumSetToList => {
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let items : Array[Datum] = []
          let mut i = 0
          while i < set.universe.length() {
            if set.members[i] {
              items.push(Datum::Symbol(set.universe[i]))
            }
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharEq => bool_value(compare_chain_char(args, CompareMode::Eq, false))
    Primitive::CharLess => bool_value(compare_chain_char(args, CompareMode::Lt, false))
    Primitive::CharGreater => bool_value(compare_chain_char(args, CompareMode::Gt, false))
    Primitive::CharLessEq => bool_value(compare_chain_char(args, CompareMode::Le, false))
    Primitive::CharGreaterEq => bool_value(compare_chain_char(args, CompareMode::Ge, false))
    Primitive::CharCiEq => bool_value(compare_chain_char(args, CompareMode::Eq, true))
    Primitive::CharCiLess => bool_value(compare_chain_char(args, CompareMode::Lt, true))
    Primitive::CharCiGreater => bool_value(compare_chain_char(args, CompareMode::Gt, true))
    Primitive::CharCiLessEq => bool_value(compare_chain_char(args, CompareMode::Le, true))
    Primitive::CharCiGreaterEq => bool_value(compare_chain_char(args, CompareMode::Ge, true))
    Primitive::CharP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Char(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharToInteger => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Int(ch.to_int()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::IntegerToChar => {
      match args.length() {
        1 => {
          let ch = char_from_int_value(args[0])
          Value::Datum(Datum::Char(ch))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharAlphabeticP => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_ascii_alphabetic())
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharNumericP => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_numeric())
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharWhitespaceP => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_whitespace())
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharUpperCaseP => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_ascii_uppercase())
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharLowerCaseP => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          bool_value(ch.is_ascii_lowercase())
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharUpcase => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(ch.to_ascii_uppercase()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharDowncase => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(ch.to_ascii_lowercase()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharFoldcase => {
      match args.length() {
        1 => {
          let ch = value_as_char(args[0])
          Value::Datum(Datum::Char(ch.to_ascii_lowercase()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringEq => bool_value(compare_chain_string(args, CompareMode::Eq, false))
    Primitive::StringLess => bool_value(compare_chain_string(args, CompareMode::Lt, false))
    Primitive::StringGreater => bool_value(compare_chain_string(args, CompareMode::Gt, false))
    Primitive::StringLessEq => bool_value(compare_chain_string(args, CompareMode::Le, false))
    Primitive::StringGreaterEq => bool_value(compare_chain_string(args, CompareMode::Ge, false))
    Primitive::StringCiEq => bool_value(compare_chain_string(args, CompareMode::Eq, true))
    Primitive::StringCiLess => bool_value(compare_chain_string(args, CompareMode::Lt, true))
    Primitive::StringCiGreater => bool_value(compare_chain_string(args, CompareMode::Gt, true))
    Primitive::StringCiLessEq => bool_value(compare_chain_string(args, CompareMode::Le, true))
    Primitive::StringCiGreaterEq => bool_value(compare_chain_string(args, CompareMode::Ge, true))
    Primitive::String => {
      let buf = @buffer.new()
      for arg in args {
        let ch = value_as_char(arg)
        buf.write_char(ch)
      }
      Value::Datum(Datum::String(Ref::new(buf.to_string())))
    }
    Primitive::MakeString => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill =
            if args.length() == 2 {
              value_as_char(args[1])
            } else {
              let zero = 0
              match zero.to_char() {
                Some(ch) => ch
                None => raise EvalError("internal error: invalid default char")
              }
            }
          let buf = @buffer.new()
          let mut i = 0
          while i < len {
            buf.write_char(fill)
            i = i + 1
          }
          Value::Datum(Datum::String(Ref::new(buf.to_string())))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::StringP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::String(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringLength => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::Int(s.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringAppend => {
      let buf = @buffer.new()
      for arg in args {
        buf.write_string(value_as_string(arg))
      }
      Value::Datum(Datum::String(Ref::new(buf.to_string())))
    }
    Primitive::StringRef => {
      match args.length() {
        2 => {
          let s = value_as_string(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, s.length())
          match s.get_char(idx) {
            Some(ch) => Value::Datum(Datum::Char(ch))
            None => raise EvalError("index out of range")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::StringSet => {
      match args.length() {
        3 => {
          let sref = value_as_string_ref(args[0])
          let idx = value_as_int_index(args[1])
          let ch = value_as_char(args[2])
          let updated = string_replace_range(sref.val, idx, idx + 1, ch)
          sref.val = updated
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::StringCopy => {
      match args.length() {
        1 | 2 | 3 => {
          let s = value_as_string(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            s.length()
          }
          if start < 0 || end < start || end > s.length() {
            raise EvalError("index out of range")
          }
          let view = try? s[start:end]
          match view {
            Ok(slice) => Value::Datum(Datum::String(Ref::new(slice.to_string())))
            Err(_) => raise EvalError("index out of range")
          }
        }
        n => raise EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    }
    Primitive::Substring => {
      match args.length() {
        3 => {
          let s = value_as_string(args[0])
          let start = value_as_int_index(args[1])
          let end = value_as_int_index(args[2])
          if start < 0 || end < start || end > s.length() {
            raise EvalError("index out of range")
          }
          let view = try? s[start:end]
          match view {
            Ok(slice) => Value::Datum(Datum::String(Ref::new(slice.to_string())))
            Err(_) => raise EvalError("index out of range")
          }
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::StringCopyBang => {
      match args.length() {
        3 | 4 | 5 => {
          let to_ref = value_as_string_ref(args[0])
          let at = value_as_int_index(args[1])
          let from_str = value_as_string(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_str.length()
          }
          if start < 0 || end < start || end > from_str.length() || at < 0 {
            raise EvalError("index out of range")
          }
          let count = end - start
          let to_str = to_ref.val
          let to_len = to_str.length()
          if at + count > to_len {
            raise EvalError("index out of range")
          }
          let buf = @buffer.new()
          let mut i = 0
          while i < to_len {
            match to_str.get_char(i) {
              Some(ch) => {
                if i >= at && i < at + count {
                  let from_idx = start + (i - at)
                  match from_str.get_char(from_idx) {
                    Some(fch) => buf.write_char(fch)
                    None => raise EvalError("index out of range")
                  }
                } else {
                  buf.write_char(ch)
                }
              }
              None => raise EvalError("index out of range")
            }
            i = i + 1
          }
          to_ref.val = buf.to_string()
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    }
    Primitive::StringFill => {
      match args.length() {
        2 | 3 | 4 => {
          let sref = value_as_string_ref(args[0])
          let fill = value_as_char(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            sref.val.length()
          }
          let updated = string_replace_range(sref.val, start, end, fill)
          sref.val = updated
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    }
    Primitive::StringToList => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          let items : Array[Datum] = []
          let mut i = 0
          while i < s.length() {
            match s.get_char(i) {
              Some(ch) => items.push(Datum::Char(ch))
              None => raise EvalError("index out of range")
            }
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListToString => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let buf = @buffer.new()
          for item in items {
            match item {
              Datum::Char(ch) => buf.write_char(ch)
              _ => raise EvalError("type error: char expected")
            }
          }
          Value::Datum(Datum::String(Ref::new(buf.to_string())))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringUpcase => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(s.to_upper())))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringDowncase => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(s.to_lower())))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringFoldcase => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::String(Ref::new(s.to_lower())))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Vector => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_to_datum_element(arg))
      }
      Value::Datum(Datum::Vector(items))
    }
    Primitive::MakeVector => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_to_datum_element(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::VectorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Vector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::VectorLength => {
      match args.length() {
        1 => {
          let items = value_as_vector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::VectorRef => {
      match args.length() {
        2 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          value_from_datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::VectorSet => {
      match args.length() {
        3 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_to_datum_element(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::VectorFill => {
      match args.length() {
        2 | 3 | 4 => {
          let items = value_as_vector(args[0])
          let fill = value_to_datum_element(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise EvalError("index out of range")
          }
          let mut i = start
          while i < end {
            items[i] = fill
            i = i + 1
          }
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    }
    Primitive::VectorCopy => {
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_vector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise EvalError("index out of range")
          }
          let slice = items.sub(start=start, end=end).to_array()
          Value::Datum(Datum::Vector(slice))
        }
        n => raise EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    }
    Primitive::VectorCopyBang => {
      match args.length() {
        3 | 4 | 5 => {
          let to_items = value_as_vector(args[0])
          let at = value_as_int_index(args[1])
          let from_items = value_as_vector(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_items.length()
          }
          if start < 0 || end < start || end > from_items.length() || at < 0 {
            raise EvalError("index out of range")
          }
          let count = end - start
          if at + count > to_items.length() {
            raise EvalError("index out of range")
          }
          let temp : Array[Datum] = []
          let mut i = 0
          while i < count {
            temp.push(from_items[start + i])
            i = i + 1
          }
          let mut j = 0
          while j < count {
            to_items[at + j] = temp[j]
            j = j + 1
          }
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    }
    Primitive::VectorAppend => {
      let items : Array[Datum] = []
      for arg in args {
        let vec = value_as_vector(arg)
        for item in vec {
          items.push(item)
        }
      }
      Value::Datum(Datum::Vector(items))
    }
    Primitive::VectorToList => {
      match args.length() {
        1 => {
          let items = value_as_vector(args[0])
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListToVector => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVector => {
      let items : Array[Int] = []
      for arg in args {
        items.push(value_as_byte(arg))
      }
      Value::Datum(Datum::ByteVector(items))
    }
    Primitive::MakeByteVector => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 { value_as_byte(args[1]) } else { 0 }
          let items : Array[Int] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(Datum::ByteVector(items))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::ByteVectorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::ByteVector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVectorLength => {
      match args.length() {
        1 => {
          let items = value_as_bytevector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVectorU8Ref => {
      match args.length() {
        2 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          Value::Datum(Datum::Int(items[idx]))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::ByteVectorU8Set => {
      match args.length() {
        3 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_as_byte(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::ByteVectorCopy => {
      match args.length() {
        1 | 2 | 3 => {
          let items = value_as_bytevector(args[0])
          let start = if args.length() >= 2 {
            value_as_int_index(args[1])
          } else {
            0
          }
          let end = if args.length() == 3 {
            value_as_int_index(args[2])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise EvalError("index out of range")
          }
          let slice = items.sub(start=start, end=end).to_array()
          Value::Datum(Datum::ByteVector(slice))
        }
        n => raise EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    }
    Primitive::ByteVectorCopyBang => {
      match args.length() {
        3 | 4 | 5 => {
          let to_items = value_as_bytevector(args[0])
          let at = value_as_int_index(args[1])
          let from_items = value_as_bytevector(args[2])
          let start = if args.length() >= 4 {
            value_as_int_index(args[3])
          } else {
            0
          }
          let end = if args.length() == 5 {
            value_as_int_index(args[4])
          } else {
            from_items.length()
          }
          if start < 0 || end < start || end > from_items.length() || at < 0 {
            raise EvalError("index out of range")
          }
          let count = end - start
          if at + count > to_items.length() {
            raise EvalError("index out of range")
          }
          let temp : Array[Int] = []
          let mut i = 0
          while i < count {
            temp.push(from_items[start + i])
            i = i + 1
          }
          let mut j = 0
          while j < count {
            to_items[at + j] = temp[j]
            j = j + 1
          }
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    }
    Primitive::ByteVectorAppend => {
      let items : Array[Int] = []
      for arg in args {
        let vec = value_as_bytevector(arg)
        for item in vec {
          items.push(item)
        }
      }
      Value::Datum(Datum::ByteVector(items))
    }
    Primitive::ByteVectorFill => {
      match args.length() {
        2 | 3 | 4 => {
          let items = value_as_bytevector(args[0])
          let fill = value_as_byte(args[1])
          let start = if args.length() >= 3 {
            value_as_int_index(args[2])
          } else {
            0
          }
          let end = if args.length() == 4 {
            value_as_int_index(args[3])
          } else {
            items.length()
          }
          if start < 0 || end < start || end > items.length() {
            raise EvalError("index out of range")
          }
          let mut i = start
          while i < end {
            items[i] = fill
            i = i + 1
          }
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    }
    Primitive::ByteVectorToU8List => {
      match args.length() {
        1 => {
          let items = value_as_bytevector(args[0])
          let datums : Array[Datum] = []
          for byte in items {
            datums.push(Datum::Int(byte))
          }
          Value::Datum(list_from_array(datums))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::U8ListToByteVector => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let bytes : Array[Int] = []
          for item in items {
            match item {
              Datum::Int(n) => {
                if n < 0 || n > 255 {
                  raise EvalError("type error: byte expected")
                }
                bytes.push(n)
              }
              _ => raise EvalError("type error: byte expected")
            }
          }
          Value::Datum(Datum::ByteVector(bytes))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Display => {
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, display_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, display_string(args[0]))
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::Write => {
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::Newline => {
      match args.length() {
        0 => {
          let port = get_current_output_port()
          port_write(port, "\n")
          Value::Void
        }
        1 => {
          let port = value_as_port(args[0])
          port_write(port, "\n")
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 0 or 1 got \{n}")
      }
    }
    Primitive::OpenOutputString => {
      match args.length() {
        0 => Value::Port(new_output_string_port())
        n => raise arity_mismatch(0, n)
      }
    }
    Primitive::GetOutputString => {
      match args.length() {
        1 => {
          let port = value_as_port(args[0])
          Value::Datum(Datum::String(Ref::new(port_get_output_string(port))))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CurrentOutputPort => {
      match args.length() {
        0 => Value::Port(get_current_output_port())
        n => raise arity_mismatch(0, n)
      }
    }
    Primitive::WithExceptionHandler => {
      raise EvalError("internal error: with-exception-handler should be handled in apply_proc")
    }
    Primitive::Raise => {
      raise EvalError("internal error: raise should be handled in apply_proc")
    }
    Primitive::RaiseContinuable => {
      raise EvalError("internal error: raise-continuable should be handled in apply_proc")
    }
    Primitive::Error => {
      raise EvalError("internal error: error should be handled in apply_proc")
    }
    Primitive::AssertionViolation => {
      raise EvalError("internal error: assertion-violation should be handled in apply_proc")
    }
    Primitive::ImplementationRestrictionViolation => {
      raise EvalError(
        "internal error: implementation-restriction-violation should be handled in apply_proc",
      )
    }
    Primitive::UndefinedViolation => {
      raise EvalError("internal error: undefined-violation should be handled in apply_proc")
    }
    Primitive::SyntaxViolation => {
      raise EvalError("internal error: syntax-violation should be handled in apply_proc")
    }
    Primitive::Not => {
      match args.length() {
        1 => bool_value(is_false(args[0]))
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Apply => {
      // handled by apply_proc to allow proper argument flattening
      raise EvalError("internal error: apply should be handled in apply_proc")
    }
    Primitive::CallCC => {
      raise EvalError("internal error: call/cc should be handled in apply_proc")
    }
    Primitive::Map => {
      raise EvalError("internal error: map should be handled in apply_proc")
    }
    Primitive::ForEach => {
      raise EvalError("internal error: for-each should be handled in apply_proc")
    }
    Primitive::VectorMap => {
      raise EvalError("internal error: vector-map should be handled in apply_proc")
    }
    Primitive::VectorForEach => {
      raise EvalError("internal error: vector-for-each should be handled in apply_proc")
    }
    Primitive::StringMap => {
      raise EvalError("internal error: string-map should be handled in apply_proc")
    }
    Primitive::StringForEach => {
      raise EvalError("internal error: string-for-each should be handled in apply_proc")
    }
    Primitive::Values => Value::Values(args)
    Primitive::CallWithValues => {
      raise EvalError("internal error: call-with-values should be handled in apply_proc")
    }
    Primitive::MakeParameter => {
      raise EvalError("internal error: make-parameter should be handled in apply_proc")
    }
    Primitive::DynamicWind => {
      raise EvalError("internal error: dynamic-wind should be handled in apply_proc")
    }
    Primitive::Eval => {
      raise EvalError("internal error: eval should be handled in apply_proc")
    }
    Primitive::Environment => {
      raise EvalError("internal error: environment should be handled in apply_proc")
    }
    Primitive::PromiseP => {
      match args.length() {
        1 =>
          match args[0] {
            Value::Promise(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::MakePromise => {
      raise EvalError("internal error: make-promise should be handled in apply_proc")
    }
    Primitive::Force => {
      raise EvalError("internal error: force should be handled in apply_proc")
    }
    Primitive::ListP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(d) => bool_value(is_proper_list(d))
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Length => {
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Append => {
      match args.length() {
        0 => Value::Datum(Datum::Nil)
        1 => args[0]
        _ => {
          let items : Array[Datum] = []
          let mut i = 0
          while i < args.length() - 1 {
            let datum = match args[i] {
              Value::Datum(d) => d
              _ => raise EvalError("type error: list expected")
            }
            let part = datum_list_to_array(datum)
            for item in part {
              items.push(item)
            }
            i = i + 1
          }
          let tail = value_to_datum_element(args[args.length() - 1])
          Value::Datum(list_with_tail(items, tail))
        }
      }
    }
    Primitive::Reverse => {
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          let mut result = Datum::Nil
          for item in items {
            result = pair_new(item, result)
          }
          Value::Datum(result)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListRef => {
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          check_index(idx, items.length())
          value_from_datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::ListTail => {
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          if idx < 0 || idx > items.length() {
            raise EvalError("index out of range")
          }
          Value::Datum(list_from_array(items.sub(start=idx).to_array()))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::ListCopy => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Member => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Equal, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Memq => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Memv => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_member(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Assoc => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Equal, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Assq => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Assv => {
      match args.length() {
        2 => {
          let list = value_as_datum(args[1])
          list_assoc(EqualityMode::Eqv, args[0], list)
        }
        n => raise arity_mismatch(2, n)
      }
    }
  }
}

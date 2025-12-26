///|
fn init_env() -> Env {
  let env = env_new()
  let condition_rtd = match lookup_record_type_descriptor("&condition") {
    Some(desc) => desc
    None => {
      let condition_type = make_record_type("&condition", None, false, false, None, [])
      let condition_ctor_desc = make_record_constructor_descriptor(condition_type, None, None)
      let condition_rtd = make_record_type_descriptor(condition_type, condition_ctor_desc)
      let _ = register_record_type("&condition", condition_rtd)
      condition_rtd
    }
  }
  env_define(env, "&condition", Value::RecordTypeDescriptor(condition_rtd))
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
  env_define(
    env,
    "make-variable-transformer",
    Value::Primitive(Primitive::MakeVariableTransformer),
  )
  env_define(
    env,
    "generate-temporaries",
    Value::Primitive(Primitive::GenerateTemporaries),
  )
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
  env_define(env, "condition", Value::Primitive(Primitive::Condition))
  env_define(env, "condition?", Value::Primitive(Primitive::ConditionP))
  env_define(env, "simple-conditions", Value::Primitive(Primitive::SimpleConditions))
  env_define(env, "condition-predicate", Value::Primitive(Primitive::ConditionPredicate))
  env_define(env, "condition-accessor", Value::Primitive(Primitive::ConditionAccessor))
  env_define(env, "make-eq-hashtable", Value::Primitive(Primitive::MakeEqHashtable))
  env_define(env, "make-eqv-hashtable", Value::Primitive(Primitive::MakeEqvHashtable))
  env_define(env, "make-hashtable", Value::Primitive(Primitive::MakeHashtable))
  env_define(env, "hashtable?", Value::Primitive(Primitive::HashtableP))
  env_define(env, "hashtable-size", Value::Primitive(Primitive::HashtableSize))
  env_define(env, "hashtable-ref", Value::Primitive(Primitive::HashtableRef))
  env_define(env, "hashtable-set!", Value::Primitive(Primitive::HashtableSet))
  env_define(env, "hashtable-delete!", Value::Primitive(Primitive::HashtableDelete))
  env_define(env, "hashtable-contains?", Value::Primitive(Primitive::HashtableContainsP))
  env_define(env, "hashtable-update!", Value::Primitive(Primitive::HashtableUpdate))
  env_define(env, "hashtable-copy", Value::Primitive(Primitive::HashtableCopy))
  env_define(env, "hashtable-clear!", Value::Primitive(Primitive::HashtableClear))
  env_define(env, "hashtable-keys", Value::Primitive(Primitive::HashtableKeys))
  env_define(env, "hashtable-entries", Value::Primitive(Primitive::HashtableEntries))
  env_define(
    env,
    "hashtable-equivalence-function",
    Value::Primitive(Primitive::HashtableEquivalenceFunction),
  )
  env_define(
    env,
    "hashtable-hash-function",
    Value::Primitive(Primitive::HashtableHashFunction),
  )
  env_define(env, "hashtable-mutable?", Value::Primitive(Primitive::HashtableMutableP))
  env_define(env, "make-enumeration", Value::Primitive(Primitive::MakeEnumeration))
  env_define(env, "enum-set-universe", Value::Primitive(Primitive::EnumSetUniverse))
  env_define(env, "enum-set-indexer", Value::Primitive(Primitive::EnumSetIndexer))
  env_define(env, "enum-set-constructor", Value::Primitive(Primitive::EnumSetConstructor))
  env_define(env, "enum-set?", Value::Primitive(Primitive::EnumSetP))
  env_define(env, "enum-set-member?", Value::Primitive(Primitive::EnumSetMemberP))
  env_define(env, "enum-set-subset?", Value::Primitive(Primitive::EnumSetSubsetP))
  env_define(env, "enum-set=?", Value::Primitive(Primitive::EnumSetEq))
  env_define(env, "enum-set-union", Value::Primitive(Primitive::EnumSetUnion))
  env_define(env, "enum-set-intersection", Value::Primitive(Primitive::EnumSetIntersection))
  env_define(env, "enum-set-difference", Value::Primitive(Primitive::EnumSetDifference))
  env_define(env, "enum-set-complement", Value::Primitive(Primitive::EnumSetComplement))
  env_define(env, "enum-set-projection", Value::Primitive(Primitive::EnumSetProjection))
  env_define(env, "enum-set->list", Value::Primitive(Primitive::EnumSetToList))
  env_define(env, "not", Value::Primitive(Primitive::Not))
  env_define(env, "apply", Value::Primitive(Primitive::Apply))
  env_define(env, "values", Value::Primitive(Primitive::Values))
  env_define(env, "call-with-values", Value::Primitive(Primitive::CallWithValues))
  env_define(env, "make-parameter", Value::Primitive(Primitive::MakeParameter))
  env_define(env, "dynamic-wind", Value::Primitive(Primitive::DynamicWind))
  env_define(env, "promise?", Value::Primitive(Primitive::PromiseP))
  env_define(env, "make-promise", Value::Primitive(Primitive::MakePromise))
  env_define(env, "force", Value::Primitive(Primitive::Force))
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
  env_define(env, "bitwise-and", Value::Primitive(Primitive::BitwiseAnd))
  env_define(env, "bitwise-ior", Value::Primitive(Primitive::BitwiseIor))
  env_define(env, "bitwise-xor", Value::Primitive(Primitive::BitwiseXor))
  env_define(env, "bitwise-not", Value::Primitive(Primitive::BitwiseNot))
  env_define(env, "bitwise-if", Value::Primitive(Primitive::BitwiseIf))
  env_define(env, "arithmetic-shift", Value::Primitive(Primitive::ArithmeticShift))
  env_define(env, "bitwise-bit-count", Value::Primitive(Primitive::BitwiseBitCount))
  env_define(env, "bitwise-length", Value::Primitive(Primitive::BitwiseLength))
  env_define(env, "bitwise-first-bit-set", Value::Primitive(Primitive::BitwiseFirstBitSet))
  env_define(env, "bitwise-bit-set?", Value::Primitive(Primitive::BitwiseBitSetP))
  env_define(env, "bitwise-copy-bit", Value::Primitive(Primitive::BitwiseCopyBit))
  env_define(env, "bitwise-bit-field", Value::Primitive(Primitive::BitwiseBitField))
  env_define(env, "bitwise-copy-bit-field", Value::Primitive(Primitive::BitwiseCopyBitField))
  env_define(env, "bitwise-rotate-bit-field", Value::Primitive(Primitive::BitwiseRotateBitField))
  env_define(env, "bitwise-reverse-bit-field", Value::Primitive(Primitive::BitwiseReverseBitField))
  env_define(env, "fixnum?", Value::Primitive(Primitive::FixnumP))
  env_define(env, "fixnum-width", Value::Primitive(Primitive::FixnumWidth))
  env_define(env, "least-fixnum", Value::Primitive(Primitive::LeastFixnum))
  env_define(env, "greatest-fixnum", Value::Primitive(Primitive::GreatestFixnum))
  env_define(env, "fx=?", Value::Primitive(Primitive::FxEq))
  env_define(env, "fx<?", Value::Primitive(Primitive::FxLess))
  env_define(env, "fx>?", Value::Primitive(Primitive::FxGreater))
  env_define(env, "fx<=?", Value::Primitive(Primitive::FxLessEq))
  env_define(env, "fx>=?", Value::Primitive(Primitive::FxGreaterEq))
  env_define(env, "fxzero?", Value::Primitive(Primitive::FxZeroP))
  env_define(env, "fxpositive?", Value::Primitive(Primitive::FxPositiveP))
  env_define(env, "fxnegative?", Value::Primitive(Primitive::FxNegativeP))
  env_define(env, "fxodd?", Value::Primitive(Primitive::FxOddP))
  env_define(env, "fxeven?", Value::Primitive(Primitive::FxEvenP))
  env_define(env, "fxmin", Value::Primitive(Primitive::FxMin))
  env_define(env, "fxmax", Value::Primitive(Primitive::FxMax))
  env_define(env, "fx+", Value::Primitive(Primitive::FxAdd))
  env_define(env, "fx-", Value::Primitive(Primitive::FxSub))
  env_define(env, "fx*", Value::Primitive(Primitive::FxMul))
  env_define(env, "fxdiv", Value::Primitive(Primitive::FxDiv))
  env_define(env, "fxmod", Value::Primitive(Primitive::FxMod))
  env_define(env, "fxdiv0", Value::Primitive(Primitive::FxDiv0))
  env_define(env, "fxmod0", Value::Primitive(Primitive::FxMod0))
  env_define(env, "fx+/carry", Value::Primitive(Primitive::FxAddCarry))
  env_define(env, "fx-/carry", Value::Primitive(Primitive::FxSubCarry))
  env_define(env, "fx*/carry", Value::Primitive(Primitive::FxMulCarry))
  env_define(env, "fxnot", Value::Primitive(Primitive::FxNot))
  env_define(env, "fxand", Value::Primitive(Primitive::FxAnd))
  env_define(env, "fxior", Value::Primitive(Primitive::FxIor))
  env_define(env, "fxxor", Value::Primitive(Primitive::FxXor))
  env_define(env, "fxif", Value::Primitive(Primitive::FxIf))
  env_define(env, "fxbit-count", Value::Primitive(Primitive::FxBitCount))
  env_define(env, "fxlength", Value::Primitive(Primitive::FxLength))
  env_define(env, "fxfirst-bit-set", Value::Primitive(Primitive::FxFirstBitSet))
  env_define(env, "fxbit-set?", Value::Primitive(Primitive::FxBitSetP))
  env_define(env, "fxcopy-bit", Value::Primitive(Primitive::FxCopyBit))
  env_define(env, "fxbit-field", Value::Primitive(Primitive::FxBitField))
  env_define(env, "fxcopy-bit-field", Value::Primitive(Primitive::FxCopyBitField))
  env_define(env, "fxrotate-bit-field", Value::Primitive(Primitive::FxRotateBitField))
  env_define(env, "fxreverse-bit-field", Value::Primitive(Primitive::FxReverseBitField))
  env_define(env, "fxarithmetic-shift", Value::Primitive(Primitive::FxArithmeticShift))
  env_define(env, "fxarithmetic-shift-left", Value::Primitive(Primitive::FxArithmeticShiftLeft))
  env_define(env, "fxarithmetic-shift-right", Value::Primitive(Primitive::FxArithmeticShiftRight))
  env_define(env, "flonum?", Value::Primitive(Primitive::FlonumP))
  env_define(env, "real->flonum", Value::Primitive(Primitive::RealToFlonum))
  env_define(env, "fixnum->flonum", Value::Primitive(Primitive::FixnumToFlonum))
  env_define(env, "fl=?", Value::Primitive(Primitive::FlEq))
  env_define(env, "fl<?", Value::Primitive(Primitive::FlLess))
  env_define(env, "fl>?", Value::Primitive(Primitive::FlGreater))
  env_define(env, "fl<=?", Value::Primitive(Primitive::FlLessEq))
  env_define(env, "fl>=?", Value::Primitive(Primitive::FlGreaterEq))
  env_define(env, "flinteger?", Value::Primitive(Primitive::FlIntegerP))
  env_define(env, "flzero?", Value::Primitive(Primitive::FlZeroP))
  env_define(env, "flpositive?", Value::Primitive(Primitive::FlPositiveP))
  env_define(env, "flnegative?", Value::Primitive(Primitive::FlNegativeP))
  env_define(env, "flodd?", Value::Primitive(Primitive::FlOddP))
  env_define(env, "fleven?", Value::Primitive(Primitive::FlEvenP))
  env_define(env, "flfinite?", Value::Primitive(Primitive::FlFiniteP))
  env_define(env, "flinfinite?", Value::Primitive(Primitive::FlInfiniteP))
  env_define(env, "flnan?", Value::Primitive(Primitive::FlNanP))
  env_define(env, "flmax", Value::Primitive(Primitive::FlMax))
  env_define(env, "flmin", Value::Primitive(Primitive::FlMin))
  env_define(env, "fl+", Value::Primitive(Primitive::FlAdd))
  env_define(env, "fl*", Value::Primitive(Primitive::FlMul))
  env_define(env, "fl-", Value::Primitive(Primitive::FlSub))
  env_define(env, "fl/", Value::Primitive(Primitive::FlDiv))
  env_define(env, "flabs", Value::Primitive(Primitive::FlAbs))
  env_define(env, "fldiv-and-mod", Value::Primitive(Primitive::FlDivAndMod))
  env_define(env, "fldiv", Value::Primitive(Primitive::FlDivInt))
  env_define(env, "flmod", Value::Primitive(Primitive::FlMod))
  env_define(env, "fldiv0-and-mod0", Value::Primitive(Primitive::FlDiv0AndMod0))
  env_define(env, "fldiv0", Value::Primitive(Primitive::FlDiv0))
  env_define(env, "flmod0", Value::Primitive(Primitive::FlMod0))
  env_define(env, "flnumerator", Value::Primitive(Primitive::FlNumerator))
  env_define(env, "fldenominator", Value::Primitive(Primitive::FlDenominator))
  env_define(env, "flfloor", Value::Primitive(Primitive::FlFloor))
  env_define(env, "flceiling", Value::Primitive(Primitive::FlCeiling))
  env_define(env, "fltruncate", Value::Primitive(Primitive::FlTruncate))
  env_define(env, "flround", Value::Primitive(Primitive::FlRound))
  env_define(env, "flexp", Value::Primitive(Primitive::FlExp))
  env_define(env, "fllog", Value::Primitive(Primitive::FlLog))
  env_define(env, "flsin", Value::Primitive(Primitive::FlSin))
  env_define(env, "flcos", Value::Primitive(Primitive::FlCos))
  env_define(env, "fltan", Value::Primitive(Primitive::FlTan))
  env_define(env, "flasin", Value::Primitive(Primitive::FlAsin))
  env_define(env, "flacos", Value::Primitive(Primitive::FlAcos))
  env_define(env, "flatan", Value::Primitive(Primitive::FlAtan))
  env_define(env, "flsqrt", Value::Primitive(Primitive::FlSqrt))
  env_define(env, "flexpt", Value::Primitive(Primitive::FlExpt))
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
  env_define(env, "error", Value::Primitive(Primitive::Error))
  env_define(env, "assertion-violation", Value::Primitive(Primitive::AssertionViolation))
  env_define(
    env,
    "implementation-restriction-violation",
    Value::Primitive(Primitive::ImplementationRestrictionViolation),
  )
  env_define(env, "undefined-violation", Value::Primitive(Primitive::UndefinedViolation))
  env_define(env, "syntax-violation", Value::Primitive(Primitive::SyntaxViolation))
  env_define(env, "call/cc", Value::Primitive(Primitive::CallCC))
  env_define(
    env,
    "call-with-current-continuation",
    Value::Primitive(Primitive::CallCC),
  )
  env_define(env, "eval", Value::Primitive(Primitive::Eval))
  env_define(env, "environment", Value::Primitive(Primitive::Environment))
  env
}

///|
fn env_export_map(env : Env) -> Map[String, Value] {
  let exports : Map[String, Value] = {}
  let mut i = 0
  while i < env.length() {
    let frame = env[i]
    for key in frame.keys() {
      match frame.get(key) {
        Some(value) => exports[key] = value
        None => ()
      }
    }
    i = i + 1
  }
  exports
}

///|
fn register_builtin_libraries(env : Env) -> Unit raise EvalError {
  let exports = env_export_map(env)
  for name in base_syntax_keywords() {
    exports[name] = Value::SyntaxKeyword(name)
  }
  register_library_entry("rnrs.base", Some([6]), exports)
  let eval_exports : Map[String, Value] = {}
  eval_exports["eval"] = Value::Primitive(Primitive::Eval)
  eval_exports["environment"] = Value::Primitive(Primitive::Environment)
  register_library_entry("rnrs.eval", Some([6]), eval_exports)
  let condition_exports : Map[String, Value] = {}
  let condition_names : Array[String] = [
    "condition",
    "condition?",
    "simple-conditions",
    "condition-predicate",
    "condition-accessor",
    "&condition",
    "&message",
    "make-message-condition",
    "message-condition?",
    "condition-message",
    "&warning",
    "make-warning",
    "warning?",
    "&serious",
    "make-serious-condition",
    "serious-condition?",
    "&error",
    "make-error",
    "error?",
    "&violation",
    "make-violation",
    "violation?",
    "&assertion",
    "make-assertion-violation",
    "assertion-violation?",
    "&irritants",
    "make-irritants-condition",
    "irritants-condition?",
    "condition-irritants",
    "&who",
    "make-who-condition",
    "who-condition?",
    "condition-who",
    "&non-continuable",
    "make-non-continuable-violation",
    "non-continuable-violation?",
    "&implementation-restriction",
    "make-implementation-restriction-violation",
    "implementation-restriction-violation?",
    "&lexical",
    "make-lexical-violation",
    "lexical-violation?",
    "&syntax",
    "make-syntax-violation",
    "syntax-violation?",
    "syntax-violation-form",
    "syntax-violation-subform",
    "&undefined",
    "make-undefined-violation",
    "undefined-violation?",
  ]
  for name in condition_names {
    condition_exports[name] = env_lookup(env, name)
  }
  condition_exports["define-condition-type"] = Value::SyntaxKeyword("define-condition-type")
  register_library_entry("rnrs.conditions", Some([6]), condition_exports)
  let flonum_exports : Map[String, Value] = {}
  let flonum_names : Array[String] = [
    "flonum?",
    "real->flonum",
    "fixnum->flonum",
    "fl=?",
    "fl<?",
    "fl>?",
    "fl<=?",
    "fl>=?",
    "flinteger?",
    "flzero?",
    "flpositive?",
    "flnegative?",
    "flodd?",
    "fleven?",
    "flfinite?",
    "flinfinite?",
    "flnan?",
    "flmax",
    "flmin",
    "fl+",
    "fl*",
    "fl-",
    "fl/",
    "flabs",
    "fldiv-and-mod",
    "fldiv",
    "flmod",
    "fldiv0-and-mod0",
    "fldiv0",
    "flmod0",
    "flnumerator",
    "fldenominator",
    "flfloor",
    "flceiling",
    "fltruncate",
    "flround",
    "flexp",
    "fllog",
    "flsin",
    "flcos",
    "fltan",
    "flasin",
    "flacos",
    "flatan",
    "flsqrt",
    "flexpt",
    "&no-infinities",
    "make-no-infinities-violation",
    "no-infinities-violation?",
    "&no-nans",
    "make-no-nans-violation",
    "no-nans-violation?",
  ]
  for name in flonum_names {
    flonum_exports[name] = env_lookup(env, name)
  }
  register_library_entry("rnrs.arithmetic.flonums", Some([6]), flonum_exports)
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
fn make_closure(
  params : Array[String],
  rest : String?,
  body : Array[Datum],
  env : Env,
) -> Closure {
  { id: next_closure_id(), params, rest, body, env }
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
    _ => raise @core.EvalError("type error: parameter expected")
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
let promise_counter : Ref[Int] = Ref::new(0)

///|
fn next_promise_id() -> Int {
  let id = promise_counter.val
  promise_counter.val = id + 1
  id
}

///|
fn make_promise(thunk : Value) -> Promise {
  { id: next_promise_id(), state: Ref::new(PromiseState::Thunk(thunk)) }
}

///|
let eval_env_counter : Ref[Int] = Ref::new(0)

///|
fn next_eval_env_id() -> Int {
  let id = eval_env_counter.val
  eval_env_counter.val = id + 1
  id
}

///|
fn make_eval_env(env : Env) -> EvalEnv {
  { id: next_eval_env_id(), env }
}

///|
fn value_as_eval_env(value : Value) -> EvalEnv raise EvalError {
  match value {
    Value::EvalEnv(env) => env
    _ => raise @core.EvalError("type error: environment expected")
  }
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
  reset_library_version_registry()
  reset_record_type_registry()
  set_current_output_port(new_output_string_port())
  wind_stack.val = []
  let env = init_env()
  load_stdlib(env)
  register_builtin_libraries(env)
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
    "(define-condition-type &message &condition",
    "  make-message-condition message-condition?",
    "  (message condition-message))",
    "(define-condition-type &warning &condition",
    "  make-warning warning?)",
    "(define-condition-type &serious &condition",
    "  make-serious-condition serious-condition?)",
    "(define-condition-type &error &serious",
    "  make-error error?)",
    "(define-condition-type &violation &serious",
    "  make-violation violation?)",
    "(define-condition-type &assertion &violation",
    "  make-assertion-violation assertion-violation?)",
    "(define-condition-type &irritants &condition",
    "  make-irritants-condition irritants-condition?",
    "  (irritants condition-irritants))",
    "(define-condition-type &who &condition",
    "  make-who-condition who-condition?",
    "  (who condition-who))",
    "(define-condition-type &non-continuable &violation",
    "  make-non-continuable-violation non-continuable-violation?)",
    "(define-condition-type &implementation-restriction &violation",
    "  make-implementation-restriction-violation",
    "  implementation-restriction-violation?)",
    "(define-condition-type &lexical &violation",
    "  make-lexical-violation lexical-violation?)",
    "(define-condition-type &syntax &violation",
    "  make-syntax-violation syntax-violation?",
    "  (form syntax-violation-form)",
    "  (subform syntax-violation-subform))",
    "(define-condition-type &undefined &violation",
    "  make-undefined-violation undefined-violation?)",
    "(define-condition-type &no-infinities &implementation-restriction",
    "  make-no-infinities-violation no-infinities-violation?)",
    "(define-condition-type &no-nans &implementation-restriction",
    "  make-no-nans-violation no-nans-violation?)",
    "(library (rnrs control (6))",
    "  (export case-lambda do when unless)",
    "  (import)",
    "  (define-syntax when",
    "    (syntax-rules ()",
    "      ((_ test body ...) (if test (begin body ...) (begin)))))",
    "  (define-syntax unless",
    "    (syntax-rules ()",
    "      ((_ test body ...) (if (not test) (begin body ...) (begin)))))",
    ")",
    "(library (rnrs syntax-case (6))",
    "  (export syntax-case syntax quasisyntax unsyntax unsyntax-splicing ... _",
    "          make-variable-transformer generate-temporaries",
    "          syntax->datum datum->syntax free-identifier=? bound-identifier=?",
    "          identifier? syntax? syntax-violation)",
    "  (import)",
    ")",
    "(library (rnrs arithmetic fixnums (6))",
    "  (export fixnum? fixnum-width least-fixnum greatest-fixnum",
    "          fx=? fx<? fx>? fx<=? fx>=?",
    "          fxzero? fxpositive? fxnegative? fxodd? fxeven?",
    "          fxmax fxmin",
    "          fx+ fx- fx* fxdiv fxmod fxdiv0 fxmod0 fx+/carry fx-/carry fx*/carry",
    "          fxnot fxand fxior fxxor fxif",
    "          fxbit-count fxlength fxfirst-bit-set fxbit-set? fxcopy-bit fxbit-field",
    "          fxcopy-bit-field fxrotate-bit-field fxreverse-bit-field",
    "          fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)",
    "  (import)",
    ")",
    "(library (rnrs arithmetic bitwise (6))",
    "  (export bitwise-and bitwise-ior bitwise-xor bitwise-not bitwise-if",
    "          arithmetic-shift bitwise-bit-count bitwise-length bitwise-first-bit-set",
    "          bitwise-bit-set? bitwise-copy-bit bitwise-bit-field bitwise-copy-bit-field",
    "          bitwise-rotate-bit-field bitwise-reverse-bit-field)",
    "  (import)",
    ")",
    "(library (rnrs mutable-pairs (6))",
    "  (export set-car! set-cdr!)",
    "  (import)",
    ")",
    "(library (rnrs mutable-strings (6))",
    "  (export string-set! string-fill! string-copy!)",
    "  (import)",
    ")",
    "(library (rnrs bytevectors (6))",
    "  (export bytevector make-bytevector bytevector? bytevector-length",
    "          bytevector-u8-ref bytevector-u8-set! bytevector-copy",
    "          bytevector-copy! bytevector-append bytevector-fill!",
    "          bytevector->u8-list u8-list->bytevector)",
    "  (import)",
    ")",
    "(library (rnrs hashtables (6))",
    "  (export make-eq-hashtable make-eqv-hashtable make-hashtable hashtable?",
    "          hashtable-size hashtable-ref hashtable-set! hashtable-delete!",
    "          hashtable-contains? hashtable-update! hashtable-copy hashtable-clear!",
    "          hashtable-keys hashtable-entries hashtable-equivalence-function",
    "          hashtable-hash-function hashtable-mutable?)",
    "  (import)",
    ")",
    "(library (rnrs enums (6))",
    "  (export define-enumeration make-enumeration enum-set-universe enum-set-indexer",
    "          enum-set-constructor enum-set? enum-set-member? enum-set-subset?",
    "          enum-set=? enum-set-union enum-set-intersection enum-set-difference",
    "          enum-set-complement enum-set-projection enum-set->list)",
    "  (import)",
    ")",
    "(library (rnrs unicode (6))",
    "  (export char-alphabetic? char-numeric? char-whitespace?",
    "          char-upper-case? char-lower-case? char-upcase char-downcase",
    "          char-foldcase string-upcase string-downcase string-foldcase)",
    "  (import)",
    ")",
    "(library (rnrs exceptions (6))",
    "  (export guard with-exception-handler raise raise-continuable error",
    "          assertion-violation implementation-restriction-violation",
    "          undefined-violation syntax-violation)",
    "  (import)",
    ")",
    "(library (rnrs records syntactic (6))",
    "  (export define-record-type fields immutable mutable nongenerative opaque",
    "          parent parent-rtd protocol record-constructor-descriptor",
    "          record-type-descriptor sealed)",
    "  (import)",
    ")",
    "(library (rnrs records procedural (6))",
    "  (export make-record-type-descriptor record-type-descriptor?",
    "          record-type-name record-type-parent record-type-uid",
    "          record-type-generative? record-type-sealed? record-type-opaque?",
    "          record-type-field-names record-type-field-mutable?",
    "          make-record-constructor-descriptor record-constructor-descriptor?",
    "          record-constructor record-predicate record-accessor record-mutator)",
    "  (import)",
    ")",
    "(library (rnrs records inspection (6))",
    "  (export record? record-rtd record-type-name record-type-parent record-type-uid",
    "          record-type-generative? record-type-sealed? record-type-opaque?",
    "          record-type-field-names record-type-field-mutable?)",
    "  (import)",
    ")",
    "(library (rnrs lists (6))",
    "  (export find find-tail for-all exists filter partition fold-left fold-right",
    "          remp remove remq remv memp memq memv assp assq assv assoc",
    "          member cons*)",
    "  (import)",
    "  (define (fold-left f acc lst)",
    "    (let loop ((acc acc) (lst lst))",
    "      (if (null? lst) acc",
    "          (loop (f acc (car lst)) (cdr lst)))))",
    "  (define (fold-right f acc lst)",
    "    (if (null? lst) acc",
    "        (f (car lst) (fold-right f acc (cdr lst)))))",
    "  (define (filter pred lst)",
    "    (cond ((null? lst) '())",
    "          ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))",
    "          (else (filter pred (cdr lst)))))",
    "  (define (remove pred lst)",
    "    (filter (lambda (x) (not (pred x))) lst))",
    "  (define (remp pred lst)",
    "    (remove pred lst))",
    "  (define (remq obj lst)",
    "    (remp (lambda (x) (eq? x obj)) lst))",
    "  (define (remv obj lst)",
    "    (remp (lambda (x) (eqv? x obj)) lst))",
    "  (define (find pred lst)",
    "    (cond ((null? lst) #f)",
    "          ((pred (car lst)) (car lst))",
    "          (else (find pred (cdr lst)))))",
    "  (define (find-tail pred lst)",
    "    (cond ((null? lst) #f)",
    "          ((pred (car lst)) lst)",
    "          (else (find-tail pred (cdr lst)))))",
    "  (define (partition pred lst)",
    "    (let loop ((lst lst) (yes '()) (no '()))",
    "      (if (null? lst)",
    "          (list (reverse yes) (reverse no))",
    "          (if (pred (car lst))",
    "              (loop (cdr lst) (cons (car lst) yes) no)",
    "              (loop (cdr lst) yes (cons (car lst) no))))))",
    "  (define (any-null? lists)",
    "    (cond ((null? lists) #f)",
    "          ((null? (car lists)) #t)",
    "          (else (any-null? (cdr lists)))))",
    "  (define (for-all pred . lists)",
    "    (if (null? lists) #t",
    "        (let loop ((lists lists))",
    "          (if (any-null? lists)",
    "              #t",
    "              (and (apply pred (map car lists))",
    "                   (loop (map cdr lists)))))))",
    "  (define (exists pred . lists)",
    "    (if (null? lists) #f",
    "        (let loop ((lists lists))",
    "          (if (any-null? lists)",
    "              #f",
    "              (let ((value (apply pred (map car lists))))",
    "                (if value value (loop (map cdr lists))))))))",
    "  (define (memp pred lst)",
    "    (cond ((null? lst) #f)",
    "          ((pred (car lst)) lst)",
    "          (else (memp pred (cdr lst)))))",
    "  (define (assp pred alist)",
    "    (cond ((null? alist) #f)",
    "          ((pred (caar alist)) (car alist))",
    "          (else (assp pred (cdr alist)))))",
    "  (define (cons* . args)",
    "    (cond ((null? args) '())",
    "          ((null? (cdr args)) (car args))",
    "          (else (cons (car args) (apply cons* (cdr args))))))",
    ")",
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
          Datum::Label(_, cell) => {
            state = MachineState::Eval(cell.val, current_env, kont, handlers)
          }
          Datum::Nil
          | Datum::Bool(_)
          | Datum::Int(_)
          | Datum::BigInt(_)
          | Datum::Rat(_, _)
          | Datum::BigRat(_, _)
          | Datum::Float(_)
          | Datum::Complex(_, _)
          | Datum::Char(_)
          | Datum::String(_)
          | Datum::Vector(_)
          | Datum::ByteVector(_)
          | Datum::Record(_)
          | Datum::Condition(_)
          | Datum::Value(_) =>
            state = MachineState::Apply(value_from_datum(current_expr), kont, handlers)
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
                raise @core.EvalError("invalid cond")
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
          Kont::LetRecInit(bindings, index, collected, env, body, next) => {
            let single = expect_single(value)
            let next_values : Array[Value] = []
            for item in collected {
              next_values.push(item)
            }
            next_values.push(single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::LetRecInit(bindings, next_index, next_values, env, body, next),
                handlers,
              )
            } else {
              let mut i = 0
              while i < bindings.length() {
                let (name, _) = bindings[i]
                env_set(env, name, next_values[i])
                i = i + 1
              }
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          Kont::LetValues(bindings, index, collected, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let bound = formals_bindings(formals, values)
            let next_collected : Array[Array[(String, Value)]] = []
            for item in collected {
              next_collected.push(item)
            }
            next_collected.push(bound)
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
                let binding_values = next_collected[i]
                for binding in binding_values {
                  let (name, bound_value) = binding
                  env_define(new_env, name, bound_value)
                }
                i = i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          Kont::LetStarValues(bindings, index, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let binding_values = formals_bindings(formals, values)
            let new_env = env_extend(env)
            for binding in binding_values {
              let (name, bound_value) = binding
              env_define(new_env, name, bound_value)
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
          Kont::DefineValues(formals, env, next) => {
            let values = value_to_values(value)
            let binding_values = formals_bindings(formals, values)
            for binding in binding_values {
              let (name, bound_value) = binding
              env_define(env, name, bound_value)
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
                raise @core.EvalError("invalid guard")
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
              raise @core.EvalError("type error: procedure expected")
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
              None => raise @core.EvalError("invalid record constructor descriptor")
            }
            if !record_type_is_a(record.record_type, parent_type) {
              raise @core.EvalError("type error: record type mismatch: expected \{parent_type.name}")
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
              raise @core.EvalError("type error: procedure expected")
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
          Kont::DefineConditionType(ctor_name, pred_name, record_type, bindings, env, next) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            env_define(env, ctor_name, ctor_value)
            env_define(
              env,
              pred_name,
              Value::ConditionProc(
                make_condition_proc(ConditionProcKind::Predicate(record_type)),
              ),
            )
            for binding in bindings {
              env_define(
                env,
                binding.accessor,
                Value::ConditionProc(
                  make_condition_proc(
                    ConditionProcKind::Accessor(record_type, binding.index),
                  ),
                ),
              )
            }
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::HashtableFindResult(table, key, idx, op, next) => {
            let result_value = expect_single(value)
            if is_false(result_value) {
              state = hashtable_find_state(table, key, idx + 1, op, next, handlers)
            } else {
              state = hashtable_found_state(table, key, idx, op, next, handlers)
            }
          }
          Kont::HashtableUpdateApply(table, key, idx, found, next) => {
            let next_value = expect_single(value)
            if !table.mutable {
              raise @core.EvalError("hashtable is immutable")
            }
            if found {
              let entries = table.entries.val
              let entry = entries[idx]
              entry.value.val = next_value
            } else {
              let entries = table.entries.val
              entries.push(HashtableEntry::{ key, value: Ref::new(next_value) })
            }
            state = MachineState::Apply(Value::Void, next, handlers)
          }
          Kont::MapStep(proc, lists, results, for_each, next) => {
            let single = expect_single(value)
            let next_results = results
            if !for_each {
              next_results.push(value_to_datum_element(single))
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
          Kont::ForcePromise(promise, next) => {
            promise.state.val = PromiseState::Value(value)
            state = MachineState::Apply(value, next, handlers)
          }
          Kont::RaiseNonCont => raise @core.EvalError("non-continuable exception")
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
fn eval_syntax(args : Array[Datum]) -> Value raise EvalError {
  match args.length() {
    1 => Value::SyntaxObject(SyntaxObject::{ datum: args[0], context: next_syntax_context() })
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
fn eval_quasisyntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    1 => Value::SyntaxObject(
      SyntaxObject::{ datum: quasisyntax_expand(args[0], env, 1), context: next_syntax_context() },
    )
    n => raise arity_mismatch(1, n)
  }
}

///|
fn eval_delay(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    1 => {
      let thunk = Value::Closure(make_closure([], None, [args[0]], env))
      Value::Promise(make_promise(thunk))
    }
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
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([Datum::Symbol("unquote"), quasiquote_expand(arg, env, depth - 1)])
      }
    }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) => {
      if depth == 1 {
        raise @core.EvalError("unquote-splicing not in list context")
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
fn quasisyntax_expand(expr : Datum, env : Env, depth : Int) -> Datum raise EvalError {
  match unwrap_form(expr, "quasisyntax") {
    Some(arg) =>
      return list_from_array([Datum::Symbol("quasisyntax"), quasisyntax_expand(arg, env, depth + 1)])
    None => ()
  }
  match unwrap_form(expr, "unsyntax") {
    Some(arg) => {
      if depth == 1 {
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([Datum::Symbol("unsyntax"), quasisyntax_expand(arg, env, depth - 1)])
      }
    }
    None => ()
  }
  match unwrap_form(expr, "unsyntax-splicing") {
    Some(arg) => {
      if depth == 1 {
        raise @core.EvalError("unsyntax-splicing not in list context")
      } else {
        return list_from_array([
          Datum::Symbol("unsyntax-splicing"),
          quasisyntax_expand(arg, env, depth - 1),
        ])
      }
    }
    None => ()
  }
  match expr {
    Datum::Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unsyntax-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasisyntax_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasisyntax_expand(car.val, env, depth),
        quasisyntax_expand(cdr.val, env, depth),
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
fn parse_formals(expr : Datum) -> Formals raise EvalError {
  let names : Array[String] = []
  let mut cur = expr
  while true {
    match cur {
      Datum::Nil => return Formals::{ params: names, rest: None }
      Datum::Symbol(name) => return Formals::{ params: names, rest: Some(name) }
      Datum::Pair(car, cdr) => {
        match car.val {
          Datum::Symbol(name) => names.push(name)
          _ => raise @core.EvalError("type error: symbol expected")
        }
        cur = cdr.val
      }
      _ => raise @core.EvalError("type error: symbol expected")
    }
  }
  Formals::{ params: names, rest: None }
}

///|
fn arity_mismatch_at_least(expected : Int, got : Int) -> EvalError {
  @core.EvalError("arity mismatch: expected at least \{expected} got \{got}")
}

///|
fn values_to_list_value(values : Array[Value]) -> Value {
  let items : Array[Datum] = []
  for value in values {
    items.push(value_to_datum_element(value))
  }
  Value::Datum(list_from_array(items))
}

///|
fn formals_bindings(formals : Formals, values : Array[Value]) -> Array[(String, Value)] raise EvalError {
  let params = formals.params
  let min_len = params.length()
  match formals.rest {
    None =>
      if values.length() != min_len {
        raise arity_mismatch(min_len, values.length())
      }
    Some(_) =>
      if values.length() < min_len {
        raise arity_mismatch_at_least(min_len, values.length())
      }
  }
  let bindings : Array[(String, Value)] = []
  let mut i = 0
  while i < params.length() {
    bindings.push((params[i], values[i]))
    i = i + 1
  }
  match formals.rest {
    Some(name) => {
      let rest_values : Array[Value] = []
      let mut j = min_len
      while j < values.length() {
        rest_values.push(values[j])
        j = j + 1
      }
      bindings.push((name, values_to_list_value(rest_values)))
    }
    None => ()
  }
  bindings
}

///|
fn formals_accept_len(formals : Formals, arg_len : Int) -> Bool {
  match formals.rest {
    None => arg_len == formals.params.length()
    Some(_) => arg_len >= formals.params.length()
  }
}

///|
fn formals_expected_desc(formals : Formals) -> String {
  match formals.rest {
    None => formals.params.length().to_string()
    Some(_) => ">=\{formals.params.length()}"
  }
}

///|
fn eval_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let formals = parse_formals(args[0])
    let body = args.sub(start=1).to_array()
    Value::Closure(make_closure(formals.params, formals.rest, body, env))
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
      raise @core.EvalError("invalid case-lambda")
    }
    let formals = parse_formals(parts[0])
    let body = parts.sub(start=1).to_array()
    clauses.push(CaseClause::{ params: formals.params, rest: formals.rest, body })
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
        _ => raise @core.EvalError("type error: symbol expected")
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
      raise @core.EvalError("type error: syntax binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => {
          let rules = parse_transformer(parts[1], name)
          parsed.push((name, rules))
        }
        _ => raise @core.EvalError("type error: symbol expected")
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
      raise @core.EvalError("type error: binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => parsed.push((name, parts[1]))
        _ => raise @core.EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_value_bindings(expr : Datum) -> Array[(Formals, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(Formals, Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise @core.EvalError("invalid let-values")
    } else {
      let formals = parse_formals(parts[0])
      parsed.push((formals, parts[1]))
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
      raise @core.EvalError("invalid parameterize")
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
    _ => raise @core.EvalError("type error: symbol expected")
  }
}

///|
fn parse_bool(expr : Datum) -> Bool raise EvalError {
  match expr {
    Datum::Bool(value) => value
    _ => raise @core.EvalError("invalid define-record-type")
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
        raise @core.EvalError("multiple values in single-value context")
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
    raise @core.EvalError("uncaught exception")
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
fn eval_begin_for_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let syntax_env = env_clone(env)
  for form in args {
    let _ = eval_expr(form, syntax_env)
  }
  let frame = syntax_env[syntax_env.length() - 1]
  for key in frame.keys() {
    match frame.get(key) {
      Some(Value::Macro(rules)) => env_define(env, key, Value::Macro(rules))
      _ => ()
    }
  }
  MachineState::Apply(Value::Void, kont, handlers)
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
      Datum::Pair(head, tail) => {
        let name = parse_symbol(head.val)
        let formals = parse_formals(tail.val)
        let body = args.sub(start=1).to_array()
        env_define(env, name, Value::Closure(make_closure(formals.params, formals.rest, body, env)))
        MachineState::Apply(Value::Void, kont, handlers)
      }
      _ => raise @core.EvalError("type error: symbol expected")
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
      let formals = parse_formals(args[0])
      MachineState::Eval(args[1], env, Kont::DefineValues(formals, env, kont), handlers)
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
      raise @core.EvalError("invalid guard")
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
        _ => raise @core.EvalError("type error: symbol expected")
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
        Kont::LetRecInit(bindings, 0, [], new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_letrec_star_state(
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
      raise @core.EvalError("invalid do")
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
    raise @core.EvalError("invalid do")
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
    raise @core.EvalError("invalid define-record-type")
  }
  let record_name = parse_symbol(args[0])
  let ctor_parts = datum_list_to_array(args[1])
  if ctor_parts.is_empty() {
    raise @core.EvalError("invalid define-record-type")
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
            raise @core.EvalError("invalid define-record-type")
          }
          parent_name = Some(parse_symbol(parts[1]))
          continue
        }
        Some("protocol") if parts.length() == 2 => {
          if protocol_expr is Some(_) {
            raise @core.EvalError("invalid define-record-type")
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
            raise @core.EvalError("invalid define-record-type")
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
        None => raise @core.EvalError("invalid define-record-type")
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
        raise @core.EvalError("invalid define-record-type")
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
      raise @core.EvalError("invalid define-record-type")
    }
    let field_name = parse_symbol(parts[0])
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
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
      raise @core.EvalError("invalid define-record-type")
    }
  } else {
    if ctor_fields.length() != parent_count + new_field_names.length() {
      raise @core.EvalError("invalid define-record-type")
    }
  }
  let fields : Array[RecordField] = []
  let field_index : Map[String, Int] = {}
  let mut i = 0
  while i < ctor_fields.length() {
    let field_name = ctor_fields[i]
    if field_index.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if i < parent_count {
      let parent_field = parent_fields[i]
      if parent_field.name != field_name {
        raise @core.EvalError("invalid define-record-type")
      }
      fields.push(parent_field)
    } else {
      let is_mutable = match mutable.get(field_name) {
        Some(flag) => flag
        None => raise @core.EvalError("invalid define-record-type")
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
      None => raise @core.EvalError("invalid define-record-type")
    }
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-record-type")
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
        raise @core.EvalError("invalid define-record-type")
      }
      if protocol_expr is Some(_) {
        raise @core.EvalError("invalid define-record-type")
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
            raise @core.EvalError("type error: procedure expected")
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
fn eval_define_condition_type_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 4 {
    raise @core.EvalError("invalid define-condition-type")
  }
  let record_name = parse_symbol(args[0])
  let parent_name = parse_symbol(args[1])
  let ctor_name = parse_symbol(args[2])
  let pred_name = parse_symbol(args[3])
  let field_specs = args.sub(start=4).to_array()
  match lookup_record_type_descriptor(record_name) {
    Some(_) => raise @core.EvalError("invalid define-condition-type")
    None => ()
  }
  let parent_desc = match lookup_record_type_descriptor(parent_name) {
    Some(desc) => desc
    None => raise @core.EvalError("invalid define-condition-type")
  }
  let parent_type = parent_desc.record_type
  if parent_type.is_sealed || parent_type.is_opaque {
    raise @core.EvalError("invalid define-condition-type")
  }
  let base_type = condition_base_type()
  if !record_type_is_a(parent_type, base_type) {
    raise @core.EvalError("invalid define-condition-type")
  }
  let parent_fields = parent_type.fields
  let parent_field_names : Array[String] = []
  for field in parent_fields {
    parent_field_names.push(field.name)
  }
  let accessors : Map[String, String] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    let parts = datum_list_to_array(spec)
    if parts.length() != 2 {
      raise @core.EvalError("invalid define-condition-type")
    }
    let field_name = parse_symbol(parts[0])
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    let accessor = parse_symbol(parts[1])
    accessors[field_name] = accessor
    new_field_names.push(field_name)
  }
  let fields : Array[RecordField] = []
  for field in parent_fields {
    fields.push(field)
  }
  for field_name in new_field_names {
    fields.push(RecordField::{ name: field_name, mutable: false })
  }
  let bindings : Array[RecordFieldBinding] = []
  let parent_count = parent_fields.length()
  let mut i = 0
  while i < new_field_names.length() {
    let field_name = new_field_names[i]
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-condition-type")
    }
    bindings.push(
      RecordFieldBinding::{ accessor: accessor_name, index: parent_count + i, mutator: None },
    )
    i = i + 1
  }
  let record_type = make_record_type(
    record_name,
    Some(parent_type),
    false,
    false,
    None,
    fields,
  )
  let ctor_desc =
    make_record_constructor_descriptor(record_type, Some(parent_desc.constructor_desc), None)
  let rtd = make_record_type_descriptor(record_type, ctor_desc)
  let _ = register_record_type(record_name, rtd)
  env_define(env, record_name, Value::RecordTypeDescriptor(rtd))
  let next_kont =
    Kont::DefineConditionType(ctor_name, pred_name, record_type, bindings, env, kont)
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn eval_define_enumeration_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise @core.EvalError("invalid define-enumeration")
  }
  let type_name = parse_symbol(args[0])
  let symbols = parse_symbol_list(datum_list_to_array(args[1]))
  let ctor_name = parse_symbol(args[2])
  let seen : Map[String, Bool] = {}
  for name in symbols {
    if seen.contains(name) {
      raise @core.EvalError("invalid define-enumeration")
    }
    seen[name] = true
  }
  let members = enum_set_members_all_true(symbols)
  let universe = make_enum_set(symbols, members)
  env_define(env, type_name, Value::EnumSet(universe))
  env_define(env, ctor_name, Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Constructor(universe))))
  MachineState::Apply(Value::Void, kont, handlers)
}

///|
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
    _ => raise @core.EvalError("type error: record expected")
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
        raise @core.EvalError("type error: procedure expected")
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
          raise @core.EvalError("type error: record type mismatch: expected \{record_type.name}")
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
          raise @core.EvalError("type error: record type mismatch: expected \{record_type.name}")
        }
        let field = record.record_type.fields[idx]
        if !field.mutable {
          raise @core.EvalError("record field is immutable: \{field.name}")
        }
        record.fields[idx].val = args[1]
        MachineState::Apply(Value::Void, kont, handlers)
      }
    }
    RecordProcKind::ProtocolN(record_type, parent_desc) => {
      let parent_type = match record_type.parent {
        Some(parent) => parent
        None => raise @core.EvalError("invalid record constructor descriptor")
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
        raise @core.EvalError("invalid record constructor descriptor")
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
fn apply_condition_proc(
  proc : ConditionProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    ConditionProcKind::Predicate(record_type) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let mut ok = false
        match condition_components_opt(args[0], base) {
          Some(components) => {
            for record in components {
              if record_type_is_a(record.record_type, record_type) {
                ok = true
                break
              }
            }
          }
          None => ()
        }
        MachineState::Apply(bool_value(ok), kont, handlers)
      }
    }
    ConditionProcKind::Accessor(record_type, idx) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let components = condition_components(args[0], base)
        let mut found : Record? = None
        for record in components {
          if record_type_is_a(record.record_type, record_type) {
            found = Some(record)
            break
          }
        }
        match found {
          Some(record) => MachineState::Apply(record.fields[idx].val, kont, handlers)
          None => raise @core.EvalError(
            "type error: condition type mismatch: expected \{record_type.name}",
          )
        }
      }
    }
  }
}

///|
fn apply_enum_set_proc(
  proc : EnumSetProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    EnumSetProcKind::Indexer(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let name = parse_symbol(value_as_datum(args[0]))
      match enum_set_index_of(set.universe, name) {
        Some(idx) => MachineState::Apply(Value::Datum(Datum::Int(idx)), kont, handlers)
        None => MachineState::Apply(Value::Datum(Datum::Bool(false)), kont, handlers)
      }
    }
    EnumSetProcKind::Constructor(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let names = parse_symbol_list(datum_list_to_array(value_as_datum(args[0])))
      let subset = enum_set_from_names(set.universe, names)
      MachineState::Apply(Value::EnumSet(subset), kont, handlers)
    }
  }
}

///|
fn make_condition_value(components : Array[Record]) -> Value {
  if components.length() == 1 {
    Value::Record(components[0])
  } else {
    Value::Datum(Datum::Condition(make_condition(components)))
  }
}

///|
fn lookup_condition_record_type(name : String) -> RecordType raise EvalError {
  match lookup_record_type_descriptor(name) {
    Some(desc) => desc.record_type
    None => raise @core.EvalError("type error: condition type expected")
  }
}

///|
fn make_condition_record(name : String, values : Array[Value]) -> Record raise EvalError {
  let record_type = lookup_condition_record_type(name)
  if record_type.fields.length() != values.length() {
    raise @core.EvalError("type error: condition field count mismatch")
  }
  make_record_instance(record_type, values)
}

///|
fn irritants_list_value(values : Array[Value]) -> Value {
  values_to_list_value(values)
}

///|
fn raise_condition(
  components : Array[Record],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let cond_value = make_condition_value(components)
  apply_proc(Value::Primitive(Primitive::Raise), [cond_value], kont, handlers)
}

///|
fn hashtable_key_equal(equiv : HashtableEquiv, left : Value, right : Value) -> Bool {
  match equiv {
    HashtableEquiv::Eq => value_eqv(left, right)
    HashtableEquiv::Eqv => value_eqv(left, right)
    HashtableEquiv::Equal => value_equal(left, right)
    HashtableEquiv::Proc(_) => false
  }
}

///|
fn hashtable_find_state(
  table : Hashtable,
  key : Value,
  idx : Int,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let entries = table.entries.val
  if idx >= entries.length() {
    return hashtable_not_found_state(table, key, op, kont, handlers)
  }
  let entry = entries[idx]
  match table.equiv {
    HashtableEquiv::Proc(proc) =>
      apply_proc(proc, [key, entry.key], Kont::HashtableFindResult(table, key, idx, op, kont), handlers)
    _ =>
      if hashtable_key_equal(table.equiv, key, entry.key) {
        hashtable_found_state(table, key, idx, op, kont, handlers)
      } else {
        hashtable_find_state(table, key, idx + 1, op, kont, handlers)
      }
  }
}

///|
fn hashtable_found_state(
  table : Hashtable,
  key : Value,
  idx : Int,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let entries = table.entries.val
  let entry = entries[idx]
  match op {
    HashtableOp::Ref(_) => MachineState::Apply(entry.value.val, kont, handlers)
    HashtableOp::Contains => MachineState::Apply(bool_value(true), kont, handlers)
    HashtableOp::Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      entry.value.val = value
      MachineState::Apply(Value::Void, kont, handlers)
    }
    HashtableOp::Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let next_entries : Array[HashtableEntry] = []
      let mut i = 0
      while i < entries.length() {
        if i != idx {
          next_entries.push(entries[i])
        }
        i = i + 1
      }
      table.entries.val = next_entries
      MachineState::Apply(Value::Void, kont, handlers)
    }
    HashtableOp::Update(proc, _) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [entry.value.val],
        Kont::HashtableUpdateApply(table, key, idx, true, kont),
        handlers,
      )
    }
  }
}

///|
fn hashtable_not_found_state(
  table : Hashtable,
  key : Value,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match op {
    HashtableOp::Ref(default_value) => MachineState::Apply(default_value, kont, handlers)
    HashtableOp::Contains => MachineState::Apply(bool_value(false), kont, handlers)
    HashtableOp::Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let entries = table.entries.val
      entries.push(HashtableEntry::{ key, value: Ref::new(value) })
      MachineState::Apply(Value::Void, kont, handlers)
    }
    HashtableOp::Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      MachineState::Apply(Value::Void, kont, handlers)
    }
    HashtableOp::Update(proc, default_value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [default_value],
        Kont::HashtableUpdateApply(table, key, -1, false, kont),
        handlers,
      )
    }
  }
}

///|
fn apply_hashtable_ref(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let default_value = args[2]
  hashtable_find_state(table, key, 0, HashtableOp::Ref(default_value), kont, handlers)
}

///|
fn apply_hashtable_set(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let value = args[2]
  hashtable_find_state(table, key, 0, HashtableOp::Set(value), kont, handlers)
}

///|
fn apply_hashtable_delete(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(table, key, 0, HashtableOp::Delete, kont, handlers)
}

///|
fn apply_hashtable_contains(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(table, key, 0, HashtableOp::Contains, kont, handlers)
}

///|
fn apply_hashtable_update(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 4 {
    raise arity_mismatch(4, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let proc = args[2]
  let default_value = args[3]
  if !is_procedure_value(proc) {
    raise @core.EvalError("type error: procedure expected")
  }
  hashtable_find_state(
    table,
    key,
    0,
    HashtableOp::Update(proc, default_value),
    kont,
    handlers,
  )
}

priv struct LibraryNameSpec {
  base : String
  version : Array[Int]?
}

priv enum VersionRef {
  Exact(Array[Int])
  Gt(Array[Int])
  Ge(Array[Int])
  Lt(Array[Int])
  Le(Array[Int])
  And(Array[VersionRef])
  Or(Array[VersionRef])
  Not(Ref[VersionRef])
}

priv struct LibraryRef {
  base : String
  version_ref : VersionRef?
}

priv struct LibraryVersionInfo {
  name : String
  version : Array[Int]
  has_version : Bool
}

let library_versions_registry : Ref[Map[String, Array[LibraryVersionInfo]]] = Ref::new({})

///|
fn reset_library_version_registry() -> Unit {
  library_versions_registry.val = {}
}

///|
fn format_version_suffix(version : Array[Int]) -> String {
  let versions : Array[String] = []
  for part in version {
    versions.push(part.to_string())
  }
  let joined = versions.join(".")
  "v\{joined}"
}

///|
fn library_full_name(base : String, version : Array[Int]?) -> String {
  match version {
    Some(value) => "\{base}.\{format_version_suffix(value)}"
    None => base
  }
}

///|
fn register_library_entry(
  base : String,
  version : Array[Int]?,
  exports : Map[String, Value],
) -> Unit {
  let name = library_full_name(base, version)
  register_library(name, exports)
  let entry = LibraryVersionInfo::{
    name,
    version: match version {
      Some(value) => value
      None => []
    },
    has_version: version is Some(_),
  }
  match library_versions_registry.val.get(base) {
    Some(existing) => {
      let updated = existing
      updated.push(entry)
      library_versions_registry.val[base] = updated
    }
    None => library_versions_registry.val[base] = [entry]
  }
}

///|
fn parse_version_items(items : Array[Datum]) -> Array[Int] raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let versions : Array[Int] = []
  for part in items {
    match part {
      Datum::Int(n) =>
        if n < 0 {
          raise @core.EvalError("invalid library name")
        } else {
          versions.push(n)
        }
      _ => raise @core.EvalError("invalid library name")
    }
  }
  versions
}

///|
fn parse_version_list(expr : Datum) -> Array[Int] raise EvalError {
  parse_version_items(datum_list_to_array(expr))
}

///|
fn parse_version_ref_version(items : Array[Datum]) -> Array[Int] raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  if items.length() == 1 {
    match items[0] {
      Datum::Pair(_, _) | Datum::Nil => parse_version_list(items[0])
      _ => parse_version_items(items)
    }
  } else {
    parse_version_items(items)
  }
}

///|
fn parse_version_ref(expr : Datum) -> VersionRef raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  match items[0] {
    Datum::Symbol("and") => {
      if items.length() == 1 {
        raise @core.EvalError("invalid library name")
      }
      let refs : Array[VersionRef] = []
      let mut i = 1
      while i < items.length() {
        refs.push(parse_version_ref(items[i]))
        i = i + 1
      }
      VersionRef::And(refs)
    }
    Datum::Symbol("or") => {
      if items.length() == 1 {
        raise @core.EvalError("invalid library name")
      }
      let refs : Array[VersionRef] = []
      let mut i = 1
      while i < items.length() {
        refs.push(parse_version_ref(items[i]))
        i = i + 1
      }
      VersionRef::Or(refs)
    }
    Datum::Symbol("not") => {
      if items.length() != 2 {
        raise @core.EvalError("invalid library name")
      }
      VersionRef::Not(Ref::new(parse_version_ref(items[1])))
    }
    Datum::Symbol(">") => VersionRef::Gt(parse_version_ref_version(items.sub(start=1).to_array()))
    Datum::Symbol(">=") => VersionRef::Ge(parse_version_ref_version(items.sub(start=1).to_array()))
    Datum::Symbol("<") => VersionRef::Lt(parse_version_ref_version(items.sub(start=1).to_array()))
    Datum::Symbol("<=") => VersionRef::Le(parse_version_ref_version(items.sub(start=1).to_array()))
    Datum::Int(_) => VersionRef::Exact(parse_version_items(items))
    _ => raise @core.EvalError("invalid library name")
  }
}

///|
fn parse_library_name_spec(expr : Datum) -> LibraryNameSpec raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let names : Array[String] = []
  let mut version : Array[Int]? = None
  let mut i = 0
  while i < parts.length() {
    match parts[i] {
      Datum::Symbol(name) => {
        if version is Some(_) {
          raise @core.EvalError("invalid library name")
        }
        names.push(name)
      }
      Datum::Pair(_, _) | Datum::Nil => {
        if i != parts.length() - 1 {
          raise @core.EvalError("invalid library name")
        }
        version = Some(parse_version_list(parts[i]))
      }
      _ => raise @core.EvalError("invalid library name")
    }
    i = i + 1
  }
  if names.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  { base: names.join("."), version }
}

///|
fn parse_library_ref(expr : Datum) -> LibraryRef raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let names : Array[String] = []
  let mut version_ref : VersionRef? = None
  let mut i = 0
  while i < parts.length() {
    match parts[i] {
      Datum::Symbol(name) => {
        if version_ref is Some(_) {
          raise @core.EvalError("invalid library name")
        }
        names.push(name)
      }
      Datum::Pair(_, _) | Datum::Nil => {
        if i != parts.length() - 1 {
          raise @core.EvalError("invalid library name")
        }
        version_ref = Some(parse_version_ref(parts[i]))
      }
      _ => raise @core.EvalError("invalid library name")
    }
    i = i + 1
  }
  if names.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  { base: names.join("."), version_ref }
}

///|
fn compare_versions(left : Array[Int], right : Array[Int]) -> Int {
  let min_len = if left.length() < right.length() {
    left.length()
  } else {
    right.length()
  }
  let mut i = 0
  while i < min_len {
    if left[i] < right[i] {
      return -1
    }
    if left[i] > right[i] {
      return 1
    }
    i = i + 1
  }
  if left.length() < right.length() {
    -1
  } else if left.length() > right.length() {
    1
  } else {
    0
  }
}

///|
fn version_ref_matches(version : Array[Int], reference : VersionRef) -> Bool {
  match reference {
    VersionRef::Exact(expected) => compare_versions(version, expected) == 0
    VersionRef::Gt(expected) => compare_versions(version, expected) > 0
    VersionRef::Ge(expected) => compare_versions(version, expected) >= 0
    VersionRef::Lt(expected) => compare_versions(version, expected) < 0
    VersionRef::Le(expected) => compare_versions(version, expected) <= 0
    VersionRef::And(items) => {
      let mut i = 0
      while i < items.length() {
        if !version_ref_matches(version, items[i]) {
          return false
        }
        i = i + 1
      }
      true
    }
    VersionRef::Or(items) => {
      let mut i = 0
      while i < items.length() {
        if version_ref_matches(version, items[i]) {
          return true
        }
        i = i + 1
      }
      false
    }
    VersionRef::Not(item) => !version_ref_matches(version, item.val)
  }
}

///|
fn select_library_for_ref(reference : LibraryRef) -> LibraryVersionInfo? {
  match library_versions_registry.val.get(reference.base) {
    None => None
    Some(available) => {
      let version_ref = reference.version_ref
      let mut best : LibraryVersionInfo? = None
      for entry in available {
        match version_ref {
          Some(ref_value) => {
            if !entry.has_version {
              continue
            }
            if !version_ref_matches(entry.version, ref_value) {
              continue
            }
          }
          None => ()
        }
        match best {
          None => best = Some(entry)
          Some(current) => {
            if version_ref is None {
              if entry.has_version && !current.has_version {
                best = Some(entry)
              } else if entry.has_version && current.has_version {
                if compare_versions(entry.version, current.version) > 0 {
                  best = Some(entry)
                }
              }
            } else if compare_versions(entry.version, current.version) > 0 {
              best = Some(entry)
            }
          }
        }
      }
      best
    }
  }
}

///|
fn library_exports_from_ref(reference : LibraryRef) -> Map[String, Value] raise EvalError {
  match select_library_for_ref(reference) {
    Some(entry) => library_exports(entry.name)
    None => raise @core.EvalError("unknown library: \{reference.base}")
  }
}

///|
fn parse_export_clause(expr : Datum) -> Array[(String, String)] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("export") => {
      let exports : Array[(String, String)] = []
      let mut i = 1
      while i < items.length() {
        match items[i] {
          Datum::Symbol(name) => exports.push((name, name))
          Datum::Pair(_, _) => {
            let spec_items = datum_list_to_array(items[i])
            if spec_items.is_empty() {
              raise @core.EvalError("invalid library")
            }
            match spec_items[0] {
              Datum::Symbol("rename") => {
                let mut j = 1
                while j < spec_items.length() {
                  let rename_parts = datum_list_to_array(spec_items[j])
                  if rename_parts.length() != 2 {
                    raise @core.EvalError("invalid library")
                  }
                  let internal = parse_symbol(rename_parts[0])
                  let external = parse_symbol(rename_parts[1])
                  exports.push((external, internal))
                  j = j + 1
                }
              }
              _ => raise @core.EvalError("invalid library")
            }
          }
          _ => raise @core.EvalError("invalid library")
        }
        i = i + 1
      }
      exports
    }
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn base_syntax_keywords() -> Array[String] {
  [
    "...",
    "_",
    "=>",
    "and",
    "begin",
    "case",
    "cond",
    "define",
    "define-syntax",
    "else",
    "identifier-syntax",
    "if",
    "lambda",
    "let",
    "let*",
    "let*-values",
    "let-syntax",
    "let-values",
    "letrec",
    "letrec*",
    "letrec-syntax",
    "or",
    "quasiquote",
    "quote",
    "set!",
    "syntax-rules",
    "unquote",
    "unquote-splicing",
  ]
}

///|
fn syntax_case_keywords() -> Array[String] {
  [
    "...",
    "_",
    "syntax",
    "quasisyntax",
    "unsyntax",
    "unsyntax-splicing",
    "syntax-case",
  ]
}

///|
fn control_syntax_keywords() -> Array[String] {
  ["case-lambda", "do"]
}

///|
fn exceptions_syntax_keywords() -> Array[String] {
  ["guard"]
}

///|
fn enums_syntax_keywords() -> Array[String] {
  ["define-enumeration"]
}

///|
fn records_syntax_keywords() -> Array[String] {
  [
    "define-record-type",
    "fields",
    "immutable",
    "mutable",
    "nongenerative",
    "opaque",
    "parent",
    "parent-rtd",
    "protocol",
    "record-constructor-descriptor",
    "record-type-descriptor",
    "sealed",
  ]
}

///|
fn conditions_syntax_keywords() -> Array[String] {
  ["define-condition-type"]
}

///|
fn is_syntax_keyword(name : String) -> Bool {
  let lists : Array[Array[String]] = [
    base_syntax_keywords(),
    syntax_case_keywords(),
    control_syntax_keywords(),
    exceptions_syntax_keywords(),
    enums_syntax_keywords(),
    records_syntax_keywords(),
    conditions_syntax_keywords(),
  ]
  let mut i = 0
  while i < lists.length() {
    let keywords = lists[i]
    let mut j = 0
    while j < keywords.length() {
      if keywords[j] == name {
        return true
      }
      j = j + 1
    }
    i = i + 1
  }
  false
}

///|
fn parse_import_clause(expr : Datum) -> Array[Datum] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library")
  }
  match items[0] {
    Datum::Symbol("import") => items.sub(start=1).to_array()
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn library_exports(name : String) -> Map[String, Value] raise EvalError {
  match lookup_library(name) {
    Some(lib) => {
      let exports : Map[String, Value] = {}
      for key in lib.exports.keys() {
        match lib.exports.get(key) {
          Some(value) => exports[key] = value
          None => ()
        }
      }
      exports
    }
    None => raise @core.EvalError("unknown library: \{name}")
  }
}

///|
fn import_set_only(
  exports : Map[String, Value],
  names : Array[String],
) -> Map[String, Value] raise EvalError {
  let result : Map[String, Value] = {}
  for name in names {
    match exports.get(name) {
      Some(value) => result[name] = value
      None => raise @core.EvalError("unknown import: \{name}")
    }
  }
  result
}

///|
fn import_set_except(
  exports : Map[String, Value],
  names : Array[String],
) -> Map[String, Value] raise EvalError {
  let excluded : Map[String, Bool] = {}
  for name in names {
    if !exports.contains(name) {
      raise @core.EvalError("unknown import: \{name}")
    }
    excluded[name] = true
  }
  let result : Map[String, Value] = {}
  for key in exports.keys() {
    if !excluded.contains(key) {
      match exports.get(key) {
        Some(value) => result[key] = value
        None => ()
      }
    }
  }
  result
}

///|
fn import_set_rename(
  exports : Map[String, Value],
  renames : Array[(String, String)],
) -> Map[String, Value] raise EvalError {
  let rename_map : Map[String, String] = {}
  for rename in renames {
    let (old_name, new_name) = rename
    if !exports.contains(old_name) {
      raise @core.EvalError("unknown import: \{old_name}")
    }
    if rename_map.contains(old_name) {
      raise @core.EvalError("invalid import set")
    }
    rename_map[old_name] = new_name
  }
  let result : Map[String, Value] = {}
  for key in exports.keys() {
    let target = match rename_map.get(key) {
      Some(name) => name
      None => key
    }
    if result.contains(target) {
      raise @core.EvalError("invalid import set")
    }
    match exports.get(key) {
      Some(value) => result[target] = value
      None => ()
    }
  }
  result
}

///|
fn import_set_prefix(exports : Map[String, Value], prefix : String) -> Map[String, Value] {
  let result : Map[String, Value] = {}
  for key in exports.keys() {
    match exports.get(key) {
      Some(value) => result["\{prefix}\{key}"] = value
      None => ()
    }
  }
  result
}

///|
fn parse_rename_specs(items : Array[Datum]) -> Array[(String, String)] raise EvalError {
  let renames : Array[(String, String)] = []
  for item in items {
    let parts = datum_list_to_array(item)
    if parts.length() != 2 {
      raise @core.EvalError("invalid import set")
    }
    let old_name = parse_symbol(parts[0])
    let new_name = parse_symbol(parts[1])
    renames.push((old_name, new_name))
  }
  renames
}

priv struct ImportPhases {
  run : Bool
  expand : Bool
}

///|
fn parse_for_phases(items : Array[Datum]) -> ImportPhases raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid import set")
  }
  let mut run = false
  let mut expand = false
  for item in items {
    match item {
      Datum::Symbol("run") => run = true
      Datum::Symbol("expand") => expand = true
      Datum::Pair(_, _) | Datum::Nil => {
        let parts = datum_list_to_array(item)
        if parts.length() != 2 {
          raise @core.EvalError("invalid import set")
        }
        match parts[0] {
          Datum::Symbol("meta") => {
            match parts[1] {
              Datum::Int(n) =>
                if n == 0 {
                  run = true
                } else if n > 0 {
                  expand = true
                } else {
                  raise @core.EvalError("invalid import set")
                }
              _ => raise @core.EvalError("invalid import set")
            }
          }
          _ => raise @core.EvalError("invalid import set")
        }
      }
      _ => raise @core.EvalError("invalid import set")
    }
  }
  if !run && !expand {
    raise @core.EvalError("invalid import set")
  }
  { run, expand }
}

///|
fn filter_exports_by_phase(
  exports : Map[String, Value],
  phases : ImportPhases,
) -> Map[String, Value] {
  if phases.run && phases.expand {
    return exports
  }
  let result : Map[String, Value] = {}
  for key in exports.keys() {
    match exports.get(key) {
      Some(Value::Macro(_)) =>
        if phases.expand {
          result[key] = exports[key]
        }
      Some(Value::SyntaxKeyword(_)) =>
        if phases.expand {
          result[key] = exports[key]
        }
      Some(_) =>
        if phases.run {
          result[key] = exports[key]
        }
      None => ()
    }
  }
  result
}

///|
fn import_set_exports(expr : Datum) -> Map[String, Value] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid import set")
  }
  match items[0] {
    Datum::Symbol("only") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let names = parse_symbol_list(items.sub(start=2).to_array())
      import_set_only(base, names)
    }
    Datum::Symbol("except") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let names = parse_symbol_list(items.sub(start=2).to_array())
      import_set_except(base, names)
    }
    Datum::Symbol("rename") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let renames = parse_rename_specs(items.sub(start=2).to_array())
      import_set_rename(base, renames)
    }
    Datum::Symbol("prefix") => {
      if items.length() != 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let prefix = parse_symbol(items[2])
      import_set_prefix(base, prefix)
    }
    Datum::Symbol("for") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let phases = parse_for_phases(items.sub(start=2).to_array())
      let base = import_set_exports(items[1])
      filter_exports_by_phase(base, phases)
    }
    _ => {
      let reference = parse_library_ref(expr)
      library_exports_from_ref(reference)
    }
  }
}

///|
fn import_set_into_env(expr : Datum, env : Env) -> Unit raise EvalError {
  let exports = import_set_exports(expr)
  for key in exports.keys() {
    match exports.get(key) {
      Some(value) => env_define(env, key, value)
      None => ()
    }
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
    raise @core.EvalError("invalid library")
  }
  let name_spec = parse_library_name_spec(args[0])
  let exports = parse_export_clause(args[1])
  let imports = parse_import_clause(args[2])
  let body = args.sub(start=3).to_array()
  let lib_env = init_env()
  for import_set in imports {
    import_set_into_env(import_set, lib_env)
  }
  for form in body {
    let _ = eval_expr(form, lib_env)
  }
  let export_map : Map[String, Value] = {}
  for export_spec in exports {
    let (external, internal) = export_spec
    if is_syntax_keyword(internal) {
      export_map[external] = Value::SyntaxKeyword(external)
    } else {
      export_map[external] = env_lookup(lib_env, internal)
    }
  }
  register_library_entry(name_spec.base, name_spec.version, export_map)
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
    import_set_into_env(spec, env)
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
            raise @core.EvalError("invalid case")
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
fn feature_enabled(name : String) -> Bool {
  match name {
    "r6rs" => true
    "ratios" => true
    "ieee-float" => true
    "full-unicode" => true
    "exact-closed" => false
    "exact-complex" => false
    _ => false
  }
}

///|
fn cond_expand_feature(expr : Datum) -> Bool raise EvalError {
  match expr {
    Datum::Symbol(name) => feature_enabled(name)
    Datum::Pair(_, _) => {
      let items = datum_list_to_array(expr)
      if items.is_empty() {
        raise @core.EvalError("invalid cond-expand")
      }
      match items[0] {
        Datum::Symbol("and") => {
          if items.length() == 1 {
            true
          } else {
            let mut i = 1
            while i < items.length() {
              if !cond_expand_feature(items[i]) {
                return false
              }
              i = i + 1
            }
            true
          }
        }
        Datum::Symbol("or") => {
          let mut i = 1
          while i < items.length() {
            if cond_expand_feature(items[i]) {
              return true
            }
            i = i + 1
          }
          false
        }
        Datum::Symbol("not") => {
          if items.length() != 2 {
            raise @core.EvalError("invalid cond-expand")
          }
          !cond_expand_feature(items[1])
        }
        Datum::Symbol("library") => {
          if items.length() != 2 {
            raise @core.EvalError("invalid cond-expand")
          }
          let reference = parse_library_ref(items[1])
          select_library_for_ref(reference) is Some(_)
        }
        _ => raise @core.EvalError("invalid cond-expand")
      }
    }
    _ => raise @core.EvalError("invalid cond-expand")
  }
}

///|
fn eval_cond_expand_state(
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      raise @core.EvalError("invalid cond-expand")
    }
    match parts[0] {
      Datum::Symbol("else") => {
        if i != clauses.length() - 1 {
          raise @core.EvalError("invalid cond-expand")
        }
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      }
      _ => {
        if cond_expand_feature(parts[0]) {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
      }
    }
    i = i + 1
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
      Some("define-condition-type") => eval_define_condition_type_state(args, env, kont, handlers)
      Some("define-enumeration") => eval_define_enumeration_state(args, env, kont, handlers)
      Some("quote") => MachineState::Apply(eval_quote(args), kont, handlers)
      Some("quasiquote") => MachineState::Apply(eval_quasiquote(args, env), kont, handlers)
      Some("syntax") => MachineState::Apply(eval_syntax(args), kont, handlers)
      Some("quasisyntax") => MachineState::Apply(eval_quasisyntax(args, env), kont, handlers)
      Some("delay") => MachineState::Apply(eval_delay(args, env), kont, handlers)
      Some("if") => eval_if_state(args, env, kont, handlers)
      Some("begin") => eval_sequence_state(args, env, kont, handlers)
      Some("begin-for-syntax") => eval_begin_for_syntax_state(args, env, kont, handlers)
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
      Some("letrec*") => eval_letrec_star_state(args, env, kont, handlers)
      Some("parameterize") => eval_parameterize_state(args, env, kont, handlers)
      Some("let-syntax") => eval_let_syntax_state(args, env, kont, handlers)
      Some("letrec-syntax") => eval_letrec_syntax_state(args, env, kont, handlers)
      Some("and") => eval_and_state(args, env, kont, handlers)
      Some("or") => eval_or_state(args, env, kont, handlers)
      Some("cond") => cond_start(args, env, kont, handlers)
      Some("cond-expand") => eval_cond_expand_state(args, env, kont, handlers)
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
    raise @core.EvalError("arity mismatch: expected at least 1 list")
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
        args.push(value_from_datum(car.val))
        next_lists.push(cdr.val)
      }
      _ => raise @core.EvalError("type error: proper list expected")
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
      None => raise @core.EvalError("index out of range")
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
    Value::Primitive(Primitive::HashtableRef) =>
      apply_hashtable_ref(args, kont, handlers)
    Value::Primitive(Primitive::HashtableSet) =>
      apply_hashtable_set(args, kont, handlers)
    Value::Primitive(Primitive::HashtableDelete) =>
      apply_hashtable_delete(args, kont, handlers)
    Value::Primitive(Primitive::HashtableContainsP) =>
      apply_hashtable_contains(args, kont, handlers)
    Value::Primitive(Primitive::HashtableUpdate) =>
      apply_hashtable_update(args, kont, handlers)
    Value::Primitive(Primitive::MakeParameter) => {
      match args.length() {
        1 => MachineState::Apply(Value::Parameter(make_parameter(args[0])), kont, handlers)
        2 => {
          let converter = args[1]
          apply_proc(converter, [args[0]], Kont::MakeParameter(converter, kont), handlers)
        }
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
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
    Value::Primitive(Primitive::Eval) => {
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let expr = value_as_datum(args[0])
        let eval_env = value_as_eval_env(args[1])
        MachineState::Eval(expr, eval_env.env, kont, handlers)
      }
    }
    Value::Primitive(Primitive::Environment) => {
      if args.is_empty() {
        raise arity_mismatch(1, 0)
      }
      let env = env_new()
      for arg in args {
        let spec = value_as_datum(arg)
        import_set_into_env(spec, env)
      }
      MachineState::Apply(Value::EvalEnv(make_eval_env(env)), kont, handlers)
    }
    Value::Primitive(Primitive::MakePromise) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let thunk = args[0]
        if !is_procedure_value(thunk) {
          raise @core.EvalError("type error: procedure expected")
        }
        MachineState::Apply(Value::Promise(make_promise(thunk)), kont, handlers)
      }
    }
    Value::Primitive(Primitive::Force) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        match args[0] {
          Value::Promise(promise) =>
            match promise.state.val {
              PromiseState::Value(result) => MachineState::Apply(result, kont, handlers)
              PromiseState::Thunk(thunk) =>
                apply_proc(thunk, [], Kont::ForcePromise(promise, kont), handlers)
            }
          _ => raise @core.EvalError("type error: promise expected")
        }
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
    Value::Primitive(Primitive::Error) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&error", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(make_condition_record("&irritants", [irritants_list_value(irritants)]))
        raise_condition(components, kont, handlers)
      }
    }
    Value::Primitive(Primitive::AssertionViolation) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&assertion", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(make_condition_record("&irritants", [irritants_list_value(irritants)]))
        raise_condition(components, kont, handlers)
      }
    }
    Value::Primitive(Primitive::ImplementationRestrictionViolation) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&implementation-restriction", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(make_condition_record("&irritants", [irritants_list_value(irritants)]))
        raise_condition(components, kont, handlers)
      }
    }
    Value::Primitive(Primitive::UndefinedViolation) => {
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&undefined", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(make_condition_record("&irritants", [irritants_list_value(irritants)]))
        raise_condition(components, kont, handlers)
      }
    }
    Value::Primitive(Primitive::SyntaxViolation) => {
      if args.length() < 4 {
        raise arity_mismatch(4, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let form = args[2]
        let subform = args[3]
        let irritants = args.sub(start=4).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&syntax", [form, subform]))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(make_condition_record("&irritants", [irritants_list_value(irritants)]))
        raise_condition(components, kont, handlers)
      }
    }
    Value::Primitive(Primitive::Raise) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else if handlers.is_empty() {
        raise @core.EvalError("uncaught exception")
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
        raise @core.EvalError("uncaught exception")
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
            raise @core.EvalError("vector length mismatch")
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
            raise @core.EvalError("vector length mismatch")
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
            raise @core.EvalError("string length mismatch")
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
            raise @core.EvalError("string length mismatch")
          }
          lists.push(string_to_char_list(s))
          i = i + 1
        }
        map_step(proc_value, lists, [], true, kont, handlers)
      }
    }
    Value::Primitive(prim) => MachineState::Apply(apply_primitive(prim, args), kont, handlers)
    Value::Closure(cl) => {
      let formals = Formals::{ params: cl.params, rest: cl.rest }
      let bindings = formals_bindings(formals, args)
      let new_env = env_extend(cl.env)
      for binding in bindings {
        let (name, bound_value) = binding
        env_define(new_env, name, bound_value)
      }
      eval_sequence_state(cl.body, new_env, kont, handlers)
    }
    Value::CaseClosure(cl) => {
      let mut matched = false
      let mut matched_formals : Formals? = None
      let mut matched_body : Array[Datum] = []
      for clause in cl.clauses {
        let clause_formals = Formals::{ params: clause.params, rest: clause.rest }
        if formals_accept_len(clause_formals, args.length()) {
          matched = true
          matched_formals = Some(clause_formals)
          matched_body = clause.body
          break
        }
      }
      if !matched {
        if cl.clauses.length() == 1 {
          let clause = cl.clauses[0]
          match clause.rest {
            None => raise arity_mismatch(clause.params.length(), args.length())
            Some(_) => raise arity_mismatch_at_least(clause.params.length(), args.length())
          }
        }
        let expected_parts : Array[String] = []
        for clause in cl.clauses {
          let clause_formals = Formals::{ params: clause.params, rest: clause.rest }
          expected_parts.push(formals_expected_desc(clause_formals))
        }
        let expected = expected_parts.join(", ")
        raise @core.EvalError("arity mismatch: expected one of \{expected} got \{args.length()}")
      }
      let new_env = env_extend(cl.env)
      match matched_formals {
        Some(formals) => {
          let bindings = formals_bindings(formals, args)
          for binding in bindings {
            let (name, bound_value) = binding
            env_define(new_env, name, bound_value)
          }
        }
        None => ()
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
        n => raise @core.EvalError("arity mismatch: expected 0 or 1 got \{n}")
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
    Value::ConditionProc(proc) => apply_condition_proc(proc, args, kont, handlers)
    Value::EnumSetProc(proc) => apply_enum_set_proc(proc, args, kont, handlers)
    _ => raise @core.EvalError("type error: procedure expected")
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

///|
/// Apply hashtable and enumset primitives.
fn apply_hashtable_enumset_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::MakeEqHashtable
    | Primitive::MakeEqvHashtable
    | Primitive::MakeHashtable
    | Primitive::HashtableP
    | Primitive::HashtableSize
    | Primitive::HashtableRef
    | Primitive::HashtableSet
    | Primitive::HashtableDelete
    | Primitive::HashtableContainsP
    | Primitive::HashtableUpdate
    | Primitive::HashtableCopy
    | Primitive::HashtableClear
    | Primitive::HashtableKeys
    | Primitive::HashtableEntries
    | Primitive::HashtableEquivalenceFunction
    | Primitive::HashtableHashFunction
    | Primitive::HashtableMutableP
    | Primitive::MakeEnumeration
    | Primitive::EnumSetUniverse
    | Primitive::EnumSetIndexer
    | Primitive::EnumSetConstructor
    | Primitive::EnumSetP
    | Primitive::EnumSetMemberP
    | Primitive::EnumSetSubsetP
    | Primitive::EnumSetEq
    | Primitive::EnumSetUnion
    | Primitive::EnumSetIntersection
    | Primitive::EnumSetDifference
    | Primitive::EnumSetComplement
    | Primitive::EnumSetProjection
    | Primitive::EnumSetToList
    => Some(apply_hashtable_enumset_primitive_core(prim, args))
    _ => None
  }
}

///|
/// Apply hashtable and enumset primitives (core match).
fn apply_hashtable_enumset_primitive_core(
  prim : Primitive,
  args : Array[Value],
) -> Value raise EvalError {
  match prim {
    Primitive::MakeEqHashtable =>
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeEqvHashtable =>
      match args.length() {
        0 => Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        1 => {
          validate_hashtable_size_arg(args[0])
          Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeHashtable =>
      match args.length() {
        2 | 3 => {
          let hash = args[0]
          if !is_procedure_value(hash) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(args[1])
          if args.length() == 3 {
            validate_hashtable_size_arg(args[2])
          }
          Value::Hashtable(make_hashtable(equiv, Some(hash), true))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::HashtableP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::Hashtable(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableSize =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          Value::Datum(Datum::Int(table.entries.val.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableRef =>
      raise @core.EvalError(
        "internal error: hashtable-ref should be handled in apply_proc",
      )
    Primitive::HashtableSet =>
      raise @core.EvalError(
        "internal error: hashtable-set! should be handled in apply_proc",
      )
    Primitive::HashtableDelete =>
      raise @core.EvalError(
        "internal error: hashtable-delete! should be handled in apply_proc",
      )
    Primitive::HashtableContainsP =>
      raise @core.EvalError(
        "internal error: hashtable-contains? should be handled in apply_proc",
      )
    Primitive::HashtableUpdate =>
      raise @core.EvalError(
        "internal error: hashtable-update! should be handled in apply_proc",
      )
    Primitive::HashtableCopy =>
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          let mutable = if args.length() == 2 {
            value_as_bool(args[1])
          } else {
            table.mutable
          }
          let entries : Array[HashtableEntry] = []
          for entry in table.entries.val {
            // invariant : table.entries.val.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : table.entries.val.length() >= 0
            entries.push(HashtableEntry::{
              key: entry.key,
              value: Ref::new(entry.value.val),
            })
          }
          Value::Hashtable(Hashtable::{
            id: next_hashtable_id(),
            mutable,
            equiv: table.equiv,
            hash: table.hash,
            entries: Ref::new(entries),
          })
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableClear =>
      match args.length() {
        1 | 2 => {
          let table = value_as_hashtable(args[0])
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          if args.length() == 2 {
            validate_hashtable_size_arg(args[1])
          }
          table.entries.val = []
          Value::Void
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableKeys =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let items : Array[Datum] = []
          for entry in table.entries.val {
            // invariant : table.entries.val.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : table.entries.val.length() >= 0
            items.push(value_to_datum_element(entry.key))
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableEntries =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          let key_items : Array[Datum] = []
          let value_items : Array[Datum] = []
          for entry in table.entries.val {
            // invariant : table.entries.val.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : table.entries.val.length() >= 0
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
    Primitive::HashtableEquivalenceFunction =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          hashtable_equiv_to_value(table.equiv)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::HashtableHashFunction =>
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
    Primitive::HashtableMutableP =>
      match args.length() {
        1 => {
          let table = value_as_hashtable(args[0])
          bool_value(table.mutable)
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::MakeEnumeration =>
      match args.length() {
        1 => {
          let names = enum_set_symbol_list(args[0])
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          Value::EnumSet(make_enum_set(names, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetUniverse =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_true(set.universe)
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetIndexer =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Indexer(set)))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetConstructor =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          Value::EnumSetProc(
            make_enum_set_proc(EnumSetProcKind::Constructor(set)),
          )
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetP =>
      match args.length() {
        1 =>
          match args[0] {
            Value::EnumSet(_) => bool_value(true)
            _ => bool_value(false)
          }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetMemberP =>
      match args.length() {
        2 => {
          let name = parse_symbol(value_as_datum(args[0]))
          let set = value_as_enum_set(args[1])
          bool_value(enum_set_member_by_name(set, name))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetSubsetP =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let ok = for i = 0; i < left.members.length(); {
            // invariant : i >= 0 && i <= left.members.length()
            // decreases : left.members.length() - i
            // assert : i <= left.members.length()
            if left.members[i] && !right.members[i] {
              break false
            }
            continue i + 1
          } else {
            true
          }
          bool_value(ok)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetEq =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let ok = for i = 0; i < left.members.length(); {
            // invariant : i >= 0 && i <= left.members.length()
            // decreases : left.members.length() - i
            // assert : i <= left.members.length()
            if left.members[i] != right.members[i] {
              break false
            }
            continue i + 1
          } else {
            true
          }
          bool_value(ok)
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetUnion =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          for i = 0; i < members.length(); {
            // invariant : i >= 0 && i <= members.length()
            // decreases : members.length() - i
            // assert : i <= members.length()
            members[i] = left.members[i] || right.members[i]
            continue i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetIntersection =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          for i = 0; i < members.length(); {
            // invariant : i >= 0 && i <= members.length()
            // decreases : members.length() - i
            // assert : i <= members.length()
            members[i] = left.members[i] && right.members[i]
            continue i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetDifference =>
      match args.length() {
        2 => {
          let left = value_as_enum_set(args[0])
          let right = value_as_enum_set(args[1])
          enum_set_require_same_universe(left, right)
          let members = enum_set_members_all_false(left.universe)
          for i = 0; i < members.length(); {
            // invariant : i >= 0 && i <= members.length()
            // decreases : members.length() - i
            // assert : i <= members.length()
            members[i] = left.members[i] && !right.members[i]
            continue i + 1
          }
          Value::EnumSet(make_enum_set(left.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetComplement =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let members = enum_set_members_all_false(set.universe)
          for i = 0; i < members.length(); {
            // invariant : i >= 0 && i <= members.length()
            // decreases : members.length() - i
            // assert : i <= members.length()
            members[i] = !set.members[i]
            continue i + 1
          }
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        n => raise arity_mismatch(1, n)
      }
    Primitive::EnumSetProjection =>
      match args.length() {
        2 => {
          let source = value_as_enum_set(args[0])
          let target = value_as_enum_set(args[1])
          let members = enum_set_members_all_false(target.universe)
          for i = 0; i < target.universe.length(); {
            // invariant : i >= 0 && i <= target.universe.length()
            // decreases : target.universe.length() - i
            // assert : i <= target.universe.length()
            let name = target.universe[i]
            members[i] = enum_set_member_by_name(source, name)
            continue i + 1
          }
          Value::EnumSet(make_enum_set(target.universe, members))
        }
        n => raise arity_mismatch(2, n)
      }
    Primitive::EnumSetToList =>
      match args.length() {
        1 => {
          let set = value_as_enum_set(args[0])
          let items : Array[Datum] = []
          for i = 0; i < set.universe.length(); {
            // invariant : i >= 0 && i <= set.universe.length()
            // decreases : set.universe.length() - i
            // assert : i <= set.universe.length()
            if set.members[i] {
              items.push(Datum::Symbol(set.universe[i]))
            }
            continue i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    _ =>
      raise @core.EvalError("internal error: apply_hashtable_enumset_primitive dispatch")
  }
}

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
      match args {
        [] => Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          Value::Hashtable(make_hashtable(HashtableEquiv::Eq, None, true))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::MakeEqvHashtable =>
      match args {
        [] => Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          Value::Hashtable(make_hashtable(HashtableEquiv::Eqv, None, true))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::MakeHashtable =>
      match args {
        [hash_value, equiv_value] => {
          if !is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          Value::Hashtable(make_hashtable(equiv, Some(hash_value), true))
        }
        [hash_value, equiv_value, size_value] => {
          if !is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          validate_hashtable_size_arg(size_value)
          Value::Hashtable(make_hashtable(equiv, Some(hash_value), true))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::HashtableP =>
      match args {
        [Value::Hashtable(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableSize =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          Value::Datum(Datum::Int(table.entries.val.length()))
        }
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          let mutable = table.mutable
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
        [table_value, mutable_value] => {
          let table = value_as_hashtable(table_value)
          let mutable = value_as_bool(mutable_value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableClear =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          table.entries.val = []
          Value::Void
        }
        [table_value, size_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          validate_hashtable_size_arg(size_value)
          table.entries.val = []
          Value::Void
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableKeys =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          let items : Array[Datum] = []
          for entry in table.entries.val {
            // invariant : table.entries.val.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : table.entries.val.length() >= 0
            items.push(value_to_datum_element(entry.key))
          }
          Value::Datum(Datum::Vector(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableEntries =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableEquivalenceFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          hashtable_equiv_to_value(table.equiv)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableHashFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          match table.hash {
            Some(hash) => hash
            None => Value::Datum(Datum::Bool(false))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::HashtableMutableP =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          bool_value(table.mutable)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::MakeEnumeration =>
      match args {
        [names_value] => {
          let names = enum_set_symbol_list(names_value)
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          Value::EnumSet(make_enum_set(names, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetUniverse =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          let members = enum_set_members_all_true(set.universe)
          Value::EnumSet(make_enum_set(set.universe, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetIndexer =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          Value::EnumSetProc(make_enum_set_proc(EnumSetProcKind::Indexer(set)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetConstructor =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          Value::EnumSetProc(
            make_enum_set_proc(EnumSetProcKind::Constructor(set)),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetP =>
      match args {
        [Value::EnumSet(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetMemberP =>
      match args {
        [name_value, set_value] => {
          let name = parse_symbol(value_as_datum(name_value))
          let set = value_as_enum_set(set_value)
          bool_value(enum_set_member_by_name(set, name))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetSubsetP =>
      match args {
        [left_value, right_value] => {
          let left = value_as_enum_set(left_value)
          let right = value_as_enum_set(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetEq =>
      match args {
        [left_value, right_value] => {
          let left = value_as_enum_set(left_value)
          let right = value_as_enum_set(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetUnion =>
      match args {
        [left_value, right_value] => {
          let left = value_as_enum_set(left_value)
          let right = value_as_enum_set(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetIntersection =>
      match args {
        [left_value, right_value] => {
          let left = value_as_enum_set(left_value)
          let right = value_as_enum_set(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetDifference =>
      match args {
        [left_value, right_value] => {
          let left = value_as_enum_set(left_value)
          let right = value_as_enum_set(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetComplement =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::EnumSetProjection =>
      match args {
        [source_value, target_value] => {
          let source = value_as_enum_set(source_value)
          let target = value_as_enum_set(target_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::EnumSetToList =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_hashtable_enumset_primitive dispatch")
  }
}

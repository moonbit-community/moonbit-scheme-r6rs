///|
/// Apply hashtable and enumset primitives.
fn apply_hashtable_enumset_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    MakeEqHashtable
    | MakeEqvHashtable
    | MakeHashtable
    | HashtableP
    | HashtableSize
    | HashtableRef
    | HashtableSet
    | HashtableDelete
    | HashtableContainsP
    | HashtableUpdate
    | HashtableCopy
    | HashtableClear
    | HashtableKeys
    | HashtableEntries
    | HashtableEquivalenceFunction
    | HashtableHashFunction
    | HashtableMutableP
    | MakeEnumeration
    | EnumSetUniverse
    | EnumSetIndexer
    | EnumSetConstructor
    | EnumSetP
    | EnumSetMemberP
    | EnumSetSubsetP
    | EnumSetEq
    | EnumSetUnion
    | EnumSetIntersection
    | EnumSetDifference
    | EnumSetComplement
    | EnumSetProjection
    | EnumSetToList
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
    MakeEqHashtable =>
      match args {
        [] => Hashtable(make_hashtable(Eq, None, true))
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          Hashtable(make_hashtable(Eq, None, true))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    MakeEqvHashtable =>
      match args {
        [] => Hashtable(make_hashtable(Eqv, None, true))
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          Hashtable(make_hashtable(Eqv, None, true))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    MakeHashtable =>
      match args {
        [hash_value, equiv_value] => {
          if !is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          Hashtable(make_hashtable(equiv, Some(hash_value), true))
        }
        [hash_value, equiv_value, size_value] => {
          if !is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          validate_hashtable_size_arg(size_value)
          Hashtable(make_hashtable(equiv, Some(hash_value), true))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    HashtableP =>
      match args {
        [Hashtable(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableSize =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          Datum(Int(table.entries.val.length()))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableRef =>
      raise @core.EvalError(
        "internal error: hashtable-ref should be handled in apply_proc",
      )
    HashtableSet =>
      raise @core.EvalError(
        "internal error: hashtable-set! should be handled in apply_proc",
      )
    HashtableDelete =>
      raise @core.EvalError(
        "internal error: hashtable-delete! should be handled in apply_proc",
      )
    HashtableContainsP =>
      raise @core.EvalError(
        "internal error: hashtable-contains? should be handled in apply_proc",
      )
    HashtableUpdate =>
      raise @core.EvalError(
        "internal error: hashtable-update! should be handled in apply_proc",
      )
    HashtableCopy =>
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
          Hashtable(Hashtable::{
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
          Hashtable(Hashtable::{
            id: next_hashtable_id(),
            mutable,
            equiv: table.equiv,
            hash: table.hash,
            entries: Ref::new(entries),
          })
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableClear =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          table.entries.val = []
          Void
        }
        [table_value, size_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          validate_hashtable_size_arg(size_value)
          table.entries.val = []
          Void
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableKeys =>
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
          Datum(Vector(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableEntries =>
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
          Values([
            Datum(Vector(key_items)),
            Datum(Vector(value_items)),
          ])
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableEquivalenceFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          hashtable_equiv_to_value(table.equiv)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableHashFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          match table.hash {
            Some(hash) => hash
            None => Datum(Bool(false))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    HashtableMutableP =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          bool_value(table.mutable)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    MakeEnumeration =>
      match args {
        [names_value] => {
          let names = enum_set_symbol_list(names_value)
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          EnumSet(make_enum_set(names, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetUniverse =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          let members = enum_set_members_all_true(set.universe)
          EnumSet(make_enum_set(set.universe, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetIndexer =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          EnumSetProc(make_enum_set_proc(Indexer(set)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetConstructor =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          EnumSetProc(
            make_enum_set_proc(Constructor(set)),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetP =>
      match args {
        [EnumSet(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetMemberP =>
      match args {
        [name_value, set_value] => {
          let name = parse_symbol(value_as_datum(name_value))
          let set = value_as_enum_set(set_value)
          bool_value(enum_set_member_by_name(set, name))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    EnumSetSubsetP =>
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
    EnumSetEq =>
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
    EnumSetUnion =>
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
          EnumSet(make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    EnumSetIntersection =>
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
          EnumSet(make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    EnumSetDifference =>
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
          EnumSet(make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    EnumSetComplement =>
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
          EnumSet(make_enum_set(set.universe, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    EnumSetProjection =>
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
          EnumSet(make_enum_set(target.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    EnumSetToList =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          let items : Array[Datum] = []
          for i = 0; i < set.universe.length(); {
            // invariant : i >= 0 && i <= set.universe.length()
            // decreases : set.universe.length() - i
            // assert : i <= set.universe.length()
            if set.members[i] {
              items.push(Symbol(set.universe[i]))
            }
            continue i + 1
          }
          Datum(list_from_array(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_hashtable_enumset_primitive dispatch")
  }
}

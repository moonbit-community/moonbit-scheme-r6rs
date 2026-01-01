///|
/// @core.Primitive::Apply hashtable and enumset primitives.
fn apply_hashtable_enumset_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    @core.Primitive::MakeEqHashtable
    | @core.Primitive::MakeEqvHashtable
    | @core.Primitive::MakeHashtable
    | @core.Primitive::HashtableP
    | @core.Primitive::HashtableSize
    | @core.Primitive::HashtableRef
    | @core.Primitive::HashtableSet
    | @core.Primitive::HashtableDelete
    | @core.Primitive::HashtableContainsP
    | @core.Primitive::HashtableUpdate
    | @core.Primitive::HashtableCopy
    | @core.Primitive::HashtableClear
    | @core.Primitive::HashtableKeys
    | @core.Primitive::HashtableEntries
    | @core.Primitive::HashtableEquivalenceFunction
    | @core.Primitive::HashtableHashFunction
    | @core.Primitive::HashtableMutableP
    | @core.Primitive::MakeEnumeration
    | @core.Primitive::EnumSetUniverse
    | @core.Primitive::EnumSetIndexer
    | @core.Primitive::EnumSetConstructor
    | @core.Primitive::EnumSetP
    | @core.Primitive::EnumSetMemberP
    | @core.Primitive::EnumSetSubsetP
    | @core.Primitive::EnumSetEq
    | @core.Primitive::EnumSetUnion
    | @core.Primitive::EnumSetIntersection
    | @core.Primitive::EnumSetDifference
    | @core.Primitive::EnumSetComplement
    | @core.Primitive::EnumSetProjection
    | @core.Primitive::EnumSetToList
    => Some(apply_hashtable_enumset_primitive_core(prim, args))
    _ => None
  }
}

///|
fn clone_hashtable_entries(
  entries : Array[@core.HashtableEntry],
) -> Array[@core.HashtableEntry] {
  entries.map((entry) =>
   @core.HashtableEntry::{
      key: entry.key,
      value: Ref::new(entry.value.val),
    }
  )
}

///|
fn hashtable_clone(table : @core.Hashtable, mutable : Bool) -> @core.Value {
  let entries = clone_hashtable_entries(table.entries.val)
  let cloned = @runtime.make_hashtable(table.equiv, table.hash, mutable)
  cloned.entries.val = entries
  @core.Value::Hashtable(cloned)
}

///|
fn enum_set_members_fill(
  universe : Array[String],
  value : Bool,
) -> Array[Bool] {
  universe.map((_name) => value)
}

///|
fn enum_set_members_all_true(universe : Array[String]) -> Array[Bool] {
  enum_set_members_fill(universe, true)
}

///|
fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] {
  enum_set_members_fill(universe, false)
}

///|
/// @core.Primitive::Apply hashtable and enumset primitives (core match).
fn apply_hashtable_enumset_primitive_core(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    @core.Primitive::MakeEqHashtable =>
      match args {
        [] => @core.Value::Hashtable(
          @runtime.make_hashtable(@core.HashtableEquiv::Eq, None, true),
        )
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          @core.Value::Hashtable(
            @runtime.make_hashtable(@core.HashtableEquiv::Eq, None, true),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::MakeEqvHashtable =>
      match args {
        [] => @core.Value::Hashtable(
          @runtime.make_hashtable(@core.HashtableEquiv::Eqv, None, true),
        )
        [size_value] => {
          validate_hashtable_size_arg(size_value)
          @core.Value::Hashtable(
            @runtime.make_hashtable(@core.HashtableEquiv::Eqv, None, true),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::MakeHashtable =>
      match args {
        [hash_value, equiv_value] => {
          if !@runtime.is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          @core.Value::Hashtable(@runtime.make_hashtable(equiv, Some(hash_value), true))
        }
        [hash_value, equiv_value, size_value] => {
          if !@runtime.is_procedure_value(hash_value) {
            raise @core.EvalError("type error: procedure expected")
          }
          let equiv = hashtable_equiv_from_value(equiv_value)
          validate_hashtable_size_arg(size_value)
          @core.Value::Hashtable(@runtime.make_hashtable(equiv, Some(hash_value), true))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::HashtableP =>
      match args {
        [@core.Value::Hashtable(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableSize =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          @core.Value::Datum(@core.Datum::Int(table.entries.val.length()))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableRef =>
      raise @core.EvalError(
        "internal error: hashtable-ref should be handled in apply_proc",
      )
    @core.Primitive::HashtableSet =>
      raise @core.EvalError(
        "internal error: hashtable-set! should be handled in apply_proc",
      )
    @core.Primitive::HashtableDelete =>
      raise @core.EvalError(
        "internal error: hashtable-delete! should be handled in apply_proc",
      )
    @core.Primitive::HashtableContainsP =>
      raise @core.EvalError(
        "internal error: hashtable-contains? should be handled in apply_proc",
      )
    @core.Primitive::HashtableUpdate =>
      raise @core.EvalError(
        "internal error: hashtable-update! should be handled in apply_proc",
      )
    @core.Primitive::HashtableCopy =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          hashtable_clone(table, table.mutable)
        }
        [table_value, mutable_value] => {
          let table = value_as_hashtable(table_value)
          let mutable = value_as_bool(mutable_value)
          hashtable_clone(table, mutable)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableClear =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          table.entries.val = []
          @core.Value::Void
        }
        [table_value, size_value] => {
          let table = value_as_hashtable(table_value)
          if !table.mutable {
            raise @core.EvalError("hashtable is immutable")
          }
          validate_hashtable_size_arg(size_value)
          table.entries.val = []
          @core.Value::Void
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableKeys =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          let items : Array[@core.Datum] =
            table.entries.val.map((entry) =>
              value_to_datum_element(entry.key)
            )
          @core.Value::Datum(@core.Datum::Vector(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableEntries =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          let key_items : Array[@core.Datum] =
            table.entries.val.map((entry) =>
              value_to_datum_element(entry.key)
            )
          let value_items : Array[@core.Datum] =
            table.entries.val.map((entry) =>
              value_to_datum_element(entry.value.val)
            )
          @core.Value::Values([
            @core.Value::Datum(@core.Datum::Vector(key_items)),
            @core.Value::Datum(@core.Datum::Vector(value_items)),
          ])
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableEquivalenceFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          hashtable_equiv_to_value(table.equiv)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableHashFunction =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          match table.hash {
            Some(hash) => hash
            None => @core.Value::Datum(@core.Datum::Bool(false))
          }
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::HashtableMutableP =>
      match args {
        [table_value] => {
          let table = value_as_hashtable(table_value)
          bool_value(table.mutable)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::MakeEnumeration =>
      match args {
        [names_value] => {
          let names = enum_set_symbol_list(names_value)
          ensure_unique_symbols(names)
          let members = enum_set_members_all_true(names)
          @core.Value::EnumSet(@runtime.make_enum_set(names, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetUniverse =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          let members = enum_set_members_all_true(set.universe)
          @core.Value::EnumSet(@runtime.make_enum_set(set.universe, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetIndexer =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          @core.Value::EnumSetProc(@runtime.make_enum_set_proc(Indexer(set)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetConstructor =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          @core.Value::EnumSetProc(
            @runtime.make_enum_set_proc(Constructor(set)),
          )
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetP =>
      match args {
        [@core.Value::EnumSet(_)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetMemberP =>
      match args {
        [name_value, set_value] => {
          let name = parse_symbol(value_as_datum(name_value))
          let set = value_as_enum_set(set_value)
          bool_value(@runtime.enum_set_member_by_name(set, name))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::EnumSetSubsetP =>
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
    @core.Primitive::EnumSetEq =>
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
    @core.Primitive::EnumSetUnion =>
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
          @core.Value::EnumSet(@runtime.make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::EnumSetIntersection =>
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
          @core.Value::EnumSet(@runtime.make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::EnumSetDifference =>
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
          @core.Value::EnumSet(@runtime.make_enum_set(left.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::EnumSetComplement =>
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
          @core.Value::EnumSet(@runtime.make_enum_set(set.universe, members))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::EnumSetProjection =>
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
            members[i] = @runtime.enum_set_member_by_name(source, name)
            continue i + 1
          }
          @core.Value::EnumSet(@runtime.make_enum_set(target.universe, members))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::EnumSetToList =>
      match args {
        [set_value] => {
          let set = value_as_enum_set(set_value)
          let items : Array[@core.Datum] = []
          for i = 0; i < set.universe.length(); {
            // invariant : i >= 0 && i <= set.universe.length()
            // decreases : set.universe.length() - i
            // assert : i <= set.universe.length()
            if set.members[i] {
              items.push(@core.Datum::Symbol(set.universe[i]))
            }
            continue i + 1
          }
          @core.Value::Datum(list_from_array(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    _ =>
      raise @core.EvalError("internal error: apply_hashtable_enumset_primitive dispatch")
  }
}

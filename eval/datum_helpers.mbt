///|
/// Convert a @core.Datum back to a @core.Value.
fn value_from_datum(datum : @core.Datum) -> @core.Value {
  match datum {
    Record(record) => Record(record)
    Value(value) => value
    _ => Datum(datum)
  }
}

///|
/// Allocate a new pair datum.
fn pair_new(car : @core.Datum, cdr : @core.Datum) -> @core.Datum {
  Pair(Ref::new(car), Ref::new(cdr))
}

///|
/// Convert a proper list datum into an array.
fn datum_list_to_array(
  list : @core.Datum,
) -> Array[@core.Datum] raise @core.EvalError {
  let items : Array[@core.Datum] = []
  for cur = list; true; {
    // invariant : items.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert :
    //   match @runtime.datum_unlabel(cur) {
    //     Pair(_, _) | Nil => true
    //     _ => false
    //   }
    match @runtime.datum_unlabel(cur) {
      Nil => break
      Pair(a, b) => {
        items.push(a.val)
        continue b.val
      }
      _ => raise EvalError("type error: proper list expected")
    }
  } else {
    ()
  }
  items
}

///|
/// Build a list datum from an array.
fn list_from_array(items : Array[@core.Datum]) -> @core.Datum {
  for i = items.length(), tail = Datum::Nil; i > 0; {
    // invariant : i >= 0 && i <= items.length()
    // decreases : i
    // assert : i <= items.length()
    let idx = i - 1
    continue i - 1, pair_new(items[idx], tail)
  } else {
    tail
  }
}

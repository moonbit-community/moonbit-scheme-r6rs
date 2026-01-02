///|
pub fn Port::new_output_string(id : Int, initial : String) -> Port {
  let buffer = Ref::new(initial)
  { id, kind: OutputString(buffer) }
}

///|
pub fn Winder::new_proc(id : Int, before : Value, after : Value) -> Winder {
  { id, kind: Proc(before, after) }
}

///|
pub fn Winder::new_params(id : Int, bindings : Array[ParamBinding]) -> Winder {
  { id, kind: Params(bindings) }
}

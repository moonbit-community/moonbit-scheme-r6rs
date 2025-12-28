///|
test "r6rs do: accumulation" {
  let program =
    #|(do ((k 0 (+ k 1))
    #|     (acc 0 (+ acc k)))
    #|    ((= k 5) acc))
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs do: default step and commands" {
  let program =
    #|(let ((x 0))
    #|  (do ((k 0 (+ k 1))
    #|       (y 5))
    #|      ((= k 3) (list x y))
    #|    (set! x (+ x 2))))
  inspect(value_to_string(eval_program(program)), content="(6 5)")
}

///|
test "r6rs do: no bindings" {
  let program =
    #|(do () ((> 1 0) 42))
  inspect(value_to_string(eval_program(program)), content="42")
}

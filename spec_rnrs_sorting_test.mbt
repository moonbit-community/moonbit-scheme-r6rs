///|
test "rnrs sorting: list-sort" {
  let program = (
    (
      #|(import (rnrs sorting))
      #|(list-sort < '(3 1 2 2))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 2 3)")
}

///|
test "rnrs sorting: list-sort stability" {
  let program = (
    (
      #|(import (rnrs sorting))
      #|(list-sort (lambda (a b) (< (car a) (car b)))
      #|           '((1 . a) (1 . b) (2 . c) (1 . d)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="((1 . a) (1 . b) (1 . d) (2 . c))",
  )
}

///|
test "rnrs sorting: vector-sort" {
  let program = (
    (
      #|(import (rnrs sorting))
      #|(let ((v (vector 3 1 2)))
      #|  (list (vector->list (vector-sort < v))
      #|        (vector->list v)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="((1 2 3) (3 1 2))")
}

///|
test "rnrs sorting: vector-sort!" {
  let program = (
    (
      #|(import (rnrs sorting))
      #|(let ((v (vector 3 1 2)))
      #|  (vector-sort! < v)
      #|  (vector->list v))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3)")
}

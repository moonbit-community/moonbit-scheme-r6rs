///|
test "r6rs libraries: control when/unless" {
  let program = (
    (
      #|(import (rnrs control))
      #|(define x 0)
      #|(when #t (set! x 1) (set! x (+ x 1)))
      #|(unless #t (set! x (+ x 10)))
      #|(define y 0)
      #|(unless #f (set! y 5))
      #|(list x y)
    )
  )
  inspect(value_to_string(eval_program(program)), content="(2 5)")
}

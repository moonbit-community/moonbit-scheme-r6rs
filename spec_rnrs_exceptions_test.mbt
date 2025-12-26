///|
test "r6rs libraries: exceptions" {
  let program = (
    (
      #|(import (rnrs exceptions))
      #|(with-exception-handler
      #|  (lambda (e) 99)
      #|  (lambda () (raise-continuable 1)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="99")
}

///|
test "r6rs libraries: exceptions guard export" {
  let program = (
    (
      #|(import (only (rnrs exceptions) guard))
      #|#t
    )
  )
  inspect(value_to_string(eval_program(program)), content="#t")
}

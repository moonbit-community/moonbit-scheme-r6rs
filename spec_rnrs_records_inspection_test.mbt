///|
test "r6rs libraries: records inspection" {
  let program =
    #|(import (rnrs records inspection))
    #|(record? (cons 1 2))
  inspect(value_to_string(eval_program(program)), content="#f")
}

///|
test "r6rs libraries: records inspection details" {
  let program =
    #|(import (rnrs records syntactic) (rnrs records inspection))
    #|(define-record-type point
    #|  (make-point x y)
    #|  point?
    #|  (x point-x)
    #|  (y point-y))
    #|(define p (make-point 1 2))
    #|(list (record? p)
    #|      (record-type-name (record-rtd p))
    #|      (record-type-field-names (record-rtd p)))
  inspect(value_to_string(eval_program(program)), content="(#t point (x y))")
  expect_error(() => {
    eval_program(
      (
        #|(begin (import (rnrs records inspection)) (record-rtd 1))
      ),
    )
  })
}

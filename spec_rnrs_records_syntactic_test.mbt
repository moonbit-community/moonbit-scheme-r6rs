///|
test "r6rs libraries: records syntactic" {
  let program =
    #|(import (rnrs records syntactic))
    #|(define-record-type point
    #|  (make-point x y)
    #|  point?
    #|  (x point-x)
    #|  (y point-y))
    #|(let ((p (make-point 1 2)))
    #|  (list (point? p) (point-x p) (point-y p)))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="(#t 1 2)",
  )
}

///|
test "r6rs libraries: records syntactic errors" {
  expect_error(() => {
    @scheme-r6rs.eval_program(
      (
        #|(begin (import (rnrs records syntactic)) (define-record-type))
      ),
    )
  })
  expect_error(() => {
    @scheme-r6rs.eval_program(
      (
        #|(begin
        #|  (import (rnrs records syntactic))
        #|  (define-record-type point
        #|    (make-point x)
        #|    point?
        #|    (x)))
      ),
    )
  })
}

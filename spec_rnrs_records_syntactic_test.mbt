///|
test "r6rs libraries: records syntactic" {
  let program = (
    (
      #|(import (rnrs records syntactic))
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(let ((p (make-point 1 2)))
      #|  (list (point? p) (point-x p) (point-y p)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t 1 2)")
}

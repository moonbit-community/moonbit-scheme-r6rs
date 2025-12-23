///|
test "r6rs records: constructor and accessors" {
  let program = (
    #|(define-record-type point
    #|  (make-point x y)
    #|  point?
    #|  (x point-x set-point-x!)
    #|  (y point-y))
    #|(let ((p (make-point 1 2)))
    #|  (list (point? p) (point-x p) (point-y p)))
  )
  inspect(value_to_string(eval_program(program)), content="(#t 1 2)")
}

///|
test "r6rs records: mutator" {
  let program = (
    #|(define-record-type point
    #|  (make-point x y)
    #|  point?
    #|  (x point-x set-point-x!)
    #|  (y point-y))
    #|(let ((p (make-point 1 2)))
    #|  (set-point-x! p 9)
    #|  (point-x p))
  )
  inspect(value_to_string(eval_program(program)), content="9")
}

///|
test "r6rs records: predicate false" {
  let program = (
    #|(define-record-type point
    #|  (make-point x y)
    #|  point?
    #|  (x point-x)
    #|  (y point-y))
    #|(point? 1)
  )
  inspect(value_to_string(eval_program(program)), content="#f")
}

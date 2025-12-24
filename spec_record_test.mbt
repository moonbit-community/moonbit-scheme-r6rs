///|
test "r6rs records: constructor and accessors" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x set-point-x!)
      #|  (y point-y))
      #|(let ((p (make-point 1 2)))
      #|  (list (point? p) (point-x p) (point-y p)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t 1 2)")
}

///|
test "r6rs records: mutator" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x set-point-x!)
      #|  (y point-y))
      #|(let ((p (make-point 1 2)))
      #|  (set-point-x! p 9)
      #|  (point-x p))
    )
  )
  inspect(value_to_string(eval_program(program)), content="9")
}

///|
test "r6rs records: predicate false" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(point? 1)
    )
  )
  inspect(value_to_string(eval_program(program)), content="#f")
}

///|
test "r6rs records: inheritance" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(define-record-type color-point
      #|  (make-color-point x y color)
      #|  color-point?
      #|  (parent point)
      #|  (color color-point-color))
      #|(let ((p (make-color-point 1 2 'red)))
      #|  (list (point? p) (color-point? p) (point-x p) (color-point-color p)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t 1 red)")
}

///|
test "r6rs records: protocol" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(define-record-type color-point
      #|  (make-color-point x y color)
      #|  color-point?
      #|  (parent point)
      #|  (protocol (lambda (p) (lambda (x y) (p x y 'blue))))
      #|  (color color-point-color))
      #|(let ((p (make-color-point 1 2)))
      #|  (color-point-color p))
    )
  )
  inspect(value_to_string(eval_program(program)), content="blue")
}

///|
test "r6rs records: descriptors and flags" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(define-record-type color-point
      #|  (make-color-point x y color)
      #|  color-point?
      #|  (parent point)
      #|  (sealed #t)
      #|  (opaque #t)
      #|  (color color-point-color))
      #|(let* ((p (make-color-point 1 2 'red))
      #|       (rtd (record-rtd p))
      #|       (parent (record-type-parent rtd))
      #|       (ctor (record-constructor (record-constructor-descriptor rtd))))
      #|  (list (record? p)
      #|        (record-type-name rtd)
      #|        (record-type-name parent)
      #|        (record-type-sealed? rtd)
      #|        (record-type-opaque? rtd)
      #|        (record-type-field-names rtd)
      #|        (color-point-color (ctor 3 4 'blue))))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t color-point point #t #t (x y color) blue)",
  )
}

///|
test "r6rs records: procedural accessors and mutators" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(define-record-type color-point
      #|  (make-color-point x y color)
      #|  color-point?
      #|  (parent point)
      #|  (color color-point-color set-color-point-color!))
      #|(let* ((p (make-point 1 2))
      #|       (cp (make-color-point 3 4 'red))
      #|       (rtd-point (record-rtd p))
      #|       (rtd-color (record-rtd cp))
      #|       (pred-point (record-predicate rtd-point))
      #|       (pred-color (record-predicate rtd-color))
      #|       (acc-color (record-accessor rtd-color 2))
      #|       (mut-color (record-mutator rtd-color 2)))
      #|  (mut-color cp 'blue)
      #|  (list (pred-point p)
      #|        (pred-point cp)
      #|        (pred-color cp)
      #|        (pred-color p)
      #|        (acc-color cp)
      #|        (record-type-field-mutable? rtd-color 0)
      #|        (record-type-field-mutable? rtd-color 2)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #f blue #f #t)",
  )
}

///|
test "r6rs records: uid and generative" {
  let program = (
    (
      #|(define-record-type point
      #|  (make-point x y)
      #|  point?
      #|  (x point-x)
      #|  (y point-y))
      #|(define-record-type color-point
      #|  (make-color-point x y color)
      #|  color-point?
      #|  (nongenerative color-uid)
      #|  (x color-point-x)
      #|  (y color-point-y)
      #|  (color color-point-color))
      #|(let* ((p (make-point 1 2))
      #|       (cp (make-color-point 3 4 'red))
      #|       (rtd-p (record-rtd p))
      #|       (rtd-c (record-rtd cp)))
      #|  (list (record-type-generative? rtd-p)
      #|        (record-type-uid rtd-p)
      #|        (record-type-generative? rtd-c)
      #|        (record-type-uid rtd-c)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #f #f color-uid)",
  )
}

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
      #|  (protocol
      #|    (lambda (n)
      #|      (lambda (x y)
      #|        (let ((p (n x y)))
      #|          (p 'blue)))))
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
      #|  (opaque #f)
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
    content="(#t color-point point #t #f (x y color) blue)",
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

///|
test "r6rs records: opaque behavior" {
  let program = (
    (
      #|(define-record-type secret
      #|  (make-secret x)
      #|  secret?
      #|  (opaque #t)
      #|  (x secret-x))
      #|(define-record-type child
      #|  (make-child x y)
      #|  child?
      #|  (parent secret)
      #|  (y child-y))
      #|(let ((s (make-secret 1))
      #|      (c (make-child 2 3)))
      #|  (list (record? s) (record? c)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#f #f)",
  )
}

///|
test "r6rs records: opaque record-rtd error" {
  let program = (
    (
      #|(define-record-type secret
      #|  (make-secret x)
      #|  secret?
      #|  (opaque #t)
      #|  (x secret-x))
      #|(let ((s (make-secret 1)))
      #|  (record-rtd s))
    )
  )
  let result = try? eval_program(program)
  match result {
    Err(err) => inspect(err, content="EvalError(\"record type is opaque\")")
    _ => fail("expected record type is opaque")
  }
}

///|
test "r6rs records: sealed parent error" {
  let program = (
    (
      #|(define-record-type base
      #|  (make-base x)
      #|  base?
      #|  (sealed #t)
      #|  (x base-x))
      #|(define-record-type child
      #|  (make-child x y)
      #|  child?
      #|  (parent base)
      #|  (y child-y))
    )
  )
  let result = try? eval_program(program)
  match result {
    Err(err) => inspect(err, content="EvalError(\"invalid define-record-type\")")
    _ => fail("expected invalid define-record-type")
  }
}

///|
test "r6rs records: procedural descriptors" {
  let program = (
    (
      #|(define rtd
      #|  (make-record-type-descriptor 'point #f #f #f #f
      #|    '#((mutable x) (immutable y))))
      #|(define rcd (make-record-constructor-descriptor rtd #f #f))
      #|(define make-point (record-constructor rcd))
      #|(define acc-x (record-accessor rtd 0))
      #|(define acc-y (record-accessor rtd 1))
      #|(let ((p (make-point 1 2)))
      #|  (list (record? p)
      #|        (record-type-name (record-rtd p))
      #|        (acc-x p)
      #|        (acc-y p)
      #|        (record-type-field-mutable? rtd 0)
      #|        (record-type-field-mutable? rtd 1)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t point 1 2 #t #f)",
  )
}

///|
test "r6rs records: procedural protocol with parent" {
  let program = (
    (
      #|(define rtd1
      #|  (make-record-type-descriptor 'r1 #f #f #f #f
      #|    '#((immutable x) (immutable y))))
      #|(define rtd2
      #|  (make-record-type-descriptor 'r2 rtd1 #f #f #f
      #|    '#((immutable z))))
      #|(define cd1
      #|  (make-record-constructor-descriptor
      #|    rtd1 #f
      #|    (lambda (p)
      #|      (lambda (a b) (p (+ a 1) (+ b 1))))))
      #|(define cd2
      #|  (make-record-constructor-descriptor
      #|    rtd2 cd1
      #|    (lambda (n)
      #|      (lambda (a b c)
      #|        (let ((p (n a b)))
      #|          (p (+ c 1)))))))
      #|(define make-r2 (record-constructor cd2))
      #|(define acc0 (record-accessor rtd2 0))
      #|(define acc1 (record-accessor rtd2 1))
      #|(define acc2 (record-accessor rtd2 2))
      #|(let ((r (make-r2 1 2 3)))
      #|  (list (acc0 r) (acc1 r) (acc2 r)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(2 3 4)",
  )
}

///|
test "r6rs records: procedural nongenerative uid" {
  let program = (
    (
      #|(define rtd1
      #|  (make-record-type-descriptor 'a #f 'u #f #f
      #|    '#((immutable x))))
      #|(define rtd2
      #|  (make-record-type-descriptor 'b #f 'u #f #f
      #|    '#((immutable x))))
      #|(eqv? rtd1 rtd2)
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="#t",
  )
}

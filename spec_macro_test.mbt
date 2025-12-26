///|
test "r6rs macro: define-syntax basics" {
  let program = (
    (
      #|(define-syntax inc
      #|  (syntax-rules ()
      #|    ((inc x) (set! x (+ x 1)))))
      #|(let ((x 1))
      #|  (inc x)
      #|  x)
    )
  )
  inspect(value_to_string(eval_program(program)), content="2")

  let program2 = (
    (
      #|(define-syntax swap!
      #|  (syntax-rules ()
      #|    ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
      #|(let ((x 1) (y 2))
      #|  (swap! x y)
      #|  (list x y))
    )
  )
  inspect(value_to_string(eval_program(program2)), content="(2 1)")
}

///|
test "r6rs macro: syntax-rules literals" {
  let program = (
    (
      #|(define-syntax my-if
      #|  (syntax-rules (then else)
      #|    ((my-if test then a else b) (if test a b))))
      #|(my-if #t then 1 else 2)
    )
  )
  inspect(value_to_string(eval_program(program)), content="1")

  let program2 = (
    (
      #|(define-syntax my-if
      #|  (syntax-rules (then else)
      #|    ((my-if test then a else b) (if test a b))))
      #|(my-if #f then 1 else 2)
    )
  )
  inspect(value_to_string(eval_program(program2)), content="2")
}

///|
test "r6rs macro: quote strips syntax objects" {
  let program = (
    (
      #|(define-syntax literal-pair
      #|  (syntax-rules ()
      #|    ((literal-pair) '(a b))))
      #|(literal-pair)
    )
  )
  inspect(value_to_string(eval_program(program)), content="(a b)")
}

///|
test "r6rs macro: hygiene with internal bindings" {
  let program = (
    (
      #|(define-syntax or2
      #|  (syntax-rules ()
      #|    ((or2 a b)
      #|      (let ((t a))
      #|        (if t t b)))))
      #|(let ((t 1))
      #|  (or2 #f t))
    )
  )
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs macro: dotted list pattern" {
  let program = (
    (
      #|(define-syntax split-dot
      #|  (syntax-rules ()
      #|    ((split-dot (a . b)) (list a b))))
      #|(split-dot (1 . 2))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2)")
}

///|
test "r6rs macro: chained expansions" {
  let program = (
    (
      #|(define-syntax add1
      #|  (syntax-rules ()
      #|    ((add1 x) (+ x 1))))
      #|(define-syntax add2
      #|  (syntax-rules ()
      #|    ((add2 x) (add1 (add1 x)))))
      #|(add2 3)
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs equality predicates" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(eq? 1 1)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(boolean=? #t #t #t)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(boolean=? #t #f)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(symbol=? 'a 'a 'a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(symbol=? 'a 'b)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(eqv? #t #t)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(equal? '(1 2) '(1 2))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(eq? "hi" "hi")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(eq? 'a 'b)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(eq? '(1 2) '(1 3))
        ),
      ),
    ),
    content="#f",
  )
}

///|
test "r6rs list utilities" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list? '())
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list? '(1 2 3))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list? '(1 . 2))
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(length '())
        ),
      ),
    ),
    content="0",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(length '(1 2 3))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(make-list 3 'a)
        ),
      ),
    ),
    content="(a a a)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(length (make-list 4))
        ),
      ),
    ),
    content="4",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list-ref '(a b c) 1)
        ),
      ),
    ),
    content="b",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list-tail '(a b c d) 2)
        ),
      ),
    ),
    content="(c d)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cadr '(1 2 3))
        ),
      ),
    ),
    content="2",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(caddr '(1 2 3 4))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cddr '(1 2 3))
        ),
      ),
    ),
    content="(3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(caar '((1 2) (3 4)))
        ),
      ),
    ),
    content="1",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(member 2 '(1 2 3))
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(memq 2 '(1 2 3))
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(memv 2 '(1 2 3))
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(assoc 'b '((a . 1) (b . 2)))
        ),
      ),
    ),
    content="(b . 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(assq 'b '((a . 1) (b . 2)))
        ),
      ),
    ),
    content="(b . 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(assv 'b '((a . 1) (b . 2)))
        ),
      ),
    ),
    content="(b . 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(map (lambda (x) (+ x 1)) '(1 2 3))
        ),
      ),
    ),
    content="(2 3 4)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(map + '(1 2 3) '(4 5 6))
        ),
      ),
    ),
    content="(5 7 9)",
  )
  let foreach_prog =
    #|(let ((acc 0))
    #|  (for-each (lambda (x) (set! acc (+ acc x))) '(1 2 3))
    #|  acc)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(foreach_prog)),
    content="6",
  )
  let set_car_prog =
    #|(let ((p (cons 1 2)))
    #|  (set-car! p 9)
    #|  p)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(set_car_prog)),
    content="(9 . 2)",
  )
  let set_cdr_prog =
    #|(let ((p (cons 1 2)))
    #|  (set-cdr! p 9)
    #|  p)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(set_cdr_prog)),
    content="(1 . 9)",
  )
  let copy_prog =
    #|(let ((xs (list 1 2 3))
    #|      (ys (list-copy (list 1 2 3))))
    #|  ys)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(copy_prog)),
    content="(1 2 3)",
  )
  let copy_mutate_prog =
    #|(let* ((xs (list 1 2 3))
    #|       (ys (list-copy xs)))
    #|  (set-car! xs 9)
    #|  ys)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(copy_mutate_prog)),
    content="(1 2 3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(append)
        ),
      ),
    ),
    content="()",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(append '(1 2) '(3 4))
        ),
      ),
    ),
    content="(1 2 3 4)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(append '(1 2) '(3 . 4))
        ),
      ),
    ),
    content="(1 2 3 . 4)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((p (lambda (x) x)))
          #|  (eq? p (car (list p))))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((p (lambda (x) x))
          #|      (cell (cons 1 2)))
          #|  (set-car! cell p)
          #|  (procedure? (car cell)))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((p (lambda (x) x))
          #|      (cell (cons 1 2)))
          #|  (set-cdr! cell p)
          #|  (procedure? (cdr cell)))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((p (lambda (x) x)))
          #|  (append '(1) p))
        ),
      ),
    ),
    content="(1 . #<procedure>)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(reverse '(1 2 3))
        ),
      ),
    ),
    content="(3 2 1)",
  )
}

///|
test "r6rs list utilities: errors" {
  try
    @scheme-r6rs.eval_program(
      (
        #|(length '(1 . 2))
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: proper list expected\")")
  } noraise {
    _ => fail("expected EvalError for improper list")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(list-ref '(1 2) 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for list-ref out of range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(list-tail '(1 2) 3)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for list-tail out of range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(make-list -1)
      ),
    )
  catch {
    err =>
      inspect(
        err,
        content="EvalError(\"type error: exact nonnegative integer expected\")",
      )
  } noraise {
    _ => fail("expected EvalError for negative make-list length")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(member 3 '(1 . 2))
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: proper list expected\")")
  } noraise {
    _ => fail("expected EvalError for improper list")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(assoc 'a '(1 2))
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: pair expected\")")
  } noraise {
    _ => fail("expected EvalError for non-pair alist entry")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(map (lambda (x) x) '(1 . 2))
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: proper list expected\")")
  } noraise {
    _ => fail("expected EvalError for improper list in map")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(set-car! '() 1)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: pair expected\")")
  } noraise {
    _ => fail("expected EvalError for set-car! on non-pair")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(list-copy '(1 . 2))
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: proper list expected\")")
  } noraise {
    _ => fail("expected EvalError for improper list in list-copy")
  }
}

///|
test "r6rs equality predicates: arity" {
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(boolean=?)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(boolean=? #t)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(symbol=?)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(symbol=? 'a)")),
    content="#t",
  )
}

///|
test "r6rs equality predicates: type errors" {
  expect_error(() => @scheme-r6rs.eval_program("(boolean=? #t 1)"))
  expect_error(() => @scheme-r6rs.eval_program("(symbol=? 'a 1)"))
}

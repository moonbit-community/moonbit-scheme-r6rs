///|
test "r6rs syntax-case: basic pattern" {
  let program = (
    (
      #|(begin
      #|  (define-syntax when
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((when test body ...)
      #|         #'(if test (begin body ...) (begin))))))
      #|  (when #t 1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program)), content="3")
}

///|
test "r6rs syntax-case: literals respect bindings" {
  let program = (
    (
      #|(begin
      #|  (define x 'top)
      #|  (define-syntax pick
      #|    (lambda (stx)
      #|      (syntax-case stx (x)
      #|        ((_ x) #'(quote literal))
      #|        ((_ y) #'(quote other)))))
      #|  (list (pick x) (let ((x 'local)) (pick x))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(literal other)")
}

///|
test "r6rs syntax-case: fender clauses" {
  let program = (
    (
      #|(begin
      #|  (define-syntax pair-form?
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ x) (pair? (syntax->datum x)) #t)
      #|        ((_ x) #f))))
      #|  (list (pair-form? (a b)) (pair-form? 1)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #f)")
}

///|
test "r6rs syntax-case: with-syntax bindings" {
  let program = (
    (
      #|(begin
      #|  (import (rnrs syntax-case))
      #|  (define-syntax add1
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ n)
      #|         (with-syntax ((m (datum->syntax n (+ (syntax->datum n) 1))))
      #|           m)))))
      #|  (add1 4))
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs syntax-case: syntax template shorthand" {
  let program = (
    (
      #|(begin
      #|  (define-syntax id
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ x) #'x))))
      #|  (id 42))
    )
  )
  inspect(value_to_string(eval_program(program)), content="42")
}

///|
test "r6rs syntax-case: quasisyntax template forms" {
  let program = (
    (
      #|(begin
      #|  (define-syntax make-list
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ a b) #`(list #,a #,b)))))
      #|  (define-syntax splice-list
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ xs) #`(list #,@xs)))))
      #|  (list (make-list 1 2) (splice-list (1 2 3))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="((1 2) (1 2 3))")
}

///|
test "r6rs syntax-case: evaluated transformer expression" {
  let program = (
    (
      #|(begin
      #|  (define-syntax add1-const
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ n)
      #|         (let ((x (syntax->datum n)))
      #|           (datum->syntax n (+ x 1)))))))
      #|  (add1-const 2))
    )
  )
  inspect(value_to_string(eval_program(program)), content="3")
}

///|
test "r6rs syntax-case: vector pattern and template" {
  let program = (
    (
      #|(begin
      #|  (define-syntax swap-vec
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ #(a b)) #'#(b a)))))
      #|  (swap-vec #(1 2)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="#(2 1)")
}

///|
test "r6rs syntax-case: dotted list pattern" {
  let program = (
    (
      #|(begin
      #|  (define-syntax split-dot
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ (a . b)) #'(list a b)))))
      #|  (split-dot (1 . 2)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2)")
}

///|
test "r6rs syntax-case: repeated identifiers match" {
  let program = (
    (
      #|(begin
      #|  (define-syntax same?
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ x x) #'#t)
      #|        ((_ x y) #'#f))))
      #|  (list (same? 1 1) (same? 1 2)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #f)")
}

///|
test "r6rs syntax-case: wildcard with ellipsis" {
  let program = (
    (
      #|(begin
      #|  (define-syntax drop-first
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ _ rest ...) #'(list rest ...)))))
      #|  (drop-first a 1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3)")
}

///|
test "r6rs syntax-case: invalid ellipsis in pattern" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ x ... ...) #'ok))))
      #|  (bad 1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for invalid ellipsis")
  }
}

///|
test "r6rs syntax-case: invalid ellipsis in template" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ x) #'(list ...)))))
      #|  (bad 1))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for invalid template ellipsis")
  }
}

///|
test "r6rs syntax-case: invalid ellipsis depth" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ (x ...)) #'x))))
      #|  (bad (1 2)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for mismatched ellipsis depth")
  }
}

///|
test "r6rs syntax-case: invalid nested ellipsis depth (fewer in template)" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ ((x ...) ...)) #'(list x ...)))))
      #|  (bad ((1 2) (3 4))))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for nested ellipsis mismatch")
  }
}

///|
test "r6rs syntax-case: invalid nested ellipsis depth (more in template)" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ (x ...)) #'(list (list x ...) ...)))))
      #|  (bad (1 2)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for nested ellipsis mismatch")
  }
}

///|
test "r6rs syntax-case: invalid nested ellipsis depth (vector fewer in template)" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ #((x ...) ...)) #'#(x ...)))))
      #|  (bad #((1 2) (3 4))))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for vector ellipsis mismatch")
  }
}

///|
test "r6rs syntax-case: invalid nested ellipsis depth (vector more in template)" {
  let bad = try? eval_program(
    (
      #|(begin
      #|  (define-syntax bad
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ #(x ...)) #'#((x ...) ...)))))
      #|  (bad #(1 2)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-case\")")
    _ => fail("expected syntax-case error for vector ellipsis mismatch")
  }
}

///|
test "r6rs syntax-case: literal ellipsis pattern" {
  let program = (
    (
      #|(begin
      #|  (define-syntax ellipsis-literal
      #|    (lambda (stx)
      #|      (syntax-case stx (...)
      #|        ((_ x ...) #'(quote literal))
      #|        ((_ x y) #'(quote pair))
      #|        ((_ x) #'(quote single)))))
      #|  (list (ellipsis-literal 1 ...) (ellipsis-literal 1 2)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(literal pair)")
}

///|
test "r6rs syntax-case: literal ellipsis template" {
  let program = (
    (
      #|(begin
      #|  (define-syntax show
      #|    (lambda (stx)
      #|      (syntax-case stx (...)
      #|        ((_ x)
      #|         #`(quote (#,x ...))))))
      #|  (show 1))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 ...)")
}

///|
test "r6rs syntax-case: datum->syntax use-site capture" {
  let program = (
    (
      #|(begin
      #|  (define-syntax make-x
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ id) (datum->syntax #'id 'x)))))
      #|  (let ((x 10))
      #|    (make-x y)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs syntax-case: datum->syntax pattern variable context" {
  let program = (
    (
      #|(begin
      #|  (define x 'top)
      #|  (define-syntax make-x
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ id)
      #|         (datum->syntax id 'x)))))
      #|  (let ((x 10))
      #|    (make-x y)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs syntax-case: hygienic internal binding" {
  let program = (
    (
      #|(begin
      #|  (define-syntax or2
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((_ a b) #'(let ((t a)) (if t t b))))))
      #|  (let ((t 1))
      #|    (or2 #f t)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs syntax-case: expression form" {
  let program = (
    (
      #|(begin
      #|  (define stx (syntax (a b)))
      #|  (syntax?
      #|    (syntax-case stx ()
      #|      ((a b) #'ok))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="#t")
}

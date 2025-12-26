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

///|
test "r6rs libraries: syntax-case make-variable-transformer" {
  let program = (
    (
      #|(begin
      #|  (import (rnrs syntax-case))
      #|  (define storage 1)
      #|  (define-syntax counter
      #|    (make-variable-transformer
      #|      (syntax-rules (set! storage)
      #|        ((set! counter v) (set! storage v))
      #|        (counter storage))))
      #|  (list counter
      #|        (begin (set! counter 5) counter)
      #|        (procedure? (make-variable-transformer (lambda (x) x)))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 5 #t)")
}

///|
test "r6rs libraries: syntax-case generate-temporaries" {
  let program = (
    (
      #|(begin
      #|  (import (rnrs syntax-case))
      #|  (define temps (generate-temporaries '(a a)))
      #|  (list (symbol? (car temps))
      #|        (symbol? (cadr temps))
      #|        (eq? (car temps) (cadr temps))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t #f)")
}

///|
test "r6rs libraries: syntax-case value exports" {
  let program = (
    (
      #|(begin
      #|  (define env (environment '(rnrs syntax-case)))
      #|  (list (eval '(identifier? 'x) env)
      #|        (eval '(syntax? 'x) env)
      #|        (eval '(free-identifier=? 'x 'x) env)
      #|        (eval '(bound-identifier=? 'x 'x) env)
      #|        (eval '(syntax->datum (syntax (a b))) env)
      #|        (eval '(datum->syntax 'a '(1 2)) env)
      #|        (procedure? (eval 'syntax-violation env))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t #t #t (a b) (1 2) #t)")
}

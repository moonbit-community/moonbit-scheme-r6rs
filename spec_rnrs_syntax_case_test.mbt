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
      #|  (list (identifier? (car temps))
      #|        (identifier? (cadr temps))
      #|        (syntax? (car temps))
      #|        (syntax? (cadr temps))
      #|        (free-identifier=? (car temps) (cadr temps))
      #|        (bound-identifier=? (car temps) (cadr temps))
      #|        (eq? (syntax->datum (car temps))
      #|             (syntax->datum (cadr temps)))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t #t #t #f #f #f)")
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
      #|        (eval '(syntax->datum (datum->syntax 'a '(1 2))) env)
      #|        (procedure? (eval 'syntax-violation env))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #f #t #t (a b) (1 2) #t)")
}

///|
test "r6rs libraries: syntax-case syntactic keyword exports" {
  let program = (
    (
      #|(begin
      #|  (import (only (rnrs syntax-case)
      #|                syntax-case with-syntax syntax quasisyntax unsyntax unsyntax-splicing))
      #|  #t)
    )
  )
  inspect(value_to_string(eval_program(program)), content="#t")
}

///|
test "r6rs libraries: syntax-case excludes syntax-rules" {
  let bad = try? eval_program(
    (
      #|(import (only (rnrs syntax-case) syntax-rules))
      #|#t
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unknown import: syntax-rules\")")
    _ => fail("expected import error for syntax-rules")
  }
}

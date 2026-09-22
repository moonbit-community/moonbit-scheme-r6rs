///|
test "r6rs libraries: exceptions" {
  let program =
    #|(import (rnrs exceptions))
    #|(with-exception-handler
    #|  (lambda (e) 99)
    #|  (lambda () (raise-continuable 1)))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="99",
  )
}

///|
test "r6rs libraries: exceptions guard export" {
  let program =
    #|(import (only (rnrs exceptions) guard))
    #|#t
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="#t",
  )
}

///|
test "r6rs libraries: syntax-violation optional subform" {
  let program =
    #|(import (rnrs exceptions) (rnrs conditions))
    #|(guard (ex ((syntax-violation? ex)
    #|             (list (syntax-violation-form ex)
    #|                   (syntax-violation-subform ex)))
    #|            (else 'bad))
    #|  (syntax-violation 'who "msg" 'form))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="(form #f)",
  )
}

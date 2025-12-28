///|
test "include: top-level" {
  register_include_source("include/basic.scm", "(define (inc x) (+ x 1))")
  let program =
    #|(begin
    #|  (include "include/basic.scm")
    #|  (inc 2))
  inspect(value_to_string(eval_program(program)), content="3")
}

///|
test "include-ci: fold case" {
  register_include_source("include/case.scm", "(define (MIXED x) (+ x 2))")
  let program =
    #|(begin
    #|  (include-ci "include/case.scm")
    #|  (mixed 3))
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "include: library body" {
  register_include_source("include/lib-body.scm", "(define (square x) (* x x))")
  let program =
    #|(library (test include-lib)
    #|  (export square)
    #|  (import)
    #|  (include "include/lib-body.scm"))
    #|(import (test include-lib))
    #|(square 4)
  inspect(value_to_string(eval_program(program)), content="16")
}

///|
test "include-library-declarations: library body" {
  register_include_source(
    "include/lib-decls.scm", "(define (double x) (+ x x))",
  )
  let program =
    #|(library (test include-decls)
    #|  (export double)
    #|  (import)
    #|  (include-library-declarations "include/lib-decls.scm"))
    #|(import (test include-decls))
    #|(double 5)
  inspect(value_to_string(eval_program(program)), content="10")
}

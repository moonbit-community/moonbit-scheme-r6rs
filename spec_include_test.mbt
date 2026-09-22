///|
test "include: top-level" {
  @scheme-r6rs.register_include_source(
    "include/basic.scm", "(define (inc x) (+ x 1))",
  )
  let program =
    #|(begin
    #|  (include "include/basic.scm")
    #|  (inc 2))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="3",
  )
}

///|
test "include-ci: fold case" {
  @scheme-r6rs.register_include_source(
    "include/case.scm", "(define (MIXED x) (+ x 2))",
  )
  let program =
    #|(begin
    #|  (include-ci "include/case.scm")
    #|  (mixed 3))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="5",
  )
}

///|
test "include: library body" {
  @scheme-r6rs.register_include_source(
    "include/lib-body.scm", "(define (square x) (* x x))",
  )
  let program =
    #|(library (test include-lib)
    #|  (export square)
    #|  (import)
    #|  (include "include/lib-body.scm"))
    #|(import (test include-lib))
    #|(square 4)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="16",
  )
}

///|
test "include-library-declarations: library body" {
  @scheme-r6rs.register_include_source(
    "include/lib-decls.scm", "(define (double x) (+ x x))",
  )
  let program =
    #|(library (test include-decls)
    #|  (export double)
    #|  (import)
    #|  (include-library-declarations "include/lib-decls.scm"))
    #|(import (test include-decls))
    #|(double 5)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="10",
  )
}

///|
test "begin-for-syntax defines macros" {
  let program =
    #|(begin-for-syntax
    #|  (define-syntax mk (syntax-rules () ((_ x) x))))
    #|(mk 7)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="7",
  )
}

///|
test "begin-for-syntax does not export values" {
  try
    @scheme-r6rs.eval_program(
      (
        #|(begin-for-syntax (define x 1))
        #|x
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"unbound variable: x\")")
  } noraise {
    _ => fail("expected EvalError for begin-for-syntax value")
  }
}

///|
test "import for run filters macros" {
  let program =
    #|(library (phase-lib)
    #|  (export val m)
    #|  (import)
    #|  (define val 5)
    #|  (define-syntax m (syntax-rules () ((_ ) 42))))
    #|(import (for (phase-lib) run))
    #|val
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="5",
  )
  try
    @scheme-r6rs.eval_program(
      (
        #|(library (phase-lib)
        #|  (export val m)
        #|  (import)
        #|  (define val 5)
        #|  (define-syntax m (syntax-rules () ((_ ) 42))))
        #|(import (for (phase-lib) run))
        #|(m)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"unbound variable: m\")")
  } noraise {
    _ => fail("expected EvalError for run-only macro")
  }
}

///|
test "import for expand filters values" {
  let program =
    #|(library (phase-lib)
    #|  (export val m)
    #|  (import)
    #|  (define val 5)
    #|  (define-syntax m (syntax-rules () ((_ ) 42))))
    #|(import (for (phase-lib) expand))
    #|(m)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="42",
  )
  try
    @scheme-r6rs.eval_program(
      (
        #|(library (phase-lib)
        #|  (export val m)
        #|  (import)
        #|  (define val 5)
        #|  (define-syntax m (syntax-rules () ((_ ) 42))))
        #|(import (for (phase-lib) expand))
        #|val
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"unbound variable: val\")")
  } noraise {
    _ => fail("expected EvalError for expand-only value")
  }
}

///|
test "import for meta 2 behaves like expand" {
  let program =
    #|(library (phase-lib2)
    #|  (export val m)
    #|  (import)
    #|  (define val 5)
    #|  (define-syntax m (syntax-rules () ((_ ) 42))))
    #|(import (for (phase-lib2) (meta 2)))
    #|(m)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="42",
  )
  try
    @scheme-r6rs.eval_program(
      (
        #|(library (phase-lib2)
        #|  (export val m)
        #|  (import)
        #|  (define val 5)
        #|  (define-syntax m (syntax-rules () ((_ ) 42))))
        #|(import (for (phase-lib2) (meta 2)))
        #|val
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"unbound variable: val\")")
  } noraise {
    _ => fail("expected EvalError for meta-only value")
  }
}

///|
test "import for run expand includes macros and values" {
  let program =
    #|(library (phase-both)
    #|  (export val m)
    #|  (import)
    #|  (define val 7)
    #|  (define-syntax m (syntax-rules () ((_ ) 42))))
    #|(import (for (phase-both) run expand))
    #|(list val (m))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="(7 42)",
  )
}

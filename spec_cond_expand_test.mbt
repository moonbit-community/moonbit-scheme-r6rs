///|
test "r6rs cond-expand" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond-expand (r6rs 1) (else 2))
        ),
      ),
    ),
    content="1",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond-expand ((and r6rs (not no-such)) 3) (else 4))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond-expand (no-such 5) (else 6))
        ),
      ),
    ),
    content="6",
  )
  let lib_prog =
    #|(library (ce-test)
    #|  (export x)
    #|  (import)
    #|  (define x 5))
    #|(import (ce-test))
    #|(cond-expand ((library (ce-test)) x) (else 0))
  inspect(value_to_string(eval_program(lib_prog)), content="5")
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond-expand (ratios 7) (else 8))
        ),
      ),
    ),
    content="7",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond-expand (exact-closed 9) (else 10))
        ),
      ),
    ),
    content="10",
  )
}

///|
test "r6rs extra: pairs, apply, and predicates" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|'(1 . 2)
        ),
      ),
    ),
    content="(1 . 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cdr '(1 . 2))
        ),
      ),
    ),
    content="2",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list)
        ),
      ),
    ),
    content="()",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(apply + 1 2 '(3 4))
        ),
      ),
    ),
    content="10",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cond ((< 1 2)) (else 3))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(procedure? 1)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(symbol? 'a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(boolean? 1)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(number? #t)
        ),
      ),
    ),
    content="#f",
  )
}

///|
test "r6rs extra: mutation and short-circuit" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((x 1))
          #|  (let ((f (lambda () x)))
          #|    (set! x 2)
          #|    (f)))
        ),
      ),
    ),
    content="2",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((x 0))
          #|  (and #f (set! x 1))
          #|  x)
        ),
      ),
    ),
    content="0",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((x 0))
          #|  (or #t (set! x 1))
          #|  x)
        ),
      ),
    ),
    content="0",
  )
}

///|
test "r6rs extra: string escapes" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|"a\n"
        ),
      ),
    ),
    content="\"a\\n\"",
  )
}

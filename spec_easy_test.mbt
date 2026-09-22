///|
test "r6rs easy: numbers and booleans" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(+ 1 2 3)
        ),
      ),
    ),
    content="6",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(+)
        ),
      ),
    ),
    content="0",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(*)
        ),
      ),
    ),
    content="1",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(- 10)
        ),
      ),
    ),
    content="-10",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(- 10 3 2)
        ),
      ),
    ),
    content="5",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(= 2 2 2)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(< 1 2 3)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(< 1 2 2)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|#t
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|#f
        ),
      ),
    ),
    content="#f",
  )
}

///|
test "r6rs easy: lists and quote" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|'abc
        ),
      ),
    ),
    content="abc",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|'()
        ),
      ),
    ),
    content="()",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|'(1 2 3)
        ),
      ),
    ),
    content="(1 2 3)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cons 1 2)
        ),
      ),
    ),
    content="(1 . 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cons 1 (cons 2 ()))
        ),
      ),
    ),
    content="(1 2)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(car '(9 8))
        ),
      ),
    ),
    content="9",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(cdr '(9 8))
        ),
      ),
    ),
    content="(8)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(null? ())
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(pair? '(1 2))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list 1 2 3)
        ),
      ),
    ),
    content="(1 2 3)",
  )
}

///|
test "r6rs easy: strings" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|"hi"
        ),
      ),
    ),
    content="\"hi\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(begin (define s "ok") s)
        ),
      ),
    ),
    content="\"ok\"",
  )
}

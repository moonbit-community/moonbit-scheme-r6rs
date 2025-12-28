///|
test "stdlib: list helpers" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(fold-left + 0 '(1 2 3))
        ),
      ),
    ),
    content="6",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(fold-right cons '() '(1 2 3))
        ),
      ),
    ),
    content="(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(filter (lambda (x) (> x 2)) '(1 2 3 4))
        ),
      ),
    ),
    content="(3 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(remove (lambda (x) (= x 2)) '(1 2 3 2))
        ),
      ),
    ),
    content="(1 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(find (lambda (x) (> x 2)) '(1 2 3 4))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(any odd? '(2 4 5))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(every odd? '(1 3 5))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(take '(1 2 3 4) 2)
        ),
      ),
    ),
    content="(1 2)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(drop '(1 2 3 4) 2)
        ),
      ),
    ),
    content="(3 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(take-while (lambda (x) (< x 3)) '(1 2 3 1))
        ),
      ),
    ),
    content="(1 2)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(drop-while (lambda (x) (< x 3)) '(1 2 3 1))
        ),
      ),
    ),
    content="(3 1)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(append-map (lambda (x) (list x x)) '(1 2))
        ),
      ),
    ),
    content="(1 1 2 2)",
  )
}

///|
test "stdlib: extended helpers" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(count odd? '(1 2 3 4 5))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(list-index (lambda (x) (= x 3)) '(1 2 3 4))
        ),
      ),
    ),
    content="2",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(list-index (lambda (x) (= x 9)) '(1 2 3))
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(partition odd? '(1 2 3 4))
        ),
      ),
    ),
    content="((1 3) (2 4))",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(zip '(1 2) '(a b c))
        ),
      ),
    ),
    content="((1 a) (2 b))",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(unzip '((1 a) (2 b)))
        ),
      ),
    ),
    content="((1 2) (a b))",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(last '(1 2 3))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(string-join '("a" "b" "c") ",")
        ),
      ),
    ),
    content="\"a,b,c\"",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(string-join '() ",")
        ),
      ),
    ),
    content="\"\"",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(list-tabulate 5 (lambda (n) (* n n)))
        ),
      ),
    ),
    content="(0 1 4 9 16)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(list-tabulate 0 (lambda (n) n))
        ),
      ),
    ),
    content="()",
  )
}

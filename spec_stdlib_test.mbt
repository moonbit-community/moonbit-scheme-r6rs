///|
test "stdlib: list helpers" {
  inspect(value_to_string(eval_program("(fold-left + 0 '(1 2 3))")), content="6")
  inspect(value_to_string(eval_program("(fold-right cons '() '(1 2 3))")), content="(1 2 3)")
  inspect(
    value_to_string(eval_program("(filter (lambda (x) (> x 2)) '(1 2 3 4))")),
    content="(3 4)",
  )
  inspect(
    value_to_string(eval_program("(remove (lambda (x) (= x 2)) '(1 2 3 2))")),
    content="(1 3)",
  )
  inspect(value_to_string(eval_program("(find (lambda (x) (> x 2)) '(1 2 3 4))")), content="3")
  inspect(value_to_string(eval_program("(any odd? '(2 4 5))")), content="#t")
  inspect(value_to_string(eval_program("(every odd? '(1 3 5))")), content="#t")
  inspect(value_to_string(eval_program("(take '(1 2 3 4) 2)")), content="(1 2)")
  inspect(value_to_string(eval_program("(drop '(1 2 3 4) 2)")), content="(3 4)")
  inspect(
    value_to_string(eval_program("(take-while (lambda (x) (< x 3)) '(1 2 3 1))")),
    content="(1 2)",
  )
  inspect(
    value_to_string(eval_program("(drop-while (lambda (x) (< x 3)) '(1 2 3 1))")),
    content="(3 1)",
  )
  inspect(
    value_to_string(eval_program("(append-map (lambda (x) (list x x)) '(1 2))")),
    content="(1 1 2 2)",
  )
}

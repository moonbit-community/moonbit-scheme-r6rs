///|
test "r6rs numeric: division and rationals" {
  inspect(value_to_string(eval_program("(/ 1 2)")), content="1/2")
  inspect(value_to_string(eval_program("(/ 6 3)")), content="2")
  inspect(value_to_string(eval_program("(/ 2)")), content="1/2")
  inspect(value_to_string(eval_program("(+ 1/2 1/4)")), content="3/4")
  inspect(value_to_string(eval_program("(* 2 1/2)")), content="1")
  inspect(value_to_string(eval_program("(- 1/2 1/4)")), content="1/4")
  inspect(value_to_string(eval_program("(number? (/ 1 2))")), content="#t")
}

///|
test "r6rs numeric: comparisons and inexact" {
  inspect(value_to_string(eval_program("(> 3 2 1)")), content="#t")
  inspect(value_to_string(eval_program("(> 3 3)")), content="#f")
  inspect(value_to_string(eval_program("(<= 2 2 3)")), content="#t")
  inspect(value_to_string(eval_program("(>= 3 2 2)")), content="#t")
  inspect(value_to_string(eval_program("(= (/ 1 2) 1/2)")), content="#t")
  inspect(value_to_string(eval_program("(= (/ 1.0 2.0) 0.5)")), content="#t")
  inspect(value_to_string(eval_program("(number? 1.0)")), content="#t")
}

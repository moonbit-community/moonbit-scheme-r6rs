///|
test "r6rs libraries: records inspection" {
  let program =
    #|(import (rnrs records inspection))
    #|(record? (cons 1 2))
  inspect(value_to_string(eval_program(program)), content="#f")
}

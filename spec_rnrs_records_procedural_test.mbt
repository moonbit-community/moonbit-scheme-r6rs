///|
test "r6rs libraries: records procedural" {
  let program =
    #|(import (rnrs records procedural))
    #|(define rtd
    #|  (make-record-type-descriptor 'point #f #f #f #f
    #|    '#((mutable x))))
    #|(record-type-descriptor? rtd)
  inspect(value_to_string(eval_program(program)), content="#t")
}

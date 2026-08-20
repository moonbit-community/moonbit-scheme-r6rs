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

///|
test "r6rs libraries: records procedural errors" {
  expect_error(() => {
    eval_program(
      (
        #|(begin
        #|  (import (rnrs records procedural))
        #|  (make-record-constructor-descriptor 1 #f #f))
      ),
    )
  })
  expect_error(() => {
    eval_program(
      (
        #|(begin
        #|  (import (rnrs records procedural))
        #|  (define rtd
        #|    (make-record-type-descriptor 'point #f #f #f #f
        #|      '#((mutable x))))
        #|  (define rcd (make-record-constructor-descriptor rtd #f #f))
        #|  (define ctor (record-constructor rcd))
        #|  (ctor))
      ),
    )
  })
}

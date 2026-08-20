///|
test "r6rs vectors" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|#(1 2 3)
        ),
      ),
    ),
    content="#(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector 1 2 3)
        ),
      ),
    ),
    content="#(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(make-vector 3 'a)
        ),
      ),
    ),
    content="#(a a a)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-length (make-vector 4))
        ),
      ),
    ),
    content="4",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector? #(1 2))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-length #(1 2 3))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-ref #(1 2 3) 1)
        ),
      ),
    ),
    content="2",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((p (lambda (x) x)))
          #|  (procedure? (vector-ref (vector p) 0)))
        ),
      ),
    ),
    content="#t",
  )
  let program =
    #|(let ((v (vector 1 2 3)))
    #|  (vector-set! v 1 9)
    #|  v)
  inspect(value_to_string(eval_program(program)), content="#(1 9 3)")
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((p (lambda (x) x))
          #|      (v (make-vector 2)))
          #|  (vector-set! v 1 p)
          #|  (procedure? (vector-ref v 1)))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((p (lambda (x) x)))
          #|  (let ((v (make-vector 2 p)))
          #|    (eq? p (vector-ref v 0))))
        ),
      ),
    ),
    content="#t",
  )
  let fill_program =
    #|(let ((v (vector 1 2 3)))
    #|  (vector-fill! v 0)
    #|  v)
  inspect(value_to_string(eval_program(fill_program)), content="#(0 0 0)")
  let fill_range_program =
    #|(let ((v (vector 1 2 3 4)))
    #|  (vector-fill! v 9 1 3)
    #|  v)
  inspect(
    value_to_string(eval_program(fill_range_program)),
    content="#(1 9 9 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-copy #(1 2 3))
        ),
      ),
    ),
    content="#(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-copy #(1 2 3) 1)
        ),
      ),
    ),
    content="#(2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-copy #(1 2 3) 1 2)
        ),
      ),
    ),
    content="#(2)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-append)
        ),
      ),
    ),
    content="#()",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-append #(1 2) #(3) (vector 4 5))
        ),
      ),
    ),
    content="#(1 2 3 4 5)",
  )
  let copy_bang_program =
    #|(let ((to (vector 1 2 3 4))
    #|      (from (vector 9 8 7)))
    #|  (vector-copy! to 1 from 0 2)
    #|  to)
  inspect(
    value_to_string(eval_program(copy_bang_program)),
    content="#(1 9 8 4)",
  )
  let copy_overlap_program =
    #|(let ((v (vector 1 2 3 4)))
    #|  (vector-copy! v 1 v 0 3)
    #|  v)
  inspect(
    value_to_string(eval_program(copy_overlap_program)),
    content="#(1 1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-map (lambda (x) (+ x 1)) #(1 2 3))
        ),
      ),
    ),
    content="#(2 3 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector-map + #(1 2) #(3 4))
        ),
      ),
    ),
    content="#(4 6)",
  )
  let for_each_program =
    #|(let ((acc 0))
    #|  (vector-for-each (lambda (x) (set! acc (+ acc x))) #(1 2 3))
    #|  acc)
  inspect(value_to_string(eval_program(for_each_program)), content="6")
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector->list #(1 2 3))
        ),
      ),
    ),
    content="(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector->list #(1 2 3 4) 1)
        ),
      ),
    ),
    content="(2 3 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(vector->list #(1 2 3 4) 1 3)
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(list->vector '(1 2 3))
        ),
      ),
    ),
    content="#(1 2 3)",
  )
}

///|
test "r6rs vectors: equal? comparisons" {
  let program =
    #|(list (equal? #(1 2) #(1 2))
    #|      (equal? #(1 2) #(1 2 3))
    #|      (equal? #(1 2) #(1 3)))
  inspect(value_to_string(eval_program(program)), content="(#t #f #f)")
}

///|
test "r6rs vectors: type errors" {
  expect_error(() => eval_program("(vector-length 'a)"))
  expect_error(() => eval_program("(vector-ref 'a 0)"))
  expect_error(() => eval_program("(bytevector-length 'a)"))
  expect_error(() => eval_program("(bytevector-u8-ref 'a 0)"))
  expect_error(() => {
    eval_program("(vector-ref #(1 2) 123456789012345678901234567890)")
  })
  expect_error(() => {
    eval_program("(bytevector-u8-ref #vu8(1 2) 123456789012345678901234567890)")
  })
}

///|
test "r6rs bytevectors" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|#vu8(1 2 3)
        ),
      ),
    ),
    content="#vu8(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector 1 2 3)
        ),
      ),
    ),
    content="#vu8(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector? #vu8(1 2))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-length #vu8(1 2 3))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-u8-ref #vu8(1 2 3) 2)
        ),
      ),
    ),
    content="3",
  )
  let program =
    #|(let ((b (bytevector 1 2)))
    #|  (bytevector-u8-set! b 1 9)
    #|  b)
  inspect(value_to_string(eval_program(program)), content="#vu8(1 9)")
  let fill_program =
    #|(let ((b (bytevector 1 2 3)))
    #|  (bytevector-fill! b 9)
    #|  b)
  inspect(value_to_string(eval_program(fill_program)), content="#vu8(9 9 9)")
  let fill_range_program =
    #|(let ((b (bytevector 1 2 3 4)))
    #|  (bytevector-fill! b 0 1 3)
    #|  b)
  inspect(
    value_to_string(eval_program(fill_range_program)),
    content="#vu8(1 0 0 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(make-bytevector 3 7)
        ),
      ),
    ),
    content="#vu8(7 7 7)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(make-bytevector 0)
        ),
      ),
    ),
    content="#vu8()",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector->u8-list #vu8(1 2 3))
        ),
      ),
    ),
    content="(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector->u8-list #vu8(1 2 3 4) 1)
        ),
      ),
    ),
    content="(2 3 4)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector->u8-list #vu8(1 2 3 4) 1 3)
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(u8-list->bytevector '(1 2 3))
        ),
      ),
    ),
    content="#vu8(1 2 3)",
  )
  let byte_copy_bang_program =
    #|(let ((to (bytevector 1 2 3 4))
    #|      (from (bytevector 9 8 7)))
    #|  (bytevector-copy! to 1 from 0 2)
    #|  to)
  inspect(
    value_to_string(eval_program(byte_copy_bang_program)),
    content="#vu8(1 9 8 4)",
  )
  let byte_overlap_program =
    #|(let ((b (bytevector 1 2 3 4)))
    #|  (bytevector-copy! b 1 b 0 3)
    #|  b)
  inspect(
    value_to_string(eval_program(byte_overlap_program)),
    content="#vu8(1 1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-copy #vu8(1 2 3))
        ),
      ),
    ),
    content="#vu8(1 2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-copy #vu8(1 2 3) 1)
        ),
      ),
    ),
    content="#vu8(2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-copy #vu8(1 2 3) 1 2)
        ),
      ),
    ),
    content="#vu8(2)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-append)
        ),
      ),
    ),
    content="#vu8()",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(bytevector-append #vu8(1 2) #vu8(3) (bytevector 4 5))
        ),
      ),
    ),
    content="#vu8(1 2 3 4 5)",
  )
}

///|
test "r6rs vectors/bytevectors: errors" {
  try
    eval_program(
      (
        #|(vector-ref #(1 2) 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for out of range")
  }
  try
    eval_program(
      (
        #|(make-vector -1)
      ),
    )
  catch {
    err =>
      inspect(
        err,
        content="EvalError(\"type error: exact nonnegative integer expected\")",
      )
  } noraise {
    _ => fail("expected EvalError for negative length")
  }
  try
    eval_program(
      (
        #|(vector-copy #(1 2 3) 2 1)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid range")
  }
  try
    eval_program(
      (
        #|(vector-append #(1 2) 3)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: vector expected\")")
  } noraise {
    _ => fail("expected EvalError for invalid vector-append argument")
  }
  try
    eval_program(
      (
        #|(vector-map + #(1 2) #(3))
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"vector length mismatch\")")
  } noraise {
    _ => fail("expected EvalError for vector length mismatch")
  }
  try
    eval_program(
      (
        #|(vector-fill! #(1 2 3) 0 3 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid fill range")
  }
  try
    eval_program(
      (
        #|(vector-copy! #(1 2) 1 #(3 4) 0 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid vector-copy! range")
  }
  try
    eval_program(
      (
        #|(bytevector-copy #vu8(1 2 3) 2 1)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid bytevector-copy range")
  }
  try
    eval_program(
      (
        #|(bytevector-append #vu8(1 2) 3)
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: bytevector expected\")")
  } noraise {
    _ => fail("expected EvalError for invalid bytevector-append argument")
  }
  try
    eval_program(
      (
        #|(bytevector-fill! #vu8(1 2 3) 0 3 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid bytevector-fill! range")
  }
  try
    eval_program(
      (
        #|(bytevector-copy! #vu8(1 2) 1 #vu8(3 4) 0 2)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid bytevector-copy! range")
  }
  try
    eval_program(
      (
        #|(list->vector '(1 . 2))
      ),
    )
  catch {
    err =>
      inspect(err, content="EvalError(\"type error: proper list expected\")")
  } noraise {
    _ => fail("expected EvalError for improper list")
  }
  try
    eval_program(
      (
        #|(bytevector 256)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: byte expected\")")
  } noraise {
    _ => fail("expected EvalError for invalid byte")
  }
  try
    eval_program(
      (
        #|(u8-list->bytevector '(1 256))
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: byte expected\")")
  } noraise {
    _ => fail("expected EvalError for invalid u8 list")
  }
}

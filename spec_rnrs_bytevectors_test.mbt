///|
test "r6rs libraries: bytevectors" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(bytevector->u8-list (bytevector 1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3)")
}

///|
test "r6rs libraries: bytevector integer refs" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(let ((bv #vu8(1 2 3 4 255 255 255 255)))
      #|  (list (bytevector-u16-ref bv 0 (endianness big))
      #|        (bytevector-u16-ref bv 0 (endianness little))
      #|        (bytevector-s16-ref bv 4 (endianness big))
      #|        (bytevector-u32-ref bv 0 (endianness big))
      #|        (bytevector-u32-ref bv 0 (endianness little))
      #|        (bytevector-s32-ref bv 4 (endianness big))
      #|        (bytevector-uint-ref bv 0 (endianness big) 2)
      #|        (bytevector-sint-ref bv 4 (endianness little) 2)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(258 513 -1 16909060 67305985 -1 258 -1)",
  )
}

///|
test "r6rs libraries: bytevector integer set!" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(list
      #|  (let ((bv (make-bytevector 4 0)))
      #|    (bytevector-u16-set! bv 0 (endianness big) 258)
      #|    (bytevector-u16-set! bv 2 (endianness little) 772)
      #|    (bytevector->u8-list bv))
      #|  (let ((bv (make-bytevector 2 0)))
      #|    (bytevector-s16-set! bv 0 (endianness big) -1)
      #|    (bytevector->u8-list bv)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="((1 2 4 3) (255 255))",
  )
}

///|
test "r6rs libraries: bytevector s8 ref/set" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(list
      #|  (let ((bv (bytevector 0 127 128 255)))
      #|    (list (bytevector-s8-ref bv 0)
      #|          (bytevector-s8-ref bv 1)
      #|          (bytevector-s8-ref bv 2)
      #|          (bytevector-s8-ref bv 3)))
      #|  (let ((bv (make-bytevector 2 0)))
      #|    (bytevector-s8-set! bv 0 -1)
      #|    (bytevector-s8-set! bv 1 -128)
      #|    (bytevector->u8-list bv)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="((0 127 -128 -1) (255 128))",
  )
}

///|
test "r6rs libraries: bytevector native endianness" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(let ((bv #vu8(1 2)))
      #|  (list (eq? (endianness big) 'big)
      #|        (= (bytevector-u16-native-ref bv 0)
      #|           (bytevector-u16-ref bv 0 (native-endianness)))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t)")
}

///|
test "r6rs libraries: bytevector=?" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(let ((a (bytevector 1 2 3))
      #|      (b (bytevector 1 2 3))
      #|      (c (bytevector 1 2 4))
      #|      (d (bytevector 1 2 3 4)))
      #|  (list (bytevector=? a b)
      #|        (bytevector=? a c)
      #|        (bytevector=? a d)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #f #f)")
}

///|
test "r6rs libraries: bytevector=? errors" {
  let bad = try? eval_program(
    (
      #|(import (rnrs bytevectors))
      #|(bytevector=? #vu8(1 2) '(1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"type error: bytevector expected\")")
    _ => fail("expected EvalError for invalid bytevector=? argument")
  }
}

///|
test "r6rs libraries: utf8 conversions" {
  let program = (
    (
      #|(import (rnrs bytevectors))
      #|(let* ((s (string #\x41 #\x3bb #\x1f600))
      #|       (bv (string->utf8 s)))
      #|  (list (bytevector->u8-list bv)
      #|        (string=? (utf8->string bv) s)
      #|        (bytevector->u8-list (string->utf8 s 1 3))
      #|        (utf8->string (bytevector 65 66 67) 1 3)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="((65 206 187 240 159 152 128) #t (206 187 240 159 152 128) \"BC\")",
  )
}

///|
test "r6rs libraries: utf8->string invalid" {
  let bad = try? eval_program(
    (
      #|(import (rnrs bytevectors))
      #|(utf8->string (bytevector 128))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid utf8 sequence\")")
    _ => fail("expected EvalError for invalid utf8 sequence")
  }
}

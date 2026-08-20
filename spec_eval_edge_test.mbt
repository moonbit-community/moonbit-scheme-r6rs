///|
test "r6rs eval: empty binding forms" {
  let program =
    #|(list
    #|  (let () 1)
    #|  (let* () 2)
    #|  (letrec () 3)
    #|  (letrec* () 4)
    #|  (let-values () 5)
    #|  (let*-values () 6)
    #|  (parameterize () 7))
  inspect(value_to_string(eval_program(program)), content="(1 2 3 4 5 6 7)")
}

///|
test "r6rs eval: invalid binding shapes" {
  try eval_program("(let ((x)) x)") catch {
    err => inspect(err, content="EvalError(\"type error: binding expected\")")
  } noraise {
    _ => fail("expected binding error")
  }
  try eval_program("(let-values ((x 1 2)) x)") catch {
    err => inspect(err, content="EvalError(\"invalid let-values\")")
  } noraise {
    _ => fail("expected let-values error")
  }
  try eval_program("(parameterize ((current-output-port)) 1)") catch {
    err => inspect(err, content="EvalError(\"invalid parameterize\")")
  } noraise {
    _ => fail("expected parameterize error")
  }
}

///|
test "r6rs eval: invalid guard and case" {
  try eval_program("(guard () 1)") catch {
    err => inspect(err, content="EvalError(\"invalid guard\")")
  } noraise {
    _ => fail("expected guard error")
  }
  try eval_program("(case 1 ((1) => (lambda (x) x) 2) (else 0))") catch {
    err => inspect(err, content="EvalError(\"invalid case\")")
  } noraise {
    _ => fail("expected case error")
  }
}

///|
test "r6rs eval: invalid quasisyntax splicing" {
  try eval_program("(quasisyntax (unsyntax-splicing 1))") catch {
    err =>
      inspect(
        err,
        content="EvalError(\"unsyntax-splicing not in list context\")",
      )
  } noraise {
    _ => fail("expected unsyntax-splicing error")
  }
}

///|
test "r6rs eval: invalid import set" {
  try eval_program("(import (for (rnrs)))") catch {
    err => inspect(err, content="EvalError(\"invalid import set\")")
  } noraise {
    _ => fail("expected import set error")
  }
}

///|
test "r6rs eval: invalid library name" {
  try eval_program("(import (1))") catch {
    err => inspect(err, content="EvalError(\"invalid library name\")")
  } noraise {
    _ => fail("expected library name error")
  }
}

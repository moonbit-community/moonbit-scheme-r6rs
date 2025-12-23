///|
test "r6rs numeric: division and rationals" {
  inspect(
    value_to_string(eval_program(
      (
        #|(/ 1 2)
      )
    )),
    content="1/2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(/ 6 3)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(/ 2)
      )
    )),
    content="1/2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(+ 1/2 1/4)
      )
    )),
    content="3/4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(* 2 1/2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(- 1/2 1/4)
      )
    )),
    content="1/4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number? (/ 1 2))
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: comparisons and inexact" {
  inspect(
    value_to_string(eval_program(
      (
        #|(> 3 2 1)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(> 3 3)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(<= 2 2 3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(>= 3 2 2)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= (/ 1 2) 1/2)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= (/ 1.0 2.0) 0.5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number? 1.0)
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: predicates and exactness" {
  inspect(
    value_to_string(eval_program(
      (
        #|(integer? 3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(integer? 1/2)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(rational? 1/2)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(real? 2.5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(exact? 1/2)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? 1/2)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? 2.5)
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: sign and parity predicates" {
  inspect(
    value_to_string(eval_program(
      (
        #|(zero? 0)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(zero? 1/2)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(zero? 0.0)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(positive? 3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(positive? -1/2)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(negative? -2.5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(odd? 3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(odd? -3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(even? 4)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(even? -4)
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: finite/infinite/nan" {
  inspect(
    value_to_string(eval_program(
      (
        #|(finite? 1)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(finite? 1.0)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(infinite? 1.0)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(nan? 1.0)
      )
    )),
    content="#f",
  )
}

///|
test "r6rs numeric: conversions and fractions" {
  inspect(
    value_to_string(eval_program(
      (
        #|(exact->inexact 1/2)
      )
    )),
    content="0.5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact->exact 2.0)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number->string 3)
      )
    )),
    content="\"3\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number->string 1/2)
      )
    )),
    content="\"1/2\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number->string 10 2)
      )
    )),
    content="\"1010\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(number->string -15 16)
      )
    )),
    content="\"-f\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "3")
      )
    )),
    content="3",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "1/2")
      )
    )),
    content="1/2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "2.5")
      )
    )),
    content="2.5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "1010" 2)
      )
    )),
    content="10",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "ff" 16)
      )
    )),
    content="255",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "1/2" 16)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "nope")
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(numerator 3/4)
      )
    )),
    content="3",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(denominator 3/4)
      )
    )),
    content="4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(denominator 5)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(abs -3)
      )
    )),
    content="3",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(abs -1/2)
      )
    )),
    content="1/2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(abs -2.5)
      )
    )),
    content="2.5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(quotient 5 2)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(quotient -5 2)
      )
    )),
    content="-2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(remainder 5 2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(remainder -5 2)
      )
    )),
    content="-1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(modulo 5 2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(modulo -5 2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(modulo 5 -2)
      )
    )),
    content="-1",
  )
}

///|
test "r6rs numeric: reader prefixes and exponent" {
  inspect(
    value_to_string(eval_program(
      (
        #|#b1010
      )
    )),
    content="10",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#o17
      )
    )),
    content="15",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#x1f
      )
    )),
    content="31",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#e1.25
      )
    )),
    content="5/4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= #i3/2 1.5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= 1e3 1000.0)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= 1.25e1 12.5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->number "#x1f")
      )
    )),
    content="31",
  )
}

///|
test "r6rs numeric: gcd" {
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd)
      )
    )),
    content="0",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd 5)
      )
    )),
    content="5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd 54 24)
      )
    )),
    content="6",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd -54 24)
      )
    )),
    content="6",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd 0 10)
      )
    )),
    content="10",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(gcd 48 18 30)
      )
    )),
    content="6",
  )
}

///|
test "r6rs numeric: lcm" {
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm 5)
      )
    )),
    content="5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm 6 8)
      )
    )),
    content="24",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm -6 8)
      )
    )),
    content="24",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm 0 10)
      )
    )),
    content="0",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(lcm 4 6 10)
      )
    )),
    content="60",
  )
}

///|
test "r6rs numeric: min/max" {
  inspect(
    value_to_string(eval_program(
      (
        #|(max 1 3 2)
      )
    )),
    content="3",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(min 1 3 2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(max 1/2 3/4)
      )
    )),
    content="3/4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(min -1 2.5)
      )
    )),
    content="-1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? (min -1 2.5))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(max 2 1.0)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? (max 2 1.0))
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: rounding" {
  inspect(
    value_to_string(eval_program(
      (
        #|(floor 3/2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(ceiling 3/2)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(truncate -3/2)
      )
    )),
    content="-1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(round 5/2)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(round -5/2)
      )
    )),
    content="-2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(floor 1.2)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? (floor 1.2))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(ceiling -1.2)
      )
    )),
    content="-1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? (ceiling -1.2))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(truncate -1.7)
      )
    )),
    content="-1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(round 2.5)
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(inexact? (round 2.5))
      )
    )),
    content="#t",
  )
}

///|
test "r6rs numeric: exact-integer-sqrt" {
  inspect(
    value_to_string(eval_program(
      (
        #|(call-with-values (lambda () (exact-integer-sqrt 10)) (lambda (s r) (list s r)))
      )
    )),
    content="(3 1)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(call-with-values (lambda () (exact-integer-sqrt 9)) (lambda (s r) (list s r)))
      )
    )),
    content="(3 0)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(call-with-values (lambda () (exact-integer-sqrt 0)) (lambda (s r) (list s r)))
      )
    )),
    content="(0 0)",
  )
}

///|
test "r6rs numeric: rationalize" {
  inspect(
    value_to_string(eval_program(
      (
        #|(rationalize 3/2 0)
      )
    )),
    content="3/2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(rationalize 1.25 0.01)
      )
    )),
    content="5/4",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(rationalize -2.5 0.1)
      )
    )),
    content="-5/2",
  )
}

///|
test "r6rs numeric: predicate errors" {
  let bad_odd = try? eval_program(
    (
      #|(odd? 1/2)
    )
  )
  match bad_odd {
    Err(err) => inspect(err, content="EvalError(\"type error: exact integer expected\")")
    _ => fail("expected EvalError for odd? on non-integer")
  }

  let bad_zero = try? eval_program(
    (
      #|(zero? "a")
    )
  )
  match bad_zero {
    Err(err) => inspect(err, content="EvalError(\"type error: number expected\")")
    _ => fail("expected EvalError for zero? on non-number")
  }

  let bad_exact_sqrt = try? eval_program(
    (
      #|(exact-integer-sqrt -1)
    )
  )
  match bad_exact_sqrt {
    Err(err) => inspect(err, content="EvalError(\"type error: exact nonnegative integer expected\")")
    _ => fail("expected EvalError for exact-integer-sqrt on negative")
  }

  let bad_rationalize = try? eval_program(
    (
      #|(rationalize 1 -1)
    )
  )
  match bad_rationalize {
    Err(err) => inspect(err, content="EvalError(\"type error: nonnegative real expected\")")
    _ => fail("expected EvalError for rationalize on negative tolerance")
  }

  let bad_finite = try? eval_program(
    (
      #|(finite? "a")
    )
  )
  match bad_finite {
    Err(err) => inspect(err, content="EvalError(\"type error: number expected\")")
    _ => fail("expected EvalError for finite? on non-number")
  }
}

///|
test "r6rs numeric: radix errors" {
  let bad_radix = try? eval_program(
    (
      #|(string->number "10" 1)
    )
  )
  match bad_radix {
    Err(err) => inspect(err, content="EvalError(\"radix out of range\")")
    _ => fail("expected EvalError for invalid radix")
  }

  let bad_number_radix = try? eval_program(
    (
      #|(number->string 10 37)
    )
  )
  match bad_number_radix {
    Err(err) => inspect(err, content="EvalError(\"radix out of range\")")
    _ => fail("expected EvalError for invalid radix")
  }

  let bad_nonint_radix = try? eval_program(
    (
      #|(number->string 1/2 16)
    )
  )
  match bad_nonint_radix {
    Err(err) => inspect(err, content="EvalError(\"type error: exact integer expected\")")
    _ => fail("expected EvalError for non-integer radix conversion")
  }
}

///|
test "r6rs numeric: quotient errors" {
  let bad_div = try? eval_program(
    (
      #|(quotient 1 0)
    )
  )
  match bad_div {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for quotient by zero")
  }

  let bad_rem = try? eval_program(
    (
      #|(remainder 1 0)
    )
  )
  match bad_rem {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for remainder by zero")
  }

  let bad_mod = try? eval_program(
    (
      #|(modulo 1 0)
    )
  )
  match bad_mod {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for modulo by zero")
  }
}

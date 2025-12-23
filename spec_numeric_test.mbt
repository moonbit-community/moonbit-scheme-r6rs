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

///|
test "r6rs numeric: predicates and exactness" {
  inspect(value_to_string(eval_program("(integer? 3)")), content="#t")
  inspect(value_to_string(eval_program("(integer? 1/2)")), content="#f")
  inspect(value_to_string(eval_program("(rational? 1/2)")), content="#t")
  inspect(value_to_string(eval_program("(real? 2.5)")), content="#t")
  inspect(value_to_string(eval_program("(exact? 1/2)")), content="#t")
  inspect(value_to_string(eval_program("(inexact? 1/2)")), content="#f")
  inspect(value_to_string(eval_program("(inexact? 2.5)")), content="#t")
}

///|
test "r6rs numeric: conversions and fractions" {
  inspect(value_to_string(eval_program("(exact->inexact 1/2)")), content="0.5")
  inspect(value_to_string(eval_program("(inexact->exact 2.0)")), content="2")
  inspect(value_to_string(eval_program("(numerator 3/4)")), content="3")
  inspect(value_to_string(eval_program("(denominator 3/4)")), content="4")
  inspect(value_to_string(eval_program("(denominator 5)")), content="1")
  inspect(value_to_string(eval_program("(abs -3)")), content="3")
  inspect(value_to_string(eval_program("(abs -1/2)")), content="1/2")
  inspect(value_to_string(eval_program("(abs -2.5)")), content="2.5")
  inspect(value_to_string(eval_program("(quotient 5 2)")), content="2")
  inspect(value_to_string(eval_program("(quotient -5 2)")), content="-2")
  inspect(value_to_string(eval_program("(remainder 5 2)")), content="1")
  inspect(value_to_string(eval_program("(remainder -5 2)")), content="-1")
  inspect(value_to_string(eval_program("(modulo 5 2)")), content="1")
  inspect(value_to_string(eval_program("(modulo -5 2)")), content="1")
  inspect(value_to_string(eval_program("(modulo 5 -2)")), content="-1")
}

///|
test "r6rs numeric: gcd" {
  inspect(value_to_string(eval_program("(gcd)")), content="0")
  inspect(value_to_string(eval_program("(gcd 5)")), content="5")
  inspect(value_to_string(eval_program("(gcd 54 24)")), content="6")
  inspect(value_to_string(eval_program("(gcd -54 24)")), content="6")
  inspect(value_to_string(eval_program("(gcd 0 10)")), content="10")
  inspect(value_to_string(eval_program("(gcd 48 18 30)")), content="6")
}

///|
test "r6rs numeric: quotient errors" {
  let bad_div = try? eval_program("(quotient 1 0)")
  match bad_div {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for quotient by zero")
  }

  let bad_rem = try? eval_program("(remainder 1 0)")
  match bad_rem {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for remainder by zero")
  }

  let bad_mod = try? eval_program("(modulo 1 0)")
  match bad_mod {
    Err(err) => inspect(err, content="EvalError(\"division by zero\")")
    _ => fail("expected EvalError for modulo by zero")
  }
}

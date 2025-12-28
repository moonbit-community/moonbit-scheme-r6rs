///|
test "r6rs dynamic-wind: basic ordering" {
  let program =
    #|(let ((out (open-output-string)))
    #|  (dynamic-wind (lambda () (display "a" out))
    #|                (lambda () (display "b" out))
    #|                (lambda () (display "c" out)))
    #|  (get-output-string out))
  inspect(value_to_string(eval_program(program)), content="\"abc\"")
}

///|
test "r6rs dynamic-wind: continuation re-entry" {
  let program =
    #|(begin
    #|  (define out (open-output-string))
    #|  (define k #f)
    #|  (dynamic-wind (lambda () (display "A" out))
    #|                (lambda ()
    #|                  (call/cc (lambda (kk)
    #|                             (set! k kk)
    #|                             (display "B" out)))
    #|                  (display "D" out))
    #|                (lambda () (display "C" out)))
    #|  (if k (let ((kk k)) (set! k #f) (kk 0)) #f)
    #|  (get-output-string out))
  inspect(value_to_string(eval_program(program)), content="\"ABDCADC\"")
}

///|
test "r6rs parameterize: dynamic parameters" {
  let program =
    #|(begin
    #|  (define p (make-parameter 10))
    #|  (list (p) (parameterize ((p 20)) (p)) (p)))
  inspect(value_to_string(eval_program(program)), content="(10 20 10)")
}

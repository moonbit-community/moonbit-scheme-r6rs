///|
test "r6rs libraries: unicode" {
  let program =
    #|(import (rnrs unicode))
    #|(string-upcase "aB")
  inspect(value_to_string(eval_program(program)), content="\"AB\"")
}

///|
test "r6rs libraries: unicode categories" {
  let program =
    #|(import (rnrs unicode))
    #|(list (char-general-category #\A)
    #|      (char-general-category #\a)
    #|      (char-general-category #\5)
    #|      (char-general-category #\space)
    #|      (char-general-category #\newline)
    #|      (char-general-category #\x28)
    #|      (char-general-category #\x29)
    #|      (char-general-category #\x2D)
    #|      (char-general-category #\x5F)
    #|      (char-general-category #\x24)
    #|      (char-general-category #\x2B)
    #|      (char-general-category #\x2E))
  inspect(
    value_to_string(eval_program(program)),
    content="(Lu Ll Nd Zs Cc Ps Pe Pd Pc Sc Sm Po)",
  )
}

///|
test "r6rs libraries: unicode normalization" {
  let program =
    #|(import (rnrs unicode))
    #|(list (map char->integer (string->list (string-normalize-nfd "\x00C5;")))
    #|      (map char->integer (string->list (string-normalize-nfc "A\x030A;")))
    #|      (map char->integer (string->list (string-normalize-nfkd "\x212B;")))
    #|      (map char->integer (string->list (string-normalize-nfkc "\x212B;")))
    #|      (map char->integer (string->list (string-normalize-nfd "a\x0301;\x0327;")))
    #|      (map char->integer (string->list (string-normalize-nfd "\xAC00;")))
    #|      (map char->integer (string->list (string-normalize-nfc "\x1100;\x1161;"))))
  inspect(
    value_to_string(eval_program(program)),
    content="((65 778) (197) (65 778) (197) (97 807 769) (4352 4449) (44032))",
  )
}

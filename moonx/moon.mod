name = "bobzhang/scheme"

version = "0.1.0"

import {
  "bobzhang/scheme-r6rs@0.1.0",
  "moonbitlang/async@0.21.0",
}

license = "MIT"

description = "Run R6RS Scheme programs directly with moonx"

keywords = [ "scheme", "r6rs", "interpreter", "cli", "wasm" ]

readme = "README.md"

repository = "https://github.com/moonbit-community/moonbit-scheme-r6rs"

source = "."

preferred_target = "wasm"

warnings = "+test_unqualified_package+unnecessary_view_op+unnecessary_annotation+deprecated+missing_doc"

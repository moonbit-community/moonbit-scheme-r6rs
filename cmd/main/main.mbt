///|
let help_message : String =
  #|R6RS Scheme interpreter (MoonBit)
  #|
  #|Usage:
  #|  scheme-r6rs [OPTIONS] [FILE]
  #|
  #|Options:
  #|  -e, --eval EXPR     Evaluate EXPR and print result
  #|  -                  Read program from stdin
  #|  -h, --help          Show this help
  #|  --version           Print version
  #|
  #|Notes:
  #|  - If FILE is omitted, stdin is used.
  #|

///|
const VERSION : String = "0.1.0"

///|
struct Options {
  mut eval : String?
  mut path : String?
  mut show_help : Bool
  mut show_version : Bool
}

///|
async fn print_usage() -> Unit {
  @stdio.stdout.write(help_message)
}

///|
async fn print_version() -> Unit {
  @stdio.stdout.write("scheme-r6rs \{VERSION}\n")
}

///|
async fn read_program_source(path : String?) -> String {
  match path {
    None | Some("-") => @stdio.stdin.read_all().text()
    Some(p) => @fs.read_file(p).text()
  }
}

///|
async fn eval_and_print(src : String) -> Unit {
  let result = try? @scheme_r6rs.eval_program(src)
  match result {
    Ok(value) =>
      @stdio.stdout..write(@scheme_r6rs.value_to_string(value))..write("\n")
    Err(err) => {
      @stdio.stderr.write("error: \{err}\n")
      exit(1)
    }
  }
}

///|
async fn main {
  let options : Options = {
    eval: None,
    path: None,
    show_help: false,
    show_version: false,
  }

  loop @env.args()[1:] {
    [] => ()
    ["-h" | "--help", ..] => {
      options.show_help = true
      break
    }
    ["--version", ..] => {
      options.show_version = true
      break
    }
    ["-e" | "--eval" as opt, .. rest] => {
      guard rest is [expr, .. rest] else {
        @stdio.stderr.write("error: expected EXPR after \{opt}\n")
        print_usage()
        exit(2)
      }
      options.eval = Some(expr)
      continue rest
    }
    ["-", .. rest] => {
      options.path = Some("-")
      continue rest
    }
    [arg, ..] if arg.has_prefix("-") => {
      @stdio.stderr.write("error: unknown option `\{arg}`\n")
      print_usage()
      exit(2)
    }
    [path, .. rest] => {
      options.path = Some(path)
      continue rest
    }
  }

  if options.show_help {
    print_usage()
    return
  }
  if options.show_version {
    print_version()
    return
  }

  match options.eval {
    Some(src) => eval_and_print(src)
    None => {
      let src = read_program_source(options.path)
      eval_and_print(src)
    }
  }
}

///|
#cfg(target="native")
extern "C" fn exit(code : Int) = "exit"

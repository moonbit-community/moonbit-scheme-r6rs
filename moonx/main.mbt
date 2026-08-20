///|
let help_message : String =
  #|R6RS Scheme interpreter (MoonBit)
  #|
  #|Usage:
  #|  scheme [OPTIONS] [FILE]
  #|
  #|Options:
  #|  -e, --eval EXPR     Evaluate EXPR and print the result
  #|  -                   Read a program from stdin
  #|  -h, --help          Show this help
  #|  --version           Print the version
  #|
  #|If FILE is omitted, the program is read from stdin.
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
fn write_stdout(text : StringView) -> Unit {
  @miniio.stdout.write_text(text) catch {
    _ => ()
  }
}

///|
fn write_stderr(text : StringView) -> Unit {
  @miniio.stderr.write_text(text) catch {
    _ => ()
  }
}

///|
fn print_usage() -> Unit {
  write_stdout(help_message)
}

///|
fn print_version() -> Unit {
  write_stdout("scheme \{VERSION}\n")
}

///|
fn read_program_source(path : String?) -> String raise {
  match path {
    None | Some("-") => @miniio.stdin.read_all().text()
    Some(path) => @miniio.read_text_file(path)
  }
}

///|
fn eval_and_print(source : String) -> Unit {
  try @scheme_r6rs.eval_program(source) catch {
    err => {
      write_stderr("scheme: \{err}\n")
      @miniio.exit(1)
    }
  } noraise {
    value => {
      write_stdout(@scheme_r6rs.value_to_string(value))
      write_stdout("\n")
    }
  }
}

///|
fn main {
  let argv = @miniio.args_get() catch {
    err => {
      write_stderr("scheme: failed to read arguments: \{err}\n")
      @miniio.exit(2)
      return
    }
  }
  let options : Options = {
    eval: None,
    path: None,
    show_help: false,
    show_version: false,
  }

  for args = argv[1:] {
    match args {
      [] => break
      ["-h" | "--help", ..] => {
        options.show_help = true
        break
      }
      ["--version", ..] => {
        options.show_version = true
        break
      }
      ["-e" | "--eval" as option, .. rest] => {
        guard rest is [expression, .. rest] else {
          write_stderr("scheme: expected EXPR after \{option}\n")
          print_usage()
          @miniio.exit(2)
          return
        }
        options.eval = Some(expression)
        continue rest
      }
      ["-", .. rest] => {
        options.path = Some("-")
        continue rest
      }
      [arg, ..] if arg.has_prefix("-") => {
        write_stderr("scheme: unknown option `\{arg}`\n")
        print_usage()
        @miniio.exit(2)
        return
      }
      [path, .. rest] => {
        options.path = Some(path)
        continue rest
      }
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
    Some(source) => eval_and_print(source)
    None => {
      let source = read_program_source(options.path) catch {
        err => {
          write_stderr("scheme: \{err}\n")
          @miniio.exit(1)
          return
        }
      }
      eval_and_print(source)
    }
  }
}

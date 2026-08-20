///|
const VERSION : String = "0.1.1"

///|
priv enum InputSource {
  Eval(String)
  Stdin
  File(String)
}

///|
priv enum CliParseResult {
  Error(String)
  Run(InputSource)
}

///|
fn scheme_command() -> @argparse.Command {
  Command(
    "scheme",
    about="Run an R6RS Scheme expression or program.",
    version="scheme \{VERSION}",
    options=[
      OptionArg(
        "eval",
        short='e',
        about="Evaluate EXPR and print the result.",
        allow_hyphen_values=true,
        conflicts_with=["file"],
      ),
    ],
    positionals=[
      PositionArg(
        "file",
        about="Scheme source file, or - for stdin.",
        num_args=ValueRange(lower=0, upper=1),
        conflicts_with=["eval"],
      ),
    ],
    disable_help_subcommand=true,
  )
}

///|
fn parse_cli() -> CliParseResult {
  let command = scheme_command()
  let matches = command.parse() catch { err => return Error(err.to_string()) }
  match matches.values.get("eval").unwrap_or([]) {
    [source] => Run(Eval(source))
    [] =>
      match matches.values.get("file").unwrap_or([]) {
        [] | ["-"] => Run(Stdin)
        [path] => Run(File(path))
        _ => Error("scheme: expected at most one FILE")
      }
    _ => Error("scheme: expected one EXPR after --eval")
  }
}

///|
async fn read_program_source(source : InputSource) -> String {
  match source {
    Eval(source) => source
    Stdin => @stdio.stdin.read_all().text()
    File(path) => @fs.read_file(path).text()
  }
}

///|
async fn eval_and_print(source : String) -> Unit {
  try @scheme_r6rs.eval_program(source) catch {
    err => {
      @stdio.stderr.write("scheme: \{err}\n")
      exit_process(1)
    }
  } noraise {
    value => @stdio.stdout.write("\{@scheme_r6rs.value_to_string(value)}\n")
  }
}

///|
async fn main {
  match parse_cli() {
    Error(message) => {
      @stdio.stderr.write(message)
      if !message.has_suffix("\n") {
        @stdio.stderr.write("\n")
      }
      exit_process(2)
    }
    Run(input) => {
      let source = read_program_source(input) catch {
        err => {
          @stdio.stderr.write("scheme: \{err}\n")
          exit_process(1)
          return
        }
      }
      eval_and_print(source)
    }
  }
}

///|
fn exit_process(code : Int) = "wasi_snapshot_preview1" "proc_exit"

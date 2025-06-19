import clip
import clip/arg
import clip/opt
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import shellout
import simplifile

import starc/codegen
import starc/codegen/env.{type CodegenError}
import starc/lexer
import starc/parser
import starc/serializer

type Command {
  Command(input: String, output: Option(String))
}

type CompileError {
  FileError(simplifile.FileError)
  LexerError(lexer.LexerError)
  ParserError(String)
  CodegenError(CodegenError)
}

fn compile(cmd: Command) -> Result(Nil, String) {
  let Command(input:, output:) = cmd

  let output =
    option.lazy_unwrap(output, fn() {
      let filename = case string.split(input, ".") {
        [name] -> name
        parts ->
          list.take(parts, list.length(parts) - 1)
          |> string.join(".")
      }
      filename <> ".s"
    })

  let res = {
    use input <- result.try(
      simplifile.read(input)
      |> result.map_error(FileError),
    )

    use tokens <- result.try(
      lexer.lex_program(input)
      |> result.map_error(LexerError),
    )

    use tree <- result.try(
      parser.parse_program(tokens)
      |> result.map_error(ParserError),
    )

    use ir <- result.try(
      codegen.generate_program(tree)
      |> result.map_error(CodegenError),
    )

    let assembly = serializer.serialize_program(ir)

    use _ <- result.try(
      simplifile.write(output, assembly)
      |> result.map_error(FileError),
    )

    Ok(Nil)
  }

  result.map_error(res, fn(err) {
    case err {
      FileError(err) -> "File error: " <> simplifile.describe_error(err)
      LexerError(lexer.UnexpectedToken(at:, next:)) ->
        "Lexer error: Unexpected token "
        <> next
        <> " at row "
        <> int.to_string(at.line)
        <> " col "
        <> int.to_string(at.char)
      ParserError(msg) -> "Parser error: " <> msg
      CodegenError(err) -> "Codegen error: " <> string.inspect(err)
    }
  })
}

pub fn main() {
  let res = {
    use cmd <- result.try(
      clip.command({
        use input <- clip.parameter
        use output <- clip.parameter
        Command(input:, output: option.from_result(output))
      })
      |> clip.arg(arg.new("input"))
      |> clip.opt(opt.new("output") |> opt.short("o") |> opt.optional())
      |> clip.run(shellout.arguments()),
    )

    compile(cmd)
  }

  case res {
    Ok(_) -> io.println("Success")
    Error(err) -> {
      io.println_error("Error: " <> err)
      shellout.exit(1)
    }
  }
}

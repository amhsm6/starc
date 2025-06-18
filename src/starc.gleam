import clip
import clip/arg
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import shellout
import simplifile

import starc/codegen
import starc/codegen/env.{type CodegenError}
import starc/lexer
import starc/parser

type Command {
  Command(input: String)
}

type CompileError {
  FileError(simplifile.FileError)
  LexerError(lexer.LexerError)
  ParserError(String)
  CodegenError(CodegenError)
}

fn compile(path: String) -> Result(Nil, String) {
  let filename = case string.split(path, ".") {
    [name] -> name
    parts ->
      list.take(parts, list.length(parts) - 1)
      |> string.join(".")
  }
  let output = filename <> ".s"

  let res = {
    use input <- result.try(
      simplifile.read(path)
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

    let assembly = codegen.serialize_program(ir)

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
        <> " at row"
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
    use Command(input:) <- result.try(
      clip.command(Command)
      |> clip.arg(arg.new("input"))
      |> clip.run(shellout.arguments()),
    )

    compile(input)
  }

  case res {
    Ok(_) -> io.println("Success")
    Error(err) -> {
      io.println_error("Error: " <> err)
      shellout.exit(1)
    }
  }
}

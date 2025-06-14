import gleam/io
import gleam/result
import gleam/string
import simplifile

import starc/codegen
import starc/codegen/env.{type CodegenError}
import starc/lexer
import starc/parser

type Error {
  FileError(simplifile.FileError)
  LexerError(lexer.LexerError)
  ParserError(String)
  CodegenError(CodegenError)
}

pub fn main() {
  let res = {
    use input <- result.try(
      simplifile.read("./test.star")
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

    Ok(ir)
  }

  case res {
    Ok(program) -> io.println(string.inspect(program))
    Error(err) -> io.println("Error: " <> string.inspect(err))
  }
}

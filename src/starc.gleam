import gleam/io
import gleam/result
import gleam/string
import simplifile

import starc/lexer
import starc/parser
import starc/sa

type Error {
  FileError(simplifile.FileError)
  LexerError(lexer.LexerError)
  ParserError(String)
  SemanticError(sa.Error)
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

    use _ <- result.try(
      sa.analyze(tree)
      |> result.map_error(SemanticError),
    )

    Ok(tree)
  }

  case res {
    Ok(program) -> io.println(string.inspect(program))
    Error(err) -> io.println("Error: " <> string.inspect(err))
  }
}

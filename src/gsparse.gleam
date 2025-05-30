import gleam/io
import gleam/result
import gleam/string
import gsparse/lexer
import simplifile

type Error {
  FileError(simplifile.FileError)
  LexerError(lexer.LexerError)
}

pub fn main() {
  let res = {
    use input <- result.try(
      simplifile.read("./prog")
      |> result.map_error(FileError),
    )

    use tokens <- result.try(
      lexer.lex(input)
      |> result.map_error(LexerError),
    )

    io.println("Successfully parsed: " <> string.inspect(tokens))

    Ok(Nil)
  }

  case res {
    Ok(_) -> Nil
    Error(err) -> io.println("Error: " <> string.inspect(err))
  }
}

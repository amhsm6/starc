import gleam/int
import gleam/list
import gleam/string
import simplifile

pub fn main() {
  let assert Ok(_) =
    list.repeat("", times: 1_000_000)
    |> list.map(fn(_) {
      case int.random(10 + 2) {
        10 -> " "
        11 -> "\n"
        digit -> int.to_string(digit)
      }
    })
    |> string.join(with: "")
    |> simplifile.write(to: "./prog")
}

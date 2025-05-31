import gleam/set.{type Set}

pub fn alpha() -> Set(String) {
  set.from_list([
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o",
    "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
  ])
}

pub fn digits() -> Set(String) {
  set.from_list(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])
}

pub fn underscore() -> Set(String) {
  set.from_list(["_"])
}

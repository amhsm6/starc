pub type Program =
  List(Procedure)

pub type Procedure {
  Procedure(label: String, ty: Type, body: List(Statement))
}

pub type Statement {
  StackPush
}

pub type Type {
  Void
  Int
  Float
  Bool
  Function(args: List(Type), return: Type)
}

pub fn size_of(ty: Type) -> Int {
  case ty {
    Void -> panic
    Int -> 4
    Float -> 4
    Bool -> 1
    Function(..) -> panic
  }
}

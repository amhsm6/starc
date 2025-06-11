pub type Program =
  List(Function)

pub type Function {
  Function(label: String, body: List(Statement))
}

pub type Statement {
  ReserveStack(bytes: Int)
  ClearStack(bytes: Int)

  Load(reg: Register, addr: Int)
  Store(addr: Int, reg: Register)
}

pub type Register {
  RAX
  RBX
  RCX
  RDX
  RDI
  RSI
}

pub type Type {
  Void
  Int
  Float
  Bool
  Pointer(Type)
}

pub fn size_of(ty: Type) -> Int {
  case ty {
    Void -> panic
    Int -> 4
    Float -> 4
    Bool -> 1
    Pointer(..) -> 8
  }
}

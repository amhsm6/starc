pub type Program =
  List(Function)

pub type Function {
  Function(label: String, body: List(Statement))
}

pub type Statement {
  Prologue(reserve_bytes: Int)
  Epilogue(clear_bytes: Int)

  Move(to: Value, from: Value)
}

pub type Value {
  Immediate(Int)
  Register(Register)
  FrameOffset(Int)
  Address(Int)
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
  Bool
  Pointer(Type)
}

pub fn size_of(ty: Type) -> Int {
  case ty {
    Void -> panic
    Int -> 4
    Bool -> 1
    Pointer(..) -> 8
  }
}

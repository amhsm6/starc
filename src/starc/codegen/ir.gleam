pub type Program =
  List(Statement)

pub type Statement {
  Label(String)

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
  EAX
  AX
  AH
  AL

  RBX
  EBX
  BX
  BH
  BL

  RCX
  ECX
  CX
  CH
  CL

  RDX
  EDX
  DX
  DH
  DL

  RDI
  EDI
  DI
  DIL

  RSI
  ESI
  SI
  SIL
}

pub type Type {
  Void
  Bool
  Int8
  Int16
  Int32
  Int64
  Pointer(Type)
}

pub fn size_of(ty: Type) -> Int {
  case ty {
    Void -> panic
    Bool -> 1
    Int8 -> 1
    Int16 -> 2
    Int32 -> 4
    Int64 -> 8
    Pointer(..) -> 8
  }
}

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

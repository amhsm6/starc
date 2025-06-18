pub type Program =
  List(Statement)

pub type Statement {
  Label(String)

  Prologue(reserve_bytes: Int)
  Epilogue

  Move(to: Value, from: Value)
  Lea(to: Value, from: Value)

  Push(Value)
  Pop(Value)

  Call(Value)

  Add(to: Value, from: Value)
  Sub(to: Value, from: Value)
  Mul(to: Value, from: Value)
  Div(to: Value, from: Value)
}

pub type Value {
  Immediate(Int)
  Register(Register)
  LabelAddress(String)
  Deref(value: Value, offset: Value, multiplier: Int, size: Int)
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

  RBP
  RSP
}

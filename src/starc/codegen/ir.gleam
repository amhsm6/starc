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

  Cmp(Value, Value)
  ExtractZF(Value)
  AndN(to: Value, from: Value, mask: Value)
}

pub type Value {
  Immediate(Int)
  Register(reg: Register, size: Int)
  LabelAddress(String)
  Deref(value: Value, offset: Value, multiplier: Int, size: Int)
  RBP
  RSP
  RSI
}

pub type Register {
  RegA
  RegB
  RegC
  RegD
}

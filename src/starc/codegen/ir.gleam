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

  Jump(Value)
  JGT(Value)
  JGE(Value)
  JLT(Value)
  JLE(Value)

  Add(to: Value, from: Value)
  Sub(to: Value, from: Value)
  Mul(to: Value, from: Value)
  Div(to: Value, from: Value)
  Neg(Value)

  Cmp(Value, Value)
  ExtractZF(Value)

  And(to: Value, from: Value)
  Or(to: Value, from: Value)
  Not(Value)
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

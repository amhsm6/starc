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
  JE(Value)
  JNE(Value)
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
  Immediate(value: Int, size: Int)
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

pub fn size_of(x: Value) -> Int {
  case x {
    Immediate(size:, ..) -> size
    Register(size:, ..) -> size
    LabelAddress(..) -> 8
    Deref(size:, ..) -> size
    RBP -> 8
    RSI -> 8
    RSP -> 8
  }
}

pub fn deref(x: Value, offset: Int, size: Int) -> Value {
  assert size_of(x) == 8
  Deref(
    value: x,
    offset: Immediate(value: offset, size: 8),
    multiplier: 1,
    size:,
  )
}

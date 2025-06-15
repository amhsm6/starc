import gleam/option.{type Option}

pub type Identifier =
  String

pub type TypeId =
  Identifier

pub type Program =
  List(Declaration)

pub type Declaration {
  UntypedDeclaration(UntypedDeclaration)
  TypedDeclaration(TypedDeclaration)
}

pub type UntypedDeclaration {
  UntypedFunctionDeclaration(
    name: Identifier,
    parameters: List(#(Identifier, TypeId)),
    result: Option(TypeId),
    body: Block,
  )
}

pub type TypedDeclaration {
  TypedFunctionDeclaration(
    name: Identifier,
    parameters: List(#(Identifier, Type)),
    result: Type,
    body: Block,
    reserve_bytes: Int,
  )
}

pub type Block =
  List(Statement)

pub type Statement {
  UntypedStatement(UntypedStatement)
  TypedStatement(TypedStatement)
}

pub type UntypedStatement {
  UntypedReturnStatement(Expression)
  UntypedCallStatement(Expression)
  UntypedDefineStatement(
    name: Expression,
    typeid: Option(TypeId),
    expr: Expression,
  )
  UntypedAssignStatement(cell: Expression, expr: Expression)
  UntypedIfStatement(
    condition: Expression,
    block: Block,
    elseifs: List(#(Expression, Block)),
    elseblock: Option(Block),
  )
}

pub type TypedStatement {
  TypedReturnStatement(Expression)
  TypedCallStatement(Expression)
  TypedDefineStatement(name: Expression, expr: Expression)
  TypedAssignStatement(cell: Expression, expr: Expression)
  TypedIfStatement(
    condition: Expression,
    block: Block,
    elseifs: List(#(Expression, Block)),
    elseblock: Option(Block),
  )
}

pub type Expression {
  TypedExpression(expr: UntypedExpression, ty: Type)
  UntypedExpression(UntypedExpression)
}

pub type UntypedExpression {
  IntExpr(Int)
  BoolExpr(Bool)
  StringExpr(String)
  VarExpr(Identifier)

  AddExpr(Expression, Expression)
  SubExpr(Expression, Expression)
  MulExpr(Expression, Expression)
  DivExpr(Expression, Expression)

  EQExpr(Expression, Expression)
  NEQExpr(Expression, Expression)
  LTExpr(Expression, Expression)
  LEExpr(Expression, Expression)
  GTExpr(Expression, Expression)
  GEExpr(Expression, Expression)

  NotExpr(Expression)
  AddrOfExpr(Expression)
  DerefExpr(Expression)

  CallExpression(f: Expression, args: List(Expression))
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

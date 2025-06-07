import gleam/option.{type Option}

pub type Identifier =
  String

pub type TypeId =
  Identifier

pub type Program =
  List(Declaration)

pub type Declaration {
  FunctionDeclaration(
    name: Identifier,
    parameters: List(#(Identifier, TypeId)),
    result: Option(TypeId),
    body: Block,
  )
}

pub type Block =
  List(Statement)

pub type Statement {
  ReturnStatement(Expression)
  CallStatement(Expression)
  DefineStatement(name: Expression, ty: Option(TypeId), expr: Expression)
  AssignStatement(cell: Expression, expr: Expression)
  IfStatement(
    condition: Expression,
    block: Block,
    elseifs: List(#(Expression, Block)),
    elseblock: Option(Block),
  )
}

pub type Expression {
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

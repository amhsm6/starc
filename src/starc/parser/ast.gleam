import gleam/option.{type Option}

pub type Identifier =
  String

pub type Program =
  List(Declaration)

pub type Declaration {
  FunctionDeclaration(
    name: Identifier,
    parameters: List(#(Identifier, Type)),
    result: Option(Type),
    body: Block,
  )
}

pub type Block =
  List(Statement)

pub type Statement {
  EvalStatement(Expression)
  DefineStatement(name: Expression, ty: Option(Type), expr: Expression)
  AssignStatement(cell: Expression, expr: Expression)
}

pub type Type =
  Identifier

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

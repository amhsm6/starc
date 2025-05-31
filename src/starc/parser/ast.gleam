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
  IfStatement
}

pub type Type =
  String

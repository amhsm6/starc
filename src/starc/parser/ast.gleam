import gleam/option.{type Option}

import starc/lexer/token.{type Span}

pub type Identifier {
  Identifier(name: String, span: Span)
}

pub type TypeIdentifier {
  TypeName(name: String, span: Span)
  TypePointer(typeid: TypeIdentifier, span: Span)
}

pub type UntypedProgram =
  List(UntypedDeclaration)

pub type UntypedDeclaration {
  UntypedFunctionDeclaration(
    id: Identifier,
    parameters: List(#(Identifier, TypeIdentifier)),
    result: Option(TypeIdentifier),
    body: UntypedBlock,
    span: Span,
  )
}

pub type UntypedBlock =
  List(UntypedStatement)

pub type UntypedStatement {
  UntypedReturnStatement(UntypedExpression)
  UntypedCallStatement(UntypedExpression)
  UntypedDefineStatement(
    id: Identifier,
    typeid: Option(TypeIdentifier),
    expr: UntypedExpression,
  )
  UntypedAssignStatement(cell: UntypedExpression, expr: UntypedExpression)
  UntypedIfStatement(
    condition: UntypedExpression,
    block: UntypedBlock,
    elseifs: List(#(UntypedExpression, UntypedBlock)),
    elseblock: Option(UntypedBlock),
  )
}

pub type UntypedExpression {
  UntypedIntExpr(value: Int, span: Span)
  UntypedBoolExpr(value: Bool, span: Span)
  UntypedStringExpr(value: String, span: Span)
  UntypedVarExpr(Identifier)

  UntypedAddrOfExpr(e: UntypedExpression, span: Span)
  UntypedDerefExpr(e: UntypedExpression, span: Span)

  UntypedAddExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedSubExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedMulExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedDivExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedNegExpr(e: UntypedExpression, span: Span)

  UntypedEQExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedNEQExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedLTExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedLEExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedGTExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedGEExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)

  UntypedAndExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedOrExpr(e1: UntypedExpression, e2: UntypedExpression, span: Span)
  UntypedNotExpr(e: UntypedExpression, span: Span)

  UntypedCallExpression(
    f: UntypedExpression,
    args: List(UntypedExpression),
    span: Span,
  )
}

pub fn span_of(expr: UntypedExpression) -> Span {
  case expr {
    UntypedAddExpr(span:, ..) -> span
    UntypedAddrOfExpr(span:, ..) -> span
    UntypedAndExpr(span:, ..) -> span
    UntypedBoolExpr(span:, ..) -> span
    UntypedCallExpression(span:, ..) -> span
    UntypedDerefExpr(span:, ..) -> span
    UntypedDivExpr(span:, ..) -> span
    UntypedEQExpr(span:, ..) -> span
    UntypedGEExpr(span:, ..) -> span
    UntypedGTExpr(span:, ..) -> span
    UntypedIntExpr(span:, ..) -> span
    UntypedLEExpr(span:, ..) -> span
    UntypedLTExpr(span:, ..) -> span
    UntypedMulExpr(span:, ..) -> span
    UntypedNEQExpr(span:, ..) -> span
    UntypedNegExpr(span:, ..) -> span
    UntypedNotExpr(span:, ..) -> span
    UntypedOrExpr(span:, ..) -> span
    UntypedStringExpr(span:, ..) -> span
    UntypedSubExpr(span:, ..) -> span
    UntypedVarExpr(id) -> id.span
  }
}

pub type TypedProgram =
  List(TypedDeclaration)

pub type TypedDeclaration {
  TypedFunctionDeclaration(label: String, body: TypedBlock, reserve_bytes: Int)
}

pub type TypedBlock =
  List(TypedStatement)

pub type TypedStatement {
  TypedReturnStatement(TypedExpression)
  TypedCallStatement(TypedExpression)
  TypedDefineStatement(var: TypedExpression, expr: TypedExpression)
  TypedAssignStatement(cell: TypedExpression, expr: TypedExpression)
  TypedIfStatement(
    condition: TypedExpression,
    block: TypedBlock,
    elseifs: List(#(TypedExpression, TypedBlock)),
    elseblock: Option(TypedBlock),
  )
}

pub type TypedExpression {
  TypedIntExpr(value: Int, ty: Type)
  TypedBoolExpr(Bool)
  TypedVarExpr(ty: Type, frame_offset: Int)

  TypedAddrOfExpr(e: TypedExpression, ty: Type)
  TypedDerefExpr(e: TypedExpression, ty: Type)

  TypedAddExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedSubExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedMulExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedDivExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedNegExpr(e: TypedExpression, ty: Type)

  TypedEQExpr(e1: TypedExpression, e2: TypedExpression)
  TypedNEQExpr(e1: TypedExpression, e2: TypedExpression)
  TypedLTExpr(e1: TypedExpression, e2: TypedExpression)
  TypedLEExpr(e1: TypedExpression, e2: TypedExpression)
  TypedGTExpr(e1: TypedExpression, e2: TypedExpression)
  TypedGEExpr(e1: TypedExpression, e2: TypedExpression)

  TypedAndExpr(e1: TypedExpression, e2: TypedExpression)
  TypedOrExpr(e1: TypedExpression, e2: TypedExpression)
  TypedNotExpr(TypedExpression)

  TypedCallExpression(
    label: String,
    args: List(TypedExpression),
    return_type: Type,
    return_frame_offset: Int,
  )
}

pub type Type {
  Void
  Bool
  Int8
  Int16
  Int32
  Int64
  Pointer(Type)
  IntConst
}

pub fn size_of(ty: Type) -> Int {
  case ty {
    Void -> 0
    Bool -> 1
    Int8 -> 1
    Int16 -> 2
    Int32 -> 4
    Int64 -> 8
    Pointer(..) -> 8
    IntConst -> panic
  }
}

pub fn type_of(expr: TypedExpression) -> Type {
  case expr {
    TypedIntExpr(ty:, ..) -> ty
    TypedBoolExpr(..) -> Bool
    TypedVarExpr(ty:, ..) -> ty

    TypedAddrOfExpr(ty:, ..) -> ty
    TypedDerefExpr(ty:, ..) -> ty

    TypedAddExpr(ty:, ..) -> ty
    TypedSubExpr(ty:, ..) -> ty
    TypedMulExpr(ty:, ..) -> ty
    TypedDivExpr(ty:, ..) -> ty
    TypedNegExpr(ty:, ..) -> ty

    TypedEQExpr(..) -> Bool
    TypedNEQExpr(..) -> Bool
    TypedLTExpr(..) -> Bool
    TypedLEExpr(..) -> Bool
    TypedGTExpr(..) -> Bool
    TypedGEExpr(..) -> Bool

    TypedAndExpr(..) -> Bool
    TypedOrExpr(..) -> Bool
    TypedNotExpr(..) -> Bool

    TypedCallExpression(return_type:, ..) -> return_type
  }
}

import gleam/option.{type Option}

pub type Identifier =
  String

pub type TypeId {
  TypeName(Identifier)
  TypePointer(TypeId)
}

pub type UntypedProgram =
  List(UntypedDeclaration)

pub type TypedProgram =
  List(TypedDeclaration)

pub type UntypedDeclaration {
  UntypedFunctionDeclaration(
    name: Identifier,
    parameters: List(#(Identifier, TypeId)),
    result: Option(TypeId),
    body: UntypedBlock,
  )
}

pub type TypedDeclaration {
  TypedFunctionDeclaration(label: String, body: TypedBlock, reserve_bytes: Int)
}

pub type UntypedBlock =
  List(UntypedStatement)

pub type TypedBlock =
  List(TypedStatement)

pub type UntypedStatement {
  UntypedReturnStatement(UntypedExpression)
  UntypedCallStatement(UntypedExpression)
  UntypedDefineStatement(
    name: Identifier,
    typeid: Option(TypeId),
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

pub type TypedStatement {
  TypedReturnStatement(TypedExpression)
  TypedCallStatement(TypedExpression)
  TypedDefineStatement(name: TypedExpression, expr: TypedExpression)
  TypedAssignStatement(cell: TypedExpression, expr: TypedExpression)
  TypedIfStatement(
    condition: TypedExpression,
    block: TypedBlock,
    elseifs: List(#(TypedExpression, TypedBlock)),
    elseblock: Option(TypedBlock),
  )
}

pub type UntypedExpression {
  UntypedIntExpr(Int)
  UntypedBoolExpr(Bool)
  UntypedStringExpr(String)
  UntypedVarExpr(Identifier)

  UntypedAddExpr(UntypedExpression, UntypedExpression)
  UntypedSubExpr(UntypedExpression, UntypedExpression)
  UntypedMulExpr(UntypedExpression, UntypedExpression)
  UntypedDivExpr(UntypedExpression, UntypedExpression)

  UntypedEQExpr(UntypedExpression, UntypedExpression)
  UntypedNEQExpr(UntypedExpression, UntypedExpression)
  UntypedLTExpr(UntypedExpression, UntypedExpression)
  UntypedLEExpr(UntypedExpression, UntypedExpression)
  UntypedGTExpr(UntypedExpression, UntypedExpression)
  UntypedGEExpr(UntypedExpression, UntypedExpression)

  UntypedNotExpr(UntypedExpression)
  UntypedAddrOfExpr(UntypedExpression)
  UntypedDerefExpr(UntypedExpression)

  UntypedCallExpression(f: UntypedExpression, args: List(UntypedExpression))
}

pub type TypedExpression {
  TypedIntExpr(value: Int, ty: Type)
  TypedBoolExpr(Bool)
  TypedVarExpr(id: Identifier, ty: Type, frame_offset: Int)

  TypedAddExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedSubExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedMulExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedDivExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)

  TypedEQExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedNEQExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedLTExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedLEExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedGTExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedGEExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)

  TypedNotExpr(e: TypedExpression, ty: Type)
  TypedAddrOfExpr(e: TypedExpression, ty: Type)
  TypedDerefExpr(e: TypedExpression, ty: Type)

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
  }
}

pub fn type_of(expr: TypedExpression) -> Type {
  case expr {
    TypedIntExpr(ty:, ..) -> ty
    TypedBoolExpr(..) -> Bool
    TypedVarExpr(ty:, ..) -> ty

    TypedAddExpr(ty:, ..) -> ty
    TypedSubExpr(ty:, ..) -> ty
    TypedMulExpr(ty:, ..) -> ty
    TypedDivExpr(ty:, ..) -> ty

    TypedEQExpr(ty:, ..) -> ty
    TypedNEQExpr(ty:, ..) -> ty
    TypedLTExpr(ty:, ..) -> ty
    TypedLEExpr(ty:, ..) -> ty
    TypedGTExpr(ty:, ..) -> ty
    TypedGEExpr(ty:, ..) -> ty

    TypedNotExpr(ty:, ..) -> ty
    TypedAddrOfExpr(ty:, ..) -> ty
    TypedDerefExpr(ty:, ..) -> ty

    TypedCallExpression(return_type:, ..) -> return_type
  }
}

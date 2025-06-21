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

  UntypedAddrOfExpr(UntypedExpression)
  UntypedDerefExpr(UntypedExpression)

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

  UntypedCallExpression(f: UntypedExpression, args: List(UntypedExpression))
}

pub type TypedExpression {
  TypedIntExpr(value: Int, ty: Type)
  TypedBoolExpr(Bool)
  TypedVarExpr(id: Identifier, ty: Type, frame_offset: Int)

  TypedAddrOfExpr(e: TypedExpression, ty: Type)
  TypedDerefExpr(e: TypedExpression, ty: Type)

  TypedAddExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedSubExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedMulExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)
  TypedDivExpr(e1: TypedExpression, e2: TypedExpression, ty: Type)

  TypedEQExpr(e1: TypedExpression, e2: TypedExpression)
  TypedNEQExpr(e1: TypedExpression, e2: TypedExpression)
  TypedLTExpr(e1: TypedExpression, e2: TypedExpression)
  TypedLEExpr(e1: TypedExpression, e2: TypedExpression)
  TypedGTExpr(e1: TypedExpression, e2: TypedExpression)
  TypedGEExpr(e1: TypedExpression, e2: TypedExpression)

  TypedNotExpr(e: TypedExpression)

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

    TypedEQExpr(..) -> Bool
    TypedNEQExpr(..) -> Bool
    TypedLTExpr(..) -> Bool
    TypedLEExpr(..) -> Bool
    TypedGTExpr(..) -> Bool
    TypedGEExpr(..) -> Bool

    TypedNotExpr(..) -> Bool

    TypedCallExpression(return_type:, ..) -> return_type
  }
}

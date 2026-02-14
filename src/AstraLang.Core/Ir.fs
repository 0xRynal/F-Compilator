namespace AstraLang.Core

// IR: ANF + closures.
[<RequireQualifiedAccess>]
module Ir =

    type PrimOp =
        | Add | Sub | Mul | Div | Mod
        | Eq | Neq | Lt | Le | Gt | Ge
        | And | Or

    type Atom =
        | AInt of int
        | ABool of bool
        | AUnit
        | AVar of string
        | AClosure of ClosureRef

    and ClosureRef = int

    type Expr =
        | Ret of Atom
        | Let of name: string * value: LetValue * body: Expr
        | If of cond: Atom * thenBranch: Expr * elseBranch: Expr
        | Switch of Atom * cases: (Atom option * Expr) list * defaultBranch: Expr option

    and LetValue =
        | Atom of Atom
        | PrimOp of PrimOp * Atom list
        | MakeTuple of Atom list
        | MakeClosure of ClosureRef * Atom list
        | Call of Atom * Atom list
        | TailCall of Atom * Atom list
        | GetTupleItem of Atom * index: int
        | Lambda of param: string * body: Expr

    type ClosureDef =
        { Id: ClosureRef
          Params: string list
          FreeVars: string list
          Body: Expr }

    type Program =
        { Closures: Map<ClosureRef, ClosureDef>
          Main: Expr }

    let primOpFromAst (op: Ast.BinOp) : PrimOp =
        match op with
        | Ast.BinOp.Add -> Add
        | Ast.BinOp.Sub -> Sub
        | Ast.BinOp.Mul -> Mul
        | Ast.BinOp.Div -> Div
        | Ast.BinOp.Mod -> Mod
        | Ast.BinOp.Eq -> Eq
        | Ast.BinOp.Neq -> Neq
        | Ast.BinOp.Lt -> Lt
        | Ast.BinOp.Le -> Le
        | Ast.BinOp.Gt -> Gt
        | Ast.BinOp.Ge -> Ge
        | Ast.BinOp.And -> And
        | Ast.BinOp.Or -> Or

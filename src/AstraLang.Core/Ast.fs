namespace AstraLang.Core

// AST before type inference.
[<RequireQualifiedAccess>]
module Ast =

    type BinOp =
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | Eq
        | Neq
        | Lt
        | Le
        | Gt
        | Ge
        | And
        | Or

    type Pattern =
        | PVar of name: string * span: SourceSpan
        | PWild of SourceSpan
        | PLit of Literal * SourceSpan
        | PTuple of Pattern list * SourceSpan

    and Literal =
        | LInt of int
        | LBool of bool
        | LUnit

    type Expr =
        | Lit of Literal * SourceSpan
        | Var of name: string * span: SourceSpan
        | Lambda of param: string * body: Expr * span: SourceSpan
        | App of func: Expr * arg: Expr * span: SourceSpan
        | Let of name: string * binding: Expr * body: Expr * span: SourceSpan
        | LetRec of name: string * param: string * binding: Expr * body: Expr * span: SourceSpan
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr * span: SourceSpan
        | BinaryOp of BinOp * left: Expr * right: Expr * span: SourceSpan
        | Tuple of Expr list * SourceSpan
        | Match of scrutinee: Expr * cases: (Pattern * Expr) list * span: SourceSpan

    type Decl =
        | DLet of name: string * expr: Expr * span: SourceSpan
        | DLetRec of name: string * param: string * expr: Expr * span: SourceSpan

    type Program = Program of Decl list * Expr option

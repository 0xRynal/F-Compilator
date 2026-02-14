namespace AstraLang.Core

// AST with types on every node.
[<RequireQualifiedAccess>]
module TypedAst =

    type BinOp = Ast.BinOp
    type Literal = Ast.Literal

    type Pattern =
        | PVar of name: string * typ: Type * span: SourceSpan
        | PWild of Type * SourceSpan
        | PLit of Literal * Type * SourceSpan
        | PTuple of Pattern list * Type * SourceSpan

    type Expr =
        | Lit of Literal * Type * SourceSpan
        | Var of name: string * Type * span: SourceSpan
        | Lambda of param: string * paramType: Type * body: Expr * resultType: Type * span: SourceSpan
        | App of func: Expr * arg: Expr * typ: Type * span: SourceSpan
        | Let of name: string * binding: Expr * body: Expr * typ: Type * span: SourceSpan
        | LetRec of name: string * param: string * binding: Expr * body: Expr * typ: Type * span: SourceSpan
        | If of cond: Expr * thenBranch: Expr * elseBranch: Expr * typ: Type * span: SourceSpan
        | BinaryOp of BinOp * left: Expr * right: Expr * typ: Type * span: SourceSpan
        | Tuple of Expr list * Type * SourceSpan
        | Match of scrutinee: Expr * cases: (Pattern * Expr) list * typ: Type * span: SourceSpan

    type Decl =
        | DLet of name: string * expr: Expr * span: SourceSpan
        | DLetRec of name: string * param: string * expr: Expr * span: SourceSpan

    type Program = Program of Decl list * Expr option

    // Get type of any expr node.
    let rec typeOf = function
        | Lit (_, t, _) | Var (_, t, _) | Lambda (_, _, _, t, _) | App (_, _, t, _)
        | Let (_, _, _, t, _) | LetRec (_, _, _, _, t, _) | If (_, _, _, t, _)
        | BinaryOp (_, _, _, t, _) | Tuple (_, t, _) | Match (_, _, t, _) -> t

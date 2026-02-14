namespace AstraLang.Optimizer

open AstraLang.Core.Ir

[<RequireQualifiedAccess>]
module Optimizer =

    let private tryEvalPrimOp (op: PrimOp) (atoms: Atom list) : Atom option =  // constant fold
        match op, atoms with
        | Add, [ AInt a; AInt b ] -> Some (AInt (a + b))
        | Sub, [ AInt a; AInt b ] -> Some (AInt (a - b))
        | Mul, [ AInt a; AInt b ] -> Some (AInt (a * b))
        | Div, [ AInt a; AInt b ] -> if b <> 0 then Some (AInt (a / b)) else None
        | Mod, [ AInt a; AInt b ] -> if b <> 0 then Some (AInt (a % b)) else None
        | Eq, [ AInt a; AInt b ] -> Some (ABool (a = b))
        | Eq, [ ABool a; ABool b ] -> Some (ABool (a = b))
        | Neq, [ AInt a; AInt b ] -> Some (ABool (a <> b))
        | Lt, [ AInt a; AInt b ] -> Some (ABool (a < b))
        | Le, [ AInt a; AInt b ] -> Some (ABool (a <= b))
        | Gt, [ AInt a; AInt b ] -> Some (ABool (a > b))
        | Ge, [ AInt a; AInt b ] -> Some (ABool (a >= b))
        | And, [ ABool a; ABool b ] -> Some (ABool (a && b))
        | Or, [ ABool a; ABool b ] -> Some (ABool (a || b))
        | _ -> None

    let rec constantFolding (e: Expr) : Expr =  // fold + branch elimination
        match e with
        | Ret a -> Ret a
        | Let (x, PrimOp (op, atoms), body) ->
            match tryEvalPrimOp op atoms with
            | Some a -> Let (x, Atom a, constantFolding body)
            | None -> Let (x, PrimOp (op, atoms), constantFolding body)
        | Let (x, v, body) -> Let (x, v, constantFolding body)
        | If (ABool true, t, _) -> constantFolding t
        | If (ABool false, _, e) -> constantFolding e
        | If (c, t, e) -> If (c, constantFolding t, constantFolding e)
        | Switch (a, cases, def) ->
            Switch (a, List.map (fun (a', e) -> (a', constantFolding e)) cases, Option.map constantFolding def)

    let private usedVars (e: Expr) =  // for DCE
        let rec uv acc = function
            | Ret (AVar x) -> Set.add x acc
            | Ret _ -> acc
            | Let (x, v, body) ->
                let bodyV = uv acc body
                let fromV = match v with
                            | Atom (AVar y) -> Set.add y acc
                            | Call (f, args) -> List.fold (fun s a -> match a with AVar y -> Set.add y s | _ -> s) acc (f::args)
                            | MakeClosure (_, args) -> List.fold (fun s a -> match a with AVar y -> Set.add y s | _ -> s) acc args
                            | _ -> acc
                Set.union (Set.remove x bodyV) fromV
            | If (AVar x, t, e) -> Set.union (Set.add x (Set.union (uv acc t) (uv acc e))) acc
            | If (_, t, e) -> Set.union (uv acc t) (uv acc e)
            | Switch _ -> acc
        uv Set.empty e

    let rec deadCodeElim (e: Expr) : Expr =
        match e with
        | Let (x, v, body) ->
            let body' = deadCodeElim body
            if Set.contains x (usedVars body') then Let (x, v, body')
            else body'
        | If (c, t, e) -> If (c, deadCodeElim t, deadCodeElim e)
        | Switch (a, cases, def) ->
            Switch (a, List.map (fun (a', ex) -> (a', deadCodeElim ex)) cases, Option.map deadCodeElim def)
        | Ret a -> Ret a

    /// Full optimization pipeline.
    let optimize (p: Program) : Program =
        let optExpr e = e |> constantFolding |> deadCodeElim
        { Closures = Map.map (fun _ c -> { c with Body = optExpr c.Body }) p.Closures
          Main = optExpr p.Main }

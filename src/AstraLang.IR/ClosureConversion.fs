namespace AstraLang.IR

open AstraLang.Core

// Lambdas -> top-level closures with free vars.
[<RequireQualifiedAccess>]
module ClosureConversion =

    let private nextId = ref 0
    let private freshId () =
        nextId.Value <- nextId.Value + 1
        nextId.Value

    let rec freeVarsAtom = function
        | Ir.AVar x -> Set.singleton x
        | _ -> Set.empty

    let rec freeVarsLetValue = function
        | Ir.LetValue.Atom a -> freeVarsAtom a
        | Ir.LetValue.PrimOp (_, atoms) -> Set.unionMany (List.map freeVarsAtom atoms)
        | Ir.LetValue.MakeTuple atoms -> Set.unionMany (List.map freeVarsAtom atoms)
        | Ir.LetValue.MakeClosure (_, atoms) -> Set.unionMany (List.map freeVarsAtom atoms)
        | Ir.LetValue.Call (f, args) -> Set.union (freeVarsAtom f) (Set.unionMany (List.map freeVarsAtom args))
        | Ir.LetValue.TailCall (f, args) -> Set.union (freeVarsAtom f) (Set.unionMany (List.map freeVarsAtom args))
        | Ir.LetValue.GetTupleItem (a, _) -> freeVarsAtom a
        | Ir.LetValue.Lambda (param, body) -> Set.remove param (freeVars body)

    and freeVars = function
        | Ir.Ret a -> freeVarsAtom a
        | Ir.Let (x, v, body) ->
            Set.union (freeVarsLetValue v) (Set.remove x (freeVars body))
        | Ir.If (c, t, e) -> Set.union (freeVarsAtom c) (Set.union (freeVars t) (freeVars e))
        | Ir.Switch (a, cases, def) ->
            let caseVars = cases |> List.map (fun (_, e) -> freeVars e)
            let defVars = def |> Option.map freeVars |> Option.defaultValue Set.empty
            Set.union (freeVarsAtom a) (Set.union (Set.unionMany caseVars) defVars)

    /// Convert one expression: replace Lambda with MakeClosure, collect closures into map.
    let convert (e: Ir.Expr) : Map<Ir.ClosureRef, Ir.ClosureDef> * Ir.Expr =
        let closures = ref Map.empty
        let rec convExpr = function
            | Ir.Ret a -> Ir.Ret a
            | Ir.Let (x, Ir.LetValue.Lambda (param, body), rest) ->
                let body' = convExpr body
                let rest' = convExpr rest
                let fv = Set.remove param (freeVars body')
                let fvList = Set.toList fv
                let id = freshId ()
                closures.Value <- Map.add id ({ Id = id; Params = param :: fvList; FreeVars = fvList; Body = body' } : Ir.ClosureDef) closures.Value
                Ir.Let (x, Ir.LetValue.MakeClosure (id, List.map Ir.AVar fvList), rest')
            | Ir.Let (x, v, rest) ->
                let v' = convLetValue v
                Ir.Let (x, v', convExpr rest)
            | Ir.If (c, t, e) -> Ir.If (c, convExpr t, convExpr e)
            | Ir.Switch (a, cases, def) ->
                Ir.Switch (a, List.map (fun (a', e) -> (a', convExpr e)) cases, Option.map convExpr def)
        and convLetValue = function
            | Ir.LetValue.Atom a -> Ir.LetValue.Atom a
            | Ir.LetValue.PrimOp (op, atoms) -> Ir.LetValue.PrimOp (op, atoms)
            | Ir.LetValue.MakeTuple atoms -> Ir.LetValue.MakeTuple atoms
            | Ir.LetValue.MakeClosure (id, atoms) -> Ir.LetValue.MakeClosure (id, atoms)
            | Ir.LetValue.Call (f, args) -> Ir.LetValue.Call (f, args)
            | Ir.LetValue.TailCall (f, args) -> Ir.LetValue.TailCall (f, args)
            | Ir.LetValue.GetTupleItem (a, i) -> Ir.LetValue.GetTupleItem (a, i)
            | Ir.LetValue.Lambda _ -> failwith "Lambda should be handled in Let"
        let result = convExpr e
        (closures.Value, result)

    let program (e: Ir.Expr) : Ir.Program =
        let (closures, main) = convert e
        ({ Closures = closures; Main = main } : Ir.Program)

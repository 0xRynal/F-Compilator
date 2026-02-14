namespace AstraLang.IR

open AstraLang.Core

[<RequireQualifiedAccess>]
module Anf =

    let private binOpToPrimOp (op: Ast.BinOp) : Ir.PrimOp =
        Ir.primOpFromAst op

    let private fresh () = "__v_" + System.Guid.NewGuid().ToString("N").Substring(0, 8)

    // TypedAst -> ANF (continuation k).
    let rec exprToAnf (e: TypedAst.Expr) (k: Ir.Atom -> Ir.Expr) : Ir.Expr =
        match e with
        | TypedAst.Lit (Ast.LInt n, _, _) -> k (Ir.AInt n)
        | TypedAst.Lit (Ast.LBool b, _, _) -> k (Ir.ABool b)
        | TypedAst.Lit (Ast.LUnit, _, _) -> k Ir.AUnit
        | TypedAst.Var (name, _, _) -> k (Ir.AVar name)
        | TypedAst.Lambda (param, _, body, _, _) ->
            let bodyAnf = exprToAnf body (fun a -> Ir.Ret a)
            let t = fresh ()
            Ir.Let (t, Ir.LetValue.Lambda (param, bodyAnf), k (Ir.AVar t))
        | TypedAst.App (f, arg, _, _) ->
            exprToAnf f (fun fAtom ->
                exprToAnf arg (fun argAtom ->
                    let tmp = fresh ()
                    Ir.Let (tmp, Ir.LetValue.Call (fAtom, [ argAtom ]), k (Ir.AVar tmp))))
        | TypedAst.Let (name, binding, body, _, _) ->
            exprToAnf binding (fun bindAtom ->
                Ir.Let (name, Ir.LetValue.Atom bindAtom, exprToAnf body k))
        | TypedAst.LetRec (name, param, binding, body, _, _) ->
            let bindingAnf = exprToAnf binding (fun a -> Ir.Ret a)
            Ir.Let (name, Ir.LetValue.Lambda (param, bindingAnf), exprToAnf body k)
        | TypedAst.If (cond, thenB, elseB, _, _) ->
            exprToAnf cond (fun cAtom ->
                Ir.If (cAtom, exprToAnf thenB (fun a -> Ir.Ret a), exprToAnf elseB (fun a -> Ir.Ret a)))
        | TypedAst.BinaryOp (op, left, right, _, _) ->
            exprToAnf left (fun lAtom ->
                exprToAnf right (fun rAtom ->
                    let tmp = fresh ()
                    Ir.Let (tmp, Ir.LetValue.PrimOp (binOpToPrimOp op, [ lAtom; rAtom ]), k (Ir.AVar tmp))))
        | TypedAst.Tuple (elems, _, _) ->
            let rec build acc = function
                | [] ->
                    let t = fresh ()
                    Ir.Let (t, Ir.LetValue.MakeTuple (List.rev acc), k (Ir.AVar t))
                | e :: rest ->
                    exprToAnf e (fun a ->
                        let v = fresh ()
                        Ir.Let (v, Ir.LetValue.Atom a, build (Ir.AVar v :: acc) rest))
            build [] elems
        | TypedAst.Match (scrut, cases, _, _) ->
            exprToAnf scrut (fun sAtom ->
                match cases with
                | [] -> k Ir.AUnit
                | (TypedAst.PVar (pname, _, _), rhs) :: _ ->
                    Ir.Let (pname, Ir.LetValue.Atom sAtom, exprToAnf rhs k)
                | (TypedAst.PWild _, rhs) :: _ -> exprToAnf rhs k
                | (TypedAst.PLit (_, _, _), rhs) :: _ ->
                    Ir.Let (fresh (), Ir.LetValue.Atom sAtom, exprToAnf rhs k)
                | _ -> exprToAnf (snd (List.head cases)) k)

    let program (TypedAst.Program (decls, finalExpr)) : Ir.Expr =
        let main =
            match finalExpr with
            | None -> Ir.Ret Ir.AUnit
            | Some e -> exprToAnf e (fun a -> Ir.Ret a)
        List.foldBack
            (fun (d: TypedAst.Decl) rest ->
                match d with
                | TypedAst.DLet (name, e, _) ->
                    exprToAnf e (fun a -> Ir.Let (name, Ir.LetValue.Atom a, rest))
                | TypedAst.DLetRec (name, param, e, _) ->
                    Ir.Let (name, Ir.LetValue.Lambda (param, exprToAnf e (fun a -> Ir.Ret a)), rest))
            decls
            main

namespace AstraLang.TypeInference

open AstraLang.Core

[<RequireQualifiedAccess>]
module Infer =

    type BinOpTy = Ast.BinOp

    type InferError =
        | Unify of Unification.UnifyError
        | UnboundVar of string * SourceSpan
        | ExpectedFunction of Type * SourceSpan

    let private getSpan = function
        | Ast.Lit (_, s) | Ast.Var (_, s) | Ast.Lambda (_, _, s) | Ast.App (_, _, s)
        | Ast.Let (_, _, _, s) | Ast.LetRec (_, _, _, _, s) | Ast.If (_, _, _, s)
        | Ast.BinaryOp (_, _, _, s) | Ast.Tuple (_, s) | Ast.Match (_, _, s) -> s

    let private typeOfLiteral = function
        | Ast.LInt _ -> Type.TInt
        | Ast.LBool _ -> Type.TBool
        | Ast.LUnit -> Type.TUnit

    let rec infer (env: TypeEnv) (e: Ast.Expr) : Result<TypedAst.Expr * Type * Unification.Subst, InferError> =
        match e with
        | Ast.Lit (lit, sp) ->
            let t = typeOfLiteral lit
            Ok (TypedAst.Lit (lit, t, sp), t, Unification.emptySubst)

        | Ast.Var (name, sp) ->
            match TypeEnv.find name env with
            | None -> Error (UnboundVar (name, sp))
            | Some scheme ->
                let t = TypeEnv.instantiate scheme
                Ok (TypedAst.Var (name, t, sp), t, Unification.emptySubst)

        | Ast.Lambda (param, body, sp) ->
            let tParam = Type.TVar (TypeEnv.freshVar ())
            let env' = TypeEnv.add param (TypeScheme.Forall ([], tParam)) env
            infer env' body
            |> Result.map (fun (typedBody, tBody, s) ->
                let tParam' = Unification.apply s tParam
                let t = Type.TArrow (tParam', tBody)
                (TypedAst.Lambda (param, tParam', typedBody, t, sp), t, s))

        | Ast.App (f, arg, sp) ->
            infer env f
            |> Result.bind (fun (typedF, tFunc, s1) ->
                let env1 = TypeEnv.apply s1 env
                infer env1 arg
                |> Result.bind (fun (typedArg, tArg, s2) ->
                    let tResult = Type.TVar (TypeEnv.freshVar ())
                    let tExpected = Type.TArrow (Unification.apply s2 tArg, tResult)
                    Unification.unify (Unification.apply s2 tFunc) tExpected sp
                    |> Result.mapError Unify
                    |> Result.map (fun s3 ->
                        let s = Unification.compose (Unification.compose s1 s2) s3
                        let tRes = Unification.apply s tResult
                        (TypedAst.App (typedF, typedArg, tRes, sp), tRes, s))))

        | Ast.Let (name, binding, body, sp) ->
            infer env binding
            |> Result.bind (fun (typedBinding, tBind, s1) ->
                let env1 = TypeEnv.apply s1 env
                let scheme = TypeEnv.generalize env1 tBind
                let env2 = TypeEnv.add name scheme env1
                infer env2 body
                |> Result.map (fun (typedBody, tBody, s2) ->
                    let s = Unification.compose s1 s2
                    (TypedAst.Let (name, typedBinding, typedBody, Unification.apply s tBody, sp), Unification.apply s tBody, s)))

        | Ast.LetRec (name, param, binding, body, sp) ->
            let tParam = Type.TVar (TypeEnv.freshVar ())
            let tResult = Type.TVar (TypeEnv.freshVar ())
            let tFunc = Type.TArrow (tParam, tResult)
            let env' = TypeEnv.add name (TypeScheme.Forall ([], tFunc)) env
            let env'' = TypeEnv.add param (TypeScheme.Forall ([], tParam)) env'
            infer env'' binding
            |> Result.bind (fun (typedBinding, tBind, s1) ->
                Unification.unify (Unification.apply s1 tFunc) tBind sp
                |> Result.mapError Unify
                |> Result.bind (fun s2 ->
                    let s = Unification.compose s1 s2
                    let envFinal = TypeEnv.apply s env'
                    let scheme = TypeEnv.generalize envFinal (Unification.apply s tFunc)
                    let envBody = TypeEnv.add name scheme envFinal
                    infer envBody body
                    |> Result.map (fun (typedBody, tBody, s3) ->
                        let sTotal = Unification.compose s s3
                        (TypedAst.LetRec (name, param, typedBinding, typedBody, Unification.apply sTotal tBody, sp), Unification.apply sTotal tBody, sTotal))))

        | Ast.If (cond, thenB, elseB, sp) ->
            infer env cond
            |> Result.bind (fun (typedCond, tCond, s1) ->
                Unification.unify tCond Type.TBool sp |> Result.mapError Unify
                |> Result.bind (fun s2 ->
                    let s1' = Unification.compose s1 s2
                    let env1 = TypeEnv.apply s1' env
                    infer env1 thenB
                    |> Result.bind (fun (typedThen, tThen, s3) ->
                        let env2 = TypeEnv.apply s3 env1
                        infer env2 elseB
                        |> Result.bind (fun (typedElse, tElse, s4) ->
                            Unification.unify (Unification.apply s4 tThen) tElse sp
                            |> Result.mapError Unify
                            |> Result.map (fun s5 ->
                                let s = Unification.compose (Unification.compose (Unification.compose s1' s3) s4) s5
                                let t = Unification.apply s tThen
                                (TypedAst.If (typedCond, typedThen, typedElse, t, sp), t, s))))))

        | Ast.BinaryOp (op, left, right, sp) ->
            infer env left
            |> Result.bind (fun (typedLeft, tLeft, s1) ->
                infer (TypeEnv.apply s1 env) right
                |> Result.bind (fun (typedRight, tRight, s2) ->
                    let (tExpectedLeft, tExpectedRight, tResult) = binOpTypes op
                    Unification.unify (Unification.apply s2 tLeft) tExpectedLeft sp
                    |> Result.mapError Unify
                    |> Result.bind (fun s3 ->
                        Unification.unify (Unification.apply s3 (Unification.apply s2 tRight)) tExpectedRight sp
                        |> Result.mapError Unify
                        |> Result.map (fun s4 ->
                            let sTotal = Unification.compose (Unification.compose (Unification.compose s1 s2) s3) s4
                            (TypedAst.BinaryOp (op, typedLeft, typedRight, Unification.apply sTotal tResult, sp), Unification.apply sTotal tResult, sTotal)))))

        | Ast.Tuple (elems, sp) ->
            let rec inferList envAcc acc subst = function
                | [] -> Ok (List.rev acc, subst)
                | e :: rest ->
                    infer (TypeEnv.apply subst envAcc) e
                    |> Result.bind (fun (te, t, s) ->
                        inferList (TypeEnv.apply s envAcc) ((te, t) :: acc) (Unification.compose subst s) rest)
            inferList env [] Unification.emptySubst elems
            |> Result.map (fun (typedElems, s) ->
                let types = List.map snd typedElems
                let exprs = List.map fst typedElems
                let t = Type.TTuple types
                (TypedAst.Tuple (exprs, t, sp), t, s))

        | Ast.Match (scrut, cases, sp) ->
            infer env scrut
            |> Result.bind (fun (typedScrut, tScrut, s1) ->
                let env1 = TypeEnv.apply s1 env
                let rec inferCases acc subst = function
                    | [] -> Ok (List.rev acc, subst)
                    | (pat, rhs) :: rest ->
                        inferPattern env1 tScrut pat
                        |> Result.bind (fun (typedPat, env2, sPat) ->
                            let subst' = Unification.compose subst sPat
                            infer (TypeEnv.apply subst' env2) rhs
                            |> Result.bind (fun (typedRhs, tRhs, sRhs) ->
                                let subst'' = Unification.compose subst' sRhs
                                inferCases ((typedPat, typedRhs) :: acc) subst'' rest))
                inferCases [] s1 cases
                |> Result.bind (fun (typedCases, s2) ->
                    let tFirst = typedCases |> List.head |> snd |> TypedAst.typeOf
                    let sTotal = Unification.compose s1 s2
                    let tResult = Unification.apply sTotal tFirst
                    Ok (TypedAst.Match (typedScrut, typedCases, tResult, sp), tResult, sTotal)))

    and inferPattern env tScrut (pat: Ast.Pattern) : Result<TypedAst.Pattern * TypeEnv * Unification.Subst, InferError> =
        match pat with
        | Ast.PVar (name, sp) ->
            Ok (TypedAst.PVar (name, tScrut, sp), TypeEnv.add name (TypeScheme.Forall ([], tScrut)) env, Unification.emptySubst)
        | Ast.PWild sp -> Ok (TypedAst.PWild (tScrut, sp), env, Unification.emptySubst)
        | Ast.PLit (lit, sp) ->
            let t = typeOfLiteral lit
            Unification.unify t tScrut sp |> Result.mapError Unify
            |> Result.map (fun s -> (TypedAst.PLit (lit, t, sp), env, s))
        | Ast.PTuple (pats, sp) ->
            match tScrut with
            | Type.TTuple tList when List.length tList = List.length pats ->
                List.zip pats tList
                |> List.fold
                    (fun state (p, t) ->
                        state
                        |> Result.bind (fun (accPats, accEnv, accS) ->
                            inferPattern (TypeEnv.apply accS accEnv) t p
                            |> Result.map (fun (tp, env2, s) ->
                                (tp :: accPats, env2, Unification.compose accS s))))
                    (Ok ([], env, Unification.emptySubst))
                |> Result.map (fun (pats', env', s) ->
                    (TypedAst.PTuple (List.rev pats', Type.TTuple tList, sp), env', s))
            | _ -> Error (Unify (Unification.Mismatch (tScrut, Type.TTuple [], sp)))

    and binOpTypes (op: BinOpTy) =
        match op with
        | BinOpTy.Add | BinOpTy.Sub | BinOpTy.Mul | BinOpTy.Div | BinOpTy.Mod ->
            (Type.TInt, Type.TInt, Type.TInt)
        | BinOpTy.Eq | BinOpTy.Neq | BinOpTy.Lt | BinOpTy.Le | BinOpTy.Gt | BinOpTy.Ge ->
            (Type.TInt, Type.TInt, Type.TBool)
        | BinOpTy.And | BinOpTy.Or ->
            (Type.TBool, Type.TBool, Type.TBool)

    let inferDecl (env: TypeEnv) (d: Ast.Decl) : Result<TypedAst.Decl * TypeEnv * Unification.Subst, InferError> =
        match d with
        | Ast.DLet (name, expr, sp) ->
            infer env expr
            |> Result.map (fun (typedExpr, t, s) ->
                let env' = TypeEnv.apply s env
                let scheme = TypeEnv.generalize env' t
                (TypedAst.DLet (name, typedExpr, sp), TypeEnv.add name scheme env', s))
        | Ast.DLetRec (name, param, expr, sp) ->
            let tParam = Type.TVar (TypeEnv.freshVar ())
            let tResult = Type.TVar (TypeEnv.freshVar ())
            let tFunc = Type.TArrow (tParam, tResult)
            let env' = TypeEnv.add name (TypeScheme.Forall ([], tFunc)) env
            let env'' = TypeEnv.add param (TypeScheme.Forall ([], tParam)) env'
            infer env'' expr
            |> Result.bind (fun (typedExpr, tBind, s1) ->
                Unification.unify (Unification.apply s1 tFunc) tBind sp |> Result.mapError Unify
                |> Result.map (fun s2 ->
                    let s = Unification.compose s1 s2
                    let envFinal = TypeEnv.apply s env
                    let scheme = TypeEnv.generalize envFinal (Unification.apply s tFunc)
                    (TypedAst.DLetRec (name, param, typedExpr, sp), TypeEnv.add name scheme envFinal, s)))

    let inferProgram (program: Ast.Program) : Result<TypedAst.Program, InferError> =
        match program with
        | Ast.Program (decls, finalExpr) ->
            let rec inferDecls env acc = function
                | [] -> Ok (List.rev acc, env)
                | d :: rest ->
                    inferDecl env d
                    |> Result.bind (fun (typedD, env', _) ->
                        inferDecls env' (typedD :: acc) rest)
            inferDecls TypeEnv.empty [] decls
            |> Result.bind (fun (typedDecls, env) ->
                match finalExpr with
                | None -> Ok (TypedAst.Program (typedDecls, None))
                | Some e ->
                    infer env e
                    |> Result.map (fun (typedE, _, _) -> TypedAst.Program (typedDecls, Some typedE)))

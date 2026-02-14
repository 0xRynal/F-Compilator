namespace AstraLang.TypeInference

open AstraLang.Core

[<RequireQualifiedAccess>]
module Unification =

    type Subst = Map<TypeVar, Type>
    type UnifyError =
        | Mismatch of Type * Type * SourceSpan
        | OccursCheck of TypeVar * Type

    let emptySubst : Subst = Map.empty

    let compose (s1: Subst) (s2: Subst) : Subst =
        let s1' = Map.map (fun _ t -> Type.apply s2 t) s1
        Map.fold (fun m k v -> Map.add k v m) s1' s2

    let apply (s: Subst) (t: Type) = Type.apply s t

    // Occurs check (infinite recursion).
    let rec occurs (tvar: TypeVar) (t: Type) =
        match t with
        | Type.TVar v when v = tvar -> true
        | Type.TVar v -> false
        | Type.TInt | Type.TBool | Type.TUnit -> false
        | Type.TTuple ts -> List.exists (occurs tvar) ts
        | Type.TArrow (a, b) -> occurs tvar a || occurs tvar b

    let rec unify (t1: Type) (t2: Type) (span: SourceSpan) : Result<Subst, UnifyError> =
        match t1, t2 with
        | Type.TVar v, t
        | t, Type.TVar v ->
            if t = Type.TVar v then Ok emptySubst
            elif occurs v t then Error (OccursCheck (v, t))
            else Ok (Map.add v t emptySubst)
        | Type.TInt, Type.TInt
        | Type.TBool, Type.TBool
        | Type.TUnit, Type.TUnit -> Ok emptySubst
        | Type.TTuple ts1, Type.TTuple ts2 ->
            if List.length ts1 <> List.length ts2 then
                Error (Mismatch (t1, t2, span))
            else
                List.fold2
                    (fun acc a b ->
                        Result.bind (fun s ->
                            unify (apply s a) (apply s b) span
                            |> Result.map (fun s2 -> compose s s2))
                            acc)
                    (Ok emptySubst)
                    ts1
                    ts2
        | Type.TArrow (a1, b1), Type.TArrow (a2, b2) ->
            unify a1 a2 span
            |> Result.bind (fun s ->
                unify (apply s b1) (apply s b2) span
                |> Result.map (fun s2 -> compose s s2))
        | _ -> Error (Mismatch (t1, t2, span))

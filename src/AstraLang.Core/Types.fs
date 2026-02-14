namespace AstraLang.Core

// Type variables (fresh ids for inference).
[<CustomComparison; CustomEquality>]
type TypeVar =
    | TV of name: string * id: int
    override x.ToString() = match x with TV (n, i) -> sprintf "'%s%d" n i
    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? TypeVar as TV (n2, i2) ->
                match x with
                | TV (n1, i1) ->
                    match compare i1 i2 with
                    | 0 -> compare n1 n2
                    | c -> c
            | _ -> -1
    override x.Equals y = match y with :? TypeVar as t -> (x :> System.IComparable).CompareTo t = 0 | _ -> false
    override x.GetHashCode() = match x with TV (n, i) -> hash (n, i)

// Types: monomorphic + type var.
type Type =
    | TInt
    | TBool
    | TUnit
    | TVar of TypeVar
    | TTuple of Type list
    | TArrow of Type * Type

// Type scheme ∀α.τ (generalization at let).
type TypeScheme =
    | Forall of TypeVar list * Type

// Free vars, apply substitution.
module Type =
    let rec freeVars = function
        | TInt | TBool | TUnit -> Set.empty
        | TVar v -> Set.singleton v
        | TTuple ts -> List.fold (fun s t -> Set.union s (freeVars t)) Set.empty ts
        | TArrow (a, b) -> Set.union (freeVars a) (freeVars b)

    let rec apply (subst: Map<TypeVar, Type>) t =
        match t with
        | TInt | TBool | TUnit -> t
        | TVar v ->
            match Map.tryFind v subst with
            | Some t' -> apply subst t'
            | None -> t
        | TTuple ts -> TTuple (List.map (apply subst) ts)
        | TArrow (a, b) -> TArrow (apply subst a, apply subst b)

// Generalize/instantiate for let.
module TypeScheme =
    let freeVars (Forall (vars, t)) =
        Set.difference (Type.freeVars t) (Set.ofList vars)

    let apply subst (Forall (vars, t)) =
        let subst' = List.fold (fun s v -> Map.remove v s) subst vars
        Forall (vars, Type.apply subst' t)

    let instantiate (fresh: TypeVar -> TypeVar) (Forall (vars, t)) =
        let mapping = List.map (fun v -> v, TVar (fresh v)) vars
        let subst = Map.ofList mapping
        Type.apply subst t

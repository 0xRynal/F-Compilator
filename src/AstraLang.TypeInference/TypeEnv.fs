namespace AstraLang.TypeInference

open AstraLang.Core

// Name -> type scheme.
type TypeEnv = Map<string, TypeScheme>

[<RequireQualifiedAccess>]
module TypeEnv =

    let empty : TypeEnv = Map.empty
    let add (name: string) (scheme: TypeScheme) (env: TypeEnv) =
        Map.add name scheme env

    let find (name: string) (env: TypeEnv) =
        Map.tryFind name env

    let apply (subst: Unification.Subst) (env: TypeEnv) : TypeEnv =
        Map.map (fun _ scheme -> TypeScheme.apply subst scheme) env

    let freeVars (env: TypeEnv) =
        env
        |> Map.fold (fun s _ scheme -> Set.union s (TypeScheme.freeVars scheme)) Set.empty

    let generalize (env: TypeEnv) (t: Type) : TypeScheme =
        let envFv = freeVars env
        let tFv = Type.freeVars t
        let vars = Set.difference tFv envFv |> Set.toList
        TypeScheme.Forall (vars, t)

    let private nextVar = ref 0
    let freshVar () =
        let n = System.Threading.Interlocked.Increment nextVar
        TypeVar.TV ("t", n)

    let instantiate (scheme: TypeScheme) : Type =
        TypeScheme.instantiate
            (fun (TypeVar.TV (name, _)) ->
                nextVar := !nextVar + 1
                TypeVar.TV (name, !nextVar))
            scheme

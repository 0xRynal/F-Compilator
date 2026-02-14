namespace AstraLang.IR

open AstraLang.Core

[<RequireQualifiedAccess>]
module ToIr =

    // Pipeline: ANF then closure conversion.
    let program (p: TypedAst.Program) : Ir.Program =
        Anf.program p
        |> ClosureConversion.program

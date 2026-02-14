namespace AstraLang.CLI

open System
open System.IO
open AstraLang.Core
open AstraLang.Lexer
open AstraLang.Parser
open AstraLang.TypeInference
open AstraLang.IR
open AstraLang.Optimizer
open AstraLang.Compiler
open AstraLang.VM

// Full pipeline: source -> run.
[<RequireQualifiedAccess>]
module Pipeline =

    let run (source: string) (showTypes: bool) (showIr: bool) (showBytecode: bool) =
        Lexer.tokenize source
        |> Result.mapError (fun e -> sprintf "Lex error: %A" e)
        |> Result.bind (fun tokens ->
            Parser.parse tokens
            |> Result.mapError (fun e -> sprintf "Parse error: %A" e))
        |> Result.bind (fun ast ->
            Infer.inferProgram ast
            |> Result.mapError (fun e -> sprintf "Type error: %A" e)
            |> Result.map (fun typed ->
                if showTypes then
                    printfn "Typed AST (inferred types): ok"
                typed))
        |> Result.bind (fun typed ->
            let ir = ToIr.program typed
            if showIr then
                printfn "IR: %d closures, main expr present" (Map.count ir.Closures)
            let optimized = Optimizer.optimize ir
            let bytecode = CodeGen.compile optimized
            if showBytecode then
                printfn "Bytecode: %d closures" (Map.count bytecode.Closures)
                for (id, c) in Map.toSeq bytecode.Closures do
                    printfn "  Closure %d: arity %d, %d instructions" id c.Arity c.Code.Length
            Machine.run bytecode
            |> Result.mapError (fun e -> sprintf "Runtime error: %A" e))

[<EntryPoint>]
let main argv =
    let showTypes = Array.contains "--show-types" argv
    let showIr = Array.contains "--show-ir" argv
    let showBytecode = Array.contains "--show-bytecode" argv
    let args = argv |> Array.filter (fun x -> not (x.StartsWith "--"))

    if args.Length = 0 then
        printfn "AstraLang REPL - type expressions, exit with Ctrl+C"
        let rec repl () =
            printf "> "
            match Console.ReadLine() with
            | null -> 0
            | line when line.Trim().ToLower() = "exit" -> 0
            | line ->
                match Pipeline.run line showTypes showIr showBytecode with
                | Ok v ->
                    printfn "%A" v
                    repl ()
                | Error e ->
                    printfn "Error: %s" e
                    repl ()
        repl ()
    else
        let path = args[0]
        if not (File.Exists path) then
            eprintfn "File not found: %s" path
            1
        else
            let source = File.ReadAllText path
            match Pipeline.run source showTypes showIr showBytecode with
            | Ok v ->
                printfn "%A" v
                0
            | Error e ->
                eprintfn "Error: %s" e
                1

namespace AstraLang.Compiler

open AstraLang.Core
open AstraLang.Core.Ir
open Bytecode

// IR -> bytecode.
[<RequireQualifiedAccess>]
module CodeGen =

    type Env = Map<string, int>
    let private emit (code: Instruction list) (cont: int -> Instruction list * int) (slot: int) =
        let (rest, slot') = cont slot
        (code @ rest, slot')

    let private compileAtom (env: Env) (a: Atom) : Instruction list =  // atom -> instructions
        match a with
        | AInt n -> [ PushInt n ]
        | ABool b -> [ PushBool b ]
        | AUnit -> [ PushUnit ]
        | AVar x ->
            match Map.tryFind x env with
            | Some s -> [ Load s ]
            | None -> failwith (sprintf "Unknown variable: %s" x)
        | AClosure _ -> failwith "AClosure in compileAtom"

    let private compilePrimOp = function
        | Ir.PrimOp.Add -> Add
        | Ir.PrimOp.Sub -> Sub
        | Ir.PrimOp.Mul -> Mul
        | Ir.PrimOp.Div -> Div
        | Ir.PrimOp.Mod -> Mod
        | Ir.PrimOp.Eq -> Eq
        | Ir.PrimOp.Neq -> Neq
        | Ir.PrimOp.Lt -> Lt
        | Ir.PrimOp.Le -> Le
        | Ir.PrimOp.Gt -> Gt
        | Ir.PrimOp.Ge -> Ge
        | Ir.PrimOp.And -> And
        | Ir.PrimOp.Or -> Or

    let rec compileLetValue (env: Env) (v: LetValue) (slot: int) : Instruction list * int =
        match v with
        | Atom a ->
            let code = compileAtom env a
            (code, slot)
        | PrimOp (op, args) ->
            let code = List.collect (compileAtom env) args
            let opCode = compilePrimOp op
            (code @ [ opCode ], slot)
        | MakeTuple atoms ->
            let code = List.collect (compileAtom env) atoms
            (code @ [ MakeTuple (List.length atoms) ], slot)
        | MakeClosure (id, atoms) ->
            let code = List.collect (compileAtom env) atoms
            (code @ [ Bytecode.MakeClosure (id, List.length atoms) ], slot)
        | Call (f, args) ->
            let codeF = compileAtom env f
            let codeArgs = List.collect (compileAtom env) args
            let arity = List.length args
            (codeF @ codeArgs @ [ Call (arity + 1) ], slot)
        | TailCall (f, args) ->
            let codeF = compileAtom env f
            let codeArgs = List.collect (compileAtom env) args
            let arity = List.length args
            (codeF @ codeArgs @ [ TailCall (arity + 1) ], slot)
        | GetTupleItem (a, i) ->
            let code = compileAtom env a
            (code @ [ GetTupleItem i ], slot)
        | Lambda _ -> failwith "Lambda in compileLetValue (should be converted)"

    let rec compileExpr (env: Env) (slot: int) (isClosure: bool) (e: Expr) : Instruction list * int =
        match e with
        | Ret a ->
            let code = compileAtom env a
            let code' = if isClosure then code @ [ Return ] else code
            (code', slot)
        | Let (x, v, body) ->
            let (codeV, _) = compileLetValue env v slot
            let storeCode = [ Store slot ]
            let env' = Map.add x slot env
            let (codeBody, slot') = compileExpr env' (slot + 1) isClosure body
            (codeV @ storeCode @ codeBody, slot')
        | If (c, thenB, elseB) ->
            let codeC = compileAtom env c
            let (codeThen, _) = compileExpr env slot isClosure thenB
            let (codeElse, _) = compileExpr env slot isClosure elseB
            let skipThen = 1 + codeThen.Length
            let skipElse = codeElse.Length
            (codeC @ [ JumpIfFalse skipThen ] @ codeThen @ [ Jump skipElse ] @ codeElse, slot)
        | Switch (a, cases, def) ->
            let codeA = compileAtom env a
            let (codeCases, _) = List.mapFold (fun s (_, ex) -> let (c, s') = compileExpr env s isClosure ex in ((c, s'), s')) slot (cases @ (Option.toList (Option.map (fun e -> (None, e)) def)))
            let code = codeA :: (List.map fst codeCases)
            let merged = List.collect id code
            (merged, slot)

    let compileClosure (closureId: int) (def: ClosureDef) : ClosureCode =
        let env = def.Params |> List.mapi (fun i p -> (p, i)) |> Map.ofList
        let (code, _) = compileExpr env def.Params.Length true def.Body
        { Arity = List.length def.Params
          FreeSlots = List.length def.FreeVars
          Code = code }

    let compile (p: Ir.Program) : Bytecode.Program =
        let closures =
            p.Closures
            |> Map.toList
            |> List.map (fun (id, def) -> (id, compileClosure id def))
            |> Map.ofList
        let (mainCode, _) = compileExpr Map.empty 0 false p.Main
        { Closures = closures
          Main = mainCode }

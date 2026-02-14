namespace AstraLang.VM

open AstraLang.Core.Bytecode
open AstraLang.Runtime

// Run bytecode (stack + heap + GC).
[<RequireQualifiedAccess>]
module Machine =

    type Frame = { Code: Instruction list; IP: int; Slots: Value list; ReturnIP: int option; CallerStack: Value list }

    type State =
        { Stack: Value list
          Frames: Frame list
          Heap: Heap
          Program: Program }

    type RunError =
        | StackUnderflow
        | InvalidClosure of int
        | TypeError of string
        | DivisionByZero

    let private (|Pop|_|) (s: Value list) = match s with v :: rest -> Some (v, rest) | _ -> None
    let private (|PopN|_|) n (s: Value list) =
        if List.length s >= n then
            let (a, b) = List.splitAt n s
            Some (a, b)
        else None

    let run (program: Program) : Result<Value, RunError> =  // main entry
        let heap = Heap.create ()
        let code = program.Main
        let state = { Stack = []; Frames = [ { Code = code; IP = 0; Slots = []; ReturnIP = None; CallerStack = [] } ]; Heap = heap; Program = program }

        let rec step (s: State) : Result<State, RunError> =
            match s.Frames with
            | [] -> Error StackUnderflow
            | frame :: restFrames ->
                let code = frame.Code
                let ip = frame.IP
                if ip >= code.Length then
                    match restFrames with
                    | [] -> Ok s
                    | _ -> Error StackUnderflow
                else
                    let instr = code[ip]
                    let nextFrame = { frame with IP = ip + 1 }
                    match instr with
                    | PushInt n -> Ok { s with Stack = VInt n :: s.Stack; Frames = nextFrame :: restFrames }
                    | PushBool b -> Ok { s with Stack = VBool b :: s.Stack; Frames = nextFrame :: restFrames }
                    | PushUnit -> Ok { s with Stack = VUnit :: s.Stack; Frames = nextFrame :: restFrames }
                    | Load slot ->
                        let v = frame.Slots[slot]
                        Ok { s with Frames = { nextFrame with Stack = v :: s.Stack } :: restFrames }
                    | Store slot ->
                        match s.Stack with
                        | v :: stack ->
                            let pad = max 0 (slot + 1 - List.length frame.Slots)
                            let newSlots = (frame.Slots @ List.replicate pad VUnit) |> List.mapi (fun i x -> if i = slot then v else x)
                            Ok { s with Stack = stack; Frames = { nextFrame with Slots = newSlots } :: restFrames }
                        | _ -> Error StackUnderflow
                    | MakeClosure (id, freeCount) ->
                        if s.Stack.Length >= freeCount then
                            let (revEnv, rest) = List.splitAt freeCount (List.rev s.Stack)
                            let envVals = List.rev revEnv
                            let addr = Heap.alloc heap (HClosure (id, envVals))
                            Ok { s with Stack = VClosure addr :: List.rev rest; Frames = nextFrame :: restFrames }
                        else Error StackUnderflow
                    | Call arity ->
                        if s.Stack.Length >= arity then
                            let popped = List.take arity s.Stack
                            let closure = List.last popped
                            let args = List.rev (List.take (arity - 1) popped)
                            match closure with
                            | VClosure addr ->
                                match Heap.tryGet heap addr with
                                | Some (HClosure (id, env)) ->
                                    match Map.tryFind id program.Closures with
                                    | Some closure ->
                                        let slots = args @ env
                                        let callerStack = List.skip arity s.Stack
                                        let newFrame = { Code = closure.Code; IP = 0; Slots = slots; ReturnIP = Some (ip + 1); CallerStack = [] }
                                        let callerFrame = { nextFrame with CallerStack = callerStack }
                                        Ok { s with Stack = []; Frames = newFrame :: callerFrame :: restFrames }
                                    | None -> Error (InvalidClosure id)
                                | _ -> Error (InvalidClosure addr)
                            | _ -> Error (TypeError "expected closure")
                        else Error StackUnderflow
                    | Return ->
                        match s.Stack with
                        | v :: _ ->
                            match restFrames with
                            | retFrame :: rest ->
                                Ok { s with Stack = v :: retFrame.CallerStack; Frames = retFrame :: rest }
                            | [] -> Ok s
                        | _ -> Error StackUnderflow
                    | Jump offset -> Ok { s with Frames = { nextFrame with IP = ip + 1 + offset } :: restFrames }
                    | JumpIfFalse offset ->
                        match s.Stack with
                        | VBool false :: stack -> Ok { s with Stack = stack; Frames = { nextFrame with IP = ip + 1 + offset } :: restFrames }
                        | _ :: stack -> Ok { s with Stack = stack; Frames = nextFrame :: restFrames }
                        | _ -> Error StackUnderflow
                    | MakeTuple count ->
                        if s.Stack.Length >= count then
                            let elems = List.rev (List.take count s.Stack)
                            let rest = List.skip count s.Stack
                            Ok { s with Stack = VTuple elems :: rest; Frames = nextFrame :: restFrames }
                        else Error StackUnderflow
                    | GetTupleItem i ->
                        match s.Stack with
                        | VTuple vs :: stack when i < vs.Length -> Ok { s with Stack = vs[i] :: stack; Frames = nextFrame :: restFrames }
                        | VTuple _ :: _ -> Error (TypeError "tuple index")
                        | _ -> Error (TypeError "expected tuple")
                    | Add ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VInt (a + b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Add")
                    | Sub ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VInt (a - b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Sub")
                    | Mul ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VInt (a * b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Mul")
                    | Div ->
                        match s.Stack with
                        | VInt 0 :: _ -> Error DivisionByZero
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VInt (a / b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Div")
                    | Mod ->
                        match s.Stack with
                        | VInt 0 :: _ -> Error DivisionByZero
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VInt (a % b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Mod")
                    | Eq ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a = b) :: stack; Frames = nextFrame :: restFrames }
                        | VBool b :: VBool a :: stack -> Ok { s with Stack = VBool (a = b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Eq")
                    | Neq ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a <> b) :: stack; Frames = nextFrame :: restFrames }
                        | VBool b :: VBool a :: stack -> Ok { s with Stack = VBool (a <> b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Neq")
                    | Lt ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a < b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Lt")
                    | Le ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a <= b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Le")
                    | Gt ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a > b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Gt")
                    | Ge ->
                        match s.Stack with
                        | VInt b :: VInt a :: stack -> Ok { s with Stack = VBool (a >= b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Ge")
                    | And ->
                        match s.Stack with
                        | VBool b :: VBool a :: stack -> Ok { s with Stack = VBool (a && b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "And")
                    | Or ->
                        match s.Stack with
                        | VBool b :: VBool a :: stack -> Ok { s with Stack = VBool (a || b) :: stack; Frames = nextFrame :: restFrames }
                        | _ -> Error (TypeError "Or")
                    | Pop count ->
                        if s.Stack.Length >= count then
                            Ok { s with Stack = List.skip count s.Stack; Frames = nextFrame :: restFrames }
                        else Error StackUnderflow

        let rec runLoop (s: State) =
            match step s with
            | Ok s' ->
                match s'.Frames with
                | [ f ] when f.IP >= f.Code.Length ->
                    match s'.Stack with
                    | v :: _ -> Ok v
                    | [] -> Error StackUnderflow
                | _ -> runLoop s'
            | Error e -> Error e

        runLoop state

namespace AstraLang.Core

// Stack VM bytecode.
[<RequireQualifiedAccess>]
module Bytecode =

    type Instruction =
        | PushInt of int
        | PushBool of bool
        | PushUnit
        | Load of slot: int
        | Store of slot: int
        | MakeClosure of closureId: int * freeCount: int
        | Call of arity: int
        | TailCall of arity: int
        | Return
        | Jump of target: int
        | JumpIfFalse of target: int
        | MakeTuple of count: int
        | GetTupleItem of index: int
        | Add | Sub | Mul | Div | Mod
        | Eq | Neq | Lt | Le | Gt | Ge
        | And | Or
        | Pop of count: int

    // One closure: arity, free slots, code.
    type ClosureCode =
        { Arity: int
          FreeSlots: int
          Code: Instruction list }

    type Program =
        { Closures: Map<int, ClosureCode>
          Main: Instruction list }

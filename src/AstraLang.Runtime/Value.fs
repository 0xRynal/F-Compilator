namespace AstraLang.Runtime

// Stack/heap values.
type Value =
    | VInt of int
    | VBool of bool
    | VUnit
    | VTuple of Value list
    | VClosure of int  // heap addr

namespace AstraLang.Runtime

type HeapValue =
    | HTuple of Value list
    | HClosure of closureId: int * env: Value list

type Heap = System.Collections.Generic.Dictionary<int, HeapValue>

[<RequireQualifiedAccess>]
module Heap =

    let private nextAddr = ref 0
    let fresh () =
        nextAddr.Value <- nextAddr.Value + 1
        nextAddr.Value

    let create () = System.Collections.Generic.Dictionary<int, HeapValue>()
    let alloc (h: Heap) (v: HeapValue) : int =
        let a = fresh ()
        h[a] <- v
        a

    let get (h: Heap) (a: int) = h[a]
    let set (h: Heap) (a: int) (v: HeapValue) = h[a] <- v
    let tryGet (h: Heap) (a: int) = match h.TryGetValue a with true, v -> Some v | _ -> None

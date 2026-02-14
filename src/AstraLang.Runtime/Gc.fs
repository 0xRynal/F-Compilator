namespace AstraLang.Runtime

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Gc =

    let rec mark (seen: HashSet<int>) (heap: Heap) (v: Value) =  // mark reachable
        match v with
        | VInt _ | VBool _ | VUnit -> ()
        | VTuple vs -> List.iter (mark seen heap) vs
        | VClosure addr ->
            if not (seen.Contains addr) then
                seen.Add addr |> ignore
                match Heap.tryGet heap addr with
                | Some (HTuple vs) -> List.iter (mark seen heap) vs
                | Some (HClosure (_, env)) -> List.iter (mark seen heap) env
                | None -> ()

    let run (heap: Heap) (roots: Value list) =  // mark-sweep
        let seen = HashSet<int>()
        List.iter (mark seen heap) roots
        let toRemove = heap.Keys |> Seq.filter (fun k -> not (seen.Contains k)) |> Seq.toList
        for k in toRemove do heap.Remove k |> ignore

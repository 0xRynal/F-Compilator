module AstraLang.Tests.VMTests

open Xunit
open AstraLang.Core
open AstraLang.Runtime
open AstraLang.VM

let run main = Machine.run { Closures = Map.empty; Main = main }

[<Fact>]
let vm_add () =
    match run [ Bytecode.PushInt 2; Bytecode.PushInt 3; Bytecode.Add ] with Ok (VInt 5) -> () | _ -> Assert.True false
[<Fact>]
let vm_sub () =
    match run [ Bytecode.PushInt 10; Bytecode.PushInt 3; Bytecode.Sub ] with Ok (VInt 7) -> () | _ -> Assert.True false
[<Fact>]
let vm_mul () =
    match run [ Bytecode.PushInt 2; Bytecode.PushInt 5; Bytecode.Mul ] with Ok (VInt 10) -> () | _ -> Assert.True false
[<Fact>]
let vm_div () =
    match run [ Bytecode.PushInt 8; Bytecode.PushInt 2; Bytecode.Div ] with Ok (VInt 4) -> () | _ -> Assert.True false
[<Fact>]
let vm_mod () =
    match run [ Bytecode.PushInt 7; Bytecode.PushInt 3; Bytecode.Mod ] with Ok (VInt 1) -> () | _ -> Assert.True false
[<Fact>]
let vm_store_load () =
    match run [ Bytecode.PushInt 42; Bytecode.Store 0; Bytecode.Load 0 ] with Ok (VInt 42) -> () | _ -> Assert.True false
[<Fact>]
let vm_store_load_two () =
    match run [ Bytecode.PushInt 1; Bytecode.Store 0; Bytecode.PushInt 2; Bytecode.Store 1; Bytecode.Load 0; Bytecode.Load 1; Bytecode.Add ] with Ok (VInt 3) -> () | _ -> Assert.True false
[<Fact>]
let vm_eq_true () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 1; Bytecode.Eq ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_eq_false () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Eq ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_neq () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Neq ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_lt () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Lt ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_le () =
    match run [ Bytecode.PushInt 2; Bytecode.PushInt 2; Bytecode.Le ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_gt () =
    match run [ Bytecode.PushInt 3; Bytecode.PushInt 2; Bytecode.Gt ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_ge () =
    match run [ Bytecode.PushInt 2; Bytecode.PushInt 2; Bytecode.Ge ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_and () =
    match run [ Bytecode.PushBool true; Bytecode.PushBool false; Bytecode.And ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_or () =
    match run [ Bytecode.PushBool true; Bytecode.PushBool false; Bytecode.Or ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_push_bool () =
    match run [ Bytecode.PushBool true ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_push_unit () =
    match run [ Bytecode.PushUnit ] with Ok VUnit -> () | _ -> Assert.True false
[<Fact>]
let vm_arith_chain () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Add; Bytecode.PushInt 3; Bytecode.Mul ] with Ok (VInt 9) -> () | _ -> Assert.True false
[<Fact>]
let vm_pop () =
    match run [ Bytecode.PushInt 1; Bytecode.Store 0; Bytecode.PushInt 2; Bytecode.Pop 1; Bytecode.Load 0 ] with Ok (VInt 1) -> () | _ -> Assert.True false
[<Fact>]
let vm_make_tuple_2 () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.MakeTuple 2 ] with Ok (VTuple [VInt 1; VInt 2]) -> () | _ -> Assert.True false
[<Fact>]
let vm_get_tuple_item () =
    match run [ Bytecode.PushInt 10; Bytecode.PushInt 20; Bytecode.MakeTuple 2; Bytecode.GetTupleItem 0 ] with Ok (VInt 10) -> () | _ -> Assert.True false
[<Fact>]
let vm_get_tuple_item_1 () =
    match run [ Bytecode.PushInt 10; Bytecode.PushInt 20; Bytecode.MakeTuple 2; Bytecode.GetTupleItem 1 ] with Ok (VInt 20) -> () | _ -> Assert.True false
[<Fact>]
let vm_store_overwrite () =
    match run [ Bytecode.PushInt 1; Bytecode.Store 0; Bytecode.PushInt 2; Bytecode.Store 0; Bytecode.Load 0 ] with Ok (VInt 2) -> () | _ -> Assert.True false
[<Fact>]
let vm_comparison_chain () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Lt; Bytecode.PushBool true; Bytecode.Eq ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_zero () =
    match run [ Bytecode.PushInt 0 ] with Ok (VInt 0) -> () | _ -> Assert.True false
[<Fact>]
let vm_negative () =
    match run [ Bytecode.PushInt 0; Bytecode.PushInt 5; Bytecode.Sub ] with Ok (VInt -5) -> () | _ -> Assert.True false
[<Fact>]
let vm_div_floor () =
    match run [ Bytecode.PushInt 7; Bytecode.PushInt 2; Bytecode.Div ] with Ok (VInt 3) -> () | _ -> Assert.True false
[<Fact>]
let vm_make_tuple_3 () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.PushInt 3; Bytecode.MakeTuple 3 ] with Ok (VTuple [VInt 1; VInt 2; VInt 3]) -> () | _ -> Assert.True false
[<Fact>]
let vm_and_true_true () =
    match run [ Bytecode.PushBool true; Bytecode.PushBool true; Bytecode.And ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_or_false_false () =
    match run [ Bytecode.PushBool false; Bytecode.PushBool false; Bytecode.Or ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_neq_false () =
    match run [ Bytecode.PushInt 3; Bytecode.PushInt 3; Bytecode.Neq ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_lt_false () =
    match run [ Bytecode.PushInt 5; Bytecode.PushInt 3; Bytecode.Lt ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_ge_true () =
    match run [ Bytecode.PushInt 10; Bytecode.PushInt 5; Bytecode.Ge ] with Ok (VBool true) -> () | _ -> Assert.True false
[<Fact>]
let vm_le_false () =
    match run [ Bytecode.PushInt 5; Bytecode.PushInt 3; Bytecode.Le ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_gt_false () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Gt ] with Ok (VBool false) -> () | _ -> Assert.True false
[<Fact>]
let vm_add_mul_precedence () =
    match run [ Bytecode.PushInt 1; Bytecode.PushInt 2; Bytecode.Add; Bytecode.PushInt 2; Bytecode.Mul ] with Ok (VInt 6) -> () | _ -> Assert.True false
[<Fact>]
let vm_slot_multiple () =
    match run [ Bytecode.PushInt 1; Bytecode.Store 0; Bytecode.PushInt 2; Bytecode.Store 1; Bytecode.PushInt 3; Bytecode.Store 2; Bytecode.Load 0; Bytecode.Load 1; Bytecode.Add; Bytecode.Load 2; Bytecode.Add ] with Ok (VInt 6) -> () | _ -> Assert.True false

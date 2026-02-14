module AstraLang.Tests.UnificationTests

open Xunit
open AstraLang.Core
open AstraLang.TypeInference

let ok r = Result.isOk r
let err r = Result.isError r
let getOk r = match r with Ok x -> x | Error _ -> failwith "expected Ok"

[<Fact>] let u_int_int () = Assert.True(ok (Unification.unify Type.TInt Type.TInt SourceSpan.Zero))
[<Fact>] let u_bool_bool () = Assert.True(ok (Unification.unify Type.TBool Type.TBool SourceSpan.Zero))
[<Fact>] let u_unit_unit () = Assert.True(ok (Unification.unify Type.TUnit Type.TUnit SourceSpan.Zero))
[<Fact>]
let u_var_int () =
    let v = TypeVar.TV("a", 1)
    let r = Unification.unify (Type.TVar v) Type.TInt SourceSpan.Zero
    Assert.True(ok r)
    let sub = getOk r
    Assert.True(Map.containsKey v sub)
    Assert.Equal(Type.TInt, Map.find v sub)
[<Fact>]
let u_int_var () =
    let v = TypeVar.TV("x", 2)
    let r = Unification.unify Type.TInt (Type.TVar v) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TInt, Map.find v (getOk r))
[<Fact>]
let u_arrow_var_int_arrow_bool_int () =
    let a = TypeVar.TV("a", 1)
    let r = Unification.unify (Type.TArrow(Type.TVar a, Type.TInt)) (Type.TArrow(Type.TBool, Type.TInt)) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TBool, Map.find a (getOk r))
[<Fact>]
let u_arrow_int_var_arrow_int_bool () =
    let b = TypeVar.TV("b", 2)
    let r = Unification.unify (Type.TArrow(Type.TInt, Type.TVar b)) (Type.TArrow(Type.TInt, Type.TBool)) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TBool, Map.find b (getOk r))
[<Fact>]
let u_tuple_same () =
    let r = Unification.unify (Type.TTuple [Type.TInt; Type.TBool]) (Type.TTuple [Type.TInt; Type.TBool]) SourceSpan.Zero
    Assert.True(ok r)
[<Fact>]
let u_tuple_var () =
    let v = TypeVar.TV("t", 1)
    let r = Unification.unify (Type.TTuple [Type.TVar v; Type.TBool]) (Type.TTuple [Type.TInt; Type.TBool]) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TInt, Map.find v (getOk r))
[<Fact>]
let u_arrow_two_vars () =
    let a, b = TypeVar.TV("a", 1), TypeVar.TV("b", 2)
    let r = Unification.unify (Type.TArrow(Type.TVar a, Type.TVar b)) (Type.TArrow(Type.TInt, Type.TBool)) SourceSpan.Zero
    Assert.True(ok r)
    let sub = getOk r
    Assert.Equal(Type.TInt, Map.find a sub)
    Assert.Equal(Type.TBool, Map.find b sub)
[<Fact>] let u_int_bool_fail () = Assert.True(err (Unification.unify Type.TInt Type.TBool SourceSpan.Zero))
[<Fact>] let u_unit_int_fail () = Assert.True(err (Unification.unify Type.TUnit Type.TInt SourceSpan.Zero))
[<Fact>]
let u_tuple_len_fail () =
    Assert.True(err (Unification.unify (Type.TTuple [Type.TInt]) (Type.TTuple [Type.TInt; Type.TBool]) SourceSpan.Zero))
[<Fact>]
let u_arrow_arg_fail () =
    Assert.True(err (Unification.unify (Type.TArrow(Type.TInt, Type.TUnit)) (Type.TArrow(Type.TBool, Type.TUnit)) SourceSpan.Zero))
[<Fact>]
let u_nested_arrow () =
    let a = TypeVar.TV("a", 1)
    let r = Unification.unify (Type.TArrow(Type.TArrow(Type.TVar a, Type.TInt), Type.TBool)) (Type.TArrow(Type.TArrow(Type.TUnit, Type.TInt), Type.TBool)) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TUnit, Map.find a (getOk r))
[<Fact>] let u_var_self () = let v = TypeVar.TV("x", 1) in Assert.True(ok (Unification.unify (Type.TVar v) (Type.TVar v) SourceSpan.Zero))
[<Fact>]
let u_two_vars_same () =
    let a, b = TypeVar.TV("a", 1), TypeVar.TV("b", 2)
    let r = Unification.unify (Type.TArrow(Type.TVar a, Type.TVar b)) (Type.TArrow(Type.TInt, Type.TInt)) SourceSpan.Zero
    Assert.True(ok r)
    let sub = getOk r
    Assert.Equal(Type.TInt, Map.find a sub)
    Assert.Equal(Type.TInt, Map.find b sub)
[<Fact>]
let u_tuple_three () =
    let r = Unification.unify (Type.TTuple [Type.TInt; Type.TBool; Type.TUnit]) (Type.TTuple [Type.TInt; Type.TBool; Type.TUnit]) SourceSpan.Zero
    Assert.True(ok r)
[<Fact>]
let u_arrow_unit_unit () =
    Assert.True(ok (Unification.unify (Type.TArrow(Type.TUnit, Type.TUnit)) (Type.TArrow(Type.TUnit, Type.TUnit)) SourceSpan.Zero))
[<Fact>]
let u_tuple_arrow () =
    let a = TypeVar.TV("a", 1)
    let r = Unification.unify (Type.TTuple [Type.TArrow(Type.TVar a, Type.TInt)]) (Type.TTuple [Type.TArrow(Type.TBool, Type.TInt)]) SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TBool, Map.find a (getOk r))
[<Fact>]
let u_int_unit_fail () = Assert.True(err (Unification.unify Type.TInt Type.TUnit SourceSpan.Zero))
[<Fact>]
let u_bool_int_fail () = Assert.True(err (Unification.unify Type.TBool Type.TInt SourceSpan.Zero))
[<Fact>]
let u_tuple_mismatch () =
    Assert.True(err (Unification.unify (Type.TTuple [Type.TInt; Type.TBool]) (Type.TTuple [Type.TInt; Type.TInt]) SourceSpan.Zero))
[<Fact>]
let u_arrow_ret_mismatch () =
    Assert.True(err (Unification.unify (Type.TArrow(Type.TInt, Type.TInt)) (Type.TArrow(Type.TInt, Type.TBool)) SourceSpan.Zero))
[<Fact>]
let u_deep_arrow () =
    let a = TypeVar.TV("a", 1)
    let t1 = Type.TArrow(Type.TArrow(Type.TArrow(Type.TVar a, Type.TInt), Type.TBool), Type.TUnit)
    let t2 = Type.TArrow(Type.TArrow(Type.TArrow(Type.TUnit, Type.TInt), Type.TBool), Type.TUnit)
    let r = Unification.unify t1 t2 SourceSpan.Zero
    Assert.True(ok r)
    Assert.Equal(Type.TUnit, Map.find a (getOk r))

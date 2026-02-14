module AstraLang.Tests.TypeInferenceTests

open Xunit
open AstraLang.Core
open AstraLang.Lexer
open AstraLang.Parser
open AstraLang.TypeInference

let parseAndInfer s = Lexer.tokenize s |> Result.bind Parser.parse |> Result.bind Infer.inferProgram
let ok r = Result.isOk r

[<Fact>] let i_42 () = Assert.True(ok (parseAndInfer "42"))
[<Fact>] let i_true () = Assert.True(ok (parseAndInfer "true"))
[<Fact>] let i_false () = Assert.True(ok (parseAndInfer "false"))
[<Fact>] let i_unit () = Assert.True(ok (parseAndInfer "()"))
[<Fact>] let i_let_id () = Assert.True(ok (parseAndInfer "let id = fun x -> x"))
[<Fact>] let i_if_then_else () = Assert.True(ok (parseAndInfer "if true then 1 else 2"))
[<Fact>] let i_let_const () = Assert.True(ok (parseAndInfer "let x = 10"))
[<Fact>] let i_apply () = Assert.True(ok (parseAndInfer "let f = fun x -> x in f 1"))
[<Fact>] let i_add () = Assert.True(ok (parseAndInfer "1 + 2"))
[<Fact>] let i_sub () = Assert.True(ok (parseAndInfer "10 - 3"))
[<Fact>] let i_mul () = Assert.True(ok (parseAndInfer "2 * 3"))
[<Fact>] let i_div () = Assert.True(ok (parseAndInfer "6 / 2"))
[<Fact>] let i_eq () = Assert.True(ok (parseAndInfer "1 = 1"))
[<Fact>] let i_neq () = Assert.True(ok (parseAndInfer "1 <> 2"))
[<Fact>] let i_lt () = Assert.True(ok (parseAndInfer "1 < 2"))
[<Fact>] let i_le () = Assert.True(ok (parseAndInfer "1 <= 2"))
[<Fact>] let i_gt () = Assert.True(ok (parseAndInfer "3 > 2"))
[<Fact>] let i_ge () = Assert.True(ok (parseAndInfer "3 >= 2"))
[<Fact>] let i_and () = Assert.True(ok (parseAndInfer "true && false"))
[<Fact>] let i_or () = Assert.True(ok (parseAndInfer "true || false"))
[<Fact>] let i_tuple2 () = Assert.True(ok (parseAndInfer "(1, true)"))
[<Fact>] let i_tuple3 () = Assert.True(ok (parseAndInfer "(1, true, ())"))
[<Fact>] let i_let_rec () = Assert.True(ok (parseAndInfer "let rec f n = if n <= 0 then 0 else n + f (n - 1)"))
[<Fact>] let i_compose () = Assert.True(ok (parseAndInfer "let compose f g x = f (g x)"))
[<Fact>] let i_const () = Assert.True(ok (parseAndInfer "let const x y = x"))
[<Fact>] let i_chain_let () = Assert.True(ok (parseAndInfer "let a = 1 let b = 2 let c = a + b"))
[<Fact>] let i_nested_if () = Assert.True(ok (parseAndInfer "if true then if false then 0 else 1 else 2"))
[<Fact>] let i_apply_twice () = Assert.True(ok (parseAndInfer "let f = fun x -> fun y -> x in f 1 2"))
[<Fact>] let i_compare_chain () = Assert.True(ok (parseAndInfer "1 < 2 && 2 < 3"))
[<Fact>] let i_arith_chain () = Assert.True(ok (parseAndInfer "1 + 2 * 3 - 4"))
[<Fact>] let i_tuple_nested () = Assert.True(ok (parseAndInfer "((1, 2), (true, false))"))
[<Fact>] let i_let_unit () = Assert.True(ok (parseAndInfer "let u = ()"))
[<Fact>] let i_fun_unit () = Assert.True(ok (parseAndInfer "fun () -> 42"))
[<Fact>] let i_match_bool () = Assert.True(ok (parseAndInfer "if false then 0 else 1"))
[<Fact>] let i_identity_apply () = Assert.True(ok (parseAndInfer "(fun x -> x) 42"))
[<Fact>] let i_two_funs () = Assert.True(ok (parseAndInfer "let f = fun x -> x let g = fun y -> y in f (g 1)"))
[<Fact>] let i_let_pair () = Assert.True(ok (parseAndInfer "let p = (1, 2)"))

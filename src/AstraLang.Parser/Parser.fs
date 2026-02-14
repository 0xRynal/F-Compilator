namespace AstraLang.Parser

open AstraLang.Core
open Ast

// Tokens -> untyped AST.
[<RequireQualifiedAccess>]
module Parser =

    type ParseError =
        | Unexpected of Token * string option
        | Expected of string * SourceSpan
        | EmptyInput

    let private spanOf (t: Token) =
        match t with
        | Token.TInt (_, s) | Token.TBool (_, s) | Token.TUnit s | Token.TIdent (_, s)
        | Token.TLet s | Token.TRec s | Token.TFn s | Token.TIf s | Token.TThen s | Token.TElse s
        | Token.TMatch s | Token.TWith s | Token.TTrue s | Token.TFalse s | Token.TArrow s
        | Token.TLParen s | Token.TRParen s | Token.TComma s | Token.TSemicolon s
        | Token.TPlus s | Token.TMinus s | Token.TStar s | Token.TSlash s | Token.TPercent s
        | Token.TEq s | Token.TNeq s | Token.TLt s | Token.TLe s | Token.TGt s | Token.TGe s
        | Token.TAnd s | Token.TOr s | Token.TBar s | Token.TUnderscore s | Token.TEOF s -> s

    type TokenStream = Token list

    let private peek (ts: TokenStream) = List.tryHead ts
    let private advance (ts: TokenStream) = List.tail ts

    let private expect expected (ts: TokenStream) =
        match ts with
        | t :: rest when (match expected, t with
                         | "int", Token.TInt _ -> true
                         | "ident", Token.TIdent _ -> true
                         | "let", Token.TLet _ -> true
                         | "(", Token.TLParen _ -> true
                         | ")", Token.TRParen _ -> true
                         | "->", Token.TArrow _ -> true
                         | ",", Token.TComma _ -> true
                         | ";", Token.TSemicolon _ -> true
                         | "if", Token.TIf _ -> true
                         | "then", Token.TThen _ -> true
                         | "else", Token.TElse _ -> true
                         | "fun", Token.TFn _ -> true
                         | "match", Token.TMatch _ -> true
                         | "with", Token.TWith _ -> true
                         | "rec", Token.TRec _ -> true
                         | _ -> false) -> Ok (t, rest)
        | t :: _ -> Error (Unexpected (t, Some expected))
        | [] -> Error (Expected (expected, SourceSpan.Zero))

    let rec parse (tokens: Token list) : Result<Ast.Program, ParseError> =
        let ts = match tokens with | [] -> [ Token.TEOF SourceSpan.Zero ] | _ -> tokens
        parseProgram ts

    and parseProgram ts =
        let rec decls acc ts =
            match peek ts with
            | Some (Token.TLet _) ->
                match parseDecl ts with
                | Ok (d, rest) -> decls (d :: acc) rest
                | Error e -> Error e
            | _ -> Ok (List.rev acc, ts)

        match decls [] ts with
        | Error e -> Error e
        | Ok (declList, rest) ->
            let finalExpr =
                match rest with
                | Token.TEOF _ :: _ -> None
                | _ ->
                    match parseExpr rest with
                    | Ok (e, rest2) ->
                        match rest2 with
                        | Token.TEOF _ :: _ -> Some e
                        | _ -> None
                    | _ -> None
            Ok (Program (declList, finalExpr))

    and parseDecl ts =
        match expect "let" ts with
        | Error e -> Error e
        | Ok (_, ts2) ->
            let isRec = match peek ts2 with | Some (Token.TRec _) -> true | _ -> false
            let ts3 = if isRec then advance ts2 else ts2
            match expect "ident" ts3 with
            | Error _ -> Error (Expected ("identifier", SourceSpan.Zero))
            | Ok (Token.TIdent (name, sp), ts4) ->
                let (param, ts5) =
                    if isRec then
                        match expect "ident" ts4 with
                        | Ok (Token.TIdent (p, _), r) -> (p, r)
                        | _ -> ("", ts4)
                    else ("", ts4)
                match expect "=" ts5 with
                | Error e -> Error e
                | Ok (_, tsEq) ->
                    match parseExpr tsEq with
                    | Error e -> Error e
                    | Ok (binding, rest) ->
                        let rest = match peek rest with Some (Token.TSemicolon _) -> advance rest | _ -> rest
                        if isRec then Ok (DLetRec (name, param, binding, sp), rest)
                        else Ok (DLet (name, binding, sp), rest)
            | _ -> Error (Expected ("identifier", SourceSpan.Zero))

    and parseExpr ts = parseLogic ts

    and parseLogic ts =
        parseCompare ts |> Result.bind (fun (left, rest) ->
            let rec loop left rest =
                match peek rest with
                | Some (Token.TAnd sp) ->
                    parseCompare (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.And, left, e, sp)) rest2)
                | Some (Token.TOr sp) ->
                    parseCompare (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Or, left, e, sp)) rest2)
                | _ -> Ok (left, rest)
            loop left rest)

    and parseCompare ts =
        parseArithmetic ts |> Result.bind (fun (left, rest) ->
            match peek rest with
            | Some (Token.TEq sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Eq, left, e, sp), r))
            | Some (Token.TNeq sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Neq, left, e, sp), r))
            | Some (Token.TLt sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Lt, left, e, sp), r))
            | Some (Token.TLe sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Le, left, e, sp), r))
            | Some (Token.TGt sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Gt, left, e, sp), r))
            | Some (Token.TGe sp) -> parseArithmetic (advance rest) |> Result.map (fun (e, r) -> (Ast.BinaryOp (Ast.BinOp.Ge, left, e, sp), r))
            | _ -> Ok (left, rest))

    and parseArithmetic ts =
        let term t = parseApp t
        let rec loop left rest =
            match peek rest with
            | Some (Token.TPlus sp) -> term (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Add, left, e, sp)) rest2)
            | Some (Token.TMinus sp) -> term (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Sub, left, e, sp)) rest2)
            | Some (Token.TStar sp) -> term (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Mul, left, e, sp)) rest2)
            | Some (Token.TSlash sp) -> term (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Div, left, e, sp)) rest2)
            | Some (Token.TPercent sp) -> term (advance rest) |> Result.bind (fun (e, rest2) -> loop (Ast.BinaryOp (Ast.BinOp.Mod, left, e, sp)) rest2)
            | _ -> Ok (left, rest)
        term ts |> Result.bind (fun (left, rest) -> loop left rest)

    and parseApp ts =
        parseAtom ts |> Result.bind (fun (first, rest) ->
            let rec buildApp left rest =
                match parseAtom rest with
                | Error _ -> Ok (left, rest)
                | Ok (arg, rest2) ->
                    let sp = spanOf (List.head rest)
                    buildApp (App (left, arg, sp)) rest2
            buildApp first rest)

    and parseAtom ts =
        match peek ts with
        | None -> Error (Expected ("expression", SourceSpan.Zero))
        | Some (Token.TInt (n, sp)) -> Ok (Lit (LInt n, sp), advance ts)
        | Some (Token.TTrue sp) -> Ok (Lit (LBool true, sp), advance ts)
        | Some (Token.TFalse sp) -> Ok (Lit (LBool false, sp), advance ts)
        | Some (Token.TUnit sp) -> Ok (Lit (LUnit, sp), advance ts)
        | Some (Token.TIdent (name, sp)) -> Ok (Var (name, sp), advance ts)
        | Some (Token.TLParen sp1) ->
            let rest = advance ts
            match parseExpr rest with
            | Error e -> Error e
            | Ok (e, rest2) ->
                match peek rest2 with
                | Some (Token.TRParen _) -> Ok (e, advance rest2)
                | Some (Token.TComma _) ->
                    let tupleRest = advance rest2
                    let rec parseTupleElems acc tr =
                        match parseExpr tr with
                        | Ok (elem, tr2) ->
                            let acc2 = elem :: acc
                            match peek tr2 with
                            | Some (Token.TComma _) -> parseTupleElems acc2 (advance tr2)
                            | Some (Token.TRParen _) -> Ok (List.rev acc2, advance tr2)
                            | _ -> Error (Expected (")", spanOf (List.head tr2)))
                        | Error e -> Error e
                    parseTupleElems [e] tupleRest |> Result.map (fun (elems, rest3) -> (Tuple (elems, sp1), rest3))
                | _ -> Error (Expected (") or ,", spanOf (List.head rest2)))
        | Some (Token.TFn sp) ->
            let rest = advance ts
            match expect "ident" rest with
            | Error e -> Error e
            | Ok (Token.TIdent (param, _), rest2) ->
                match expect "->" rest2 with
                | Error e -> Error e
                | Ok (_, rest3) ->
                    parseExpr rest3 |> Result.map (fun (body, rest4) -> (Lambda (param, body, sp), rest4))
        | Some (Token.TIf sp) ->
            let rest = advance ts
            parseExpr rest |> Result.bind (fun (cond, rest2) ->
                match expect "then" rest2 with
                | Error e -> Error e
                | Ok (_, rest3) ->
                    parseExpr rest3 |> Result.bind (fun (thenB, rest4) ->
                        match expect "else" rest4 with
                        | Error e -> Error e
                        | Ok (_, rest5) ->
                            parseExpr rest5 |> Result.map (fun (elseB, rest6) -> (If (cond, thenB, elseB, sp), rest6))))
        | Some (Token.TLet sp) ->
            match parseDecl ts with
            | Error e -> Error e
            | Ok (DLet (name, binding, _), rest) ->
                parseExpr rest |> Result.map (fun (body, rest2) -> (Let (name, binding, body, sp), rest2))
            | Ok (DLetRec (name, param, binding, _), rest) ->
                parseExpr rest |> Result.map (fun (body, rest2) -> (LetRec (name, param, binding, body, sp), rest2))
        | Some (Token.TMatch sp) ->
            let rest = advance ts
            parseExpr rest |> Result.bind (fun (scrut, rest2) ->
                match expect "with" rest2 with
                | Error e -> Error e
                | Ok (_, rest3) -> parseMatchCases rest3 [] |> Result.map (fun (cases, rest4) -> (Match (scrut, cases, sp), rest4)))
        | Some (Token.TEOF _) -> Error (Expected ("expression", SourceSpan.Zero))
        | Some t -> Error (Unexpected (t, Some "expression"))

    and parseMatchCases ts acc =
        match parsePattern ts with
        | Error e -> Error e
        | Ok (pat, rest) ->
            match expect "->" rest with
            | Error e -> Error e
            | Ok (_, rest2) ->
                match parseExpr rest2 with
                | Error e -> Error e
                | Ok (rhs, rest3) ->
                    let acc2 = (pat, rhs) :: acc
                    match peek rest3 with
                    | Some (Token.TBar _) -> parseMatchCases (advance rest3) acc2
                    | _ -> Ok (List.rev acc2, rest3)

    and parsePattern ts =
        match peek ts with
        | Some (Token.TUnderscore sp) -> Ok (PWild sp, advance ts)
        | Some (Token.TInt (n, sp)) -> Ok (PLit (LInt n, sp), advance ts)
        | Some (Token.TTrue sp) -> Ok (PLit (LBool true, sp), advance ts)
        | Some (Token.TFalse sp) -> Ok (PLit (LBool false, sp), advance ts)
        | Some (Token.TIdent (name, sp)) -> Ok (PVar (name, sp), advance ts)
        | Some (Token.TLParen sp) ->
            let rest = advance ts
            match parsePattern rest with
            | Ok (p, rest2) ->
                match peek rest2 with
                | Some (Token.TComma _) ->
                    let rec pats acc tr =
                        match parsePattern tr with
                        | Ok (p2, tr2) ->
                            let acc2 = p2 :: acc
                            match peek tr2 with
                            | Some (Token.TComma _) -> pats acc2 (advance tr2)
                            | Some (Token.TRParen _) -> Ok (PTuple (List.rev acc2, sp), advance tr2)
                            | _ -> Error (Expected (")", spanOf (List.head tr2)))
                        | Error e -> Error e
                    pats [p] (advance rest2)
                | Some (Token.TRParen _) -> Ok (p, advance rest2)
                | _ -> Error (Expected (", or )", spanOf (List.head rest2)))
            | Error e -> Error e
        | _ -> Error (Expected ("pattern", SourceSpan.Zero))

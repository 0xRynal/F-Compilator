namespace AstraLang.Lexer

open AstraLang.Core

// Source -> token list.
[<RequireQualifiedAccess>]
module Lexer =

    type LexError =
        | InvalidChar of char * Position
        | UnclosedString of Position
        | InvalidNumber of string * Position

    let private pos (line, col) = { Position.Line = line; Column = col }
    let private span startPos endPos = { SourceSpan.Start = startPos; End = endPos }

    let tokenize (input: string) : Result<Token list, LexError> =  // main entry
        let n = input.Length
        let mutable i = 0
        let mutable line = 1
        let mutable col = 1
        let tokens = ResizeArray<Token>()

        let cur () = if i < n then input[i] else '\x00'
        let adv () = if i < n then i <- i + 1; if i > 0 && input[i - 1] = '\n' then line <- line + 1; col <- 1 else col <- col + 1
        let startPos () = pos (line, col)

        let rec skipWhite () =
            if i >= n then ()
            else
                match cur () with
                | ' ' | '\t' -> adv (); skipWhite ()
                | '\n' | '\r' -> adv (); if i < n && cur () = '\n' then adv (); skipWhite (); skipWhite () else skipWhite ()
                | '/' when i + 1 < n && input[i + 1] = '/' ->
                    adv (); adv ()
                    while i < n && cur () <> '\n' do adv ()
                    skipWhite ()
                | _ -> ()

        let readIdentOrKeyword start =
            let beginCol = col
            while i < n && (System.Char.IsLetterOrDigit (cur ()) || cur () = '_' || cur () = '\'') do adv ()
            let s = input.Substring(start, i - start)
            let endP = pos (line, col)
            let sp = { Start = pos (line, beginCol); End = endP }
            match s with
            | "let" -> Token.TLet sp
            | "rec" -> Token.TRec sp
            | "fun" -> Token.TFn sp
            | "if" -> Token.TIf sp
            | "then" -> Token.TThen sp
            | "else" -> Token.TElse sp
            | "match" -> Token.TMatch sp
            | "with" -> Token.TWith sp
            | "true" -> Token.TTrue sp
            | "false" -> Token.TFalse sp
            | "unit" -> Token.TUnit sp
            | "_" -> Token.TUnderscore sp
            | _ -> Token.TIdent (s, sp)

        let readInt start =
            let beginCol = col
            while i < n && System.Char.IsDigit (cur ()) do adv ()
            let s = input.Substring(start, i - start)
            let endP = pos (line, col)
            match System.Int32.TryParse s with
            | true, v -> tokens.Add (Token.TInt (v, span (pos (line, beginCol)) endP)); true
            | _ -> false

        let rec scan () =
            if i >= n then
                tokens.Add (Token.TEOF (span (startPos ()) (startPos ())))
            else
                skipWhite ()
                if i >= n then tokens.Add (Token.TEOF (span (startPos ()) (startPos ()))); ()
                else
                    let startP = startPos ()
                    let c = cur ()
                    match c with
                    | '(' -> adv (); tokens.Add (Token.TLParen (span startP (startPos ()))); scan ()
                    | ')' -> adv (); tokens.Add (Token.TRParen (span startP (startPos ()))); scan ()
                    | ',' -> adv (); tokens.Add (Token.TComma (span startP (startPos ()))); scan ()
                    | ';' -> adv (); tokens.Add (Token.TSemicolon (span startP (startPos ()))); scan ()
                    | '+' -> adv (); tokens.Add (Token.TPlus (span startP (startPos ()))); scan ()
                    | '-' ->
                        adv ()
                        if i < n && System.Char.IsDigit (cur ()) then
                            let start2 = i
                            let beginCol = col
                            while i < n && System.Char.IsDigit (cur ()) do adv ()
                            let s = input.Substring(start2, i - start2)
                            match System.Int32.TryParse s with
                            | true, v -> tokens.Add (Token.TInt (-v, span (pos (line, beginCol - 1)) (startPos ()))); ()
                            | _ -> ()
                        else
                            tokens.Add (Token.TMinus (span startP (startPos ())))
                        scan ()
                    | '*' -> adv (); tokens.Add (Token.TStar (span startP (startPos ()))); scan ()
                    | '/' -> adv (); tokens.Add (Token.TSlash (span startP (startPos ()))); scan ()
                    | '%' -> adv (); tokens.Add (Token.TPercent (span startP (startPos ()))); scan ()
                    | '=' ->
                        adv ()
                        if i < n && cur () = '>' then adv (); tokens.Add (Token.TArrow (span startP (startPos ())))
                        elif i < n && cur () = '=' then adv (); tokens.Add (Token.TEq (span startP (startPos ())))
                        else tokens.Add (Token.TEq (span startP (startPos ())))
                        scan ()
                    | '<' ->
                        adv ()
                        if i < n && cur () = '=' then adv (); tokens.Add (Token.TLe (span startP (startPos ())))
                        else tokens.Add (Token.TLt (span startP (startPos ())))
                        scan ()
                    | '>' ->
                        adv ()
                        if i < n && cur () = '=' then adv (); tokens.Add (Token.TGe (span startP (startPos ())))
                        else tokens.Add (Token.TGt (span startP (startPos ())))
                        scan ()
                    | '!' when i + 1 < n && input[i + 1] = '=' ->
                        adv (); adv (); tokens.Add (Token.TNeq (span startP (startPos ()))); scan ()
                    | '|' ->
                        adv ()
                        if i < n && cur () = '|' then adv (); tokens.Add (Token.TOr (span startP (startPos ())))
                        else tokens.Add (Token.TBar (span startP (startPos ())))
                        scan ()
                    | '&' when i + 1 < n && input[i + 1] = '&' ->
                        adv (); adv (); tokens.Add (Token.TAnd (span startP (startPos ()))); scan ()
                    | _ when System.Char.IsDigit c ->
                        let beginCol = col
                        if readInt i then scan () else Error (InvalidNumber (input.Substring(i, min 20 (n - i)), pos (line, beginCol))) |> ignore; scan ()
                    | _ when System.Char.IsLetter c || c = '_' || c = '\'' ->
                        tokens.Add (readIdentOrKeyword i); scan ()
                    | '\x00' when i >= n -> tokens.Add (Token.TEOF (span startP startP)); ()
                    | _ when System.Char.IsWhiteSpace c -> scan ()
                    | c -> raise (System.Exception (sprintf "Unexpected char '%c' at %d:%d" c line col))

        try
            scan ()
            Ok (List.ofSeq tokens)
        with
        | :? System.Exception as e ->
            let msg = e.Message
            if msg.StartsWith "Unexpected" then
                Error (InvalidChar (cur (), startPos ()))
            else
                reraise ()

namespace AstraLang.Core

// Lexer tokens + span.
type Token =
    | TInt of int * SourceSpan
    | TBool of bool * SourceSpan
    | TUnit of SourceSpan
    | TIdent of string * SourceSpan
    | TLet of SourceSpan
    | TRec of SourceSpan
    | TFn of SourceSpan
    | TIf of SourceSpan
    | TThen of SourceSpan
    | TElse of SourceSpan
    | TMatch of SourceSpan
    | TWith of SourceSpan
    | TTrue of SourceSpan
    | TFalse of SourceSpan
    | TArrow of SourceSpan
    | TLParen of SourceSpan
    | TRParen of SourceSpan
    | TComma of SourceSpan
    | TSemicolon of SourceSpan
    | TPlus of SourceSpan
    | TMinus of SourceSpan
    | TStar of SourceSpan
    | TSlash of SourceSpan
    | TPercent of SourceSpan
    | TEq of SourceSpan
    | TNeq of SourceSpan
    | TLt of SourceSpan
    | TLe of SourceSpan
    | TGt of SourceSpan
    | TGe of SourceSpan
    | TAnd of SourceSpan
    | TOr of SourceSpan
    | TBar of SourceSpan
    | TUnderscore of SourceSpan
    | TEOF of SourceSpan

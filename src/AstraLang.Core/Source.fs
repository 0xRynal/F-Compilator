namespace AstraLang.Core

// Source position (line, column).
[<Struct>]
type Position =
    { Line: int
      Column: int }
    static member Zero = { Line = 1; Column = 1 }

// Span for error messages.
[<Struct>]
type SourceSpan =
    { Start: Position
      End: Position }
    static member Zero = { Start = Position.Zero; End = Position.Zero }
    member s.Format() = sprintf "line %d, column %d" s.Start.Line s.Start.Column

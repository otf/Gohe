module Ast

open FParsec
open FParsec.Applicative

type Type = 
  | StringValue of string

type Parser<'t> = Parser<'t, unit>

let pStringValueChar : Parser<_> = attempt ('"' <! pstring "\\\"") <|> noneOf "\""
let pStringValue : Parser<_> = StringValue <!> pchar '"' *> manyChars pStringValueChar <* pchar '"' 
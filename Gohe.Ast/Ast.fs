module Ast

open FParsec
open FParsec.Applicative

type Type = 
  | StringValue of string | IntValue of int | FloatValue of float

type Parser<'t> = Parser<'t, unit>

let pStringValueChar : Parser<_> = attempt ('"' <! pstring "\\\"") <|> noneOf "\""
let pStringValue : Parser<_> = StringValue <!> pchar '"' *> manyChars pStringValueChar <* pchar '"' 
let pIntValue : Parser<_> = IntValue <!> pint32
let pFloatValue : Parser<_> = FloatValue <!> pfloat
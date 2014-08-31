module Ast

open FParsec
open FParsec.Applicative

type Type = 
  | StringValue of string | IntValue of int | FloatValue of float
  | Bool | String | Int  | Float | BigInt | Guid 
  | DateTime of format : string option | TimeSpan of format : string option
  | ChoiceStringValues of string list
  | IntRange of int * int
  | Regex of pattern:string

// [b, e)
let intRange b e = IntRange(b, e - 1)

// [b, e]
let intRange2 b e = IntRange(b, e)

type Parser<'t> = Parser<'t, unit>

let pStringValueChar : Parser<_> = attempt ('"' <! pstring "\\\"") <|> noneOf "\""
let pStringValue : Parser<_> = StringValue <!> pchar '"' *> manyChars pStringValueChar <* pchar '"' 
let pIntValue : Parser<_> = IntValue <!> pint32
let pFloatValue : Parser<_> = FloatValue <!> pfloat
let pPrimitiveType f typeName = f <! pstring typeName
let pFormatChar : Parser<_> =  attempt ('>' <! pstring "\\>") <|> noneOf ">"
let pFormatText = manyChars pFormatChar
let pFormat = between (pstring "<") (pstring ">") pFormatText
let pPrimitiveTypeWithFormat f typeName = f <!> pstring typeName *> (opt pFormat)
let pChoiceStringValues = 
  between (pstring "(") (pstring ")") <|
  ((List.map (function StringValue v -> v | _ -> failwith "internal error") >> ChoiceStringValues) <!> (sepBy1 (spaces *> pStringValue <* spaces) (pchar '|')))

let pIntRange : Parser<_> = 
  between (pstring "[") (pstring ")") <|
  (intRange <!> spaces *> pint32 <* spaces <* pchar ',' <* spaces <*> pint32 <* spaces)
  
let pIntRange2 : Parser<_> = 
  between (pstring "[") (spaces *> pstring "]") <|
  (intRange2 <!> spaces *> pint32 <* spaces <* pchar ',' <* spaces <*> pint32 <* spaces)

let pRegexChar : Parser<_> = attempt ('/' <! pstring "\\/") <|> noneOf "/"
let pRegex : Parser<_> = Regex <!> pchar '/' *> manyChars pRegexChar <* pchar '/' 

let pType =
  pChoiceStringValues
  <|> pIntRange |> attempt
  <|> pIntRange2
  <|> pRegex
  <|> pStringValue
  <|> pIntValue
  <|> pFloatValue
  <|> pPrimitiveType Bool "Bool"
  <|> pPrimitiveType String "String"
  <|> pPrimitiveType Int "Int" 
  <|> pPrimitiveType Float "Float" 
  <|> pPrimitiveType BigInt "BigInt" 
  <|> pPrimitiveType Guid "Guid" 
  <|> pPrimitiveTypeWithFormat DateTime "DateTime"
  <|> pPrimitiveTypeWithFormat TimeSpan "TimeSpan"
﻿module Ast

open FParsec
open FParsec.Applicative

type Type = 
  | StringValue of string | IntValue of int | FloatValue of float
  | Bool | String | Int  | Float | BigInt | Guid 
  | DateTime of format : string option | TimeSpan of format : string option
  | ChoiceStringValues of string list

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

let pType =
  pChoiceStringValues
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
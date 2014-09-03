module Ast

open FParsec
open FParsec.Applicative

type Type = 
  | StringValue of string | IntValue of int | FloatValue of float
  | Bool | String | Int  | Float | BigInt | Guid 
  | DateTime of format : string option | TimeSpan of format : string option
  | RestrictedString of string list
  | IntRange of int * int
  | Regex of pattern:string

// [b, e)
let intRange b e = IntRange(b, e - 1)

// [b, e]
let intRange2 b e = IntRange(b, e)

type XdefOccurs =
  | Required
  | Many
  | RequiredMany
  | Optional

type XdefAttribute = {
  Name : string
  Type : Type
  Comment : string option
}

let xdefAttribute nm typ comm = { Name = nm; Type = typ; Comment = comm }

type XdefSimpleElement = {
  Name : string
  Occurs : XdefOccurs
  Type : Type
  Comment : string option
}

let xdefSimpleElement nm occurs typ comm = { Name = nm; Occurs = occurs; Type = typ; Comment = comm }

type XdefSequenceElement = {
  Name : string
  Occurs : XdefOccurs
  Nodes : XdefNode list
  Comment : string option
}

and XdefNode = 
  | SequenceElement of XdefSequenceElement
  | SimpleElement of XdefSimpleElement
  | Attribute of XdefAttribute
// TODO:  | Module of string

let xdefSequenceElement nm occurs comm nodes = { Name = nm; Occurs = occurs; Nodes = nodes; Comment = comm }


type IndentLevel = int
type UserState = IndentLevel
type Parser<'t> = Parser<'t, UserState>
let indent = updateUserState ((+) 1)
let unindent = updateUserState (fun x -> System.Math.Max(x - 1, 0))

let pXdefName : Parser<_> = regex "\w+"
let pStringValueChar : Parser<_> = attempt ('"' <! pstring "\\\"") <|> noneOf "\""
let pStringValue : Parser<_> = StringValue <!> pchar '"' *> manyChars pStringValueChar <* pchar '"' 
let pIntValue : Parser<_> = IntValue <!> pint32
let pFloatValue : Parser<_> = FloatValue <!> pfloat
let pPrimitiveType f typeName = f <! pstring typeName
let pFormatChar : Parser<_> =  attempt ('>' <! pstring "\\>") <|> noneOf ">"
let pFormatText = manyChars pFormatChar
let pFormat = between (pstring "<") (pstring ">") pFormatText
let pPrimitiveTypeWithFormat f typeName = f <!> pstring typeName *> (opt pFormat)
let pRestrictedString = 
  between (pstring "(") (pstring ")") <|
  ((List.map (function StringValue v -> v | _ -> failwith "internal error") >> RestrictedString) <!> (sepBy1 (spaces *> pStringValue <* spaces) (pchar '|')))

let pIntRange : Parser<_> = 
  between (pstring "[") (pstring ")") <|
  (intRange <!> spaces *> pint32 <* spaces <* pchar ',' <* spaces <*> pint32 <* spaces)
  
let pIntRange2 : Parser<_> = 
  between (pstring "[") (spaces *> pstring "]") <|
  (intRange2 <!> spaces *> pint32 <* spaces <* pchar ',' <* spaces <*> pint32 <* spaces)

let pRegexChar : Parser<_> = attempt ('/' <! pstring "\\/") <|> noneOf "/"
let pRegex : Parser<_> = Regex <!> pchar '/' *> manyChars pRegexChar <* pchar '/' 

let pType =
  pRestrictedString
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

let pTyped = spaces *> pchar ':' *> spaces *> pType

let pOccurs : Parser<_> =
  (Many <! pstring "*")
  <|> (RequiredMany <! pstring "+")
  <|> (Optional <! pstring "?")
  <|> (preturn Required)

let pIndent = attempt <| parse { 
  let! indentLevel = getUserState
  let indentLevel = (indentLevel) * 2
  do! skipManyMinMaxSatisfy indentLevel indentLevel ((=) ' ')
}

let pCommentChar : Parser<_> = noneOf ['\n'; '\r']
let pComment : Parser<_> = 
  (spaces *> pstring "--" *> spaces *> manyChars pCommentChar <* (skipNewline <|> eof) |> opt) |> attempt
  <|> (None <! (skipNewline <|> eof))

let pXdefAttribute = 
  xdefAttribute <!> pIndent *> pchar '@' *> pXdefName <*> pTyped <*> pComment

let (pNodes, pNodesImpl) = createParserForwardedToRef ()
let (pNode, pNodeImpl) = createParserForwardedToRef ()

let pXdefSimpleElement = 
  xdefSimpleElement <!> pIndent *> pXdefName <*> pOccurs <*> pTyped <*> pComment

let pXdefSequenceElement =
  xdefSequenceElement <!> pIndent *> pXdefName <*> pOccurs <*> pComment <*> indent *> pNodes

do pNodesImpl := (many pNode) <* unindent

do pNodeImpl :=
    (Attribute <!> pXdefAttribute) |> attempt
    <|> (SimpleElement <!> pXdefSimpleElement) |> attempt
    <|> (SequenceElement <!> pXdefSequenceElement)
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

type XdefRestriction =
  | Many
  | Choice
  | Option

type XdefAttribute = {
  Name : string
  Type : Type
  Comment : string option
}

let xdefAttribute nm typ comm = { Name = nm; Type = typ; Comment = comm }

type XdefValueElement = {
  Name : string
  Restriction : XdefRestriction option
  Type : Type
  Comment : string option
}

let xdefValueElement nm r typ comm = { Name = nm; Restriction = r; Type = typ; Comment = comm }

type XdefElement = {
  Name : string
  Restriction : XdefRestriction option
  Nodes : XdefNode list
  Comment : string option
}

and XdefNode = 
  | Element of XdefElement
  | ValueElement of XdefValueElement
  | Attribute of XdefAttribute
// TODO:  | Module of string

let xdefElement nm r comm nodes = { Name = nm; Restriction = r; Nodes = nodes; Comment = comm }


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

let pTyped = spaces *> pchar ':' *> spaces *> pType

let pRestriction : Parser<_> =
  (Many <! pstring "*")
  <|> (Option <! pstring "?")
  <|> (Choice <! pstring "|")

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

let pXdefValueElement = 
  xdefValueElement <!> pIndent *> pXdefName <*> (opt pRestriction) <*> pTyped <*> pComment

let pXdefElement =
  xdefElement <!> pIndent *> pXdefName <*> (opt pRestriction) <*> pComment <*> indent *> pNodes

do pNodesImpl := (many pNode) <* unindent

do pNodeImpl :=
    (Attribute <!> pXdefAttribute) |> attempt
    <|> (ValueElement <!> pXdefValueElement) |> attempt
    <|> (Element <!> pXdefElement)
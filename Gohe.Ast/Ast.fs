module Ast

open FParsec
open FParsec.Applicative

type XdefSimpleType = 
  | FixedString of string | FixedInt of int | FixedFloat of float
  | Bool | String | Int  | Float | Decimal | Guid 
  | DateTime of format : string option | TimeSpan of format : string option
  | RestrictedString of string list
  | IntRange of int * int
  | Pattern of string

// [b, e)
let intRange b e = IntRange(b, e - 1)

// [b, e]
let intRange2 b e = IntRange(b, e)

type XdefOccurrence =
  | Required
  | Many
  | RequiredMany
  | Optional
  | Specified of min : int option * max : int option

let xdefSpecified min max = Specified (min, max)

type XdefAttribute = {
  Name : string
  Occurrence : XdefOccurrence
  Type : XdefSimpleType
  Comment : string option
}

let xdefAttribute nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm }

type XdefOrder =
  | Sequence
  | Choice
  | All

type XdefComplexType = {
  Order : XdefOrder
  Occurrence : XdefOccurrence
  Nodes : XdefNode list
}

and XdefElementType =
  | Simple of XdefSimpleType
  | Complex of XdefComplexType

and XdefElement = {
  Name : string
  Occurrence : XdefOccurrence
  Type : XdefElementType
  Comment : string option
}

and XdefNode = 
  | Element of XdefElement
  | Attribute of XdefAttribute
// TODO:  | Module of string

let xdefComplexType order occurs nodes = { Order = order; Occurrence = occurs; Nodes = nodes }
let xdefElement nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm }

type IndentLevel = int
type UserState = IndentLevel
type Parser<'t> = Parser<'t, UserState>
let indent = updateUserState ((+) 1)
let unindent = updateUserState (fun x -> System.Math.Max(x - 1, 0))

let pSpaces : Parser<_> = many (pchar ' ')
let pXdefName : Parser<_> = regex "\w+"
let pFixedStringChar : Parser<_> = attempt ('"' <! pstring "\\\"") <|> noneOf "\""
let pFixedString : Parser<_> = FixedString <!> pchar '"' *> manyChars pFixedStringChar <* pchar '"' 
let pFixedInt : Parser<_> = FixedInt <!> pint32
let pFixedFloat : Parser<_> = FixedFloat <!> pfloat
let pPrimitiveType f typeName = f <! pstring typeName
let pFormatChar : Parser<_> =  attempt ('>' <! pstring "\\>") <|> noneOf ">"
let pFormatText = manyChars pFormatChar
let pFormat = between (pstring "<") (pstring ">") pFormatText
let pPrimitiveTypeWithFormat f typeName = f <!> pstring typeName *> (opt pFormat)
let pRestrictedString = 
  between (pstring "(") (pstring ")") <|
  ((List.map (function FixedString v -> v | _ -> failwith "internal error") >> RestrictedString) <!> (sepBy1 (pSpaces *> pFixedString <* pSpaces) (pchar '|')))

let pIntRange : Parser<_> = 
  between (pstring "[") (pstring ")") <|
  (intRange <!> pSpaces *> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32 <* pSpaces)
  
let pIntRange2 : Parser<_> = 
  between (pstring "[") (pSpaces *> pstring "]") <|
  (intRange2 <!> pSpaces *> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32 <* pSpaces)

let pPatternChar : Parser<_> = attempt ('/' <! pstring "\\/") <|> noneOf "/"
let pPattern : Parser<_> = Pattern <!> pchar '/' *> manyChars pPatternChar <* pchar '/' 

let pXdefSimpleType =
  pRestrictedString
  <|> pIntRange |> attempt
  <|> pIntRange2
  <|> pPattern
  <|> pFixedString
  <|> pFixedInt
  <|> pFixedFloat
  <|> pPrimitiveType Bool "Bool"
  <|> pPrimitiveType String "String"
  <|> pPrimitiveType Int "Int" 
  <|> pPrimitiveType Float "Float" 
  <|> pPrimitiveType Decimal "Decimal" 
  <|> pPrimitiveType Guid "Guid" 
  <|> pPrimitiveTypeWithFormat DateTime "DateTime"
  <|> pPrimitiveTypeWithFormat TimeSpan "TimeSpan"

let pXdefSimpleTyped = pchar ':' *> pSpaces *> pXdefSimpleType

let pOrder =
  (Sequence <! pstring "Sequence") |> attempt
  <|> (Choice <! pstring "Choice") |> attempt
  <|> (All <! pstring "All")

let pAttributeOccurrence : Parser<_> =
  (Optional <! pstring "?")
  <|> (preturn Required)

let pOccurrence : Parser<_> =
  (between (pstring "{") (pstring "}") (xdefSpecified <!> pSpaces *> (pint32 |> opt) <* pSpaces <* pstring ".." <* pSpaces <*> (pint32 |> opt) <* pSpaces)) |> attempt
  <|> (Many <! pstring "*")
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
  (pstring "--" *> pSpaces *> manyChars pCommentChar |> opt) |> attempt
  <|> (preturn None)

let pXdefAttribute = 
  xdefAttribute <!> pIndent *> pchar '@' *> pXdefName <*> pAttributeOccurrence <*> pSpaces *> pXdefSimpleTyped <*> pSpaces *> pComment <* (newline |> opt)

let (pNodes, pNodesImpl) = createParserForwardedToRef ()
let (pNode, pNodeImpl) = createParserForwardedToRef ()

let pXdefSimpleElement = 
  xdefElement <!> pIndent *> pXdefName <*> pOccurrence <* pSpaces <*> (Simple <!> pXdefSimpleTyped) <*> pSpaces *> pComment <* (newline |> opt)

// CommentはElementに対してつけたいため、NodesだけあとでParseする
let pXdefComplexTyped = 
  (xdefComplexType <!> pstring "::" *> pSpaces *> pOrder <*> pOccurrence) |> attempt
  <|> (preturn <| xdefComplexType Sequence Required)

let pXdefComplexElement =
  (fun nm occurs fType comm nodes -> xdefElement nm occurs (Complex <| fType nodes) comm)
  <!> pIndent *> pXdefName <*> pOccurrence <* pSpaces <*> pXdefComplexTyped <*> pSpaces *> pComment <*> ((newline *> indent *> pNodes) <|> (preturn []))

do pNodesImpl := many pNode <* unindent

do pNodeImpl :=
    (Attribute <!> pXdefAttribute) |> attempt
    <|> (Element <!> pXdefSimpleElement) |> attempt
    <|> (Element <!> pXdefComplexElement)
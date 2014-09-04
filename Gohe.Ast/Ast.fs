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

/// 出現回数を表す型です。
/// 明示的に指定されなかった場合、Requiredと推論されます。
type XdefOccurrence =
  | Required
  | Many
  | RequiredMany
  | Optional
  | Specified of min : int option * max : int option

let xdefSpecified min max = Specified (min, max)

/// 属性を表す型です。
/// OccurrenceはRequiredもしくはOptionalを指定することができます。
type XdefAttribute = {
  Name : string
  Occurrence : XdefOccurrence
  Type : XdefSimpleType
  Comment : string option
}

let xdefAttribute nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm }

/// 順序インジケータを表す型です。
/// 明示的に指定されなかった場合、Sequenceと推論されます。
type XdefOrder =
  | Sequence
  | Choice
  | All

type XdefComplexType = {
  Order : XdefOrder
  Occurrence : XdefOccurrence
  Nodes : XdefNode list
}

/// 要素型を表す型です。
/// 明示的に指定されなかった場合、Complex(順序インジケータはSequence)と推論されます。
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
let pBracket openString closeString p = between (pstring openString) (pstring closeString) (pSpaces *> p <* pSpaces)
let pXdefName : Parser<_> = regex "\w+"
let pStringLiteral openChar closeChar : Parser<_> = 
  let pEscapedStringChar : Parser<_> =
    (closeChar <! pstring ("\\" + (closeChar.ToString()))) |> attempt
    <|> noneOf [closeChar]
  pchar openChar *> manyChars pEscapedStringChar <* pchar closeChar 
let pFixedString : Parser<_> = FixedString <!> pStringLiteral '"' '"'
let pFixedInt : Parser<_> = FixedInt <!> pint32
let pFixedFloat : Parser<_> = FixedFloat <!> pfloat
let pPrimitiveType f typeName = f <! pstring typeName
let pFormat = pStringLiteral '<' '>'
let pPrimitiveTypeWithFormat f typeName = f <!> pstring typeName *> (opt pFormat)
let pRestrictedString = 
  pBracket "(" ")" <|
  ((List.map (function FixedString v -> v | _ -> failwith "internal error") >> RestrictedString) <!> (sepBy1 (pSpaces *> pFixedString <* pSpaces) (pchar '|')))

let pIntRange : Parser<_> = 
  pBracket "[" ")" <|
  (intRange <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32)
  
let pIntRange2 : Parser<_> = 
  pBracket "[" "]" <|
  (intRange2 <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32)

let pPattern : Parser<_> = Pattern <!> pStringLiteral '/' '/'

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
  (pBracket "{" "}" (xdefSpecified <!> (pint32 |> opt) <* pSpaces <* pstring ".." <* pSpaces <*> (pint32 |> opt))) |> attempt
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
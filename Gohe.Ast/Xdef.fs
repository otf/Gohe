module Xdef

open FParsec
open FParsec.Applicative

type SimpleType = 
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
type Occurrence =
  | Required
  | Many
  | RequiredMany
  | Optional
  | Specified of min : int option * max : int option

let specific min max = Specified (min, max)

/// 属性を表す型です。
/// OccurrenceはRequiredもしくはOptionalを指定することができます。
type Attribute = {
  Name : string
  Occurrence : Occurrence
  Type : SimpleType
  Comment : string option
}

let attribute nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm }

/// 順序インジケータを表す型です。
/// 明示的に指定されなかった場合、Sequenceと推論されます。
type Order =
  | Sequence
  | Choice
  | All

type ComplexType = {
  Order : Order
  Occurrence : Occurrence
  Nodes : Node list
}

/// 要素型を表す型です。
/// 明示的に指定されなかった場合、Complex(順序インジケータはSequence)と推論されます。
and ElementType =
  | Simple of SimpleType
  | Complex of ComplexType

and Element = {
  Name : string
  Occurrence : Occurrence
  Type : ElementType
  Comment : string option
}

and Node = 
  | Element of Element
  | Attribute of Attribute
// TODO:  | Module of string

let complexType order occurs nodes = { Order = order; Occurrence = occurs; Nodes = nodes }
let element nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm }

type IndentLevel = int
type UserState = IndentLevel
type Parser<'t> = Parser<'t, UserState>
let indent = updateUserState ((+) 1)
let unindent = updateUserState (fun x -> x - 1)

let pSpaces : Parser<_> = many (pchar ' ')
let pBracket openString closeString p = between (pstring openString) (pstring closeString) (pSpaces *> p <* pSpaces)
let pName : Parser<_> = regex "\w+"
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

let pSimpleType =
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

let pSimpleTyped = pchar ':' *> pSpaces *> pSimpleType

let pOrder =
  (Sequence <! pstring "Sequence")
  <|> (Choice <! pstring "Choice")
  <|> (All <! pstring "All")

let pAttributeOccurrence : Parser<_> =
  (Optional <! pstring "?")
  <|> (preturn Required)

let pOccurrence : Parser<_> =
  (pBracket "{" "}" (specific <!> (pint32 |> opt) <* pSpaces <* pstring ".." <* pSpaces <*> (pint32 |> opt))) |> attempt
  <|> (Many <! pstring "*")
  <|> (RequiredMany <! pstring "+")
  <|> (Optional <! pstring "?")
  <|> (preturn Required)

let pIndent = parse { 
  let! indentLevel = getUserState
  let indentLevel = (indentLevel) * 2
  do! skipManyMinMaxSatisfy indentLevel indentLevel ((=) ' ')
}

let pCommentChar : Parser<_> = noneOf ['\n'; '\r']
let pComment : Parser<_> = 
  (pstring "--" *> pSpaces *> manyChars pCommentChar |> opt) |> attempt
  <|> (preturn None)

let pXdefAttribute = 
  attribute <!> pIndent *> pchar '@' *> pName <*> pAttributeOccurrence <*> pSpaces *> pSimpleTyped <*> pSpaces *> pComment <* (newline |> opt)

let (pNodes, pNodesImpl) = createParserForwardedToRef ()
let (pNode, pNodeImpl) = createParserForwardedToRef ()

let pSimpleElement = 
  element <!> pIndent *> pName <*> pOccurrence <* pSpaces <*> (Simple <!> pSimpleTyped) <*> pSpaces *> pComment <* (newline |> opt)

// CommentはElementに対してつけたいため、NodesだけあとでParseする
let pComplexTyped = 
  (complexType <!> pstring "::" *> pSpaces *> pOrder <*> pOccurrence) |> attempt
  <|> (preturn <| complexType Sequence Required)

let pComplexElement =
  (fun nm occurs fType comm nodes -> element nm occurs (Complex <| fType nodes) comm)
  <!> pIndent *> pName <*> pOccurrence <* pSpaces <*> pComplexTyped <*> pSpaces *> pComment <*> ((newline *> indent *> pNodes) <|> (preturn []))

do pNodesImpl := (many pNode <* unindent) |> attempt

do pNodeImpl :=
    (Attribute <!> pXdefAttribute) |> attempt
    <|> (Element <!> pSimpleElement) |> attempt
    <|> (Element <!> pComplexElement)
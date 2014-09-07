module Xdef

open FParsec
open FParsec.Applicative

type SimpleType = 
  | FixedBool of bool | FixedByte of sbyte | FixedString of string | FixedInt of int | FixedFloat of float
  | Bool | Byte | String | Int  | Float | Decimal
  | DateTime of format : string option | TimeSpan of format : string option
  | EnumeratedString of string list
  | FixedLengthString of int
  | VariableLengthString of min : int * max : int option
  | IntRange of int * int
  | Pattern of string

// [b, e)
let intRange b e = IntRange(b, e - 1)

// [b, e]
let intRange2 b e = IntRange(b, e)

let variableLengthString min max = VariableLengthString(min, max)

/// 属性の出現回数を表す型です。
/// 明示的に指定されなかった場合、Requiredと推論されます。
type AttributeOccurrence =
  | Required
  | Optional

/// 要素と順序インジケータの出現回数を表す型です。
/// 明示的に指定されなかった場合、Requiredと推論されます。
type Occurrence =
  | Required
  | Many
  | RequiredMany
  | Optional
  | Specified of min : int * max : int option

let specific min max = Specified (min, max)

/// 属性を表す型です。
/// OccurrenceはRequiredもしくはOptionalを指定することができます。
type Attribute = {
  Name : string
  Occurrence : AttributeOccurrence
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
  | Simple of SimpleType * Attribute list
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
let simple sType attrs = Simple(sType, attrs)

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
let pFixedBool : Parser<_> = FixedBool <!> ((true <! pstring "True") <|> (false <! pstring "False"))
let pFixedByte : Parser<_> = FixedByte <!> (pint8 <* pchar 'y')
let pFixedString : Parser<_> = FixedString <!> pStringLiteral '"' '"'
let pFixedInt : Parser<_> = FixedInt <!> pint32
let pFixedFloat : Parser<_> = FixedFloat <!> pfloat
let pFormat = pStringLiteral '<' '>'
let pPrimitiveTypeWithFormat f typeName = f <!> pstring typeName *> (opt pFormat)
let pEnumeratedString = 
  pBracket "(" ")" <|
  ((List.map (function FixedString v -> v | _ -> failwith "internal error") >> EnumeratedString) <!> (sepBy1 (pSpaces *> pFixedString <* pSpaces) (pchar '|')))

let pIntRange : Parser<_> = 
  pBracket "[" ")" <|
  (intRange <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32)
  
let pIntRange2 : Parser<_> = 
  pBracket "[" "]" <|
  (intRange2 <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> pint32)

let pPattern : Parser<_> = Pattern <!> pStringLiteral '/' '/'

let pFixedLengthString : Parser<_> = FixedLengthString <!> pstring "String" *> (pBracket "[" "]" pint32)
let pVariableLengthString : Parser<_> = 
  pstring "String" *> 
  pBracket "[" "]" (variableLengthString <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> (opt pint32))

let pPrimitiveType : Parser<_> = parse {
  let! nm = pName
  match nm with
  | "Bool" -> return! preturn Bool
  | "String" -> return! preturn String
  | "Byte" -> return! preturn Byte
  | "Int" -> return! preturn Int
  | "Float" -> return! preturn Float
  | "Decimal" -> return! preturn Decimal
  | _ -> return! fail ("is not primitive type") 
}

let pSimpleType =
  pEnumeratedString
  <|> pIntRange |> attempt
  <|> pIntRange2 |> attempt
  <|> pPattern |> attempt
  <|> pFixedBool |> attempt
  <|> pFixedByte |> attempt
  <|> pFixedString |> attempt
  <|> pFixedInt |> attempt
  <|> pFixedFloat |> attempt
  <|> pVariableLengthString |> attempt
  <|> pFixedLengthString |> attempt
  <|> pPrimitiveTypeWithFormat DateTime "DateTime" |> attempt
  <|> pPrimitiveTypeWithFormat TimeSpan "TimeSpan" |> attempt
  <|> pPrimitiveType |> attempt
  <?> "指定された型が未定義です。"

let pSimpleTyped = pchar ':' *> pSpaces *> pSimpleType

let pAttributeOccurrence : Parser<_> =
  (AttributeOccurrence.Optional <! pstring "?")
  <|> (preturn AttributeOccurrence.Required)

let pOccurrence : Parser<_> =
  (pBracket "{" "}" (specific <!> pint32 <* pSpaces <* pstring ".." <* pSpaces <*> (pint32 |> opt))) |> attempt
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

let pAttribute = 
  attribute 
  <!> pIndent *> pchar '@' *> pName 
  <*> pAttributeOccurrence 
  <*> pSpaces *> (pSimpleTyped <?> "属性には型指定が必要です。")
  <*> pSpaces *> pComment <* (newline |> opt)

let (pAttrs, pAttrsImpl) = createParserForwardedToRef ()
let (pNodes, pNodesImpl) = createParserForwardedToRef ()
let (pNode, pNodeImpl) = createParserForwardedToRef ()

// CommentはElementに対してつけたいため、AttributesだけあとでParseする
let pSimple = 
  (simple <!>  pchar ':' *> pSpaces *> pSimpleType) |> attempt

let pSimpleElement =
  (fun nm occurs fType comm attrs -> element nm occurs (fType attrs) comm)
  <!> pIndent *> pName <*> pOccurrence <* pSpaces <*> pSimple <*> pSpaces *> pComment <*> ((newline *> indent *> pAttrs) <|> (preturn []))

let pOrder : Parser<_> = parse {
  let! nm = pName
  match nm with
  | "Sequence" -> return! preturn Sequence
  | "Choice" -> return! preturn Choice
  | "All" -> return! preturn All
  | _ -> return! fail ("指定された順序インジケータが未定義です。") 
}

let pOrdered =
  (pstring "::" *> pSpaces *> pOrder)
  <|> (preturn Sequence)

// CommentはElementに対してつけたいため、NodesだけあとでParseする
let pComplexTyped = 
  complexType <!> pOrdered <*> pOccurrence

let pComplexElement =
  (fun nm occurs fType comm nodes -> element nm occurs (Complex <| fType nodes) comm)
  <!> pIndent *> pName <*> pOccurrence <* pSpaces <*> pComplexTyped <*> pSpaces *> pComment <*> ((newline *> indent *> pNodes) <|> (preturn []))

do pAttrsImpl := (many pAttribute <* unindent) |> attempt

do pNodesImpl := (many pNode <* unindent) |> attempt

do pNodeImpl :=
  (Attribute <!> pAttribute) |> attempt
  <|> (Element <!> pSimpleElement) |> attempt
  <|> (Element <!> pComplexElement)

let pRoot =
  (pSimpleElement) |> attempt
  <|> (pComplexElement)

let parse input = runParserOnString pRoot 0 "" input
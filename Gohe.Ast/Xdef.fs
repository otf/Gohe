module Xdef

open FParsec
open FParsec.Applicative

type SimpleType = 
  | FixedBoolean of bool | FixedByte of sbyte | FixedString of string | FixedInt of int | FixedFloat of float
  | Boolean | Byte | String | Int  | Float | Decimal | Date | Time | DateTime | Duration
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

/// 要素または、パーティクルの出現回数を表す型です。
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

/// パーティクルを表す型です。
/// 明示的に指定されなかった場合、Sequenceと推論されます。
type Particle =
  | Sequence
  | Choice
  | All

type ComplexType = {
  Particle : Particle
  Occurrence : Occurrence
  Nodes : Node list
}

/// 要素型を表す型です。
/// 明示的に指定されなかった場合、Complex(パーティクルはSequence)と推論されます。
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
  | NodeGeneratorInvoke of NodeGeneratorInvoke
// TODO:  | Module of string

and NodeGeneratorInvoke = {
  Name : string
  Occurrence : Occurrence
  Parameters : SimpleType list
  Nodes : Node list
  Comment : string option
}

let (<||>) p1 p2 = attempt p1 <|> p2

let complexType particle occurs nodes = { Particle = particle; Occurrence = occurs; Nodes = nodes }
let element nm occurs typ comm = { Name = nm; Occurrence = occurs; Type = typ; Comment = comm } : Element
let simple sType attrs = Simple(sType, attrs)
let nodeGeneratorInvoke nm occurs comm parameters nodes = { Name = nm; Occurrence = occurs; Parameters = parameters; Nodes = nodes; Comment = comm } : NodeGeneratorInvoke

type Schema = {
  Nodes : Node list
}

let schema nodes = { Nodes = nodes }

type IndentLevel = int
type UserState = IndentLevel
type Parser<'t> = Parser<'t, UserState>
let indent = updateUserState ((+) 1)
let unindent = updateUserState (fun x -> x - 1)

let pSpaces : Parser<_> = many (pchar ' ')
let pBracket openString closeString p = between (pstring openString) (pstring closeString) (pSpaces *> p <* pSpaces)
let symbol : Parser<_> = anyOf "_"
let pToken : Parser<_> = many1Chars (letter <|> digit <|> symbol)
let pStringLiteral openChar closeChar : Parser<_> = 
  let pEscapedStringChar : Parser<_> =
    (closeChar <! pstring ("\\" + (closeChar.ToString()))) |> attempt
    <|> noneOf [closeChar]
  pchar openChar *> manyChars pEscapedStringChar <* pchar closeChar 
let pFixedBoolean : Parser<_> = FixedBoolean <!> ((true <! pstring "true") <|> (false <! pstring "false"))
let pFixedByte : Parser<_> = FixedByte <!> (pint8 <* pchar 'y')
let pFixedString : Parser<_> = FixedString <!> pStringLiteral '"' '"'
let pFixedInt : Parser<_> = FixedInt <!> pint32
let pFixedFloat : Parser<_> = FixedFloat <!> pfloat
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

let pFixedLengthString : Parser<_> = FixedLengthString <!> pstring "string" *> (pBracket "[" "]" pint32)
let pVariableLengthString : Parser<_> = 
  pstring "string" *> 
  pBracket "[" "]" (variableLengthString <!> pint32 <* pSpaces <* pchar ',' <* pSpaces <*> (opt pint32))

let pPrimitiveType : Parser<_> = parse {
  let! nm = pToken
  match nm with
  | "boolean" -> return Boolean
  | "string" -> return String
  | "byte" -> return Byte
  | "int" -> return Int
  | "float" -> return Float
  | "decimal" -> return Decimal
  | "date" -> return Date
  | "time" -> return Time
  | "dateTime" -> return DateTime
  | "duration" -> return Duration
  | _ -> return! fail ("is not primitive type") 
}

let pSimpleType =
  pEnumeratedString
  <||> pIntRange
  <||> pIntRange2
  <||> pPattern 
  <||> pFixedBoolean
  <||> pFixedByte 
  <||> pFixedString
  <||> pFixedInt 
  <||> pFixedFloat 
  <||> pVariableLengthString 
  <||> pFixedLengthString
  <||> pPrimitiveType
  <??> "指定された型が未定義です。"

let pResolveSimpleType = pchar ':' *> pSpaces *> pSimpleType

let pAttributeOccurrence : Parser<_> =
  (AttributeOccurrence.Optional <! pstring "?")
  <|> (preturn AttributeOccurrence.Required)

let pOccurrence : Parser<_> =
  (Required <! ((skipNoneOf "[*+?" <|> eof) |> lookAhead)) // 指定がなかった場合はRequired 
  <||> (pBracket "[" "]" (specific <!> pint32 <* pSpaces <* pstring ".." <* pSpaces <*> ((Some <!> pint32) <|> (None <! pchar '*'))))
  <||> (Many <! pstring "*")
  <||> (RequiredMany <! pstring "+")
  <||> (Optional <! pstring "?")

let pIndentCheck = (pSpaces *> fail "インデントが不正です。") <|> (preturn ()) 

let pIndent = parse { 
  let! indentLevel = getUserState
  let indentLevel = (indentLevel) * 2
  do! skipManyMinMaxSatisfy indentLevel indentLevel ((=) ' ')
  do! pIndentCheck
}

let pComment : Parser<_> = 
  Some 
  <!> pstring "#" *> pSpaces *> manyChars (noneOf ['\n'; '\r'])
  <|> (preturn None)

let pAttribute = 
  attribute 
  <!> pIndent *> pchar '@' *> pToken 
  <*> pAttributeOccurrence 
  <*> pSpaces *> (pResolveSimpleType <??> "属性には型指定が必要です。")
  <*> pSpaces *> pComment <* (newline |> optional)

let (pAttrs, pAttrsImpl) = createParserForwardedToRef ()
let (pNodes, pNodesImpl) = createParserForwardedToRef ()
let (pNode, pNodeImpl) = createParserForwardedToRef ()

// CommentはElementに対してつけたいため、AttributesだけあとでParseする
let pSimple = 
  simple <!>  pResolveSimpleType

let pSimpleElement =
  (fun nm occurs fType comm attrs -> element nm occurs (fType attrs) comm)
  <!> pIndent *> pToken 
  <*> pOccurrence <* pSpaces 
  <*> pSimple 
  <*> pSpaces *> pComment 
  <*> ((eof *> (preturn [])) <|> (newline *> indent *> pAttrs))

let pParticle : Parser<_> = parse {
  let! nm = pToken
  match nm with
  | "Sequence" -> return Sequence
  | "Choice" -> return Choice
  | "All" -> return All
  | _ -> return! failFatally ("指定されたパーティクルが未定義です。") 
}

let pResolveParticle =
  (Sequence <! notFollowedBy (pstring "::"))
  <|> (pstring "::" *> pSpaces *> pParticle)

// CommentはElementに対してつけたいため、NodesだけあとでParseする
let pResolveComplexType = 
  complexType 
  <!> pResolveParticle 
  <*> pOccurrence

let pComplexElement =
  (fun nm occurs fType comm nodes -> element nm occurs (Complex <| fType nodes) comm)
  <!> pIndent *> pToken
  <*> pOccurrence <* pSpaces 
  <*> pResolveComplexType 
  <*> pSpaces *> pComment 
  <*> ((eof *> (preturn [])) <|> (newline *> indent *> pNodes))

let pNodeGeneratorInvoke = 
  nodeGeneratorInvoke
  <!> pIndent *> pchar '!' *> pToken
  <*> pOccurrence
  <*> pSpaces *> pComment 
  <*> many (pSpaces *> pSimpleType)
  <*> ((eof *> (preturn [])) <|> (newline *> indent *> pNodes))

do pAttrsImpl := (List.choose id) <!> (many ((None <! pSpaces *> newline) <||> (Some <!> pAttribute)) <* unindent)

do pNodesImpl := (List.choose id) <!> (many ((None <! pSpaces *> newline) <||> (Some <!> pNode)) <* unindent)

do pNodeImpl :=
  (Attribute <!> pAttribute)
  <||> (NodeGeneratorInvoke <!> pNodeGeneratorInvoke)
  <||> (Element <!> pSimpleElement)
  <||> (Element <!> pComplexElement)

let pRoot = pSimpleElement <||> pComplexElement

let pSchema = 
  schema
  <!> pNodes

let parse input = runParserOnString pSchema 0 "" input
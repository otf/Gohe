module Test.``Xsd Attribute Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

let schemaQName nm =  XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
let primitiveTypeTestCases : obj [][] = [|
  [|Xdef.TypeRef("boolean"); schemaQName "boolean"|]
  [|Xdef.TypeRef("byte"); schemaQName "byte"|]
  [|Xdef.TypeRef("string"); schemaQName "string"|]
  [|Xdef.TypeRef("int"); schemaQName "int"|]
  [|Xdef.TypeRef("float"); schemaQName "float"|]
  [|Xdef.TypeRef("decimal"); schemaQName "decimal"|]
  [|Xdef.TypeRef("date"); schemaQName "date"|]
  [|Xdef.TypeRef("time"); schemaQName "time"|]
  [|Xdef.TypeRef("dateTime"); schemaQName "dateTime"|]
  [|Xdef.TypeRef("duration"); schemaQName "duration"|]
  [|Xdef.TypeRef("base64Binary"); schemaQName "base64Binary"|]
  [|Xdef.TypeRef("hexBinary"); schemaQName "hexBinary"|]
  [|Xdef.TypeRef("anyURI"); schemaQName "anyURI"|]
  [|Xdef.TypeRef("language"); schemaQName "language"|]
  [|Xdef.TypeRef("normalizedStrign"); schemaQName "normalizedStrign"|]
  [|Xdef.TypeRef("token"); schemaQName "token"|]
  [|Xdef.TypeRef("double"); schemaQName "double"|]
  [|Xdef.TypeRef("integer"); schemaQName "integer"|]
  [|Xdef.TypeRef("long"); schemaQName "long"|]
  [|Xdef.TypeRef("negativeInteger"); schemaQName "negativeInteger"|]
  [|Xdef.TypeRef("nonNegativeInteger"); schemaQName "nonNegativeInteger"|]
  [|Xdef.TypeRef("nonPositiveInteger"); schemaQName "nonPositiveInteger"|]
  [|Xdef.TypeRef("positiveInteger"); schemaQName "positiveInteger"|]
  [|Xdef.TypeRef("short"); schemaQName "short"|]
  [|Xdef.TypeRef("unsignedByte"); schemaQName "unsignedByte"|]
  [|Xdef.TypeRef("unsignedInt"); schemaQName "unsignedInt"|]
  [|Xdef.TypeRef("unsignedLong"); schemaQName "unsignedLong"|]
  [|Xdef.TypeRef("unsignedShort"); schemaQName "unsignedShort"|]
  [|Xdef.TypeRef("gDay"); schemaQName "gDay"|]
  [|Xdef.TypeRef("gMonth"); schemaQName "gMonth"|]
  [|Xdef.TypeRef("gMonthDay"); schemaQName "gMonthDay"|]
  [|Xdef.TypeRef("gYear"); schemaQName "gYear"|]
  [|Xdef.TypeRef("gYearMonth"); schemaQName "gYearMonth"|]
  [|Xdef.TypeRef("Name"); schemaQName "Name"|]
  [|Xdef.TypeRef("NOTATION"); schemaQName "NOTATION"|]
  [|Xdef.TypeRef("QName"); schemaQName "QName"|]
  [|Xdef.TypeRef("ENTITY"); schemaQName "ENTITY"|]
  [|Xdef.TypeRef("ENTITIES"); schemaQName "ENTITIES"|]
  [|Xdef.TypeRef("ID"); schemaQName "ID"|]
  [|Xdef.TypeRef("IDREF"); schemaQName "IDREF"|]
  [|Xdef.TypeRef("IDREFS"); schemaQName "IDREFS"|]
  [|Xdef.TypeRef("NMTOKEN"); schemaQName "NMTOKEN"|]
  [|Xdef.TypeRef("NMTOKENS"); schemaQName "NMTOKENS"|]
  [|Xdef.TypeRef("anyType"); schemaQName "anyType"|]
  [|Xdef.TypeRef("anySimpleType"); schemaQName "anySimpleType"|]
|]

[<TestCaseSource("primitiveTypeTestCases")>]
let ``PrimitiveTypeの属性をXsd化できる`` inputType expected = 
  let input = attr "Attr" useRequired None inputType
  
  Xsd.fromNode "" input |> asAttr  |> name |> should equal "Attr"
  Xsd.fromNode "" input |> asAttr  |> typeNameOf |> should equal expected

let fixedTypeTestCases : obj [][] = [|
  [|Xdef.FixedBoolean(true); "true"|]
  [|Xdef.FixedByte(100y); "100"|]
  [|Xdef.FixedString("hello"); "hello"|]
  [|Xdef.FixedInt(100); "100"|]
  [|Xdef.FixedFloat(100.001); "100.001"|]
|]

[<TestCaseSource("fixedTypeTestCases")>]
let ``PrimitiveType(Fixed)の属性をXsd化できる`` inputType expected = 
  let input = attr "Attr" useRequired None inputType
  
  Xsd.fromNode "" input |> asAttr  |> name |> should equal "Attr"
  Xsd.fromNode "" input |> asAttr  |> fixedValue |> should equal expected

let occursTestFactors = [
  useRequired, XmlSchemaUse.Required
  useOptional, XmlSchemaUse.Optional
]

let occursTestCases : obj [][] = [|
  for (occursInput, occursExpected) in occursTestFactors do
    yield [| occursInput; occursExpected |]
|]

[<TestCaseSource("occursTestCases")>]
let ``PrimitiveTypeの属性(出現回数指定)をXsd化できる`` occursInput occursExpected = 
  let input = attr "Attr" occursInput None (Xdef.TypeRef "string")
  
  Xsd.fromNode "" input |> asAttr |> useOfAttr |> should equal occursExpected
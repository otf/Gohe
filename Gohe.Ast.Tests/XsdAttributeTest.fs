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
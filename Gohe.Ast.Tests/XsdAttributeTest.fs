module Test.``Xsd Attribute Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

let primitiveTypeTestCases : obj [][] = [|
  [|Xdef.Boolean; "boolean"|]
  [|Xdef.Byte; "byte"|]
  [|Xdef.String; "string"|]
  [|Xdef.Int; "int"|]
  [|Xdef.Float; "float"|]
  [|Xdef.Decimal; "decimal"|]
  [|Xdef.Date; "date"|]
  [|Xdef.Time; "time"|]
  [|Xdef.DateTime; "dateTime"|]
  [|Xdef.Duration; "duration"|]
|]

let fixedTypeTestCases : obj [][] = [|
  [|Xdef.FixedBoolean(true); "true"|]
  [|Xdef.FixedByte(100y); "100"|]
  [|Xdef.FixedString("hello"); "hello"|]
  [|Xdef.FixedInt(100); "100"|]
  [|Xdef.FixedFloat(100.001); "100.001"|]
|]

[<TestCaseSource("primitiveTypeTestCases")>]
let ``PrimitiveTypeの属性をXsd化できる`` inputType expected = 
  let input = attr "Attr" useRequired None inputType
  
  Xsd.fromNode "" input |> asAttr |> name |> should equal "Attr"
  Xsd.fromNode "" input |> asAttr |> typeNameOf |> should equal (XmlQualifiedName(expected, "http://www.w3.org/2001/XMLSchema"))

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
  let input = attr "Attr" occursInput None Xdef.String
  
  Xsd.fromNode "" input |> asAttr |> useOfAttr |> should equal occursExpected
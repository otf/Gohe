module Test.``Xsd SimpleType Element Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd

let inline name (x:^a) = ((^a) : (member Name : string) x)
let inline typeNameOf (x:^a) = ((^a) : (member SchemaTypeName : XmlQualifiedName) x)

let primitiveTypeTestCases : obj [][] = [|
  [|Xdef.Bool; "boolean"|]
  [|Xdef.String; "string"|]
  [|Xdef.Int; "integer"|]
  [|Xdef.Float; "float"|]
  [|Xdef.Decimal; "decimal"|]
//  [|Xdef.Guid, ""|]
|]

[<TestCaseSource("primitiveTypeTestCases")>]
let ``PrimitiveTypeの要素をXsd化できる`` inputType expected = 
  let input = elm "Root" required None inputType
  
  Xsd.fromNode input |> name |> should equal "Root"
  Xsd.fromNode input |> typeNameOf |> should equal (XmlQualifiedName(expected, "http://www.w3.org/2001/XMLSchema"))
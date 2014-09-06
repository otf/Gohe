﻿module Test.``Xsd ComplexType Element Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``ComplexType(Sequence)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| seq required []
  
  Xsd.fromNode input |> name |> should equal "Root"

  Xsd.fromNode input |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaSequence>

[<Test>]
let ``ComplexType(Choice)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| choice required []
  
  Xsd.fromNode input |> name |> should equal "Root"

  Xsd.fromNode input |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaChoice>

[<Test>]
let ``ComplexType(All)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| all required []
  
  Xsd.fromNode input |> name |> should equal "Root"

  Xsd.fromNode input |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaAll>

let occursTestFactors = [
  required, 1, Some 1
  optional, 0, Some 1
  many , 0, None 
  requiredMany, 1, None
  specific 0 100, 0, Some 100
]

let occursTestCases : obj [][] = [|
  for (occursInput, minOccursExpected, maxOccursExpected) in occursTestFactors do
    yield [| occursInput; minOccursExpected; maxOccursExpected |]
|]

[<TestCaseSource("occursTestCases")>]
let ``ComplexTypeの要素(出現回数指定)をXsd化できる`` occursInput (minOccursExpected : int) (maxOccursExpected : int option) = 
  let input = celm "Root" required None <| all occursInput []
  
  Xsd.fromNode input |> typeOfAsComplex |> particle |> minOccurs |> should equal minOccursExpected
  Xsd.fromNode input |> typeOfAsComplex |> particle |> maxOccurs |> should equal maxOccursExpected
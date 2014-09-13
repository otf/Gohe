module Test.``Xsd ComplexType Element Test``

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
  
  Xsd.fromNode "" input |> asElm |> name |> should equal "Root"

  Xsd.fromNode "" input |> asElm |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaSequence>

[<Test>]
let ``ComplexType(Choice)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| choice required []
  
  Xsd.fromNode "" input |> asElm |> name |> should equal "Root"

  Xsd.fromNode "" input |> asElm |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaChoice>

[<Test>]
let ``ComplexType(All)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| all required []
  
  Xsd.fromNode "" input |> asElm |> name |> should equal "Root"

  Xsd.fromNode "" input |> asElm |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaAll>

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
  
  Xsd.fromNode "" input |> asElm |> typeOfAsComplex |> particle |> minOccurs |> should equal minOccursExpected
  Xsd.fromNode "" input |> asElm |> typeOfAsComplex |> particle |> maxOccurs |> should equal maxOccursExpected

let childrenTestFactors = [
  [], 0
  [elm "Child" required None (Xdef.TypeRef "string")], 1
  [attr "Child" useRequired None (Xdef.TypeRef "string")], 0
  [attr "Child1" useRequired None (Xdef.TypeRef "string"); elm "Child2" required None (Xdef.TypeRef "string")], 1
  [elm "Child1" required None (Xdef.TypeRef "string"); elm "Child2" required None (Xdef.TypeRef "string")], 2
]

let complexTypeTestCases : obj [][] = [|
  for (childrenInput, countOfChildrenExpected) in childrenTestFactors do
    yield [| childrenInput; countOfChildrenExpected |]
|]

[<TestCaseSource("complexTypeTestCases")>]
let ``ComplexTypeの要素(子要素あり)をXsd化できる`` childrenInput countOfChildrenExpected = 
  let (Xdef.Element input) =
    celm "Root" required None <| seq required childrenInput
  
  Xsd.fromRoot "" input |> getElm |> typeOfAsComplex |> particle |> items |> should haveCount countOfChildrenExpected

[<Test>]
let ``PrimitiveTypeの要素(固定値の属性あり)をXsd化できる`` () = 
  let (Xdef.Element input) =
    celm "Root" required None <| seq required [ 
      elmWithAttrs "Elm" required None (Xdef.FixedBoolean true) [
        attr "Attr" useRequired None (Xdef.TypeRef "string")
      ]
    ]

  Xsd.fromRoot "" input |> getElm |> typeOfAsComplex |> particle |> items |> should haveCount 1

module Test.``Xdef Complex Element Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefOrder(Sequence)の指定をパースできる`` () =  
  parse Xdef.pOrder "Sequence"
  |> should equal (Some Xdef.Order.Sequence)

[<Test>]
let ``XdefElement(暗黙のSequence)をパースできる`` () =  
  let expected = celm "Root" required None <| seq required []
  parse Xdef.pNode "Root"
  |> should equal (Some expected)

let orderTestFactors = [
  "Sequence", seq
  "Choice", choice
  "All", all
]

let occursTestFactors = [
  "", required
  "?", optional
  "*", many
  "+", requiredMany
  "{0,100}", specific 0 100
  "{100,}", min 100
]

let childrenTestFactors = [
  "", []
  "\n  Child : String", [elm "Child" required None Xdef.String]
  "\n  @Child : String", [attr "Child" useRequired None Xdef.String]
  "\n  @Child1 : String\n  Child2 : String", [attr "Child1" useRequired None Xdef.String; elm "Child2" required None Xdef.String]
  "\n  Child1 : String\n  Child2 : String", [elm "Child1" required None Xdef.String; elm "Child2" required None Xdef.String]
]

let complexTypeTestCases : obj [][] = [|
  for (orderInput, orderExpected) in orderTestFactors do
  for (occursInput, occursExpected) in occursTestFactors do
  for (childrenInput, childrenExpected) in childrenTestFactors do
    yield [| orderInput; orderExpected; occursInput; occursExpected; childrenInput; childrenExpected |]
|]

[<TestCaseSource("complexTypeTestCases")>]
let ``複雑型のXdefElementをパースできる`` complexTypeInput complexTypeExpected occursInput occursExpected childrenInput childrenExpected =
  let expected = celm "Root" required None <| complexTypeExpected (occursExpected: Xdef.Occurrence) childrenExpected
  parse Xdef.pNode (sprintf "Root :: %s%s%s" complexTypeInput occursInput childrenInput)
  |> should equal (Some expected)
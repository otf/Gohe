module Test.``Xdef Complex Element Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefOrder(Sequence)の指定をパースできる`` () =  
    parse Ast.pOrder "Sequence"
    |> should equal (Some Ast.XdefOrder.Sequence)

[<Test>]
let ``XdefElement(暗黙のSequence)をパースできる`` () =  
    let expected = celm "Root" required None <| seq required []
    parse Ast.pNode "Root"
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
  "{0..100}", specific 0 100
  "{..100}", max 100
  "{100..}", min 100
]

let childrenTestFactors = [
  "", []
  "\n  Child : String", [elm "Child" required None Ast.String]
  "\n  @Child : String", [attr "Child" required None Ast.String]
  "\n  @Child1 : String\n  Child2 : String", [attr "Child1" required None Ast.String; elm "Child2" required None Ast.String]
  "\n  Child1 : String\n  Child2 : String", [elm "Child1" required None Ast.String; elm "Child2" required None Ast.String]
]

let complexTypeTestCases : obj [][] = [|
  for (orderInput, orderExpected) in orderTestFactors do
  for (occursInput, occursExpected) in occursTestFactors do
  for (childrenInput, childrenExpected) in childrenTestFactors do
    yield [| orderInput; orderExpected; occursInput; occursExpected; childrenInput; childrenExpected |]
|]

[<TestCaseSource("complexTypeTestCases")>]
let ``複雑型のXdefElementをパースできる`` complexTypeInput complexTypeExpected occursInput occursExpected childrenInput childrenExpected =
    let expected = celm "Root" required None <| complexTypeExpected (occursExpected: Ast.XdefOccurrence) childrenExpected
    parse Ast.pNode (sprintf "Root :: %s%s%s" complexTypeInput occursInput childrenInput)
    |> should equal (Some expected)
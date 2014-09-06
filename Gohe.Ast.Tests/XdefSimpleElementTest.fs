module Test.``Xdef Simple Element Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name : String" 
    |> should equal (Some <| elm "Name" required None Ast.String)

let occursTestFactors = [
  "", required
  "?", optional
  "*", many
  "+", requiredMany
  "{0..100}", specific 0 100
  "{..100}", max 100
  "{100..}", min 100
]

let occursTestCases : obj [][] = [|
  for (occursInput, occursExpected) in occursTestFactors do
    yield [| occursInput; occursExpected |]
|]

[<TestCaseSource("occursTestCases")>]
let ``出現回数が指定されたXdefSimpleElementをパースできる`` occursInput occursExpected =
    let expected = elm "Root" occursExpected None Ast.String
    parse Ast.pNode (sprintf "Root%s : String" occursInput)
    |> should equal (Some expected)
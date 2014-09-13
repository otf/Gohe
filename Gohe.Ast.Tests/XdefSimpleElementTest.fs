module Test.``Xdef Simple Element Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefSimpleElementをパースできる`` () =  
  parse Xdef.pNode "Name : string" 
  |> should equal (Some <| elm "Name" required None Xdef.String)

let occursTestFactors = [
  "", required
  "?", optional
  "*", many
  "+", requiredMany
  "[0..100]", specific 0 100
  "[100..*]", min 100
]

let occursTestCases : obj [][] = [|
  for (occursInput, occursExpected) in occursTestFactors do
    yield [| occursInput; occursExpected |]
|]

[<TestCaseSource("occursTestCases")>]
let ``出現回数が指定されたXdefSimpleElementをパースできる`` occursInput occursExpected =
  let expected = elm "Root" occursExpected None Xdef.String
  parse Xdef.pNode (sprintf "Root%s : string" occursInput)
  |> should equal (Some expected)
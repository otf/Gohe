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

let orderAndOccursTestCases : obj [][] = [|
  for (orderInput, orderExpected) in orderTestFactors do
  for (occursInput, occursExpected) in occursTestFactors do
    yield [| orderInput; orderExpected; occursInput; occursExpected |]
|]

[<TestCaseSource("orderAndOccursTestCases")>]
let ``順序インジケータと出現回数指定された複雑型XdefElementをパースできる`` complexTypeInput complexTypeExpected occursInput occursExpected =
    let expected = celm "Root" required None <| complexTypeExpected (occursExpected: Ast.XdefOccurrence) ([] : Ast.XdefNode list)
    parse Ast.pNode (sprintf "Root :: %s%s" complexTypeInput occursInput)
    |> should equal (Some expected)

[<Test>]
let ``子要素持ちのXdefElement(Sequence)をパースできる`` () =  
    let xdef = "Root\n  @Name : String\n  Description : String"

    let expected = 
      celm "Root" required None <| seq required [
          attr "Name" required  None Ast.String
          elm "Description" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``子要素持ちのXdefElement(Choice)をパースできる`` () =  
    let xdef = "AxorB :: Choice\n  A : String\n  B : String"

    let expected = 
      celm "AxorB" required None <| choice required [
          elm "A" required None Ast.String
          elm "B" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``子要素持ちのXdefElement(All)をパースできる`` () =  
    let xdef = "AorB :: All\n  A : String\n  B : String"

    let expected = 
      celm "AorB" required None <| all required [
          elm "A" required None Ast.String
          elm "B" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
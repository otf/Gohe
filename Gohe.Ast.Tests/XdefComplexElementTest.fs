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

[<Test>]
let ``XdefElement(明示的なSequence)をパースできる`` () =  
    let expected = celm "Root" required None <| seq required []
    parse Ast.pNode "Root :: Sequence"
    |> should equal (Some expected)

[<Test>]
let ``XdefElement(Sequence?)をパースできる`` () =  
    let expected = celm "Root" required None <| seq optional []
    parse Ast.pNode "Root :: Sequence?"
    |> should equal (Some expected)

[<Test>]
let ``XdefElement(Sequence*)をパースできる`` () =  
    let expected = celm "Root" required None <| seq many []
    parse Ast.pNode "Root :: Sequence*"
    |> should equal (Some expected)

[<Test>]
let ``XdefElement(Sequence+)をパースできる`` () =  
    let expected = celm "Root" required None <| seq requiredMany []
    parse Ast.pNode "Root :: Sequence+"
    |> should equal (Some expected)

[<Test>]
let ``XdefElement(Sequence{0..1})をパースできる`` () =  
    let expected = celm "Root" required None <| seq (specific 0 1) []
    parse Ast.pNode "Root :: Sequence{0..1}"
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
let ``XdefElement(Choice)をパースできる`` () =  
    let expected = celm "Root" required None <| choice required []
    parse Ast.pNode "Root :: Choice"
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
let ``XdefElement(All)をパースできる`` () =  
    let expected = 
      celm "Root" required None <| all required []

    parse Ast.pNode "Root :: All"
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
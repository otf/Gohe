module Test.``Xdef Complex Element Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefOrder(Sequence)の指定をパースできる`` () =  
    parse Ast.pOrdered "::Sequence"
    |> should equal (Some Ast.XdefOrder.Sequence)

[<Test>]
let ``XdefOrder(Sequence)の指定(空文字あり)をパースできる`` () =  
    parse Ast.pOrdered "  :: Sequence  "
    |> should equal (Some Ast.XdefOrder.Sequence)

[<Test>]
let ``XdefComplexElement(Sequence)をパースできる`` () =  
    parse Ast.pXdefComplexElement "Root"
    |> should equal (Some <| Ast.xdefComplexElement "Root" Ast.XdefOccurrence.Required Ast.XdefOrder.Sequence None [])

[<Test>]
let ``子要素持ちのXdefComplexElement(Sequence)をパースできる`` () =  
    let xdef = "Root\n  @Name : String\n  Description : String"

    let expected = 
      celm "Root" required None seq [
          attr "Name" required  None Ast.String
          elm "Description" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``XdefComplexElement(Choice)をパースできる`` () =  
    parse Ast.pXdefComplexElement"Root :: Choice"
    |> should equal (Some <| Ast.xdefComplexElement "Root" Ast.XdefOccurrence.Required Ast.XdefOrder.Choice None [])

[<Test>]
let ``子要素持ちのXdefComplexElement(Choice)をパースできる`` () =  
    let xdef = "AxorB :: Choice\n  A : String\n  B : String"

    let expected = 
      celm "AxorB" required None choice [
          elm "A" required None Ast.String
          elm "B" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``XdefComplexElement(All)をパースできる`` () =  
    parse Ast.pXdefComplexElement"Root :: All"
    |> should equal (Some <| Ast.xdefComplexElement "Root" Ast.XdefOccurrence.Required Ast.XdefOrder.All None [])

[<Test>]
let ``子要素持ちのXdefComplexElement(All)をパースできる`` () =  
    let xdef = "AorB :: All\n  A : String\n  B : String"

    let expected = 
      celm "AorB" required None all [
          elm "A" required None Ast.String
          elm "B" required None Ast.String
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``複雑なXdefNodeをパースできる`` () =  
    let xdef = """
Root
  @Id : Guid -- ID属性
  Description : String -- 詳細
  Children
    Child* : [0,10)
  Behavior
    OptionA? : "Enabled" """.Trim()

    let expected = 
      celm "Root" required None seq [
          attr "Id" required (Some "ID属性") Ast.Guid
          elm "Description" required (Some "詳細") Ast.String
          celm "Children" required None seq [
              elm "Child" Ast.Many None (Ast.intRange 0 10) 
            ] 
          celm "Behavior" required None seq [
              elm "OptionA" Ast.Optional None (Ast.FixedString "Enabled") 
            ] 
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
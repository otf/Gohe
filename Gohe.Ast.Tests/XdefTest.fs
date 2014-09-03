module Test.``Xdef Test``

open NUnit.Framework
open FsUnit
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

[<Test>]
let ``XdefAttributeをパースできる`` () =  
    parse Ast.pXdefAttribute "@Name : String"
    |> should equal (Some <| Ast.xdefAttribute "Name" Ast.Type.String None)

[<Test>]
let ``XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" None Ast.Type.String None)

[<Test>]
let ``制約付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name? : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Some Ast.XdefRestriction.Option) Ast.Type.String None)

    parse Ast.pXdefSimpleElement "Name* : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Some Ast.XdefRestriction.Many) Ast.Type.String None)

    parse Ast.pXdefSimpleElement "Name| : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Some Ast.XdefRestriction.Choice) Ast.Type.String None)

[<Test>]
let ``XdefElementをパースできる`` () =  
    parse Ast.pXdefElement "Root"
    |> should equal (Some <| Ast.xdefElement "Root" None None [])

[<Test>]
let ``子要素持ちのXdefElementをパースできる`` () =  
    let xdef = "Root\n  @Name : String\n  Description : String"

    let expected = 
      Ast.xdefElement "Root" None None [
        Ast.Attribute <| Ast.xdefAttribute "Name" Ast.String None 
        Ast.SimpleElement <| Ast.xdefSimpleElement "Description" None Ast.String None
        ]

    parse Ast.pXdefElement xdef
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
      Ast.Element <| Ast.xdefElement "Root" None None [
        Ast.Attribute <| Ast.xdefAttribute "Id" Ast.Guid (Some "ID属性") 
        Ast.SimpleElement <| Ast.xdefSimpleElement "Description" None Ast.String (Some "詳細")
        Ast.Element <| Ast.xdefElement "Children" None None [
            Ast.SimpleElement <| Ast.xdefSimpleElement "Child" (Some Ast.XdefRestriction.Many) (Ast.intRange 0 10) None
          ]
        Ast.Element <| Ast.xdefElement "Behavior" None None [
            Ast.SimpleElement <| Ast.xdefSimpleElement "OptionA" (Some Ast.XdefRestriction.Option) (Ast.StringValue "Enabled") None
          ]
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
﻿module Test.``Xdef Test``

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
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Required Ast.Type.String None)

[<Test>]
let ``出現回数(Optional)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name? : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Optional Ast.Type.String None)

[<Test>]
let ``出現回数(Many)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name* : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Many Ast.Type.String None)

[<Test>]
let ``出現回数(RequiredMany)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name+ : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.RequiredMany Ast.Type.String None)

[<Test>]
let ``出現回数(Specified)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name{1..10} : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Ast.XdefOccurrence.Specified (1, 10)) Ast.Type.String None)

[<Test>]
let ``XdefComplexElementをパースできる`` () =  
    parse Ast.pXdefComplexElement "Root"
    |> should equal (Some <| Ast.xdefComplexElement "Root" Ast.XdefOccurrence.Required None [])

[<Test>]
let ``子要素持ちのXdefComplexElementをパースできる`` () =  
    let xdef = "Root\n  @Name : String\n  Description : String"

    let expected = 
      Ast.xdefComplexElement "Root" Ast.Required None [
        Ast.Attribute <| Ast.xdefAttribute "Name" Ast.String None 
        Ast.SimpleElement <| Ast.xdefSimpleElement "Description" Ast.XdefOccurrence.Required Ast.String None
        ]

    parse Ast.pXdefComplexElement xdef
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
      Ast.ComplexElement <| Ast.xdefComplexElement "Root" Ast.XdefOccurrence.Required None [
        Ast.Attribute <| Ast.xdefAttribute "Id" Ast.Guid (Some "ID属性") 
        Ast.SimpleElement <| Ast.xdefSimpleElement "Description" Ast.XdefOccurrence.Required Ast.String (Some "詳細")
        Ast.ComplexElement <| Ast.xdefComplexElement "Children" Ast.XdefOccurrence.Required None [
            Ast.SimpleElement <| Ast.xdefSimpleElement "Child" Ast.XdefOccurrence.Many (Ast.intRange 0 10) None
          ]
        Ast.ComplexElement <| Ast.xdefComplexElement "Behavior" Ast.XdefOccurrence.Required None [
            Ast.SimpleElement <| Ast.xdefSimpleElement "OptionA" Ast.XdefOccurrence.Optional (Ast.FixedString "Enabled") None
          ]
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
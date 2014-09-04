module Test.``Xdef Simple Element Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Required Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Optional)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name? : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Optional Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Many)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name* : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.Many Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(RequiredMany)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name+ : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" Ast.XdefOccurrence.RequiredMany Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Specified)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name{1..10} : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Ast.XdefOccurrence.Specified (Some 1, Some 10)) Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Specified_開始がunbound)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name{..10} : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Ast.XdefOccurrence.Specified (None, Some 10)) Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Specified_終了がunbound)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pXdefSimpleElement "Name{1..} : String" 
    |> should equal (Some <| Ast.xdefSimpleElement "Name" (Ast.XdefOccurrence.Specified (Some 1, None)) Ast.XdefSimpleType.String None)
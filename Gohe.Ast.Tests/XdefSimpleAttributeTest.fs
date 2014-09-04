module Test.``Xdef Simple Attribute Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefAttributeをパースできる`` () =  
    parse Ast.pXdefAttribute "@Name : String"
    |> should equal (Some <| Ast.xdefAttribute "Name" Ast.XdefOccurrence.Required  Ast.XdefSimpleType.String None)

[<Test>]
let ``出現回数(Optional)付XdefAttributeをパースできる`` () =  
    parse Ast.pXdefAttribute "@Name? : String" 
    |> should equal (Some <| Ast.xdefAttribute "Name" Ast.XdefOccurrence.Optional Ast.XdefSimpleType.String None)
module Test.``Xdef Simple Attribute Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefAttributeをパースできる`` () =  
    parse Ast.pNode "@Name : String"
    |> should equal (Some <| attr "Name" required None Ast.String)

[<Test>]
let ``出現回数(Optional)付XdefAttributeをパースできる`` () =  
    parse Ast.pNode "@Name? : String" 
    |> should equal (Some <| attr "Name" optional None Ast.String)
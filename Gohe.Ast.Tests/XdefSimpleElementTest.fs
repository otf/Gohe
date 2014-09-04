module Test.``Xdef Simple Element Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name : String" 
    |> should equal (Some <| elm "Name" required None Ast.String)

[<Test>]
let ``出現回数(Optional)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name? : String" 
    |> should equal (Some <| elm "Name" optional None Ast.String)

[<Test>]
let ``出現回数(Many)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name* : String" 
    |> should equal (Some <| elm "Name" many None Ast.String)

[<Test>]
let ``出現回数(RequiredMany)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name+ : String" 
    |> should equal (Some <| elm "Name" requiredMany None Ast.String)

[<Test>]
let ``出現回数(Specified)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name{1..10} : String" 
    |> should equal (Some <| elm "Name" (specific 1 10) None Ast.String)

[<Test>]
let ``出現回数(Specified_開始がunbound)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name{..10} : String" 
    |> should equal (Some <| elm "Name" (max 10) None Ast.String)

[<Test>]
let ``出現回数(Specified_終了がunbound)付XdefSimpleElementをパースできる`` () =  
    parse Ast.pNode "Name{1..} : String" 
    |> should equal (Some <| elm "Name" (min 1) None Ast.String)
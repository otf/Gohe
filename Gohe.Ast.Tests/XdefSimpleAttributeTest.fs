module Test.``Xdef Simple Attribute Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefAttributeをパースできる`` () =  
  parse Xdef.pNode "@Name : String"
  |> should equal (Some <| attr "Name" useRequired None Xdef.String)

[<Test>]
let ``出現回数(Optional)付XdefAttributeをパースできる`` () =  
  parse Xdef.pNode "@Name? : String" 
  |> should equal (Some <| attr "Name" useOptional None Xdef.String)
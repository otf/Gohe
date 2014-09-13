module Test.``Xdef Simple Attribute Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefAttributeをパースできる`` () =  
  parse Xdef.pNode "@Name : string"
  |> should equal (Some <| attr "Name" useRequired None (Xdef.TypeRef "string"))

[<Test>]
let ``出現回数(Optional)付XdefAttributeをパースできる`` () =  
  parse Xdef.pNode "@Name? : string" 
  |> should equal (Some <| attr "Name" useOptional None (Xdef.TypeRef "string"))

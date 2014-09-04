module Test.``Xdef Acceptance Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefNodeをパースできる`` () =  
    let xdef = """
Root
  @Id : Guid -- ID属性
  Description : String -- 詳細
  Children
    Child* : [0,10)
  Behavior
    OptionA? : "Enabled" """.Trim()

    let expected = 
      celm "Root" required None <| seq required [
          attr "Id" required (Some "ID属性") Ast.Guid
          elm "Description" required (Some "詳細") Ast.String
          celm "Children" required None <| seq required [
              elm "Child" many None (Ast.intRange 0 10) 
            ] 
          celm "Behavior" required None <| seq required [
              elm "OptionA" optional None (Ast.FixedString "Enabled") 
            ] 
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
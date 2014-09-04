module Test.``Xdef Acceptance Test``

open NUnit.Framework
open FsUnit

open AstUtility

[<Test>]
let ``XdefNodeをパースできる`` () =  
    let xdef = """
Root
  @Id : Guid
  Children
    Child* : [0,10)""".Trim()

    let expected = 
      celm "Root" required None <| seq required [
          attr "Id" required None Ast.Guid
          celm "Children" required None <| seq required [
              elm "Child" many None (Ast.intRange 0 10) 
            ] 
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``コメントが指定されたXdefNodeをパースできる`` () =  
    let xdef = """
Root -- Root Element Comment
  @Id : Guid -- Attribute Comment
  Children -- Complex Element Comment
    Child* : [0,10) -- Simple Element Comment""".Trim()

    let expected = 
      celm "Root" required (Some "Root Element Comment") <| seq required [
          attr "Id" required (Some "Attribute Comment") Ast.Guid
          celm "Children" required (Some "Complex Element Comment") <| seq required [
              elm "Child" many (Some "Simple Element Comment") (Ast.intRange 0 10) 
            ] 
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)

[<Test>]
let ``順序インジケータが指定されたXdefNodeをパースできる`` () =  
    let xdef = """
Root
  MustSeqImplicitly
  MustSeq :: Sequence{0..10}
  MustChoice :: Choice{0..10}
  MustAll :: All{0..10}""".Trim()

    let expected = 
      celm "Root" required None <| seq required [
          // 順序インジケータが明示的に指定されなかった場合、Sequenceと推論される。またそのときの出現回数はrequiredになる
          celm "MustSeqImplicitly" required None <| seq required [ ]
          celm "MustSeq" required None <| seq (specific 0 10) [ ] 
          celm "MustChoice" required None <| choice (specific 0 10) [ ] 
          celm "MustAll" required None <| all (specific 0 10) [ ] 
        ]

    parse Ast.pNode xdef
    |> should equal (Some <| expected)
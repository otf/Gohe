module Test.``Xdef Acceptance Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefNodeをパースできる`` () =  
  let xdef = """
Root
  @Id : int
  @IsEnabled : true
  ElmWithAttrs : string
    @Attr : string
  Children
    Child* : [0,10)""".Trim()

  let expected = 
    celm "Root" required None <| seq required [
      attr "Id" useRequired None Xdef.Int
      attr "IsEnabled" useRequired None (Xdef.FixedBoolean true)
      elmWithAttrs "ElmWithAttrs" required None (Xdef.String) <| [
        attr "Attr" useRequired None Xdef.String
      ]
      celm "Children" required None <| seq required [
        elm "Child" many None (Xdef.intRange 0 10) 
      ] 
    ]

  parse Xdef.pNode xdef
  |> should equal (Some <| expected)

[<Test>]
let ``コメントが指定されたXdefNodeをパースできる`` () =  
  let xdef = """
Root # Root Element Comment
  @Id : int # Attribute Comment
  Children # Complex Element Comment
    Child* : [0,10) # Simple Element Comment""".Trim()

  let expected = 
    celm "Root" required (Some "Root Element Comment") <| seq required [
      attr "Id" useRequired (Some "Attribute Comment") Xdef.Int
      celm "Children" required (Some "Complex Element Comment") <| seq required [
        elm "Child" many (Some "Simple Element Comment") (Xdef.intRange 0 10) 
      ] 
    ]

  parse Xdef.pNode xdef
  |> should equal (Some <| expected)
[<Test>]
let ``空行のあるXdefNodeをパースできる`` () =  
  let xdef = """
Root # Root Element Comment
  @Id : int # Attribute Comment

  Children # Complex Element Comment

    Child* : [0,10) # Simple Element Comment""".Trim()

  let expected = 
    celm "Root" required (Some "Root Element Comment") <| seq required [
      attr "Id" useRequired (Some "Attribute Comment") Xdef.Int
      celm "Children" required (Some "Complex Element Comment") <| seq required [
        elm "Child" many (Some "Simple Element Comment") (Xdef.intRange 0 10) 
      ] 
    ]

  parse Xdef.pNode xdef
  |> should equal (Some <| expected)

[<Test>]
let ``パーティクルが指定されたXdefNodeをパースできる`` () =  
  let xdef = """
Root
  MustSeqImplicitly
  MustSeq :: sequence[0..10]
  MustChoice :: choice[0..10]
  MustAll :: all[0..10]""".Trim()

  let expected = 
    celm "Root" required None <| seq required [
      // パーティクルが明示的に指定されなかった場合、Sequenceと推論される。またそのときの出現回数はrequiredになる
      celm "MustSeqImplicitly" required None <| seq required [ ]
      celm "MustSeq" required None <| seq (specific 0 10) [ ] 
      celm "MustChoice" required None <| choice (specific 0 10) [ ] 
      celm "MustAll" required None <| all (specific 0 10) [ ] 
    ]

  parse Xdef.pNode xdef
  |> should equal (Some <| expected)

[<Test>]
let ``NodeGeneratorCallを含むXdefNodeをパースできる`` () =  
  let xdef = """
Root
  !HogeGenerator[0..10] "param1" "param2"
    @Id : int
    Elm : string""".Trim()

  let expected = 
    celm "Root" required None <| seq required [
      nodeGeneratorInvokeNode "HogeGenerator" (specific 0 10) None [Xdef.FixedString "param1"; Xdef.FixedString "param2"] [
        attr "Id" useRequired None Xdef.Int
        elm "Elm" required None Xdef.String
      ]
    ]

  parse Xdef.pNode xdef
  |> should equal (Some <| expected)
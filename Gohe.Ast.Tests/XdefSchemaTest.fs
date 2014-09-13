module Test.``Xdef Schema Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``Xmlnsを指定してSchemaをパースできる`` () =  
  let xdef = """
@xmlns : "http://example.com/myschema"

Root"""       .Trim()

  let expectedRoot = celm "Root" required None <| seq required [ ]
  let expectedNs = attr "xmlns" useRequired None (Xdef.FixedString "http://example.com/myschema")
  let expected = Xdef.schema [expectedNs; expectedRoot]

  parse Xdef.pSchema xdef
  |> should equal (Some <| expected)
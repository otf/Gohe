module Test.``Xdef Schema Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``TargetNamespaceを指定してSchemaをパースできる`` () =  
  let xdef = """
@TargetNamespace : "http://example.com/myschema"

Root"""       .Trim()

  let (Xdef.Element expectedRoot) = celm "Root" required None <| seq required [ ]
  let expected = Xdef.schema (Some "http://example.com/myschema") [Xdef.Root expectedRoot]

  parse Xdef.pSchema xdef
  |> should equal (Some <| expected)
module Test.``Xsd Schema Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``Xmlnsを指定してXdefをXsd化できる`` () = 
  let (Xdef.Element inputRoot) =
    celm "Root" required None <| seq required [ 
      elmWithAttrs "Elm" required None (Xdef.FixedBool true) [
        attr "Attr" useRequired None Xdef.String
      ]
    ]
  let input = Xdef.schema (Some "http://example.com/myschema") [Xdef.Root inputRoot]

  Xsd.fromSchema input |> targetNamespace |> should equal "http://example.com/myschema"
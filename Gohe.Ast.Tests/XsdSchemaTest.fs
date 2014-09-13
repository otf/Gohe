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
  let inputRoot =
    celm "Root" required None <| seq required [ 
      elmWithAttrs "Elm" required None (Xdef.FixedBool true) [
        attr "Attr" useRequired None Xdef.String
      ]
    ]
  let inputNs = attr "xmlns" useRequired None (Xdef.FixedString "http://example.com/myschema")
  let input = Xdef.schema [inputNs; inputRoot]

  Xsd.fromSchema input |> targetNamespace |> should equal "http://example.com/myschema"
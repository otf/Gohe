module Test.``Xsd Built-in Node Generator Invoke Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``NodeGeneratorInvoke(Choice)をXsd化できる`` () = 
  let (Xdef.Element root) = 
    celm "Root" required None <| seq required [ 
      nodeGeneratorInvokeNode "Choice" required [] [
          elm "Elm1" required None Xdef.String
          elm "Elm2" required None Xdef.String
        ]
    ]
  
  let xdefSchema = Xdef.schema None [Xdef.Root root]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> should be ofExactType<XmlSchemaChoice>
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> at 0 |> should be ofExactType<XmlSchemaElement>
  result |> atOfSchema 0  |> asElm |> typeOfAsComplex |> particle |> at 0 |> at 1 |> should be ofExactType<XmlSchemaElement>

[<Test>]
let ``NodeGeneratorInvoke(Any)をXsd化できる`` () = 
  let (Xdef.Element root) = 
    celm "Root" required None <| seq required [ 
      nodeGeneratorInvokeNode "Any" required [] []
    ]
  
  let xdefSchema = Xdef.schema None [Xdef.Root root]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> should be ofExactType<XmlSchemaAny>

[<Test>]
[<Explicit("外部のスキーマを参照するので実行が遅い")>]
let ``NodeGeneratorInvoke(Include)をXsd化できる`` () = 
  let includeInvoke =
    nodeGeneratorInvoke "Include" required [Xdef.FixedString "http://www.w3.org/2001/xml.xsd"] []

  let xdefSchema = Xdef.schema (Some "http://www.w3.org/XML/1998/namespace") [Xdef.RootNodeGeneratorInvoke includeInvoke]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchemaInclude 0 |> should be ofExactType<XmlSchemaInclude>
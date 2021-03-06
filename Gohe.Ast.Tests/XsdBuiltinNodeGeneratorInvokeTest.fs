﻿module Test.``Xsd Built-in Node Generator Invoke Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``NodeGeneratorInvoke(choice)をXsd化できる`` () = 
  let root = 
    celm "Root" required None <| seq required [ 
      nodeGeneratorInvokeNode "choice" required None [] [
          elm "Elm1" required None (Xdef.TypeRef "string")
          elm "Elm2" required None (Xdef.TypeRef "string")
        ]
    ]
  
  let xdefSchema = Xdef.schema [root]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> should be ofExactType<XmlSchemaChoice>
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> at 0 |> should be ofExactType<XmlSchemaElement>
  result |> atOfSchema 0  |> asElm |> typeOfAsComplex |> particle |> at 0 |> at 1 |> should be ofExactType<XmlSchemaElement>

[<Test>]
let ``NodeGeneratorInvoke(any)をXsd化できる`` () = 
  let root = 
    celm "Root" required None <| seq required [ 
      nodeGeneratorInvokeNode "any" required None [] []
    ]
  
  let xdefSchema = Xdef.schema [root]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> should be ofExactType<XmlSchemaAny>


[<Test>]
let ``NodeGeneratorInvoke(element)をXsd化できる`` () = 
  let elmA = elm "ElmA" required None (Xdef.TypeRef "string")
  let root = 
    celm "Root" required None <| seq required [ 
      nodeGeneratorInvokeNode "element" required None [Xdef.FixedString "ElmA"] []
    ]
  
  let xdefSchema = Xdef.schema [elmA; root]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchema 1 |> asElm |> typeOfAsComplex |> particle |> at 0 |> should be ofExactType<XmlSchemaElement>

[<Test>]
[<Explicit("外部のスキーマを参照するので実行が遅い")>]
let ``NodeGeneratorInvoke(include)をXsd化できる`` () = 
  let includeInvoke =
    nodeGeneratorInvokeNode "include" required None [Xdef.FixedString "http://www.w3.org/2001/xml.xsd"] []
  let ns = nodeGeneratorInvokeNode "targetNamespace" required None [Xdef.FixedString "http://www.w3.org/XML/1998/namespace"] []
  let xdefSchema = Xdef.schema [ns; includeInvoke]
  let result = Xsd.fromSchema xdefSchema
  result |> atOfSchemaInclude 0 |> should be ofExactType<XmlSchemaInclude>

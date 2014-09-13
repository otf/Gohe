module Test.``Xsd Documentation Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``コメント指定のあるXdefをXsd化できる`` () = 
  let inputRoot = 
    celm "Root" required (Some "Root Element Comment") <| seq required [
      attr "Id" useRequired (Some "Attribute Comment") (Xdef.TypeRef "int")
      celm "Children" required (Some "Complex Element Comment") <| seq required [ ] 
      nodeGeneratorInvokeNode "any" required (Some "Any Comment") [] []
    ]
  let input = Xdef.schema [inputRoot]

  Xsd.fromSchema input |> atOfSchema 0 |> doc |> should equal "Root Element Comment"
  Xsd.fromSchema input |> atOfSchema 0 |> asElm |> typeOf |> attrs |> List.head |> doc |> should equal "Attribute Comment"
  Xsd.fromSchema input |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 0 |> doc |> should equal "Complex Element Comment"
  Xsd.fromSchema input |> atOfSchema 0 |> asElm |> typeOfAsComplex |> particle |> at 1 |> doc |> should equal "Any Comment"
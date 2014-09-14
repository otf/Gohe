module Test.``Xsd SimpleType Define Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``型定義をXsd化できる`` () = 
  let input = simpleTypeDef "MyString" None (Xdef.TypeRef "string")
  
  Xsd.fromSchema (Xdef.schema [input]) |> atOfSchema 0 |> asType |> name |> should equal "MyString"
module Test.``Xsd ComplexType Element Test``

open NUnit.Framework
open FsUnit

open System.Xml
open System.Xml.Schema
open XdefUtility
open Xsd
open XsdUtility

[<Test>]
let ``ComplexType(Sequence)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| seq required []
  
  Xsd.fromNode input |> name |> should equal "Root"

  Xsd.fromNode input |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaSequence>

[<Test>]
let ``ComplexType(Choice)の要素をXsd化できる`` () = 
  let input = celm "Root" required None <| choice required []
  
  Xsd.fromNode input |> name |> should equal "Root"

  Xsd.fromNode input |> typeOfAsComplex |> particle |> should be ofExactType<XmlSchemaChoice>
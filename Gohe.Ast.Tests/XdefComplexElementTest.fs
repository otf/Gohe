module Test.``Xdef Complex Element Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefParticle(Sequence)の指定をパースできる`` () =  
  parse Xdef.pParticle "Sequence"
  |> should equal (Some Xdef.Particle.Sequence)

[<Test>]
let ``XdefElement(暗黙のSequence)をパースできる`` () =  
  let expected = celm "Root" required None <| seq required []
  parse Xdef.pNode "Root"
  |> should equal (Some expected)

let particleTestFactors = [
  "Sequence", seq
  "Choice", choice
  "All", all
]

let occursTestFactors = [
  "", required
  "?", optional
  "*", many
  "+", requiredMany
  "[0..100]", specific 0 100
  "[100..*]", min 100
]

let childrenTestFactors = [
  "", []
  "\n  Child : string", [elm "Child" required None Xdef.String]
  "\n  @Child : string", [attr "Child" useRequired None Xdef.String]
  "\n  @Child1 : string\n  Child2 : string", [attr "Child1" useRequired None Xdef.String; elm "Child2" required None Xdef.String]
  "\n  Child1 : string\n  Child2 : string", [elm "Child1" required None Xdef.String; elm "Child2" required None Xdef.String]
]

let complexTypeTestCases : obj [][] = [|
  for (particleInput, particleExpected) in particleTestFactors do
  for (occursInput, occursExpected) in occursTestFactors do
  for (childrenInput, childrenExpected) in childrenTestFactors do
    yield [| particleInput; particleExpected; occursInput; occursExpected; childrenInput; childrenExpected |]
|]

[<TestCaseSource("complexTypeTestCases")>]
let ``複雑型のXdefElementをパースできる`` particleInput particleExpected occursInput occursExpected childrenInput childrenExpected =
  let expected = celm "Root" required None <| particleExpected (occursExpected: Xdef.Occurrence) childrenExpected
  parse Xdef.pNode (sprintf "Root :: %s%s%s" particleInput occursInput childrenInput)
  |> should equal (Some expected)
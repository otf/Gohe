module Test.``Xdef Node Generator Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``NodeGeneratorCallをパースできる`` () =  
  let input = "!HogeGenerator".Trim()

  let expected = nodeGeneratorInvoke "HogeGenerator" []

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)

[<Test>]
let ``パラメータを指定したNodeGeneratorCallをパースできる`` () =  
  let input = sprintf "!HogeGenerator %s" "\"text\" 10"

  let expected = nodeGeneratorInvoke "HogeGenerator" [Xdef.FixedString "text"; Xdef.FixedInt 10]

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)
module Test.``Xdef Node Generator Invoke Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``NodeGeneratorInvokeをパースできる`` () =  
  let input = "!HogeGenerator".Trim()

  let expected = nodeGeneratorInvoke "HogeGenerator" required [] []

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)

[<Test>]
let ``パラメータを指定したNodeGeneratorInvokeをパースできる`` () =  
  let input = sprintf "!HogeGenerator %s" "\"text\" 10"

  let expected = nodeGeneratorInvoke "HogeGenerator" required [Xdef.FixedString "text"; Xdef.FixedInt 10] []

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)

[<Test>]
let ``出現回数指定を指定したNodeGeneratorInvokeをパースできる`` () =  
  let input = "!HogeGenerator*"

  let expected = nodeGeneratorInvoke "HogeGenerator" many [] []

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)

[<Test>]
let ``子要素を指定したNodeGeneratorInvokeをパースできる`` () =  
  let input = "!HogeGenerator\n  Child : String"

  let expected = nodeGeneratorInvoke "HogeGenerator" required [] [elm "Child" required None Xdef.String]

  parse Xdef.pNodeGeneratorInvoke input
  |> should equal (Some <| expected)
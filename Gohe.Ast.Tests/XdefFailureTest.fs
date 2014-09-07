module Test.``Xdef Failure Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``型指定のないXdefAttributeはエラーがでる`` () =  
  parseGetError Xdef.pNode "@Name"
  |> should be (substr "型指定が必要")

[<Test>]
let ``解決できない型指定のXdefAttributeはエラーがでる`` () =  
  parseGetError Xdef.pNode "@Name : UnknownType"
  |> should be (substr "指定された型が未定義")

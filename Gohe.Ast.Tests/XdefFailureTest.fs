module Test.``Xdef Failure Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``型指定のないXdefAttributeはエラーがでる`` () =  
  parseGetError Xdef.pNode "@Name"
  |> should be (substr "型指定が必要")
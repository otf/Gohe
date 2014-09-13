module Test.``Xdef Failure Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``型指定のないXdefAttributeはエラーがでる`` () =  
  parseGetError Xdef.pNode "@Name"
  |> should be (substr "型指定が必要")

[<Test>]
let ``解決できないパーティクル指定のXdefElementはエラーがでる`` () =  
  parseGetError Xdef.pNode "Elm :: Hoge"
  |> should be (substr "指定されたパーティクルが未定義")

[<Test>]
let ``過度にインデントされた子要素のXdefElementはエラーがでる`` () =  
  parseGetError Xdef.pNode "Elm\n    Child1 : string"
  |> should be (substr "インデントが不正")
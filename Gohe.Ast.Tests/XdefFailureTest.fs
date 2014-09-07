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

[<Test>]
let ``解決できない型指定(前方一致)のXdefAttributeはエラーがでる`` () =  
  parseGetError Xdef.pNode "@Name : Stringtic"
  |> should be (substr "指定された型が未定義")

[<Test>]
let ``解決できない順序インジケータ指定のXdefElementはエラーがでる`` () =  
  parseGetError Xdef.pNode "Elm :: Hoge"
  |> should be (substr "指定された順序インジケータが未定義")

[<Test>]
let ``過度にインデントされた子要素のXdefElementはエラーがでる`` () =  
  parseGetError Xdef.pNode "Elm\n    Child1 : String"
  |> should be (substr "インデントが不正")
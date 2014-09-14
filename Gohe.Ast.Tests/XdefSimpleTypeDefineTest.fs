module Test.``Xdef Simple Type Define Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<Test>]
let ``XdefSimpleTypeDefineをパースできる`` () =  
  parse Xdef.pNode "MyString = string" 
  |> should equal (Some <| simpleTypeDef "MyString" None (Xdef.TypeRef "string"))

[<Test>]
let ``属性付きXdefSimpleTypeDefineをパースできる`` () =  
  parse Xdef.pNode "MyString = string\n  @Id : int" 
  |> should equal (Some <| simpleTypeDefWithAttrs "MyString" None (Xdef.TypeRef "string") [attr "Id" useRequired None (Xdef.TypeRef "int")])
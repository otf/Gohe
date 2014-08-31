module Test.``Type Test``

open NUnit.Framework
open FsUnit
open FParsec

let parse p input = 
  match runParserOnString p () "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

[<TestCase("\"hello\"", "hello")>]
[<TestCase("\"\"", "")>]
[<TestCase("\"\\\"\"", "\"")>]
let ``StringValueをパースできる`` (input, expected) =  
    parse Ast.pStringValue input |> should equal (Some <| Ast.StringValue expected)
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

[<TestCase("100", 100)>]
[<TestCase("-100", -100)>]
let ``IntValueをパースできる`` (input, expected) =  
    parse Ast.pIntValue input |> should equal (Some <| Ast.IntValue expected)

[<TestCase("100.001", 100.001)>]
[<TestCase("-100.001", -100.001)>]
let ``FloatValueをパースできる`` (input, expected) =  
    parse Ast.pFloatValue input |> should equal (Some <| Ast.FloatValue expected)
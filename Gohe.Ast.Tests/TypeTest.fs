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

[<Test>]
let ``PrimitiveTypeをパースできる`` () =  
    (parse Ast.pType "Bool") |> should equal (Some <| Ast.Bool)
    (parse Ast.pType "String") |> should equal (Some <| Ast.String)
    (parse Ast.pType "Int") |> should equal (Some <| Ast.Int)
    (parse Ast.pType "Float") |> should equal (Some <| Ast.Float)
    (parse Ast.pType "BigInt") |> should equal (Some <| Ast.BigInt)
    (parse Ast.pType "Guid") |> should equal (Some <| Ast.Guid)

[<TestCase("DateTime", null)>]
[<TestCase("DateTime<yyyy/MM/dd>", "yyyy/MM/dd")>]
[<TestCase("DateTime<\>>", ">")>]
let ``DateTimeをパースできる`` (input, expected) =  
    (parse Ast.pType input) |> should equal (Some <| Ast.DateTime (if expected <> null then Some expected else None))

[<TestCase("TimeSpan", null)>]
[<TestCase("TimeSpan<hh:mm:ss>", "hh:mm:ss")>]
[<TestCase("TimeSpan<\>>", ">")>]
let ``TimeSpanをパースできる`` (input, expected) =  
    (parse Ast.pType input) |> should equal (Some <| Ast.TimeSpan (if expected <> null then Some expected else None))

[<TestCase("(\"aaa\"|\"bbb\")", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" )", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" | \"ccc\" )", [| "aaa"; "bbb"; "ccc" |])>]
let ``ChoiceStringValuesをパースできる`` (input, expected) =  
    (parse Ast.pType input) |> should equal (Some <| Ast.ChoiceStringValues (expected |> Array.toList))

[<TestCase("[0,100)", 0, 99)>]
[<TestCase("[0,100]", 0, 100)>]
[<TestCase("[ 0 , 100 )", 0, 99)>]
[<TestCase("[ 0 , 100 ]", 0, 100)>]
let ``IntRangeをパースできる`` (input, expectedMin, expectedMax) =  
    (parse Ast.pType input) |> should equal (Some <| Ast.IntRange(expectedMin, expectedMax))

[<TestCase("/.*/", ".*")>]
[<TestCase("/\/*/", "/*")>]
let ``Regexをパースできる`` (input, expected) =  
    (parse Ast.pType input) |> should equal (Some <| Ast.Regex(expected))
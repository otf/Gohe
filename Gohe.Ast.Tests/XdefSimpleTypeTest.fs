module Test.``XdefSimpleType Test``

open NUnit.Framework
open FsUnit
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

[<TestCase("\"hello\"", "hello")>]
[<TestCase("\"\"", "")>]
[<TestCase("\"\\\"\"", "\"")>]
let ``FixedStringをパースできる`` (input, expected) =  
    parse Ast.pFixedString input |> should equal (Some <| Ast.FixedString expected)

[<TestCase("100", 100)>]
[<TestCase("-100", -100)>]
let ``FixedIntをパースできる`` (input, expected) =  
    parse Ast.pFixedInt input |> should equal (Some <| Ast.FixedInt expected)

[<TestCase("100.001", 100.001)>]
[<TestCase("-100.001", -100.001)>]
let ``FixedFloatをパースできる`` (input, expected) =  
    parse Ast.pFixedFloat input |> should equal (Some <| Ast.FixedFloat expected)

[<Test>]
let ``PrimitiveTypeをパースできる`` () =  
    (parse Ast.pXdefSimpleType "Bool") |> should equal (Some <| Ast.Bool)
    (parse Ast.pXdefSimpleType "String") |> should equal (Some <| Ast.String)
    (parse Ast.pXdefSimpleType "Int") |> should equal (Some <| Ast.Int)
    (parse Ast.pXdefSimpleType "Float") |> should equal (Some <| Ast.Float)
    (parse Ast.pXdefSimpleType "Decimal") |> should equal (Some <| Ast.Decimal)
    (parse Ast.pXdefSimpleType "Guid") |> should equal (Some <| Ast.Guid)

[<TestCase("DateTime", null)>]
[<TestCase("DateTime<yyyy/MM/dd>", "yyyy/MM/dd")>]
[<TestCase("DateTime<\>>", ">")>]
let ``DateTimeをパースできる`` (input, expected) =  
    (parse Ast.pXdefSimpleType input) |> should equal (Some <| Ast.DateTime (if expected <> null then Some expected else None))

[<TestCase("TimeSpan", null)>]
[<TestCase("TimeSpan<hh:mm:ss>", "hh:mm:ss")>]
[<TestCase("TimeSpan<\>>", ">")>]
let ``TimeSpanをパースできる`` (input, expected) =  
    (parse Ast.pXdefSimpleType input) |> should equal (Some <| Ast.TimeSpan (if expected <> null then Some expected else None))

[<TestCase("(\"aaa\"|\"bbb\")", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" )", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" | \"ccc\" )", [| "aaa"; "bbb"; "ccc" |])>]
let ``RestrictedStringをパースできる`` (input, expected) =  
    (parse Ast.pXdefSimpleType input) |> should equal (Some <| Ast.RestrictedString (expected |> Array.toList))

[<TestCase("[0,100)", 0, 99)>]
[<TestCase("[0,100]", 0, 100)>]
[<TestCase("[ 0 , 100 )", 0, 99)>]
[<TestCase("[ 0 , 100 ]", 0, 100)>]
let ``IntRangeをパースできる`` (input, expectedMin, expectedMax) =  
    (parse Ast.pXdefSimpleType input) |> should equal (Some <| Ast.IntRange(expectedMin, expectedMax))

[<TestCase("/.*/", ".*")>]
[<TestCase("/\/*/", "/*")>]
let ``Patternをパースできる`` (input, expected) =  
    (parse Ast.pXdefSimpleType input) |> should equal (Some <| Ast.Pattern(expected))
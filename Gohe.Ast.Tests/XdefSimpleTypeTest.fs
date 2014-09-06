module Test.``XdefSimpleType Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<TestCase("\"hello\"", "hello")>]
[<TestCase("\"\"", "")>]
[<TestCase("\"\\\"\"", "\"")>]
let ``FixedStringをパースできる`` (input, expected) =  
    parse Xdef.pFixedString input |> should equal (Some <| Xdef.FixedString expected)

[<TestCase("100", 100)>]
[<TestCase("-100", -100)>]
let ``FixedIntをパースできる`` (input, expected) =  
    parse Xdef.pFixedInt input |> should equal (Some <| Xdef.FixedInt expected)

[<TestCase("100.001", 100.001)>]
[<TestCase("-100.001", -100.001)>]
let ``FixedFloatをパースできる`` (input, expected) =  
    parse Xdef.pFixedFloat input |> should equal (Some <| Xdef.FixedFloat expected)

let primitiveTypeTestCases : obj [][] = [|
  [|"Bool"; Xdef.Bool|]
  [|"String"; Xdef.String|]
  [|"Int"; Xdef.Int|]
  [|"Float"; Xdef.Float|]
  [|"Decimal"; Xdef.Decimal|]
  [|"Guid"; Xdef.Guid|]
|]

[<TestCaseSource("primitiveTypeTestCases")>]
let ``PrimitiveTypeをパースできる`` input expected =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| expected)

[<TestCase("DateTime", null)>]
[<TestCase("DateTime<yyyy/MM/dd>", "yyyy/MM/dd")>]
[<TestCase("DateTime<\>>", ">")>]
let ``DateTimeをパースできる`` (input, expected) =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.DateTime (if expected <> null then Some expected else None))

[<TestCase("TimeSpan", null)>]
[<TestCase("TimeSpan<hh:mm:ss>", "hh:mm:ss")>]
[<TestCase("TimeSpan<\>>", ">")>]
let ``TimeSpanをパースできる`` (input, expected) =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.TimeSpan (if expected <> null then Some expected else None))

[<TestCase("(\"aaa\"|\"bbb\")", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" )", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" | \"ccc\" )", [| "aaa"; "bbb"; "ccc" |])>]
let ``RestrictedStringをパースできる`` (input, expected) =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.RestrictedString (expected |> Array.toList))

[<TestCase("[0,100)", 0, 99)>]
[<TestCase("[0,100]", 0, 100)>]
[<TestCase("[ 0 , 100 )", 0, 99)>]
[<TestCase("[ 0 , 100 ]", 0, 100)>]
let ``IntRangeをパースできる`` (input, expectedMin, expectedMax) =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.IntRange(expectedMin, expectedMax))

[<TestCase("/.*/", ".*")>]
[<TestCase("/\/*/", "/*")>]
let ``Patternをパースできる`` (input, expected) =  
    (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.Pattern(expected))
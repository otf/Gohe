module Test.``XdefSimpleType Test``

open NUnit.Framework
open FsUnit

open XdefUtility

[<TestCase("true", true)>]
[<TestCase("false", false)>]
let ``FixedBooleanをパースできる`` (input, expected) =  
  parse Xdef.pFixedBoolean input |> should equal (Some <| Xdef.FixedBoolean expected)

[<TestCase("127y", 127y)>]
[<TestCase("-128y", -128y)>]
let ``FixedByteをパースできる`` (input, expected) =  
  parse Xdef.pFixedByte input |> should equal (Some <| Xdef.FixedByte expected)

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

[<TestCase("(\"aaa\"|\"bbb\")", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" )", [| "aaa"; "bbb" |])>]
[<TestCase("( \"aaa\" | \"bbb\" | \"ccc\" )", [| "aaa"; "bbb"; "ccc" |])>]
let ``EnumeratedStringをパースできる`` (input, expected) =  
  (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.EnumeratedString (expected |> Array.toList))

[<TestCase("char", 1)>]
let ``FixedLengthString(char)をパースできる`` (input, expected) =  
  (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.FixedLengthString(expected))

[<TestCase("char[100]", 100)>]
let ``FixedLengthStringをパースできる`` (input, expected) =  
  (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.FixedLengthString(expected))

[<TestCase("char[0..100]", 0, 100)>]
let ``VariableLengthStringをパースできる`` (input, minExpected, maxExpected) =  
  (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.VariableLengthString(minExpected, Some maxExpected))

[<TestCase("char[100..*]", 100)>]
let ``VariableLengthString(minのみ指定)をパースできる`` (input, minExpected) =  
  (parse Xdef.pSimpleType input) |> should equal (Some <| Xdef.VariableLengthString(minExpected, None))

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
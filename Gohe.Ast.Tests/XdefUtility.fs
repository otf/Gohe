module XdefUtility

open Xdef
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

let parseGetError p input = 
  match runParserOnString p 0 "" input with
  | Failure (msg, err, s) -> msg
  | _ -> failwith "has no error"

let substr expected = NUnit.Framework.Constraints.SubstringConstraint(expected)

let attr nm occurs comm stype = Xdef.Attribute <| Xdef.attribute nm occurs stype comm
let elm nm occurs comm stype = Xdef.Element <| Xdef.element nm occurs (Xdef.Simple <| (stype, [])) comm
let elmWithAttrs nm occurs comm stype attrs = Xdef.Element <| Xdef.element nm occurs (Xdef.Simple <| (stype, attrs |> List.map (function Xdef.Attribute attr -> attr))) comm
let celm nm occurs comm ctype = Xdef.Element <| Xdef.element nm occurs (Xdef.Complex <| ctype) comm
let seq occurs nodes = Xdef.complexType Xdef.Sequence occurs nodes
let choice occurs nodes = Xdef.complexType Xdef.Choice occurs nodes
let all occurs nodes = Xdef.complexType Xdef.All occurs nodes
let useRequired = Xdef.AttributeOccurrence.Required
let useOptional = Xdef.AttributeOccurrence.Optional
let required = Xdef.Occurrence.Required
let optional = Xdef.Occurrence.Optional
let many = Xdef.Occurrence.Many
let requiredMany = Xdef.Occurrence.RequiredMany
let specific min max = Xdef.Occurrence.Specified (min, Some max)
let min n = Xdef.Occurrence.Specified (n, None)
let nodeGeneratorInvoke nm occurs parameters nodes = Xdef.nodeGeneratorInvoke nm occurs parameters nodes
module XdefUtility

open Xdef
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

let attr nm occurs comm stype = Xdef.Attribute <| Xdef.attribute nm occurs stype comm
let elm nm occurs comm stype = Xdef.Element <| Xdef.element nm occurs (Xdef.Simple <| stype) comm
let celm nm occurs comm ctype = Xdef.Element <| Xdef.element nm occurs (Xdef.Complex <| ctype) comm
let seq occurs nodes = Xdef.complexType Xdef.Sequence occurs nodes
let choice occurs nodes = Xdef.complexType Xdef.Choice occurs nodes
let all occurs nodes = Xdef.complexType Xdef.All occurs nodes
let required = Xdef.Required
let optional = Xdef.Optional
let many = Xdef.Many
let requiredMany = Xdef.RequiredMany
let specific min max = Xdef.Specified (Some min, Some max)
let max n = Xdef.Specified (None, Some n)
let min n = Xdef.Specified (Some n, None)
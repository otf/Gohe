module AstUtility

open Ast
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

let attr nm occurs comm stype = Ast.Attribute <| Ast.xdefAttribute nm occurs stype comm
let elm nm occurs comm stype = Ast.Element <| Ast.xdefElement nm occurs (Ast.Simple <| stype) comm
let celm nm occurs comm ctype = Ast.Element <| Ast.xdefElement nm occurs (Ast.Complex <| ctype) comm
let seq occurs nodes = Ast.xdefComplexType Ast.Sequence occurs nodes
let choice occurs nodes = Ast.xdefComplexType Ast.Choice occurs nodes
let all occurs nodes = Ast.xdefComplexType Ast.All occurs nodes
let required = Ast.Required
let optional = Ast.Optional
let many = Ast.Many
let requiredMany = Ast.RequiredMany
let specific min max = Ast.Specified (Some min, Some max)
let max n = Ast.Specified (None, Some n)
let min n = Ast.Specified (Some n, None)
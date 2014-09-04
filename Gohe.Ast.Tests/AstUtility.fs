module AstUtility

open Ast
open FParsec

let parse p input = 
  match runParserOnString p 0 "" input with
  | Success (r, s, p) -> Some  r
  | Failure (msg, err, s) -> None

let elm nm occurs comm stype = Ast.SimpleElement <| Ast.xdefSimpleElement nm occurs stype comm
let celm nm occurs comm order nodes = Ast.ComplexElement <| Ast.xdefComplexElement nm occurs order comm nodes
let seq = Ast.Sequence
let choice = Ast.Choice
let all = Ast.All
let attr nm occurs comm stype = Ast.Attribute <| Ast.xdefAttribute nm occurs stype comm
let required = Ast.Required
let optional = Ast.Optional
let many = Ast.Many
let requiredMany = Ast.RequiredMany
let specific min max = Ast.Specified (Some min, Some max)
let max n = Ast.Specified (None, Some n)
let min n = Ast.Specified (Some n, None)
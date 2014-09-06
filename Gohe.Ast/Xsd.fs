module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef

let simpleTypeToQName etype = 
  let qName nm = XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
  match etype with
  | Bool -> qName "boolean"
  | String -> qName "string"
  | Int -> qName "integer"
  | Float -> qName "float"
  | Decimal -> qName "decimal"
  | _ -> failwith "unsupported type"

let fromElement  ({ Name = name; Type = Simple eType } : Element) = 
  let result = XmlSchemaElement()
  result.Name <- name
  result.SchemaTypeName <- simpleTypeToQName eType
  result

let fromNode node = 
  match node with
  | Element element -> fromElement element 
  | _ -> failwith ""
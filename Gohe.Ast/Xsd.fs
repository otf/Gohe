module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef

type private ElementType = 
  | QName of XmlQualifiedName
  | FixedValue of string

let private fromSimpleType etype = 
  let qName nm = QName <| XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
  match etype with
  | Bool -> qName "boolean"
  | String -> qName "string"
  | Int -> qName "integer"
  | Float -> qName "float"
  | Decimal -> qName "decimal"
  | FixedString value -> FixedValue value
  | FixedInt value -> FixedValue <| value.ToString()
  | FixedFloat value -> FixedValue <| value.ToString()
  | _ -> failwith "unsupported type"

let private fromComplexType order = 
  let cType particle =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    cType

  match order with
  | Sequence -> cType (XmlSchemaSequence())
  | Choice -> cType (XmlSchemaChoice())
  | All -> cType (XmlSchemaAll())

let fromElement  ({ Name = name; Type = eType } : Element) = 
  let result = XmlSchemaElement()
  result.Name <- name

  match eType with
  | Simple sType ->
      match fromSimpleType sType with
      | QName qname -> 
          result.SchemaTypeName <- qname
      | FixedValue value ->
          result.FixedValue <- value
  | Complex { Order = order } ->
      result.SchemaType <- fromComplexType order

  result

let fromNode node = 
  match node with
  | Element element -> fromElement element 
  | _ -> failwith ""
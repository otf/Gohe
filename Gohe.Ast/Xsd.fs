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

let private fromOrder order = 
  match order with
  | Sequence -> XmlSchemaSequence() :> XmlSchemaParticle
  | Choice -> XmlSchemaChoice() :> XmlSchemaParticle
  | All -> XmlSchemaAll() :> XmlSchemaParticle

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
      let c = XmlSchemaComplexType()
      c.Particle <- fromOrder order
      result.SchemaType <- c

  result

let fromNode node = 
  match node with
  | Element element -> fromElement element 
  | _ -> failwith ""
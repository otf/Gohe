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

let private fromOccurrence occurs =
  match occurs with
  | Required -> "1", "1"
  | Optional -> "0", "1"
  | Many -> "0", "unbounded"
  | RequiredMany -> "1", "unbounded"
  | Specified(min, max) -> (match min with Some n -> n.ToString() | None -> "0"), (match max with Some n -> n.ToString() | None -> "unbounded")

let private fromComplexType order occurs = 
  let cType particle =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    let (minOccursString, maxOccursString) = fromOccurrence occurs
    particle.MinOccursString <- minOccursString
    particle.MaxOccursString <- maxOccursString
    cType

  match order with
  | Sequence -> cType (XmlSchemaSequence())
  | Choice -> cType (XmlSchemaChoice())
  | All -> cType (XmlSchemaAll())

let fromElement  ({ Name = name; Occurrence = occurs; Type = eType } : Element) = 
  let result = XmlSchemaElement()
  result.Name <- name
  let (minOccursString, maxOccursString) = fromOccurrence occurs
  result.MinOccursString <- minOccursString
  result.MaxOccursString <- maxOccursString
  
  match eType with
  | Simple sType ->
      match fromSimpleType sType with
      | QName qname -> 
          result.SchemaTypeName <- qname
      | FixedValue value ->
          result.FixedValue <- value
  | Complex { Order = order; Occurrence = occurs } ->
      result.SchemaType <- fromComplexType order occurs

  result

let fromNode node = 
  match node with
  | Element element -> fromElement element 
  | _ -> failwith ""
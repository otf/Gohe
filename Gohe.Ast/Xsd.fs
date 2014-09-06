module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef

type private ElementType = 
  | QName of XmlQualifiedName
  | FixedValue of string
  | SimpleTypeWithFacets of XmlSchemaSimpleType

let private fromSimpleType etype = 
  let qName nm =  XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
  match etype with
  | Bool -> QName <| qName "boolean"
  | String -> QName <| qName "string"
  | Int -> QName <| qName "integer"
  | Float -> QName <| qName "float"
  | Decimal -> QName <| qName "decimal"
  | FixedBool value -> FixedValue ((value.ToString()).ToLower())
  | FixedString value -> FixedValue value
  | FixedInt value -> FixedValue <| value.ToString()
  | FixedFloat value -> FixedValue <| value.ToString()
  | EnumeratedString values -> 
      let simpleTypeWithFacets = XmlSchemaSimpleType()
      let restriction = XmlSchemaSimpleTypeRestriction()
      simpleTypeWithFacets.Content <- restriction
      restriction.BaseTypeName <- qName "string"
      for value in values do
        let enum = XmlSchemaEnumerationFacet()
        enum.Value <- value
        restriction.Facets.Add(enum) |> ignore
      SimpleTypeWithFacets <| simpleTypeWithFacets
  | _ -> failwith "unsupported type"

let inline private setSimpleType sType (x:^a) =
  match fromSimpleType sType with
  | QName qname -> 
      ((^a) : (member set_SchemaTypeName : XmlQualifiedName -> unit) (x, qname))
  | FixedValue value ->
      ((^a) : (member set_FixedValue : string -> unit) (x, value))
  | SimpleTypeWithFacets typ ->
      ((^a) : (member set_SchemaType : XmlSchemaSimpleType -> unit) (x, typ))

let private fromOccurrence occurs =
  match occurs with
  | Required -> "1", "1"
  | Optional -> "0", "1"
  | Many -> "0", "unbounded"
  | RequiredMany -> "1", "unbounded"
  | Specified(min, max) -> min.ToString(), (match max with Some n -> n.ToString() | None -> "unbounded")

let private setOccurrence occurs (particle:XmlSchemaParticle) =
  let (minOccursString, maxOccursString) = fromOccurrence occurs
  particle.MinOccursString <- minOccursString
  particle.MaxOccursString <- maxOccursString

let private setOccurrenceForAttr occurs (attr:XmlSchemaAttribute) =
  match occurs with
  | AttributeOccurrence.Required -> attr.Use <- XmlSchemaUse.Required
  | AttributeOccurrence.Optional -> attr.Use <- XmlSchemaUse.Optional
  
let rec private fromComplexType { Order = order; Occurrence = occurs; Nodes = nodes } = 
  let cType (particle:XmlSchemaGroupBase) =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    setOccurrence occurs particle
    for node in nodes |> List.map fromNode do
      particle.Items.Add(node) |> ignore
    cType

  match order with
  | Sequence -> cType (XmlSchemaSequence())
  | Choice -> cType (XmlSchemaChoice())
  | All -> cType (XmlSchemaAll())

and fromElement  ({ Name = name; Occurrence = occurs; Type = eType } : Element) = 
  let result = XmlSchemaElement()
  result.Name <- name
  setOccurrence occurs result
  
  match eType with
  | Simple sType ->
      setSimpleType sType result
  | Complex cType ->
      result.SchemaType <- fromComplexType cType

  result

and fromAttribute ({ Name = name; Occurrence = occurs; Type = sType } : Attribute) =
  let result = XmlSchemaAttribute()
  result.Name <- name
  setOccurrenceForAttr occurs result 
  setSimpleType sType result
  result

and fromNode node = 
  match node with
  | Element element -> fromElement element :> XmlSchemaObject
  | Attribute attr -> fromAttribute attr :> _
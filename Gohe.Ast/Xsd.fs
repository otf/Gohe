module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef

type private ElementType = 
  | QName of XmlQualifiedName
  | FixedValue of XmlQualifiedName * string
  | SimpleTypeWithFacets of XmlSchemaSimpleType

let private fromFacets qName facets =
  let simpleTypeWithFacets = XmlSchemaSimpleType()
  let restriction = XmlSchemaSimpleTypeRestriction()
  simpleTypeWithFacets.Content <- restriction
  restriction.BaseTypeName <- qName
  for facet in facets do
    restriction.Facets.Add(facet) |> ignore
  simpleTypeWithFacets


let private fromSimpleType etype = 
  let qName nm =  XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
  match etype with
  | Bool -> QName <| qName "boolean"
  | Byte -> QName <| qName "byte"
  | String -> QName <| qName "string"
  | Int -> QName <| qName "integer"
  | Float -> QName <| qName "float"
  | Decimal -> QName <| qName "decimal"
  | FixedBool value -> FixedValue (qName "boolean", (value.ToString()).ToLower())
  | FixedByte value -> FixedValue (qName "byte", value.ToString())
  | FixedString value -> FixedValue (qName "string", value)
  | FixedInt value -> FixedValue <| (qName "integer", value.ToString())
  | FixedFloat value -> FixedValue <| (qName "float", value.ToString())
  | EnumeratedString values -> 
      let facets = [ 
        for value in values do
          let r = XmlSchemaEnumerationFacet()
          r.Value <- value
          yield r
      ]
      SimpleTypeWithFacets <| fromFacets (qName "string") facets
  | FixedLengthString length -> 
      let facets =[
        let r = XmlSchemaLengthFacet()
        r.Value <- length.ToString()
        yield r
      ]
      SimpleTypeWithFacets <| fromFacets (qName "string") facets
  | VariableLengthString(min, None) -> 
      let minFacet = XmlSchemaMinLengthFacet()
      minFacet.Value <- min.ToString()
      SimpleTypeWithFacets <| fromFacets (qName "string") [minFacet]
  | VariableLengthString(min, Some max) -> 
      let minFacet = XmlSchemaMinLengthFacet()
      minFacet.Value <- min.ToString()
      let maxFacet = XmlSchemaMaxLengthFacet()
      maxFacet.Value <- max.ToString()
      SimpleTypeWithFacets <| fromFacets (qName "string") [(minFacet :> XmlSchemaFacet); (maxFacet :> _)]
  | IntRange(min, max) -> 
      let minFacet = XmlSchemaMinInclusiveFacet()
      minFacet.Value <- min.ToString()
      let maxFacet = XmlSchemaMaxInclusiveFacet()
      maxFacet.Value <- max.ToString()
      SimpleTypeWithFacets <| fromFacets (qName "integer") [(minFacet :> XmlSchemaFacet); (maxFacet :> _)]
  | Pattern(pattern) -> 
      let facet = XmlSchemaPatternFacet()
      facet.Value <- pattern
      SimpleTypeWithFacets <| fromFacets (qName "string") [facet]
  | _ -> failwith "unsupported type"

let inline private setSimpleType sType (x:^a) =
  match fromSimpleType sType with
  | QName qname -> 
      ((^a) : (member set_SchemaTypeName : XmlQualifiedName -> unit) (x, qname))
  | FixedValue (qname, value) ->
      ((^a) : (member set_SchemaTypeName : XmlQualifiedName -> unit) (x, qname))
      ((^a) : (member set_FixedValue : string -> unit) (x, value))
  | SimpleTypeWithFacets typ ->
      ((^a) : (member set_SchemaType : XmlSchemaSimpleType -> unit) (x, typ))

let inline private setBaseSimpleType sType (ext:^a) (elm:^b) =
  match fromSimpleType sType with
  | QName qname -> 
      ((^a) : (member set_BaseTypeName : XmlQualifiedName -> unit) (ext, qname))
  | FixedValue (qname, value) ->
      ((^a) : (member set_BaseTypeName : XmlQualifiedName -> unit) (ext, qname))
      ((^b) : (member set_FixedValue : string -> unit) (elm, value))
  | SimpleTypeWithFacets typ ->
      failwith "not implemented"
//      ((^c) : (member set_BaseXmlSchemaType : XmlSchemaType -> unit) (cType, typ))

let private fromOccurrence occurs =
  match occurs with
  | Required -> "1", "1"
  | Optional -> "0", "1"
  | Many -> "0", "unbounded"
  | RequiredMany -> "1", "unbounded"
  | Specified(min, max) -> min.ToString(), (match max with Some n -> n.ToString() | None -> "unbounded")

let private setOccurrence occurs (particle:XmlSchemaParticle) =
  let (minOccursString, maxOccursString) = fromOccurrence occurs
  if minOccursString <> "1" then particle.MinOccursString <- minOccursString
  if maxOccursString <> "1" then particle.MaxOccursString <- maxOccursString

let private setOccurrenceForAttr occurs (attr:XmlSchemaAttribute) =
  match occurs with
  | AttributeOccurrence.Required -> attr.Use <- XmlSchemaUse.Required
  | AttributeOccurrence.Optional -> attr.Use <- XmlSchemaUse.Optional
  
let rec private fromComplexType { Order = order; Occurrence = occurs; Nodes = nodes } = 
  let cType (particle:XmlSchemaGroupBase) =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    setOccurrence occurs particle
    for node in nodes do
      match node with
      | Element _ -> particle.Items.Add(fromNode node) |> ignore
      | Attribute _ -> cType.Attributes.Add(fromNode node) |> ignore
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
  | Simple(sType, []) ->
      setSimpleType sType result
  | Simple(sType, attrs) ->
      let typ = XmlSchemaComplexType()
      result.SchemaType <- typ

      let contentModel = XmlSchemaSimpleContent()
      typ.ContentModel <- contentModel

      let ext = XmlSchemaSimpleContentExtension()
      contentModel.Content <- ext
      for attr in attrs do
        ext.Attributes.Add(fromAttribute attr) |> ignore

      setBaseSimpleType sType ext result 
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

let fromRoot element = 
  let schema = XmlSchema()
  let root = fromElement element
  schema.Items.Add(root) |> ignore
  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema
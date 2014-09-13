module XsdInternal

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef

type ElementType = 
  | QName of XmlQualifiedName
  | FixedValue of XmlQualifiedName * string
  | SimpleTypeWithFacets of XmlSchemaSimpleType

let schemaQName nm =  XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")

let fromFacets qName facets =
  let simpleTypeWithFacets = XmlSchemaSimpleType()
  let restriction = XmlSchemaSimpleTypeRestriction()
  simpleTypeWithFacets.Content <- restriction
  restriction.BaseTypeName <- qName
  for facet in facets do
    restriction.Facets.Add(facet) |> ignore
  simpleTypeWithFacets

let fromSimpleType etype = 
  match etype with
  | Bool -> QName <| schemaQName "boolean"
  | Byte -> QName <| schemaQName "byte"
  | String -> QName <| schemaQName "string"
  | Int -> QName <| schemaQName "int"
  | Float -> QName <| schemaQName "float"
  | Decimal -> QName <| schemaQName "decimal"
  | Date -> QName <| schemaQName "date"
  | Time -> QName <| schemaQName "time"
  | DateTime -> QName <| schemaQName "dateTime"
  | Duration -> QName <| schemaQName "duration"
  | FixedBool value -> FixedValue (schemaQName "boolean", (value.ToString()).ToLower())
  | FixedByte value -> FixedValue (schemaQName "byte", value.ToString())
  | FixedString value -> FixedValue (schemaQName "string", value)
  | FixedInt value -> FixedValue <| (schemaQName "int", value.ToString())
  | FixedFloat value -> FixedValue <| (schemaQName "float", value.ToString())
  | EnumeratedString values -> 
      let facets = [ 
        for value in values do
          let r = XmlSchemaEnumerationFacet()
          r.Value <- value
          yield r
      ]
      SimpleTypeWithFacets <| fromFacets (schemaQName "string") facets
  | FixedLengthString length -> 
      let facets =[
        let r = XmlSchemaLengthFacet()
        r.Value <- length.ToString()
        yield r
      ]
      SimpleTypeWithFacets <| fromFacets (schemaQName "string") facets
  | VariableLengthString(min, None) -> 
      let minFacet = XmlSchemaMinLengthFacet()
      minFacet.Value <- min.ToString()
      SimpleTypeWithFacets <| fromFacets (schemaQName "string") [minFacet]
  | VariableLengthString(min, Some max) -> 
      let minFacet = XmlSchemaMinLengthFacet()
      minFacet.Value <- min.ToString()
      let maxFacet = XmlSchemaMaxLengthFacet()
      maxFacet.Value <- max.ToString()
      SimpleTypeWithFacets <| fromFacets (schemaQName "string") [(minFacet :> XmlSchemaFacet); (maxFacet :> _)]
  | IntRange(min, max) -> 
      let minFacet = XmlSchemaMinInclusiveFacet()
      minFacet.Value <- min.ToString()
      let maxFacet = XmlSchemaMaxInclusiveFacet()
      maxFacet.Value <- max.ToString()
      SimpleTypeWithFacets <| fromFacets (schemaQName "int") [(minFacet :> XmlSchemaFacet); (maxFacet :> _)]
  | Pattern(pattern) -> 
      let facet = XmlSchemaPatternFacet()
      facet.Value <- pattern
      SimpleTypeWithFacets <| fromFacets (schemaQName "string") [facet]
  | _ -> failwith "unsupported type"
  
let inline setSimpleType sType (x:^a) =
  match fromSimpleType sType with
  | QName qname -> 
      ((^a) : (member set_SchemaTypeName : XmlQualifiedName -> unit) (x, qname))
  | FixedValue (qname, value) ->
      ((^a) : (member set_SchemaTypeName : XmlQualifiedName -> unit) (x, qname))
      ((^a) : (member set_FixedValue : string -> unit) (x, value))
  | SimpleTypeWithFacets typ ->
      ((^a) : (member set_SchemaType : XmlSchemaSimpleType -> unit) (x, typ))

let inline setBaseSimpleType sType (ext:^a) (elm:^b) =
  match fromSimpleType sType with
  | QName qname -> 
      ((^a) : (member set_BaseTypeName : XmlQualifiedName -> unit) (ext, qname))
  | FixedValue (qname, value) ->
      ((^a) : (member set_BaseTypeName : XmlQualifiedName -> unit) (ext, qname))
      ((^b) : (member set_FixedValue : string -> unit) (elm, value))
  | SimpleTypeWithFacets typ ->
      failwith "not implemented"
//      ((^c) : (member set_BaseXmlSchemaType : XmlSchemaType -> unit) (cType, typ))

let fromOccurrence occurs =
  // occurs=1の場合は、属性自体つけたくないためNoneにする
  match occurs with
  | Required -> None, None
  | Optional -> Some "0", None
  | Many -> Some "0", Some "unbounded"
  | RequiredMany -> None, Some "unbounded"
  | Specified(1, Some 1) -> None, None
  | Specified(1, Some max) -> None, Some (max.ToString())
  | Specified(1, None) -> None, Some "unbounded"
  | Specified(min, None) -> Some (min.ToString()), Some "unbounded"
  | Specified(min, Some max) -> Some (min.ToString()), Some (max.ToString())

let setOccurrence occurs (particle:XmlSchemaParticle) =
  let (minOccursString, maxOccursString) = fromOccurrence occurs

  minOccursString |> Option.iter (fun n -> particle.MinOccursString <- n)
  maxOccursString |> Option.iter (fun n -> particle.MaxOccursString <- n)

let setOccurrenceForAttr occurs (attr:XmlSchemaAttribute) =
  match occurs with
  | AttributeOccurrence.Required -> attr.Use <- XmlSchemaUse.Required
  | AttributeOccurrence.Optional -> attr.Use <- XmlSchemaUse.Optional

let setDoc comm (x:#XmlSchemaAnnotated) =
  let anno = XmlSchemaAnnotation()
  let doc = XmlSchemaDocumentation()
  anno.Items.Add(doc) |> ignore
  let xDoc = XmlDocument()
  let textNode = xDoc.CreateTextNode(comm)
  doc.Markup <- [|textNode|]
  x.Annotation <- anno
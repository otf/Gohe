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

type NodeGeneratorSignature = {
  Name : string
  ParameterCount : int
  HasOccurrence : bool
  HasChildren : bool
}

let choiceElementGeneratorSignature = { 
  Name = "Choice"
  ParameterCount = 0
  HasOccurrence = true
  HasChildren = true
}

let anyElementGeneratorSignature = { 
  Name = "Any"
  ParameterCount = 0
  HasOccurrence = true
  HasChildren = false
}

type NodeGeneratorInvoker = NodeGeneratorInvoke -> XmlSchemaObject

let lookupElementGenerator (table : (NodeGeneratorSignature * NodeGeneratorInvoker) list) (invoke : NodeGeneratorInvoke) =
  let pred { Name = nm; ParameterCount = paramCount; HasOccurrence = hasOccurrence; HasChildren = hasChildren} =
    invoke.Name = nm
    && invoke.Parameters.Length = paramCount
    && (hasChildren || (invoke.Nodes |> List.isEmpty))

  table |> List.tryFind (fun (signature, _) -> pred signature) |> Option.map (snd)

let private fromSimpleType etype = 
  let qName nm =  XmlQualifiedName(nm, "http://www.w3.org/2001/XMLSchema")
  match etype with
  | Bool -> QName <| qName "boolean"
  | Byte -> QName <| qName "byte"
  | String -> QName <| qName "string"
  | Int -> QName <| qName "int"
  | Float -> QName <| qName "float"
  | Decimal -> QName <| qName "decimal"
  | Date -> QName <| qName "date"
  | Time -> QName <| qName "time"
  | DateTime -> QName <| qName "dateTime"
  | Duration -> QName <| qName "duration"
  | FixedBool value -> FixedValue (qName "boolean", (value.ToString()).ToLower())
  | FixedByte value -> FixedValue (qName "byte", value.ToString())
  | FixedString value -> FixedValue (qName "string", value)
  | FixedInt value -> FixedValue <| (qName "int", value.ToString())
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
      SimpleTypeWithFacets <| fromFacets (qName "int") [(minFacet :> XmlSchemaFacet); (maxFacet :> _)]
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

let private setOccurrence occurs (particle:XmlSchemaParticle) =
  let (minOccursString, maxOccursString) = fromOccurrence occurs

  minOccursString |> Option.iter (fun n -> particle.MinOccursString <- n)
  maxOccursString |> Option.iter (fun n -> particle.MaxOccursString <- n)

let private setOccurrenceForAttr occurs (attr:XmlSchemaAttribute) =
  match occurs with
  | AttributeOccurrence.Required -> attr.Use <- XmlSchemaUse.Required
  | AttributeOccurrence.Optional -> attr.Use <- XmlSchemaUse.Optional
  
let rec private fromComplexType { Particle = particle; Occurrence = occurs; Nodes = nodes } = 
  let cType (particle:XmlSchemaGroupBase) =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    setOccurrence occurs particle
    for node in nodes do
      match node with
      | Element _
      | NodeGeneratorInvoke _ -> particle.Items.Add(fromNode node) |> ignore
      | Attribute _ -> cType.Attributes.Add(fromNode node) |> ignore
    cType

  match particle with
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

and choiceElementGeneratorInvoker ({ Occurrence = occurs; Nodes = nodes } : NodeGeneratorInvoke) =
  let choice = XmlSchemaChoice()
  setOccurrence occurs choice
  for node in nodes do
    match node with
    | Element _
    | NodeGeneratorInvoke _ -> choice.Items.Add(fromNode node) |> ignore
    | Attribute _ -> failwith "Choiceに属性を含めることはできません。"
  choice :> XmlSchemaObject

and anyElementGeneratorInvoker ({ Occurrence = occurs } : NodeGeneratorInvoke) =
  let any = XmlSchemaAny()
  setOccurrence occurs any
  any :> XmlSchemaObject

and fromNodeGeneratorInvoke invoke = 
  let builtinNodeGenerators = [ 
    choiceElementGeneratorSignature, choiceElementGeneratorInvoker
    anyElementGeneratorSignature, anyElementGeneratorInvoker
  ]

  match lookupElementGenerator builtinNodeGenerators invoke with
  | Some invoker -> invoker invoke
  | _ -> failwith "未定義のNodeGeneratorが指定されました。"

and fromNode node = 
  match node with
  | Element element -> fromElement element :> XmlSchemaObject
  | Attribute attr -> fromAttribute attr :> _
  | NodeGeneratorInvoke nodeGeneratorInvoke -> fromNodeGeneratorInvoke nodeGeneratorInvoke

let fromRoot element = 
  let schema = XmlSchema()
  let root = fromElement element
  schema.Items.Add(root) |> ignore
  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema

let fromSchema { TargetNamespace = targetNs; Definitions = defs } = 
  let schema = XmlSchema()
  targetNs |> Option.iter (fun ns -> schema.TargetNamespace <- ns)

  let defToItem = function
  | Root elm ->
      let root = fromElement elm
      schema.Items.Add(root) |> ignore

  defs |> List.iter defToItem

  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema
module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef
open XsdInternal
open XsdBuildinNodeGenerators

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

and fromNodeGeneratorInvoke invoke = 
  let builtinNodeGenerators = builtinNodeGenerators fromNode

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

let fromSchema { Xmlns = xmlns; Definitions = defs } = 
  let schema = XmlSchema()
  xmlns |> Option.iter (fun ns -> schema.TargetNamespace <- ns; schema.ElementFormDefault <- XmlSchemaForm.Qualified)

  let defToItem = function
  | Root elm ->
      let root = fromElement elm
      schema.Items.Add(root) |> ignore

  defs |> List.iter defToItem

  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema
module Xsd

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef
open XsdInternal
open XsdBuildinNodeGenerators

let rec private fromComplexType ns { Particle = particle; Occurrence = occurs; Nodes = nodes } = 
  let cType (particle:XmlSchemaGroupBase) =
    let cType = XmlSchemaComplexType()
    cType.Particle <- particle
    setOccurrence occurs particle
    for node in nodes do
      match node with
      | Element _
      | NodeGeneratorInvoke _ -> particle.Items.Add(fromNode ns node) |> ignore
      | Attribute _ -> cType.Attributes.Add(fromNode ns node) |> ignore
    cType

  match particle with
  | Sequence -> cType (XmlSchemaSequence())
  | Choice -> cType (XmlSchemaChoice())
  | All -> cType (XmlSchemaAll())

and fromElement ns ({ Name = name; Occurrence = occurs; Type = eType; Comment = comm } : Element) = 
  let result = XmlSchemaElement()
  result.Name <- name
  setOccurrence occurs result
  comm |> Option.iter(fun comm -> setDoc comm result) 
  
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
      result.SchemaType <- fromComplexType ns cType

  result

and fromAttribute ({ Name = name; Occurrence = occurs; Type = sType; Comment = comm } : Attribute) =
  let result = XmlSchemaAttribute()
  result.Name <- name
  setOccurrenceForAttr occurs result 
  setSimpleType sType result
  comm |> Option.iter(fun comm -> setDoc comm result) 
  result

and fromNodeGeneratorInvoke ns invoke = 
  let builtinNodeGenerators = builtinNodeGenerators ns (fromNode ns)

  match lookupElementGenerator builtinNodeGenerators invoke with
  | Some invoker -> 
      let result = invoker invoke
      invoke.Comment |> Option.iter(fun comm -> setDoc comm (result :?> XmlSchemaAnnotated)) 
      result
  | _ -> 
      failwith "未定義のNodeGeneratorが指定されました。"

and fromNode ns node = 
  match node with
  | Element element -> fromElement ns element :> XmlSchemaObject
  | Attribute attr -> fromAttribute attr :> _
  | NodeGeneratorInvoke nodeGeneratorInvoke -> fromNodeGeneratorInvoke ns nodeGeneratorInvoke

let fromRoot ns element = 
  let schema = XmlSchema()
  let root = fromElement ns element
  schema.Items.Add(root) |> ignore
  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema

let fromSchema { Nodes = nodes } = 
  let schema = XmlSchema()
  let nodeF = 
    function
    | Element elm ->
        let root = fromElement schema.TargetNamespace elm
        schema.Items.Add(root) |> ignore
    | Attribute { Name = "xmlns"; Type = (FixedString ns)} -> 
        schema.TargetNamespace <- ns
        schema.Namespaces.Add("", schema.TargetNamespace)
        schema.ElementFormDefault <- XmlSchemaForm.Qualified
    | NodeGeneratorInvoke ({Name = "include"} as invoke) ->
        let invoke = fromNodeGeneratorInvoke schema.TargetNamespace invoke
        schema.Includes.Add(invoke) |> ignore
    | unsupported -> failwithf "このノードは、このスキーマ階層ではサポートされません。:%A" unsupported

  nodes |> List.iter nodeF

  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()
  schema
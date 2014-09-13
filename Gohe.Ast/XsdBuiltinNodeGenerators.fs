module XsdBuildinNodeGenerators

open System.Xml
open System.Xml.Schema
open FParsec

open Xdef
open XsdInternal

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

let includeGeneratorSignature = { 
  Name = "Include"
  ParameterCount = 1
  HasOccurrence = false
  HasChildren = false
}


let refGeneratorSignature = { 
  Name = "Ref"
  ParameterCount = 1
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

let choiceElementGeneratorInvoker  (nodeTrans : Node -> XmlSchemaObject) ({ Occurrence = occurs; Nodes = nodes } : NodeGeneratorInvoke) =
  let choice = XmlSchemaChoice()
  setOccurrence occurs choice
  for node in nodes do
    match node with
    | Element _
    | NodeGeneratorInvoke _ -> choice.Items.Add(nodeTrans node) |> ignore
    | Attribute _ -> failwith "Choiceに属性を含めることはできません。"
  choice :> XmlSchemaObject

let anyElementGeneratorInvoker ({ Occurrence = occurs } : NodeGeneratorInvoke) =
  let any = XmlSchemaAny()
  setOccurrence occurs any
  any :> XmlSchemaObject

let includeGeneratorInvoker ({ Parameters = [ FixedString schemaLocation ] } : NodeGeneratorInvoke) =
  let schemaInclude = XmlSchemaInclude()
  schemaInclude.SchemaLocation <- schemaLocation
  schemaInclude :> XmlSchemaObject

let refGeneratorInvoker ns ({ Occurrence = occurs; Parameters = [ FixedString name ] } : NodeGeneratorInvoke) =
  let refElement = XmlSchemaElement()
  refElement.RefName <- XmlQualifiedName(name, ns)
  setOccurrence occurs refElement
  refElement :> XmlSchemaObject

let builtinNodeGenerators (ns : string) (nodeTrans : Node -> XmlSchemaObject) = [ 
  choiceElementGeneratorSignature, (choiceElementGeneratorInvoker nodeTrans)
  anyElementGeneratorSignature, anyElementGeneratorInvoker
  includeGeneratorSignature, includeGeneratorInvoker
  refGeneratorSignature, refGeneratorInvoker ns
]
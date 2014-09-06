open System
open System.Xml
open System.Xml.Schema
open FParsec

let outputXsd (schema:XmlSchema) =
  let schemaSet = XmlSchemaSet()
  schemaSet.Add(schema) |> ignore
  schemaSet.Compile()

  for (s:obj) in schemaSet.Schemas() do
    let s = s :?> XmlSchema
    
    let nsmgr = XmlNamespaceManager(NameTable())
    nsmgr.AddNamespace("xs", "http://www.w3.org/2001/XMLSchema")
    s.Write(Console.Out, nsmgr)

[<EntryPoint>]
let main argv = 
  match Console.In.ReadToEnd() |> Xdef.parse with
  | Success (r, s, p) -> 
      let node = Xsd.fromRoot r
      let xsd = XmlSchema()
      xsd.Items.Add(node) |> ignore
      outputXsd xsd
      0
  | Failure (msg, err, s) -> 
      eprintfn "%s" msg
      -1
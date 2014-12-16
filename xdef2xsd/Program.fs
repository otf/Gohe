open System
open System.IO
open System.Text
open System.Xml
open System.Xml.Schema
open FParsec
open Nessos.UnionArgParser

let outputXsd (writer:TextWriter) (schema:XmlSchema) =
  let nsmgr = XmlNamespaceManager(NameTable())
  if schema.TargetNamespace <> null then
    nsmgr.AddNamespace("", schema.TargetNamespace)
  nsmgr.AddNamespace("xs", "http://www.w3.org/2001/XMLSchema")
  schema.Write(writer, nsmgr)

type Arguments =
  | [<First; AltCommandLine("-i")>]  Input of string
  | [<AltCommandLine("-o")>] Output of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Input _ -> "specify a input file."
      | Output _ -> "specify a output file."

let parser = UnionArgParser.Create<Arguments>()

[<EntryPoint>]
let main argv = 
  let args =
    try 
      parser.Parse(argv) |> Some
    with _ ->
      None

  match args with
  | Some args ->
      let input =
        match args.TryGetResult (<@ Input @>) with
        | Some inputPath -> File.ReadAllText(inputPath, Encoding.UTF8)
        | None -> Console.In.ReadToEnd()

      use output =
        match args.TryGetResult (<@ Output @>) with
        | Some outputPath -> new StreamWriter(File.OpenWrite(outputPath), Encoding.UTF8) :> TextWriter
        | None -> Console.Out
      
      match input |> Xdef.parse with
      | Success (r, s, p) -> 
          Xsd.fromSchema r |> outputXsd output
          0
      | Failure (msg, err, s) -> 
          eprintfn "%s" msg
          -1
  | None ->
      printfn "%s" <| parser.Usage()
      -1
module XsdUtility

open System.Xml
open System.Xml.Schema

let inline name (x:^a) = ((^a) : (member Name : string) x)
let inline typeNameOf (x:^a) = ((^a) : (member SchemaTypeName : XmlQualifiedName) x)
let inline typeOf (x:^a) = ((^a) : (member SchemaType : XmlSchemaType) x)
let inline typeOfAsComplex (x:^a) = ((^a) : (member SchemaType : XmlSchemaType) x) :?> XmlSchemaComplexType
let inline fixedValue (x:^a) = ((^a) : (member FixedValue : string) x)
let inline particle (x:^a) = ((^a) : (member Particle : XmlSchemaParticle) x) :?> XmlSchemaGroupBase
let inline items (x:^a) = ((^a) : (member Items : XmlSchemaObjectCollection) (x :?> XmlSchemaGroupBase))
let inline at (index:int) (x:XmlSchemaObject) = (((^a) : (member Items : XmlSchemaObjectCollection) (x :?> XmlSchemaGroupBase))).Item(index)
let atOfSchema (index:int) (x:XmlSchema) = (((^a) : (member Items : XmlSchemaObjectCollection) x)).Item(index)
let inline extAttrs (x:XmlSchemaType) = 
  let contentModel = (x:?>XmlSchemaComplexType).ContentModel :?> XmlSchemaSimpleContent
  let ext = contentModel.Content :?> XmlSchemaSimpleContentExtension
  [ for attr in ext.Attributes -> attr :?> XmlSchemaAttribute ]  

let targetNamespace (x:XmlSchema) = x.TargetNamespace

let minOccurs (x:XmlSchemaObject) = (x:?> XmlSchemaParticle).MinOccurs
let maxOccurs (x:XmlSchemaObject) = 
  if (x :?> XmlSchemaParticle).MaxOccursString = "unbounded" then None
  else (x :?>XmlSchemaParticle).MaxOccurs |> int |> Some
let inline useOfAttr (x:XmlSchemaAttribute) = x.Use
let getElm (x : XmlSchema) = 
  [ for (:? XmlSchemaElement as elm) in x.Elements.Values do yield elm ] |> List.head
let asElm (x : XmlSchemaObject) = x :?> XmlSchemaElement
let asAttr (x : XmlSchemaObject) = x :?> XmlSchemaAttribute
let enumString (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  [
    for facet in facets do
    let facet = facet :?> XmlSchemaEnumerationFacet
    yield facet.Value
  ]
let fixedLengthString (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  let facet = facets.[0] :?> XmlSchemaLengthFacet
  facet.Value |> int
let varLengthString (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  let minFacet = facets.[0] :?> XmlSchemaMinLengthFacet
  let maxFacet = facets.[1] :?> XmlSchemaMaxLengthFacet
  (minFacet.Value |> int, maxFacet.Value |> int)
let minLengthString (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  let minFacet = facets.[0] :?> XmlSchemaMinLengthFacet
  minFacet.Value |> int
let intRange (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  let minFacet = facets.[0] :?> XmlSchemaMinInclusiveFacet
  let maxFacet = facets.[1] :?> XmlSchemaMaxInclusiveFacet
  (minFacet.Value |> int, maxFacet.Value |> int)
let pattern (x: XmlSchemaType) = 
  let facets = (((x:?>XmlSchemaSimpleType).Content) :?> XmlSchemaSimpleTypeRestriction).Facets
  let facet = facets.[0] :?> XmlSchemaPatternFacet
  facet.Value
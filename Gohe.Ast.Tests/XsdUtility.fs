module XsdUtility

open System.Xml
open System.Xml.Schema

let inline name (x:^a) = ((^a) : (member Name : string) x)
let inline typeNameOf (x:^a) = ((^a) : (member SchemaTypeName : XmlQualifiedName) x)
let inline typeOf (x:^a) = ((^a) : (member SchemaType : XmlSchemaType) x)
let inline typeOfAsComplex (x:^a) = ((^a) : (member SchemaType : XmlSchemaType) x) :?> XmlSchemaComplexType
let inline fixedValue (x:^a) = ((^a) : (member FixedValue : string) x)
let inline particle (x:^a) = ((^a) : (member Particle : XmlSchemaParticle) x)
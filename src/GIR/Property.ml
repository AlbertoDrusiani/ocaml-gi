open Arg
open BasicTypes
open Parser
open Type
open Documentation
open Deprecation

type property_flag =
    | PropertyReadable
    | PropertyWritable
    | PropertyConstruct
    | PropertyConstructOnly
    | NotHandled


type property = { 
    propName: string;
    propType: type_ml;
    propFlags: property_flag list;
    propReadNullable: bool option;
    propWriteNullable: bool option;
    propTransfer: transfer;
    propDoc: documentation;
    propDeprecated: deprecation_info option;
    }

(* xml -> string -> property *)
let parseProperty ns aliases el =
  let name = getAttr "name" el in
  let t = parseType el ns aliases in
  let transfer = parseTransfer el in
  let deprecated = parseDeprecation el in
  let readable = optionalAttr "readable" true el parseBool in
  let writable = optionalAttr "writable" false el parseBool in
  let construct = optionalAttr "construct" false el parseBool in
  let constructOnly = optionalAttr "construct-only" false el parseBool in
  let maybeNullable = optionalAttr "nullable" None el (fun x -> Some (parseBool x)) in
  let flags = (if readable then [PropertyReadable] else [])
              @ (if writable then [PropertyWritable] else [])
              @ (if construct then [PropertyConstruct] else [])
              @ (if constructOnly then [PropertyConstructOnly] else [])
  in let doc = parseDocumentation el in
  { propName = name;
    propType = t;
    propFlags = flags;
    propTransfer = transfer;
    propDeprecated = deprecated;
    propDoc = doc;
    propReadNullable = maybeNullable;
    propWriteNullable = maybeNullable;
  }



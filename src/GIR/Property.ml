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
let parseProperty el ns =
  let name = getAttr "name" el in
  let t = parseType el ns in
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






(*module GI = GObject_introspection
module B = Bindings

open BasicTypes

type property_flag =
    | PropertyReadable
    | PropertyWritable
    | PropertyConstruct
    | PropertyConstructOnly
    | NotHandled


type property = { 
    propName: string option;
    propType: type_ml option;
    propFlags: property_flag list;
   (* propReadNullable: bool option;
    propWriteNullable: bool option;*)
    propTransfer: transfer;
   (* propDoc: documentation;*)
    propDeprecated: bool; (*deprecation_info option in haskell*)
    }


(*passo una Bindings.GParam.flags list*)
let rec parsePropertyFlag a =
    match a with
    | [] -> []
    | x::xs -> 
       let parsed (f : B.GParam.flags) = 
            match f with
            | Readable -> PropertyReadable
            | Writable -> PropertyWritable
            | Construct -> PropertyConstruct;
            | Construct_only -> PropertyConstructOnly
            | _ -> NotHandled
        in (parsed x) :: (parsePropertyFlag xs)


(*passo una Property_info*)
let parseProperty a =
    prerr_endline("ppppppppp PROPERTY ppppppppp");
    let name =  GI.Property_info.to_baseinfo a |> getOnlyName in
    { 
        propName = name;
        propType = GI.Property_info.get_type a |> cast_to_type_ml;
        propFlags = GI.Property_info.get_flags a |> parsePropertyFlag;
        (* propReadNullable = ;
         propWriteNullable = ;*)
         propTransfer = GI.Property_info.get_ownership_transfer a |> parseTransfer;
         propDeprecated = GI.Property_info.to_baseinfo a |> GI.Base_info.is_deprecated;
    }
       *) 

open Allocation
open Field
open Method
open Parser
open BasicTypes
open Type
open Deprecation
open Documentation

type struct_ml = { 
    structIsBoxed: bool;
    structAllocationInfo: allocation_info;
    structTypeInit: string option;
    structCType: string option;
    structSize: int;
    gtypeStructFor: name option;
    structIsDisguised: bool;
    structForceVisible: bool;
    structFields: field list;
    structMethods: method_ml list;
    structDeprecated: deprecation_info option;
    structDocumentation: documentation;
}


let parseStruct ns aliases el =
  prerr_endline ("___Inizio il parse Struct___");
  let name = parseName ns el in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let structFor = 
    match queryAttrWithNamespace GLibGIRNS "is-gtype-struct-for" el with
    | Some t -> Some (qualifyName t ns)
    | None -> None
  in let typeInit = queryAttrWithNamespace GLibGIRNS "get-type" el in
  let maybeCType = queryCType el in
  let disguised = optionalAttr "disguised" false el parseBool in
  (*TODO valore settato nei file di overrides, lasciamo così fino a che non arriviamo quel punto*)
  let forceVisibile = optionalAttr "haskell-gi-force-visible" false el parseBool in
  let fields = parseFields ns aliases el in
  let constructors = List.map (parseMethod ns aliases Constructor) (parseChildrenWithLocalName "constructor" el) in
  let methods = List.map (parseMethod ns aliases OrdinaryMethod) (parseChildrenWithLocalName "method" el) in
  let functions = List.map (parseMethod ns aliases MemberFunction) (parseChildrenWithLocalName "function" el) in
  name,
  { structIsBoxed = true; (*TODO solita bomba, per ora dummy value*)
    structAllocationInfo = unknownAllocationInfo;
    structTypeInit = typeInit;
    structCType = maybeCType;
    structSize = 0; (*TODO bomba, metto dummy value*)
    gtypeStructFor = structFor;
    structIsDisguised = disguised;
    structForceVisible = forceVisibile;
    structFields = fields;
    structMethods = constructors @ methods @ functions;
    structDeprecated = deprecated;
    structDocumentation = doc;
  }

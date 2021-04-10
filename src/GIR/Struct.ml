open Allocation
open Field
(*open Method*)
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
   (* structMethods: method_ml list;*)
    structDeprecated: deprecation_info option;
    structDocumentation: documentation;
}


let parseStruct el ns =
  let name = parseName el ns in
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
  let fields = parseFields el ns in
  (*TODO solito problema della parseChildrenWithLocalName
  let constructors =
  let methods =
  let functions =
    *)
  name,
  { structIsBoxed = true; (*TODO qua mette un error, non ho capito cosa serva*)
    structAllocationInfo = unknownAllocationInfo;
    structTypeInit = typeInit;
    structCType = maybeCType;
    structSize = 0; (*TODO anche qua mette un error*)
    gtypeStructFor = structFor;
    structIsDisguised = disguised;
    structForceVisible = forceVisibile;
    structFields = fields;
    (*structMethods = constructor @ methods @ functions;*)
    structDeprecated = deprecated;
    structDocumentation = doc;
  }

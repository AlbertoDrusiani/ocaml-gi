open Allocation
open Field
open Method
open Parser
open Type
open Deprecation
open Documentation


type union = {
    unionIsBoxed: bool;
    unionAllocationInfo: allocation_info;
    unionDocumentation: documentation;
    unionSize: int;
    unionTypeInit: string option;
    unionFields: field list;
    unionMethods: method_ml list;
    unionCType: string option;
    unionDeprecated: deprecation_info option;
}

let parseUnion el ns =
  let name = parseName ns el in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let typeInit = queryAttrWithNamespace GLibGIRNS "get-type" el in
  let isBoxed = 
    match typeInit with
    | Some _ -> true
    | None -> false

  in let fields = parseFields el ns in
  let constructors = List.map (parseMethod ns Constructor) (parseChildrenWithLocalName "constructor" el) in 
  let methods = List.map (parseMethod ns OrdinaryMethod) (parseChildrenWithLocalName "method" el) in
  let functions = List.map (parseMethod ns MemberFunction) (parseChildrenWithLocalName "function" el) in
  let ctype = queryCType el in
  name,
  { unionIsBoxed = isBoxed;
    unionAllocationInfo = unknownAllocationInfo;
    unionDocumentation = doc;
    unionTypeInit = typeInit;
    unionSize = 0; (*TODO bomba, dummy value*)
    unionFields = fields;
    unionMethods = constructors @ methods @ functions;
    unionCType = ctype;
    unionDeprecated = deprecated;
    }

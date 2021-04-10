open Allocation
open Field
(*open Method
open BasicTypes*)
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
   (* unionMethods: method_ml list;*)
    unionCType: string option;
    unionDeprecated: deprecation_info option;
}

let parseUnion el ns =
  let name = parseName el ns in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let typeInit = queryAttrWithNamespace GLibGIRNS "get-type" el in
  let isBoxed = 
    match typeInit with
    | Some _ -> true
    | None -> false

  in let fields = parseFields el ns in
  (*let constructors =
  let methods =
  let functions = *)
  let ctype = queryCType el in
  name,
  { unionIsBoxed = isBoxed;
    unionAllocationInfo = unknownAllocationInfo;
    unionDocumentation = doc;
    unionTypeInit = typeInit;
    unionSize = 0; (*TODO mette error*)
    unionFields = fields;
   (* unionMethods = constructors @ methods @ functions;*)
    unionCType = ctype;
    unionDeprecated = deprecated;
    }

open Arg
open BasicTypes
open Parser
open Type
open Documentation
open Deprecation

type callable ={ 
    returnType: type_ml option;
    returnMayBeNull: bool;
    returnTransfer: transfer;
    returnDocumentation: documentation;
    args: arg list;
    skipReturn: bool;
    callableThrows: bool;
    callableDeprecated: deprecation_info option;
    callableDocumentation: documentation;
    callableResolvable: bool option;
    }


let parseArgs ns aliases el =
  let parseParameters = parseChildrenWithLocalName "parameters" el in (*lista di parameters  [parameters]*)
  let parseParameter = List.map (parseChildrenWithLocalName "parameter") parseParameters in  (*[[parameter, parameter,..]]*)
  match parseParameter with
  | [] ->  []
  | [[]] -> []
  | [ps] -> (*prerr_endline ("LA LISTA È LUNGA: " ^ string_of_int(List.length ps));*)List.map (parseArg ns aliases) ps
  | _ -> assert false

(* xml -> string -> (type_ml option * bool * transfer * bool * documentation) *)
let parseOneReturn ns aliases el =
  (*prerr_endline (Xml.tag el);*)
  let returnType = parseOptionalType el ns aliases in
  let allowNone = optionalAttr "allow-none" false el parseBool in
  let nullable = optionalAttr "nullable" false el parseBool in
 (* prerr_endline ("SONO QUIIIIIIIII");*)
  let transfer = parseTransfer el in
  let doc = parseDocumentation el in
  let skip = optionalAttr "skip" false el parseBool in
  returnType, allowNone || nullable, transfer, skip, doc


(* xml -> string -> (type_ml option * bool * transfer * bool * documentation) *)
let parseReturn el ns aliases =
  let returnSets = List.map (parseOneReturn ns aliases) (parseChildrenWithLocalName "return-value" el) in
  match returnSets with
  | r::[] -> r
  | [] -> assert false
  | _ -> assert false


(*xml -> string -> callable *)
let parseCallable ns aliases el =
 (* prerr_endline ("Inizio il parse Callable");*)
  let args = parseArgs ns aliases el in
  (*prerr_endline ("Finito il parseArgs");*)
  let returnType, mayBeNull, transfer, skip, returnDoc = parseReturn el ns aliases in
  (*prerr_endline ("FINITO IL RETURN TYPE");*)
  let deprecated = parseDeprecation el in
  let docs = parseDocumentation el in
  let throws = optionalAttr "throws" false el parseBool in
 (* prerr_endline ("Finito il parse Callable");*)
  { returnType = returnType;
    returnMayBeNull = mayBeNull;
    returnTransfer = transfer;
    returnDocumentation = returnDoc;
    args = args;
    skipReturn = skip;
    callableThrows = throws;
    callableDeprecated = deprecated;
    callableDocumentation = docs;
    callableResolvable = None;
  }



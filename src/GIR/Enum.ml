open Parser
open Type
open Documentation
open Deprecation
(*open Foreign
open Ctypes
open Utils*)
open Int64

type enumeration_member = { 
    enumMemberName: string;
    enumMemberValue: int64; (*TODO è da mettere int64 ma da capire come funzia *)
    enumMemberCId: string;
    enumMemberDoc: documentation;
    }


type enumeration = { 
    enumMembers: enumeration_member list;
    enumErrorDomain: string option;
    enumTypeInit: string option;
    enumDocumentation: documentation;
    enumCType: string;
    enumStorageBytes: int;
    enumDeprecated: deprecation_info option;
    }


let parseEnumMember el =
  let name = getAttr "name" el in
  let value = getAttr "value" el |> parseIntegral |> of_int in
  let cid = getAttrWithNamespace CGIRNS "identifier" el in
  let doc = parseDocumentation el in
  { enumMemberName = name;
    enumMemberValue = value;
    enumMemberCId = cid;
    enumMemberDoc = doc;
  }


(*let get_storage_bytes =
  foreign "_gi_get_enum_storage_bytes" (int64_t @-> int64_t @-> returning Ctypes.int)

(* int list -> int *)
let extractEnumStorageBytes values = 
  get_storage_bytes (list_min values max_int) (list_max values min_int)*)

(* xml -> string -> enum*)
let parseEnum ns el =
  prerr_endline ("Inizio il parse Enum");
  let name = parseName ns el in
  let ctype = parseCType el in
  let doc = parseDocumentation el in
  let deprecated = parseDeprecation el in
  let errorDomain = queryAttrWithNamespace GLibGIRNS "error-domain" el in
  let typeInit = queryAttrWithNamespace GLibGIRNS "get_type" el in
  let members = List.map parseEnumMember (parseChildrenWithLocalName "member" el) in
  name, 
  { enumMembers = members;
    enumErrorDomain = errorDomain;
    enumDocumentation = doc;
    enumTypeInit = typeInit;
    enumCType = ctype;
    enumStorageBytes = 0; (*TODO mi da errore extractEnumStorageBytes (List.map (fun x -> x.enumMemberValue) members);*)
    enumDeprecated = deprecated;
  }


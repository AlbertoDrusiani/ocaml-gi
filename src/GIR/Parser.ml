open BasicTypes
open Deprecation
open Documentation
open XMLUtils



(* xml -> string *)
let elementDescription element =
    match lookupAttr "name" element with
    | Some n -> localName (Xml.tag element) ^ " [" ^ n ^ "]"
    | None -> localName (Xml.tag element)


let nameInCurrentNS ns n =
    { namespace = ns;  name = n;}


(* name -> Map alias type -> type *)
let rec resolveQualifiedTypeName name knownAliases =
    match AliasMap.find_opt (Alias name) knownAliases with
    | Some (TInterface n) -> resolveQualifiedTypeName n knownAliases
    | Some t -> t
    | None -> TInterface name

(* string -> xml -> string *)
let getAttr attr element =
    match lookupAttr attr element with
    | Some v -> v
    | None -> "Errore in getAttr"

(* GIRXMLNamespace -> string -> xml -> string *)
let getAttrWithNamespace ns attr element =
    match lookupAttrWithNamespace ns attr element with
    | Some v -> v
    | None -> "Errore in getAttrWithNamespace"

(* string -> xml -> string option *)
let queryAttr attr element =
    lookupAttr attr element

(* GIRXMLNamespace -> string -> xml -> string option *)
let queryAttrWithNamespace ns attr element =
    lookupAttrWithNamespace ns attr element

(* string -> 'a -> xml -> (string -> 'a) -> 'a *)
let optionalAttr attr def element func =
    match queryAttr attr element with
    | Some a -> func a
    | None -> def

(* string -> string -> BasicTypes.name*)
let qualifyName n ns =
    match String.split_on_char '.' n with
    | [ns; name] -> {namespace = ns; name = name;}
    | [name] -> nameInCurrentNS ns name
    (*| x::xs::[] -> {namespace = x; name = xs;}*)
    | _ -> assert false

(* xml -> string -> BasicTypes.name *)
let parseName ns element =
    qualifyName (getAttr "name" element) ns

(* xml -> _DeprecationInfo option *)
let parseDeprecation element =
    queryDeprecated element  

(* xml -> documentation *)
let parseDocumentation element =
   queryDocumentation element 

(* string -> int *)
let parseIntegral str =
    try int_of_string str
    with Failure _ -> assert false
 
(* string -> bool *)    
let parseBool str =
    match str with
    | "0" -> false
    | "1" -> true
    | _ -> assert false
   

(* string -> xml -> xml list *)
let parseChildrenWithLocalName n element =
    let introspectable e = 
        (not (Option.equal (fun x y -> x = y) (lookupAttr "introspectable" e) (Some "0"))) && ((lookupAttr "shadowed-by" e) = None) in
    (*TODO per debugging *)
    let s = childElemsWithLocalName n element in
    let rec f l =
      match l with
      | [] -> []
      | _:: xs -> f xs
    in let _ = f s in
    List.filter introspectable (childElemsWithLocalName n element)


(* string -> xml -> xml list *)
let parseAllChildrenWithLocalName n element =
    childElemsWithLocalName n element


(* GIRXMLNamespace -> string -> xml list *)    
let parseChildrenWithNSName ns n element =
    let introspectable e = (not(Option.equal (fun x y -> x = y) (lookupAttr "introspectable" e) (Some "0"))) in
    List.filter introspectable (childElemsWithNSName ns n element)




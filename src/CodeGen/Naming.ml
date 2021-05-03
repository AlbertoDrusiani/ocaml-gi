(*open API*)
open Util
open GIR.BasicTypes

(* move leading underscores to the end *)
(* string -> string *)
let rec sanitize s =
    let length = String.length s in
    let first = String.get s 0 in
    match first with
    | '_' -> sanitize (String.sub s 1 (length-1)) ^ "_"
    | _ -> s


let underscoresToHypens =
    Str.global_replace (Str.regexp "[_]") "-"


let hyphensToUnderscores =
    Str.global_replace (Str.regexp "[-]") "_"


let explode s =
  List.init (String.length s) (String.get s)

let is_digit = function '0' .. '9' -> true | _ -> false

(* string -> string *)
let camelCaseToSnakeCase s =
        prerr_endline ("iniziale proprio: " ^ s);
    let f c =
        (*FIXME* c'è un modo migliote per controllare se c è maiuscolo?*)
        match (c = (Char.uppercase_ascii c)) && (c != '_') && (not(is_digit(c))) with
        | true -> "_" ^ String.lowercase_ascii (String.make 1 c)
        | false -> String.make 1 c
     in String.concat "" (List.map f (explode (lcFirst s)))
 

     


(* string -> string list *)
let splitCamelCase t =
    let rec splitCamelCase' l acc = 
        match l, acc with
        | [], [] -> []
        | [], acc -> List.rev acc
        | x::xs, [] -> splitCamelCase' xs [[x]]
        | x::xs, acc when x = (String.uppercase_ascii x) -> splitCamelCase' xs ([x]::acc)
        | x::xs, ac::acs -> splitCamelCase' xs ((ac @ [x])::acs)
    in splitCamelCase' t []


let hyphensToCamelCase s =
    String.split_on_char '-' s |> List.map ucFirst |> String.concat ""

(* turn a name from snake_case to CamelCase, hnadling underscores *)
(* string -> string *)
let underscoresToCamelCase s =
    let normalize n =
        match n with
        | "" -> "_"
        | n -> n
    in String.split_on_char '_' s |> List.map ucFirst |> List.map normalize |> String.concat ""
    
(* turn the given identifier into lower camelCase *)
(* string -> string *)
let lowerSymbol s =
    match underscoresToCamelCase (sanitize s) with
    | "" -> "Errore in lowerSymbol: empty name"
    | n -> lcFirst n


let upperName nm =
  underscoresToCamelCase (sanitize nm.name)


let lowerName n =
    match n with
    | {namespace = _; name = s} -> lowerSymbol s


let span p s =
  let exp = explode s in
  takeWhile p exp, dropWhile p exp


let is_digit = function '0' .. '9' -> true | _ -> false

let escapeOCamlReserved s =
    match s with
    | "unit" -> "unit_"
    | "object" -> "object_"
    | "begin" -> "begin_"
    | "end" -> "end_"
    | "done" -> "done_"
    | "type" -> "type_"
    | "new" -> "new_"
    | "open" -> "open_"
    | "match" -> "match_"
    | "and" -> "and_"
    | "method" -> "method_"
    | t -> 
        let (nums, text) = span is_digit t in
        let text = List.to_seq text |> String.of_seq in
        let nums1 = List.to_seq nums |> String.of_seq in
        text ^ nums1



let ocamlIdentifier n =
    match n with
    |  { namespace = _; name = nm} -> 
     camelCaseToSnakeCase nm |> escapeOCamlReserved


let ocamlIdentifierNs n =
    match n with
    | { namespace = ns; name = nm;} ->   (camelCaseToSnakeCase (ns ^ nm) |> escapeOCamlReserved)


 
let nsOCamlType currNs n =
    match n, currNs with
    | { namespace = ns; name = nm;}, currNs when ns = currNs -> nm ^ "T.t"
    | { namespace = ns; name = nm;}, _ -> "GI" ^ ns ^ "." ^ nm ^ "T.t"


let nsOCamlO currNs n =
    match n, currNs with
    | {namespace = ns; name = nm;}, currNs when ns = currNs -> "#" ^ nm ^ "T." ^ (ocamlIdentifier n) ^ "_o"
    | {namespace = ns; name = nm;}, _ -> "#GI" ^ ns ^ "." ^ nm ^ "T." ^ (ocamlIdentifier n) ^ "_o"

let signalOCamlName s =
    hyphensToUnderscores s |> escapeOCamlReserved


let mlGiPrefix n t =
    match n with
    | {namespace = ns; name = _;} -> "ml_gi" ^ (String.lowercase_ascii ns) ^ "_" ^ t


let cGIPrefix = "GI_"


let enumVal n =
    match n with
    | {namespace = ns; name = nm;} -> cGIPrefix ^ ns ^ nm ^ "_val"


let flagsVal n =
    "Flags_" ^ enumVal n


let optFlagsVal n =
    "Opt" ^ flagsVal n


let valEnum n =
    match n with
    | {namespace = ns; name = nm;} -> cGIPrefix ^ "Val_" ^ ns ^ nm

let interfaceVal n =
    match n with
    | {namespace = ns; name = nm;} -> ns ^ nm ^ "_val"


let valInterface n =
    match n with
    | {namespace = ns; name = nm;} -> "Val_" ^ ns ^ nm


let objectVal = interfaceVal


let valObject = valInterface


let valOptInterface n = "Opt" ^ valInterface n


let valOptObject = valOptInterface


let structVal = interfaceVal


let valStruct = valInterface



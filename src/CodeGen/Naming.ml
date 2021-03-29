(*open API*)
open Util
open GIR.BasicTypes



let rec sanitize s =
    let length = String.length s in
    let first = String.get s 0 in
    match first with
    | '_' -> sanitize (String.sub s 1 (length-1)) ^ "_"
    | _ -> s


let underscoresToHypens =
    Str.global_replace (Str.regexp "_") "-"


let hyphensToUnderscores =
    Str.global_replace (Str.regexp "-") "_"


let camelCaseToSnakeCase s =
    let explode = List.init (String.length s) (String.get s) in
    let f c =
        match Char.uppercase_ascii c with
        | 'c' -> "_" ^ String.lowercase_ascii (String.make 1 c) (*FIXME da cambiare c char con c variabile*)
        | _ -> String.make 1 c
    in let rec g e =
        match e with
        | [] -> []
        | x::xs -> f x :: g xs
    in String.concat "" (g explode)


let splitCamelCase t =
    let rec splitCamelCase' l acc = 
        match l, acc with
        | [], [] -> []
        | [], acc -> List.rev acc
        | x::xs, [] -> splitCamelCase' xs [[x]]
        | x::xs, acc when x == (String.uppercase_ascii x) -> splitCamelCase' xs ([x]::acc)
        | x::xs, ac::acs -> splitCamelCase' xs ((ac @ [x])::acs)
    in splitCamelCase' t []


let hyphensToCamelCase s =
    String.split_on_char '-' s |> List.map ucFirst |> String.concat ""


let underscoresToCamelCase s =
    let normalize n =
        match n with
        | "" -> "_"
        | n -> n
    in String.split_on_char '_' s |> List.map ucFirst |> List.map normalize |> String.concat ""
    

let lowerSymbol s =
    match underscoresToCamelCase (sanitize s) with
    | "" -> "Errore in lowerSymbol: empty name"
    | n -> lcFirst n

 
let lowerName n =
    match n with
    | {namespace = _; name = s} -> 
            match s with
            | Some s -> lowerSymbol s
            | None -> "Errore in lowerName"


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
   (* | _ -> let (nums, text) = *) (*TODO da sistemare con span che non c'è in Ocaml*)
    | _ -> s


let ocamlIdentifier n =
    match n with
    |  { namespace = _; name = nm} -> 
            match nm with
            | Some s -> camelCaseToSnakeCase s |> escapeOCamlReserved
            | None -> "Errore in ocamlIdentifier"


let ocamlIdentifierNs n =
    match n with
    | { namespace = ns; name = nm;} -> 
            match nm with 
            | Some s ->  (camelCaseToSnakeCase (ns ^ s) |> escapeOCamlReserved)
            | None -> "Errore in ocamlIdentifierNs"


 
let nsOCamlType currNs n =
    match n, currNs with
    | { namespace = ns; name = nm;}, currNs when ns == currNs -> 
            begin
            match nm with
            | Some s -> (s ^ "T.t")
            | None -> "Errore in nsOCamlType 1"
            end
    | { namespace = ns; name = nm;}, _ -> 
            begin
            match nm with
            | Some s -> ("GI" ^ ns ^ "." ^ s ^ "T.t")
            | None -> "Errore in nsOCamlType 2"
            end


let nsOCamlO currNs n =
    match n, currNs with
    | {namespace = ns; name = nm;}, currNs when ns == currNs ->
            begin
                match nm with
                | Some s -> "#" ^ s ^ "T." ^ (ocamlIdentifier n) ^ "_o"
                | None -> "Errore in nsOCamlO"
            end
    | {namespace = ns; name = nm;}, _ ->
            begin
                match nm with
                | Some s -> "#GI" ^ ns ^ "." ^ s ^ "T." ^ (ocamlIdentifier n) ^ "_o"
                | None -> "Errore in nsOcamlO"
            end

let signalOcamlName s =
    hyphensToUnderscores s |> escapeOCamlReserved


let mlGiPrefix n t =
    match n with
    | {namespace = ns; name = _;} -> "ml_gi" ^ (String.lowercase_ascii ns) ^ "_" ^ t


let cGIPrefix = "GI_"


let enumVal n =
    match n with
    | {namespace = ns; name = nm;} -> 
        match nm with
        | Some nm -> cGIPrefix ^ ns ^ nm ^ "_val"
        | None -> "Errore in enumVal"


let flagsVal n =
    "Flags_" ^ enumVal n


let optFlagsVal n =
    "Opt" ^ flagsVal n


let valEnum n =
    match n with
    | {namespace = ns; name = nm;} ->
        match nm with
        | Some nm -> cGIPrefix ^ "Val_" ^ ns ^ nm
        | None -> "Errore in valEnum"

let interfaceVal n =
    match n with
    | {namespace = ns; name = nm;} ->
        match nm with
        | Some nm -> ns ^ nm ^ "_val"
        | None -> "Errore in interfaceVal"


let valInterface n =
    match n with
    | {namespace = ns; name = nm;} ->
        match nm with
        | Some nm -> "Val_" ^ ns ^ nm
        | None -> "Errore in valInterface"


let objectVal = interfaceVal


let valObject = valInterface


let valOptInterface n = "Opt" ^ valInterface n


let valOptObject = valOptInterface


let structVal = interfaceVal


let valStruct = valInterface



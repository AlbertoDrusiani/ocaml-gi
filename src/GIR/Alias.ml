open XMLUtils
open BasicTypes
open Type
open Parser

(*implementata in quanto forse in OCaml non c'è out-of-the-box*)
(* 'a -> 'a option -> 'a*)
let fromMaybe def a =
  match a with
  | Some a -> a
  | None -> def

(* string -> xml -> (string, type) *)
let parseAlias ns aliases el =
  let name = getAttr "name" el in
  let t = parseOptionalType el ns aliases in
  name, fromMaybe (TBasicType TPtr) t

(* string -> xml -> (string, type) list *)
let parseAliases ns aliases el =
  List.map (parseAlias ns aliases) (parseChildrenWithLocalName "alias" el)

(* string -> xml -> (alias, type) list *)
let namespaceListAliases ns alias el =
  match lookupAttr "name" el with
  | None -> assert false
  | Some nsName -> 
    let addNS (n, t) = (Alias ({namespace = nsName; name = n}), t) in
    let aliases = parseAliases ns alias el in
    (*TODO la riga sotto è commentata per semplificarmi la vita a fare la union del todo dopo*)
    (List.map addNS aliases) (*|> List.to_seq |> AliasMap.of_seq*)

(* string -> Map alias type  xml -> Map alias type*)
let documentListAliases ns aliases doc =
  let namespaces = childElemsWithLocalName "namespace" doc in
  let l = List.flatten (List.map (namespaceListAliases ns aliases) namespaces) in
  (*TODO la union/merge/fold di Map può essermi utile al posto di sta roba?*)
  let rec union acc l =
    match l with
    | [] -> acc
    | x::xs -> 
      match x with
      | (n, t) -> if AliasMap.mem n acc then union acc xs else union (AliasMap.add n t acc) xs 
   in union AliasMap.empty l

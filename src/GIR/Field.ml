open BasicTypes
open Callback
open Type
open Parser
open Documentation
open Deprecation


type field_info_flag =
    | FieldIsReadable
    | FieldIsWritable

type field =
    { fieldName: string;
      fieldVisible: bool;
      fieldType: type_ml;
      fieldIsPointer: bool option;
      fieldCallback: callback option;
      fieldOffset: int;
      fieldFlags: field_info_flag list;
      fieldDocumentation: documentation;
      fieldDeprecated: deprecation_info option;
    }


let parseField ns aliases el =
  prerr_endline ("Inzio il parse Field");
  let name = getAttr "name" el in
  let deprecated = parseDeprecation el in
  let readable = optionalAttr "readable" true el parseBool in
  let writable = optionalAttr "writable" false el parseBool in
  let flags = (if readable then [FieldIsReadable] else [])
              @ (if writable then [FieldIsWritable] else [])
  in let introspectable = optionalAttr "introspectable" true el parseBool in
  let private_field = optionalAttr "private" false el parseBool in
  let doc = parseDocumentation el in
  (*TODO skippata la parte del controllo introspectable, flip e company*)
  let t, isPtr, callback = 
    if introspectable then
      let callbacks = List.map (parseCallback ns aliases) (parseChildrenWithLocalName "callback" el) in
      let cbn, callback =
        match callbacks with
        | [] -> None, None
        | [(n, cb)] -> Some n, Some cb
        | _ -> assert false
      in let t, isPtr =
        match cbn with
        | None ->
          let t = parseType el ns aliases in
          let ct = queryElementCType el in
          let is = match ct with
                   | Some c -> (c.[(String.length c)-1] = '*')
                   | None -> false
          in t, (Some is)
        | Some n -> TInterface n, None
      in t, isPtr, callback
    else
      let callbacks = List.map (parseName ns) (parseAllChildrenWithLocalName "callback" el) in
      match callbacks with
      | [] -> 
        let t = parseType el ns aliases in
        let ct = queryElementCType el in
        let is = match ct with
                 | Some c -> (c.[(String.length c)-1] = '*')
                 | None -> false
        in t, Some (is), None
      | [n] -> TInterface n, Some true, None
      | _ -> assert false
      in
  Some { fieldName = name;
         fieldVisible = introspectable && not private_field;
         fieldType = t;
         fieldIsPointer = if Option.is_some callback then Some true else isPtr;
         fieldCallback = callback;
         fieldOffset = 0; (*TODO qua in haskell c'è un error (una bomba), per ora metto un valore dummy*)
         fieldFlags = flags;
         fieldDocumentation = doc;
         fieldDeprecated = deprecated;
       }

let parseFields ns aliases el =
  List.filter_map (fun x -> x) (List.map (parseField ns aliases) (parseChildrenWithLocalName "field" el))


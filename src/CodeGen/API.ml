(*open GIR.Allocation
open GIR.Arg*)
open GIR.BasicTypes
(*open GIR.Callable*)
open GIR.Callback
open GIR.Constant
(*open GIR.Deprecation*)
open GIR.Enum
open GIR.Field
open GIR.Flags
open GIR.Function
open GIR.Interface
(*open GIR.Method*)
open GIR.Object
(*open GIR.Parser
open GIR.Property*)
open GIR.Repository
(*open GIR.Signal*)
open GIR.Struct
open GIR.Union
open GIR.XMLUtils
open GIR.Alias
(*open Foreign*)
open LibGIRepository

type api =
  | APIConst of constant
  | APIFunction of function_ml
  | APICallback of callback
  | APIEnum of enumeration
  | APIFlags of flags
  | APIInterface of interface
  | APIObject of object_ml
  | APIStruct of struct_ml
  | APIUnion of union

type gir_info = {
    girPCPackages: string list;
    girNSName: string;
    girNSVersion: string;
    girAPIs: (GIR.BasicTypes.name*api) list;
    girCTypes: GIR.BasicTypes.name StringMap.t;
}

type gir_namespace = {
    nsName: string;
    nsVersion: string;
    nsAPIs: (GIR.BasicTypes.name*api) list;
    nsCTypes: (string*GIR.BasicTypes.name) list;
}

type gir_info_parse = {
    girIPPackage: string option list;
    girIPIncludes: (string*string) option list;
    girIPNamespaces: gir_namespace option list;
}

type gir_path =
  gir_node_spec list

and gir_node_spec =
  | GIRNamed of gir_name_tag
  | GIRType of string
  | GIRTypedName of string*gir_name_tag

and gir_name_tag =
  | GIRPlainName of string
  | GIRRegex of string

and gir_rule =
  | GIRSetAttr of (gir_path*GIR.XMLUtils.name)*string
  | GIRAddNode of gir_path*GIR.XMLUtils.name
  | GIRDeleteNode of gir_path

(* map alias type -> gir_namespace -> xml -> gir_namespace *)
let parseNSElement aliases ns element =
  prerr_endline ("################ NSElement: " ^ Xml.tag element ^ " con name = " ^ Xml.attrib element "name");
  let parse name api =
    let maybeCType = match lookupAttrWithNamespace CGIRNS "type" element with
                     | Some ctype -> (ctype, name) :: ns.nsCTypes
                     | None -> ns.nsCTypes
    in {ns with nsAPIs = (name, api)::ns.nsAPIs; nsCTypes = maybeCType} in 
  match lookupAttr "introspectable" element with
  | Some "0" -> prerr_endline ("Elemento da NON parsare"); ns
  | _ -> 
    prerr_endline ("_______PARSE " ^ (localName (Xml.tag element)) ^ "_______");
    match localName (Xml.tag element) with
    | "alias" -> ns
    | "constant" -> begin
                     match parseConstant ns.nsName aliases element with
                        | (name, api) -> parse name (APIConst api)
                     end
    | "enumeration" -> begin
                     match parseEnum ns.nsName element with
                        | (name, api) -> parse name (APIEnum api)
                     end
    | "bitfield" -> begin
                     match parseFlags ns.nsName element with
                        | (name, api) -> parse name (APIFlags api)
                     end
    | "function" -> begin
                     match parseFunction ns.nsName aliases element with
                        | (name, api) -> parse name (APIFunction api)
                     end
    | "callback" -> begin
                     match parseCallback ns.nsName aliases element with
                        | (name, api) -> parse name (APICallback api)
                     end
    | "record" -> begin
                     match parseStruct ns.nsName aliases element with
                        | (name, api) -> parse name (APIStruct api)
                     end
    | "union" -> begin
                     match parseUnion ns.nsName aliases element with
                        | (name, api) -> parse name (APIUnion api)
                     end
    | "class" -> begin
                     match parseObject ns.nsName aliases element with
                        | (name, api) -> parse name (APIObject api)
                     end
    | "interface" -> begin
                     match parseInterface ns.nsName aliases element with
                        | (name, api) -> parse name (APIInterface api)
                     end
    | "boxed" -> ns
    | _ -> assert false


(* map alias type -> xml -> gir_namespace*)
let parseNamespace aliases element =
  let name = lookupAttr "name" element in
  let version = lookupAttr "version" element in
  if Option.is_none name || Option.is_none version 
  then None
  else
   let _ =  prerr_endline ("Inizio il parsing del namespace: " ^ Option.get name) in
   let ns = { nsName = Option.get name;
             nsVersion = Option.get version;
             nsAPIs = [];
             nsCTypes = [];
           }
   in Some (List.fold_left (parseNSElement aliases) ns (subelements element))

(* xml ->  string*string option *)
let parseInclude element =
  let name = lookupAttr "name" element in
  let version = lookupAttr "version" element in
  if Option.is_none name || Option.is_none version
  then None
  else Some (Option.get name, Option.get version)

(* xml -> string option *)
let parsePackage element =
   lookupAttr "name" element

(* Map alias type -> gir_info_parse -> xml -> gir_info_parse *)
let parseRootElement aliases info element =
  prerr_endline ("Inizio il parseRootElement: " ^ (Xml.tag element));
  match localName (Xml.tag element) with
  | "include" -> {info with girIPIncludes = parseInclude element :: info.girIPIncludes;}
  | "package" -> {info with girIPPackage = parsePackage element :: info.girIPPackage;}
  | "namespace" -> {info with girIPNamespaces = parseNamespace aliases element :: info.girIPNamespaces;}
  | _ -> info

(* gir_info_parse *)
let emptyGIRInfoParse =
  {girIPPackage = []; girIPIncludes = []; girIPNamespaces = [] }

(* Map alias type -> xml -> gir_info_parse *)
let parseGIRDocument aliases doc =
  List.fold_left (parseRootElement aliases) emptyGIRInfoParse (subelements doc)

(* xml -> Set (string string) *)
let documentListIncludes doc =
  let includes = childElemsWithLocalName "include" doc in
  List.filter_map parseInclude includes |> List.to_seq |> StringStringSet.of_seq

(* gir_name_tag -> Map XMLUtils.name string -> XMLUtils.name -> bool *)
let lookupAndMatch tag attrs attr =
  match XMLNameMap.find_opt attr attrs with
  | Some s ->
    begin
     match tag with
     | GIRPlainName pn -> s = pn
     | GIRRegex r -> s = r (*TODO qua haskell usa ~=, roba per regex, da capire la semantica*)
    end
  | None -> false

(* GIRNodeSpec -> xml -> bool *)
let specMatch gn el =
  match gn, el with
  | GIRType t, e -> localName (Xml.tag e) = t
  | GIRNamed name, e -> 
    lookupAndMatch name (List.map attribute_to_name_map (Xml.attribs e) |> List.to_seq |> XMLNameMap.of_seq) (xmlLocalName "name")
  | GIRTypedName (t, name), e -> 
    (localName (Xml.tag e) = t) && (lookupAndMatch name (List.map attribute_to_name_map (Xml.attribs e) |> List.to_seq |> XMLNameMap.of_seq) (xmlLocalName "name"))

(*gir_path*Xml.name -> string -> xml -> xml *)
let rec girSetAttr gpn newVal n =
  match gpn, newVal, n with
  | (spec::rest, attr), newVal, n ->
    if specMatch spec n
    then
      match rest with
      | [] -> Xml.Element (Xml.tag n, (name_to_string attr, newVal)::(Xml.attribs n), Xml.children n)
      | _ -> Xml.Element (Xml.tag n, Xml.attribs n, List.map (girSetAttr (rest, attr) newVal) (Xml.children n))
    else n
  | _, _, n -> n

(* gir_path -> Xml.name -> xml -> xml *)
let rec girAddNode gp newNode n =
  match gp, newNode, n with
  | (spec::rest), newNode, n ->
    if specMatch spec n 
    then
      match rest with
      | [] ->
        let newElement = Xml.Element (name_to_string newNode, [], []) in
        (*TODO da capire se devo filtrare i PCData, se sta roba funzia mi regalo il divano nuovo *)
        let nodeNames = List.map (fun x -> localName (Xml.tag x)) (subelements n) in
        if List.exists (fun x -> x = (newNode.nameLocalName)) nodeNames
        then n
        else Xml.Element (Xml.tag n, Xml.attribs n, (Xml.children n) @ [newElement])
      | _ -> Xml.Element (Xml.tag n, Xml.attribs n, List.map (girAddNode rest newNode) (Xml.children n))
    else n
  | _, _, n -> n

(* gir_path -> xml -> xml option *)
let rec girDeleteNodes gp el =
  match gp with
  | (spec::rest) ->
    if specMatch spec el
    then
      match rest with
      | [] -> None
      | _ -> Some (Xml.Element (Xml.tag el, Xml.attribs el, List.filter_map (girDeleteNodes rest) (Xml.children el)))
    else Some el
  | _ -> Some el


let foldM_option f x l =
  let g a b =
    match a with
    | None -> None
    | Some a -> f a b
  in List.fold_left g (Some x) l

(* gir_rule list -> xml -> xml *)
let fixupGIR rules elem =
  let applyGIRRule n girsetattr =
    match girsetattr with
    | GIRSetAttr ((path, attr), newVal) -> Some (girSetAttr (path, attr) newVal n)
    | GIRAddNode (path, new_) -> Some (girAddNode path new_ n)
    | GIRDeleteNode (path) -> girDeleteNodes path n
  in Xml.Element (Xml.tag elem, 
                  Xml.attribs elem, 
                  List.filter_map (fun e -> foldM_option (applyGIRRule) e rules) (Xml.children elem))


(* gir_rule list -> xml -> xml *)
let fixupGIRDocument rules doc =
  fixupGIR rules doc

(* bool -> Set (string*string) -> Map (string*string) xml -> string list -> GIRRule list -> Map (string*string) xml *)
let rec loadDependencies verbose requested loaded (*extraPaths*) rules =
  match StringStringSet.is_empty requested with
  | true -> loaded
  | false ->
    (*TODO chissà se la trasformazione mantiene l'ordinamento...*)
    let name, version = List.nth (StringStringSet.to_seq requested |> List.of_seq) 0 in
    let doc = fixupGIRDocument rules (readGiRepository verbose name (Some version) (*extrapaths*)) in
    let newLoaded = StringStringMap.add (name, version) doc loaded in
    let keys = List.map (fun x -> match x with | (key, _) -> key) (StringStringMap.to_seq newLoaded |> List.of_seq) in
    let loadedSet = StringStringSet.of_seq (List.to_seq keys) in
    let newRequested = StringStringSet.union requested (documentListIncludes doc) in
    let notYetLoaded = StringStringSet.diff newRequested loadedSet in
    loadDependencies verbose notYetLoaded newLoaded (*extrapaths*) rules



(* bool -> string -> string option -> xml *)
let loadGIRFile verbose name version (*extraPaths*) rules =
  let doc = fixupGIRDocument rules (readGiRepository verbose name version (*extraPaths*)) in
  let deps = loadDependencies verbose (documentListIncludes doc) StringStringMap.empty (*extrapaths*) rules in
  doc, deps

(* gir_info_parse -> string*gir_info result*)
let toGIRInfo info =
  match List.filter_map (fun x -> x) info.girIPNamespaces with 
  | [ns] -> Ok { girPCPackages = List.rev ( List.filter_map (fun x -> x) info.girIPPackage );
                 girNSName = ns.nsName;
                 girNSVersion = ns.nsVersion;
                 girAPIs = List.rev ns.nsAPIs;
                 girCTypes = ns.nsCTypes |> List.to_seq |> StringMap.of_seq;
               }
  | [] -> Error "Found no valid namespace"
  | _ -> Error "Found multiple namespaces"


(* bool -> string -> string option -> gir_info *)
let loadRawGIRInfo verbose name version (*extrapaths*) =
  let doc = readGiRepository verbose name version (*extrapaths*) in
  match toGIRInfo (parseGIRDocument AliasMap.empty doc) with
  | Error err -> prerr_endline ("loadRawGIRInfo, API.ml riga 211: " ^ err); assert false
  | Ok docGIR -> docGIR

(*
let foldM_result f x l =
  let rec g a b =
    match a with
    | Error e -> Error e
    | Ok a -> f a b
  in List.fold_left g (Ok x) l
*)


(*FIXME dovrebbe funzionare ma fa schifo, soluzioni con una fold?*)
let check_sequence_result l =
  let rec g l =
    match l with
    | [] -> Ok []
    | x::xs -> 
      match x with
      | Ok _ -> g(xs)
      | Error e -> Error e
  in Result.is_ok (g l)


let fixupField offsetMap f =
  { f with fieldOffset = 
      match StringMap.find_opt f.fieldName offsetMap with
      | None -> assert false
      | Some o -> o.fieldInfoOffset}


let fixupUnionSizeAndOffsets nm u =
  let size, infoMap = girUnionFieldInfo nm.namespace nm.name in
  { u with unionSize= size; unionFields = List.map (fixupField infoMap) (u.unionFields)}
  

let fixupUnion _ (n, u) =
  match u with
  | APIUnion u -> 
    let fixed = fixupUnionSizeAndOffsets n u in
    n, APIUnion fixed
  | _ -> n, u


let fixupStructIsBoxed nm s =
  match nm.namespace, nm.name with
  | "GLib", "Variant" -> { s with structIsBoxed = false}
  | (*ns*) _ , _ ->
    let isBoxed =
      begin
      match s.structTypeInit with
      | None -> false
      | Some (*ti*) _ ->
        (*TODO*)
        (*let gtype = girLoadGType ns ti in
        gtypeIsBoxed gtype*)
        false (*dummy value *)
      end in
    { s with structIsBoxed = isBoxed }


let fixupStructSizeAndOffsets nm s =
  let size, infoMap = girStructFieldInfo nm.namespace nm.name in
  {s with structSize = size; structFields = List.map (fixupField infoMap) s.structFields}


let fixupStruct _ (n, s) =
  match s with
  | APIStruct s -> let fixed = fixupStructIsBoxed n s |> fixupStructSizeAndOffsets n in
      n, APIStruct fixed
  | _ -> (n, s)


let fixupInterface csymbolMap (nm, i) = 
  match i with
  | APIInterface iface ->
    let prereqs =
      match iface.ifTypeInit with
      | None -> []
      | Some (*ti*) _ ->
        (*TODO*)
        (*let gtype = girLoadGType ns ti in
        let prereqGTypes = gtypeInterfaceListPrereqs gtype in*)
        let prereqGTypes = [] in (*dummy value*)
        List.map (fun p -> 
          match StringMap.find_opt p csymbolMap with
          | Some pn -> pn
          | None -> assert false
          ) prereqGTypes
    in nm, APIInterface {iface with ifPrerequisites = prereqs}
  | _ -> (nm, i)

let fixupGIRInfos doc deps =
  let rec fixup fixer (doc, deps) =
    let fixedDoc = fixAPIs fixer doc in
    let fixedDeps = List.map (fixAPIs fixer) deps
    in fixedDoc, fixedDeps

  and fixAPIs fixer info =
    let fixedAPIs = List.map (fixer ctypes) (info.girAPIs)
    in {info with girAPIs = fixedAPIs}

  and ctypes =
    let f_union _ m1 _ =
    Some m1
    in List.fold_left (StringMap.union f_union) StringMap.empty (List.map (fun x -> x.girCTypes) (doc::deps))
  
  in (fixup fixupInterface (doc, deps)) |> (fixup fixupStruct) |> (fixup fixupUnion)



(* string -> bool -> string -> string option -> gir_info*(gir_info list)*)
let loadGIRInfo verbose name version (* extraPaths *) rules =
  let doc, deps = loadGIRFile verbose name version (* extraPaths *) rules in
  let deps_elems = List.map (fun x -> match x with | (_, value) -> value) (StringStringMap.bindings deps) in
  (*TODO da capire la questione alias, da dove lo passo? Per ora passo mappa vuota, magari è giusto*)
  let f_union _ m1 _ =
    Some m1
  in let aliases = List.fold_left (AliasMap.union f_union) (AliasMap.empty) (List.map (documentListAliases name AliasMap.empty) (doc :: deps_elems)) in
  let parsedDoc = toGIRInfo (parseGIRDocument aliases doc) in
  let parsedDeps = List.map (toGIRInfo) (List.map (parseGIRDocument aliases) (deps_elems)) in
  let combineErrors parsedDoc parsedDeps =
    let doc = parsedDoc in
    if check_sequence_result (parsedDeps) && Result.is_ok doc
    then Ok (Result.get_ok doc, List.map (Result.get_ok) parsedDeps)
    else Error "errore"
  in match combineErrors parsedDoc parsedDeps with
  | Error _ -> assert false
  | Ok (docGIR, depsGIR) ->
    if docGIR.girNSName = name
    then
      let _ = List.map (fun info -> Result.get_ok (GOI.Repository.require info.girNSName ~version:info.girNSVersion ())) (docGIR::depsGIR) in
      let fixedDoc, fixedDeps = fixupGIRInfos docGIR depsGIR in
      fixedDoc, fixedDeps
    else assert false

  
(* TODO
let g_type_interface_prerequisites =
  foreign "g_type_interface_prerequisites" (
*)

(*TODO
let gtypeInterfaceListPrereqs =l
*)

(*TODO tutto il delirio dei fixup *)



let run verbose ns version =
  let _ = loadRawGIRInfo verbose ns  version in
  prerr_endline ("PARSING COMPLETATO: " ^ ns);;


run true "Gtk" None;;




                      

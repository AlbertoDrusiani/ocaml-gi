(*open GIR.Allocation
open GIR.Arg
open GIR.BasicTypes
open GIR.Callable*)
open GIR.Callback
open GIR.Constant
(*open GIR.Deprecation*)
open GIR.Enum
(*open GIR.Field*)
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
   (* girCTypes: *)
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
  | GIRNames of gir_name_tag
  | GIRType of string
  | GIRTypeName of string*gir_name_tag

and gir_name_tag =
  | GIRPlainName of string
  | GIRRegex of string

and gir_rule =
  | GIRSetAttr of (gir_path*GIR.XMLUtils.name)*string
  | GIRAddNode of gir_path*GIR.XMLUtils.name
  | GIRDeleteNode of gir_path

(* map alias type -> gir_namespace -> xml -> gir_namespace *)
let parseNSElement (*aliases*) ns element =
  prerr_endline ("Inizio il parsing di un NSElement: " ^ Xml.tag element ^ " con name = " ^ Xml.attrib element "name");
  let parse name api =
    let maybeCType = match lookupAttrWithNamespace CGIRNS "type" element with
                     | Some ctype -> (ctype, name) :: ns.nsCTypes
                     | None -> ns.nsCTypes
    in {ns with nsAPIs = (name, api)::ns.nsAPIs; nsCTypes = maybeCType} in 
  match lookupAttr "introspectable" element with
  | Some "0" -> prerr_endline ("Elemento da NON parsare"); ns
  | _ -> 
    match localName (Xml.tag element) with
    | "alias" -> ns
    | "constant" -> begin
                     match parseConstant element ns.nsName with
                        | (name, api) -> parse name (APIConst api)
                     end
    | "enumeration" -> begin
                     match parseEnum element ns.nsName with
                        | (name, api) -> parse name (APIEnum api)
                     end
    | "bitfield" -> begin
                     match parseFlags element ns.nsName with
                        | (name, api) -> parse name (APIFlags api)
                     end
    | "function" -> begin
                     match parseFunction element ns.nsName with
                        | (name, api) -> parse name (APIFunction api)
                     end
    | "callback" -> begin
                     match parseCallback ns.nsName element with
                        | (name, api) -> parse name (APICallback api)
                     end
    | "record" -> begin
                     match parseStruct element ns.nsName with
                        | (name, api) -> parse name (APIStruct api)
                     end
    | "union" -> begin
                     match parseUnion element ns.nsName with
                        | (name, api) -> parse name (APIUnion api)
                     end
    | "class" -> begin
                     match parseObject element ns.nsName with
                        | (name, api) -> parse name (APIObject api)
                     end
    | "interface" -> begin
                     match parseInterface element ns.nsName with
                        | (name, api) -> parse name (APIInterface api)
                     end
    | "boxed" -> ns
    | _ -> assert false


(* xml -> map alias type -> gir_namespace*)
let parseNamespace element (*aliases*) =
  let name = Xml.attrib element "name" in
  prerr_endline ("Inizio il parsing del namespace: " ^ name);
  let version = Xml.attrib element "version" in
  let ns = { nsName = name;
             nsVersion = version;
             nsAPIs = [];
             nsCTypes = [];
           }
  in Some (List.fold_left parseNSElement ns (subelements element))

(* xml ->  string*string option *)
let parseInclude element =
  prerr_endline ("Inizio la parseIncude sull'elemento " ^ (Xml.tag element) ^ ", con name=" ^ (Xml.attrib element "name"));
  let name =
    prerr_endline ("provo a prendere il name");
    try
     let n = Xml.attrib element "name" in
     n (*TODO ci sono un po' di funzioni che in Haskell restituiscono option,
                         da tenere a mente per gestione errori con eccezioni nella libreria xml,
                         per ora forzo a restare aderente ad Haskell aggiungendo Some*)
    with Xml.No_attribute str -> prerr_endline ("Errore No_attribute: " ^ str); "no_name"
  in let version =
    try (*TODO (string*string) option o string option *string option*)
      prerr_endline ("provo a prendere la version");
      let v = Xml.attrib element "version" in
      v
    with Xml.No_attribute str -> prerr_endline ("Errore No_attribute: " ^ str); "0"
  in Some (name, version)

(* xml -> string *)
let parsePackage element =
  prerr_endline ("Inizio il parsing del package: " ^ Xml.attrib element "name");
  Some (Xml.attrib element "name")

(* gir_info_parse -> xml -> gir_info_parse *)
let parseRootElement (*aliases*) info element =
  prerr_endline ("Inizio il parseRootElement: " ^ (Xml.tag element));
  match localName (Xml.tag element) with
  | "include" -> {info with girIPIncludes = parseInclude element :: info.girIPIncludes;}
  | "package" -> {info with girIPPackage = parsePackage element :: info.girIPPackage;}
  | "namespace" -> {info with girIPNamespaces = parseNamespace element :: info.girIPNamespaces;}
  | _ -> info

(* gir_info_parse *)
let emptyGIRInfoParse =
  {girIPPackage = []; girIPIncludes = []; girIPNamespaces = [] }

(* xml -> gir_info_parse *)
let parseGIRDocument (*aliases*) doc =
  List.fold_left parseRootElement (*aliases*) emptyGIRInfoParse (subelements doc)

(* xml -> *)
let documentListIncludes doc =
  let includes = childElemsWithLocalName "include" doc in
  List.filter_map parseInclude includes

(*TODO troppi set e map, appena capisco come usarle la implemento*)
(*let loadDependencies verbose requested loaded extraPaths rules =*)

(* bool -> string -> string option -> xml *)
let loadGIRFile verbose name version (*extraPaths*) (*rules*) =
  let doc = (*fixupGIRDocument rules*) readGiRepository verbose name version (*extraPaths*) in
  (*let deps = loadDepenencies verbose doc *)
  doc

(* gir_info_parse -> string*gir_info result*)
let toGIRInfo info =
  match List.filter_map (fun x -> x) info.girIPNamespaces with 
  | [ns] -> Ok { girPCPackages = List.rev ( List.filter_map (fun x -> x) info.girIPPackage );
                 girNSName = ns.nsName;
                 girNSVersion = ns.nsVersion;
                 girAPIs = List.rev ns.nsAPIs;
                 (*girCTypes = ns.nsCTypes;*)
               }
  | [] -> Error "Found no valid namespace"
  | _ -> Error "Found multiple namespaces"


(* bool -> string -> string option -> gir_info *)
let loadRawGIRInfo verbose name version (*extrapaths*) =
  let doc = readGiRepository verbose name version (*extrapaths*) in
  match toGIRInfo (parseGIRDocument (*M.empty)*) doc) with
  | Error err -> prerr_endline ("loadRawGIRInfo, API.ml riga 211: " ^ err); assert false (*TODO error, da sistemare*)
  | Ok docGIR -> docGIR


(* bool -> string -> string option -> gir_info*(gir_info list)*)
(*let loadGIRInfo verbose name version (* extraPaths rules *) =
  let doc = loadGIRFile verbose name version (* extraPaths rules *) in
  (*let aliases = (*TODO*)*)
  let parseDoc = toGIRInfo (parseGIRDocument (*aliases*) doc) in
  let parseDeps =*)


let run =
  loadRawGIRInfo true "Atk" (Some "1.0") ;;

run;;

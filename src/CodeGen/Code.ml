open API
open ModulePath
open Naming
(*open Util*)
(*open ModulePath*)
open GIR.BasicTypes
open Config
open Util
open Filename

type code = Code of (code_token list)


and code_token =
      | Line of string
      | Indent of code
      | Group of code
      | IncreaseIndent


let getCode c =
  match c with
  | Code x -> x

let emptyCode =
  Code []


let isCodeEmpty c =
    match c with
    | Code seq_ -> List.length seq_ == 0


let codeSingleton t =
  Code [t]


type named_section =
  | MethodSection
  | PropertySection
  | SignalSection
  | EnumSection
  | FlagSection


type haddock_section =
  | ToplevelSection
  | NamedSubsection of named_section*string


type deps =
  StringSet.t

type symbol_name = string


type export_type =
  | ExportSymbol of haddock_section
  | ExportTypeDecl
  | ExportModule


type export =
  { exportType: export_type;
    exportSymbol: symbol_name;
  }


type module_info =
  { modulePath: module_path;
    moduleCode: code;
    bootCode: code;
    gCode: code;
    cCode: code;
    hCode: code;
    tCode: code;
    submodules: module_info StringMap.t;
    moduleDeps: deps; 
    cDeps: deps;
    moduleExports: export list;
    qualifiedImports: ModulePathSet.t;
    (*sectionDocs*)
  }


type module_flag = ImplicitPervasives

let emptyModule m =
  { modulePath = m;
    moduleCode = emptyCode;
    bootCode = emptyCode;
    cCode = emptyCode;
    tCode = emptyCode;
    hCode = emptyCode;
    gCode = emptyCode;
    submodules = StringMap.empty;
    moduleDeps = StringSet.empty;
    cDeps = StringSet.empty;
    moduleExports = [];
    qualifiedImports = ModulePathSet.empty;
   (* sectionDocs = *)
  }


type code_gen_config = {
  hConfig: config;
  loadedAPIs: api NameMap.t;
  (*c2hMap: TODO da implementare il modulo gtk-doc*) 
}



type cg_error =
  | CGErrorNotImplemented of string
  | CGErrorBadIntrospectionInfo of string
  | CGErrorMissingInfo of string


exception CGError of cg_error

type named_tyvar =
  | SingleCharTyvar of char
  | IndexedTyvar of string*int

type cg_state = {
  cgsNextAvailableTyvar: named_tyvar;
}

let emptyCGState =
  {cgsNextAvailableTyvar = SingleCharTyvar 'b'}

let cleanInfo info =
  { info with 
    moduleCode = emptyCode;
    bootCode = emptyCode;
    cCode = emptyCode;
    tCode = emptyCode;
    hCode = emptyCode;
    gCode = emptyCode;
    submodules = StringMap.empty;
    moduleDeps = StringSet.empty;
    moduleExports = [];
    qualifiedImports = ModulePathSet.empty;
    }


(*let evalCodeGen cfg apis mPath cg =
  let initialInfo = emptyModule mPath in
  let cfg' = {hConfig = cfg; loadedAPIs = apis} in
  unwrapCodeGen cg cfg' (emptyCGState, initialInfo)
*)

 (*TODO questa è la funzione che mi faccio per creare il contesto per poi passarlo in giro
   * la semantica dovrebbe essere simile allo stato della monade. Sta roba viene fatta
   * nell'evalCodegen in Haskell, potevo chiamarla così ma qua non valuto niente*)
  (* config -> Map (name, api) -> module_path -> (codegen_config, cgstate, module_info)*)
  


let notImplementedError s = raise (CGError (CGErrorNotImplemented s))

let badIntroError s = raise (CGError (CGErrorBadIntrospectionInfo s))

let missingInfoError s = raise (CGError (CGErrorMissingInfo s))





let setContext cfg apis mPath =
    let initialInfo = emptyModule mPath in
    let cfg' = { hConfig = cfg; loadedAPIs = apis; (*c2hMap = cToHaskellMap (NameMap.to_seq apis |> List.of_seq)*)} in
     cfg', emptyCGState, initialInfo 


let rec mergeInfoState oldState newState =
  let newDeps = StringSet.union (oldState.moduleDeps) (newState.moduleDeps) in
  let newCDeps = StringSet.union (oldState.cDeps) (newState.cDeps) in
  let union_f _ m1 m2 = Some (mergeInfo m1 m2) in
  prerr_endline ("I submodules dello stato vecchio sono: ");
  StringMap.iter (fun k v -> if 
      StringMap.cardinal oldState.submodules < 5
      then prerr_endline("Chiave: " ^ k ^ ". Valore: " ^ dotModulePath v.modulePath)) oldState.submodules;
  prerr_endline ("I submodules dello stato nuovo sono: ");
  StringMap.iter (fun k v -> if 
      StringMap.cardinal oldState.submodules < 5 
      then prerr_endline("Chiave: " ^ k ^ ". Valore: " ^ dotModulePath v.modulePath)) newState.submodules;
 
  let newSubmodules = StringMap.union union_f oldState.submodules newState.submodules in

  prerr_endline ("I submodules dello stato aggiornato sono: " ^ string_of_int(StringMap.cardinal newSubmodules));
  StringMap.iter (fun k v -> if (*dotModulePath v.modulePath = "GLib" || 
  dotModulePath v.modulePath = "Constant" || 
  dotModulePath v.modulePath = "GLib.Constant" ||
  dotModulePath newState.modulePath = "GLib.Constant" ||*)
  StringMap.cardinal newSubmodules < 6
  then prerr_endline ("Chiave: " ^ k ^ ". Valore :" ^ dotModulePath v.modulePath)) newSubmodules;
  prerr_endline ("I submodules dei submodules stato aggiornato sono: " ^ string_of_int(StringMap.cardinal newSubmodules));
  StringMap.iter (fun k v -> if (*dotModulePath v.modulePath = "GLib" || 
  dotModulePath v.modulePath = "Constant" || 
  dotModulePath v.modulePath = "GLib.Constant" ||
  dotModulePath newState.modulePath = "GLib.Constant" ||*)
  StringMap.cardinal newSubmodules < 6
  then prerr_endline ("Chiave: " ^ k ^ ". Valore :" ^ dotModulePath v.modulePath)) newSubmodules;

  let newExports = oldState.moduleExports @ newState.moduleExports in
  let newImports = ModulePathSet.union oldState.qualifiedImports newState.qualifiedImports in
  let newCCode = Code ((getCode oldState.cCode) @ (getCode newState.cCode)) in
  let newHCode = Code ((getCode oldState.hCode) @ (getCode newState.hCode)) in
  let newTCode = Code ((getCode oldState.tCode) @ (getCode newState.tCode)) in
  let newGCode = Code ((getCode oldState.gCode) @ (getCode newState.gCode)) in
  let newBoot = Code ((getCode oldState.bootCode) @ (getCode newState.bootCode)) in
  {oldState with 
   moduleDeps = newDeps;
   cDeps = newCDeps;
   submodules = newSubmodules;
   moduleExports = newExports;
   qualifiedImports = newImports;
   bootCode = newBoot;
   cCode = newCCode;
   tCode = newTCode;
   hCode = newHCode;
   gCode = newGCode;
  }

and mergeInfo oldInfo newInfo =
  prerr_endline ("Sto mergiando il modulo vecchio: " ^ dotModulePath oldInfo.modulePath ^ " con il nuovo: " ^ dotModulePath newInfo.modulePath);
  let info = mergeInfoState oldInfo newInfo in
  { info with moduleCode = Code (getCode (oldInfo.moduleCode) @ getCode (newInfo.moduleCode))}



let evalCodeGen cfg apis mPath action =
  let initialInfo = emptyModule mPath in
  let cfg' = {
    hConfig = cfg;
    loadedAPIs = apis;
    (*c2hMap =*) 
  }
  in action (cfg', emptyCGState, initialInfo)



let genCode cfg apis mPath action =
  let _, _, minfo = evalCodeGen cfg apis mPath action in
  minfo 


let handleCGExc (cfg, cgstate, oldInfo) fallback action =
  let info = cleanInfo oldInfo in
  try
    let (cgstate, newInfo) = Lazy.force (action cgstate info) in
    cfg, cgstate, mergeInfo oldInfo newInfo
  with CGError e -> Lazy.force (fallback cgstate oldInfo e)


let describeCGError error =
  match error with
  | CGErrorNotImplemented e -> "Not implemented: " ^ e
  | CGErrorBadIntrospectionInfo e -> "Bad introspection data: " ^ e
  | CGErrorMissingInfo e -> "Missing info: " ^ e


let currentNS minfo =
  modulePathNS minfo.modulePath


let addSubmodule currentInfo mName smInfo =
  match StringMap.mem mName currentInfo.submodules with
  | true -> prerr_endline ("Mergio il submodule \"" ^ mName ^ "\" al modulo \"" ^ dotModulePath currentInfo.modulePath);
  prerr_endline ("Il submodule ha come path: " ^ dotModulePath smInfo.modulePath); 
  prerr_endline ("Quindi sto mergianod il modulo vecchio: " ^ dotModulePath currentInfo.modulePath ^ " con il modulo nuovo " ^ dotModulePath smInfo.modulePath);
  let currentInfoModule = StringMap.find mName currentInfo.submodules in
            {currentInfo with submodules = StringMap.add mName (mergeInfo smInfo currentInfoModule) currentInfo.submodules} 
  | false -> 
  prerr_endline ("Aggiungo il submodule \"" ^ mName ^ "\" al modulo \"" ^ dotModulePath currentInfo.modulePath); 
  prerr_endline ("Il submodule ha come path: " ^ dotModulePath smInfo.modulePath);
  {currentInfo with submodules = StringMap.add mName smInfo currentInfo.submodules}


let submodule' (cfg, cgstate, minfo) mName action =
  let oldInfo = minfo in
  let info = emptyModule (concatModulePath oldInfo.modulePath mName) in
  prerr_endline ("VEDIAMO SE IL BACO È QUI: " ^ dotModulePath info.modulePath);
  let _, _, smInfo = action (cfg, emptyCGState, info) in
  cfg, cgstate, (addSubmodule oldInfo mName smInfo)


(* code_gen_config*cgstate*module_info -> module_path -> (cfg * cgstate * minfo *)
let rec submodule (cfg, cgstate, minfo) mPath action =
  match mPath.modulePathToList with
  | [] -> action (cfg, cgstate, minfo)
  | m::ms -> 
    prerr_endline ("____________SUBMODULE________" ^ dotModulePath mPath ^ " e " ^ String.concat " " ms);
    submodule' (cfg, cgstate, minfo) m (fun (cfg, cgstate, minfo) -> submodule (cfg, cgstate, minfo) {modulePathToList = ms} action)
  
  
  (*(fun (cfg, cgstate, minfo) -> submodule (cfg, cgstate, minfo) {modulePathToList = ms}) m*)


let addCDep minfo dep =
  {minfo with cDeps = StringSet.add dep (minfo.cDeps)}


let getFreshTypeVariable cgstate =
  let tyvar, next = 
    match cgstate.cgsNextAvailableTyvar with
    | SingleCharTyvar char ->
      begin
      match char with
      | 'z' -> ("z", IndexedTyvar ("a", 0))
      | 'a' -> ("b", SingleCharTyvar 'c')
      | c -> (String.make 1 c, SingleCharTyvar (char_of_int (int_of_char c + 1)))
      end
    | IndexedTyvar (root, index) ->
      (root ^ (string_of_int index), IndexedTyvar (root, index + 1))
  in { cgsNextAvailableTyvar = next}, tyvar


let findAPIByName cfg n =
  let apis = cfg.loadedAPIs in
  match NameMap.find_opt n apis with
  | Some api -> api
  | None -> assert false (*FIXME gestione errori*)


let findAPI cfg t =
  match t with
  | TError -> Some (findAPIByName cfg {namespace = "GLib"; name = "Error"})
  | TInterface n -> Some (findAPIByName cfg n)
  | _ -> None


let getAPI cfg t =
  match findAPI cfg t with
  | Some a -> a
  | None -> assert false 



let recurseWithState f oldInfo =
  let info = cleanInfo oldInfo in
  let cgstate, newInfo = f info in
  let code = newInfo.moduleCode in
  let minfo = mergeInfoState oldInfo newInfo in
  cgstate, minfo, code

(* module_info -> code_token -> module_info *)
let tellCode c minfo =
  { minfo with moduleCode = Code (getCode(minfo.moduleCode) @ [c])}

(* module_info -> string -> module_info *)
let line s minfo =
  tellCode (Line s) minfo


(* module_info -> code_token -> module_info *)
let tellGCode c minfo =
  { minfo with gCode = Code (getCode(minfo.gCode) @ [c])}

(* module_info -> string -> module_info *)
let gline s minfo =
  tellGCode (Line s) minfo 


let cBoot f minfo =
  let cgstate, minfo, code = recurseWithState f minfo in
  cgstate, { minfo with cCode = Code ((getCode(minfo.cCode)) @ getCode(code))}


let cline l minfo cgstate = cBoot (fun minfo -> cgstate, line l minfo) minfo



(*let cline l minfo =
  let info = cleanInfo minfo in
  let info = line l info in
  let code = info.moduleCode in
  let minfo = mergeInfoState minfo info in
  { minfo with cCode = Code ((getCode(minfo.cCode)) @ getCode(code))}*)


(* module_info -> code_token -> module_info *)
let tellHCode c minfo =
  { minfo with hCode = Code (getCode(minfo.hCode) @ [c])}

(* module_info -> string -> module_info *)
let hline s minfo =
  tellHCode (Line s) minfo

(* module_info -> code_token -> module_info *)
let tellTCode c minfo =
  { minfo with tCode = Code (getCode(minfo.tCode) @ [c]) }

(* module_info -> string -> module_info *)
let tline s minfo =
  tellTCode (Line s) minfo


let commentLine minfo t =
  line  ("(* " ^ t ^ " *)") minfo

let blank minfo =
  line "" minfo

let gblank minfo =
  gline "" minfo 

(* dovrebbe essere corretta: prende lo statio vecchio (la tripla), una funzione che
   computa su una tripla e restituisce cgstate e minfo nuovi. La runCodeGen la simulo
   accumulando nella funzione le cose da fare e poi passandole stati nuovi o modificati, a seconda*)
let recurseWithAPIs (cfg, cgstate, minfo) f apis =
  let info = cleanInfo minfo in
  let cfg' = { cfg with loadedAPIs = apis; (*c2hMap = cToHaskellMap (NameMap.to_seq apis |> List.of_seq) *)} in
  let _, _, newInfo = f (cfg', cgstate, info) in
  cfg, cgstate, newInfo 




let indent f minfo =
  let cgstate, minfo, code = recurseWithState f minfo in
  cgstate, tellCode (Indent code) minfo 


let gindent f minfo =
  let cgstate, minfo, code = recurseWithState f minfo in 
  cgstate, tellGCode (Indent code) minfo 


let group f minfo =
  let cgstate, minfo, code = recurseWithState f minfo in
  let minfo = tellCode (Group code) minfo  in
  cgstate, blank minfo


let ggroup f minfo =
  let cgstate, minfo, code = recurseWithState f minfo in
  let minfo = tellGCode (Group code) minfo  in
  cgstate, gblank minfo


let nameToClassType n =
  let ocamlId = ocamlIdentifier n in
  String.concat "\n"
    ["class type " ^ ocamlId ^ "_o = object";
     "  method as_" ^ ocamlId ^ " : t Gobject.obj";
     "end"]

(* name list -> string *)
let ifacesTypes n =
  match n with
  | [] -> ""
  | xs -> " | `" ^ String.concat " | `" (List.map ocamlIdentifierNs xs)

(* string -> string *)
let typeDeclText t =
  "type t = " ^ t

(* name -> name option -> name list -> string *)
let textToOCamlType n mn nl =
  match n, mn, nl with
  | {namespace = "GObject"; name = "Object"}, _, _ -> typeDeclText "[`giu]"
  | {namespace = "Gtk"; name = "Widget"}, _, _ -> typeDeclText "[`giu | `widget]"
  | n, None, ifaces -> 
    typeDeclText ("[ " ^ ocamlIdentifierNs n ^ ifacesTypes ifaces ^ "]")
  | n, (Some {namespace = "GObject"; name = "Object"}), ifaces ->
    typeDeclText ("[`giu | `]" ^ ocamlIdentifierNs n ^ ifacesTypes ifaces ^ "]")
  | n, (Some nm), ifaces when n.namespace = nm.namespace ->
    typeDeclText  ("[" ^ nsOCamlType n.namespace nm ^ " | `" ^ ocamlIdentifierNs n ^ ifacesTypes ifaces ^ "]")
  | n, Some pn, ifaces ->
    typeDeclText ("[" ^ nsOCamlType n.namespace pn ^ " | `" ^ ocamlIdentifierNs n ^ ifacesTypes ifaces ^ "]")


let getParent' api =
  let rename n =
    match n with
    | Some {namespace = "GObject"; name = "InitiallyUnowned"} -> Some ({namespace = "GObject"; name = "Object"})
    | x -> x
  in match api with
  | APIObject o -> rename o.objParent 
  | _ -> None


let rec instanceTree cfg n =
  let api = findAPIByName cfg n in
  match getParent' api with
  | Some p -> p :: (instanceTree cfg p)
  | None -> []


let dotWithPrefix mp = dotModulePath mp


(* code_gen_config -> module_info -> name -> module_info *)
let addTypeFile cfg minfo n =
  let api = findAPIByName cfg n in
  match api with
  | APIConst _ -> minfo
  | APIFunction _ -> minfo
  | APICallback _ -> minfo
  | APIEnum _ -> minfo
  | APIFlags _ -> minfo
  | APIInterface _ ->
    let minfo = tline (textToOCamlType n None []) minfo  in
    let minfo = tline "" minfo  in
    tline (nameToClassType n) minfo 
  | APIObject o ->
    let parents = instanceTree cfg n in
    let minfo = 
      begin
      match parents with
       | [] -> tline (textToOCamlType n None o.objInterfaces) minfo 
       | parent::_ -> tline (textToOCamlType n (Some parent) o.objInterfaces) minfo 
      end
    in let minfo = tline "" minfo in
    tline (nameToClassType n) minfo 
  | APIStruct _ -> tline "type t" minfo 
  | APIUnion _ -> tline "type t" minfo 



let getLibName path =
  match path with
  | Some path -> 
    explode path |> List.rev |> takeWhile (fun x -> x != '/') 
    |> List.rev |> List.to_seq |> String.of_seq
  | None -> ""


let modulePathToFilePath dirPrefix mp ext =
  let dirPrefix = match dirPrefix with
  | Some d -> d
  | None -> ""
  (*FIXME non mi convince*)
  in String.concat dir_sep (dirPrefix::mp.modulePathToList) ^ ext



let paddedLine n s = (String.make (n*4) ' ') ^ s ^ "\n"

(* provo con le stringhe normali invece che buffer o simili *)
let codeToText (Code seq_) =
  prerr_endline("____Inizio CodeToText");
  let rec genCode' i l =
    match i, l with
    | _, [] -> ""
    | n, (Line s) :: rest -> prerr_endline ("###LINEA: " ^ s); (paddedLine n s) ^ (genCode' n rest)
    | n, Indent (Code s) :: rest -> (genCode' (n + 1) s) ^ (genCode' n rest)
    | n, Group (Code s) :: rest -> (genCode' n s) ^ (genCode' n rest)
    | n, IncreaseIndent :: rest -> genCode' (n + 1) rest
  in 
  genCode' 0 seq_


let qualifiedModuleName mp =
  match mp.modulePathToList with
  | [ns; "Objects"; o] -> ns ^ "." ^ o
  | [ns; "Interfaces"; i] -> ns ^ "." ^ i
  | [ns; "Structs"; s] -> ns ^ "." ^ s
  | [ns; "Unions"; u] -> ns ^ "." ^ u
  | _ -> dotModulePath mp


let importedDeps mp l =
  match mp.modulePathToList, l with
  | _, [] -> ""
  | prefix, deps -> 

    let importSource mp =
      match mp.modulePathToList with
      | [_; "Callbacks"] -> false
      | mp -> (take (List.length prefix) mp) = prefix

    in let toImport dep =
      let impSt =
        if importSource dep
        then "import {-# SOURCE #-} qualified "
        else "import qualified "
      in impSt ^ dotWithPrefix dep ^ " as " ^ qualifiedModuleName dep

    in String.concat "\n" (List.map toImport deps)


let genTStubs minfo = codeToText minfo.tCode

let genHStubs minfo = codeToText minfo.hCode

let genCStubs minfo = codeToText minfo.cCode

let genGModule minfo = codeToText minfo.gCode



let commonCImports =
  String.concat "\n"
  ["#include <string.h>"
  ; "#include <caml/mlvalues.h>"
  ; "#include <caml/alloc.h>"
  ; "#include <caml/memory.h>"
  ; "#include <caml/callback.h>"
  ; "#include <caml/fail.h>"
  ; "#include \"wrappers.h\""
  ; "#include \"ml_glib.h\""
  ; "#include \"ml_gobject.h\""]


let cImports currLib = "#include \"" ^ String.lowercase_ascii currLib ^ "_includes.h\""


let addCFile state file = file::state

let writeModuleInfo state isVerbose dirPrefix _dependencies minfo =
  let mapElems = List.map (fun (_, v) -> v) (StringMap.bindings minfo.submodules) in
  let _submodulePaths = List.map (fun v -> v.modulePath) mapElems in
  prerr_endline ("Il module_info che sto per scrivere ha module path: " ^ dotModulePath minfo.modulePath);
  (*let _ = List.iter (fun x -> prerr_endline ("subModulePath: " ^ String.concat " " x.modulePathToList))_submodulePaths in*)
  let _submoduleExports = List.map dotWithPrefix _submodulePaths in
  (*let _pkgRoot = _ in*)
  let nspace = getLibName dirPrefix in
  let fname = modulePathToFilePath dirPrefix minfo.modulePath "" in
  (*prerr_endline ("dirPrefix è: " ^ Option.get dirPrefix);
  prerr_endline ("fname è: " ^ fname);*)
  let dirname = Filename.dirname fname in
  let code = codeToText minfo.moduleCode in
  prerr_endline ("Fine codeToText codice normale");
  (*let _deps = importedDeps *)
  if isVerbose
  then prerr_endline (dotWithPrefix (minfo.modulePath) ^ " -> " ^ fname);

  let _ = Sys.command ("mkdir -p " ^ dirname) in

  let _ = 
  match isCodeEmpty minfo.moduleCode with
  | true -> ()
  | false -> writeFile (fname ^ ".ml") (String.concat "\n" [code])


  in 
  let _ = 
  match isCodeEmpty minfo.tCode with
  | true -> ()
  | false -> writeFile (fname ^ "T.ml") (String.concat "\n" [genTStubs minfo])

  in let hPrefix = Option.value dirPrefix ~default:"" ^ dir_sep ^ "include" in

  let hName = moduleName minfo.modulePath in
  (*prerr_endline ("dirPrefix: " ^ Option.get dirPrefix);
  prerr_endline ("hPrefix: " ^ hPrefix);
  prerr_endline ("dir_sep: " ^ dir_sep);
  prerr_endline ("nspace: " ^ nspace);
  prerr_endline ("hName: " ^ hName);*)
  let hStubsFile = hPrefix ^ dir_sep ^ ("GI" ^ nspace ^ hName ^ ".h") in

  let _ = Sys.command ("mkdir -p " ^ hPrefix) in
  (*prerr_endline ("hstubsFile: " ^ hStubsFile);*)
  let _ = writeFile hStubsFile (String.concat "\n" [cImports nspace; commonCImports; genHStubs minfo]) in
  prerr_endline ("Fine codeToText hStubs");

  let state = 
  match isCodeEmpty minfo.cCode with
  | true -> state
  | false ->
    let cStubsFile = modulePathToFilePath dirPrefix minfo.modulePath ".c" in
    prerr_endline ("CSTUBS FILE: " ^ cStubsFile);
    let deps' = List.filter (fun x -> x != "Widget") (minfo.cDeps |> StringSet.to_seq |> List.of_seq) in
    let deps = String.concat "\n" (List.map (fun d -> "#include \"GI" ^ d ^ ".h\"") deps') in
    let state = addCFile state cStubsFile in
    writeFile cStubsFile (String.concat "\n" [deps; genCStubs minfo]); state
  
  in let _ = 
  match isCodeEmpty minfo.gCode with
  | true -> ()
  | false ->
    let gFileModulepath = minfo.modulePath in
    let gModuleFile = modulePathToFilePath dirPrefix gFileModulepath "G.ml" in
    writeFile gModuleFile (String.concat "\n" [genGModule minfo]);
  
  (*in assert false*)
  in state


let firstAndSecondsafe l =
  match l with
  | [] -> []
  | x::[] -> [x]
  | x::xs -> [x; List.nth xs 1]

let hdsafe l =
  match l with
  | [] -> []
  | x -> [List.hd x]

let rec writeModuleTree' state verbose_ dirPrefix dependencies minfo =
  prerr_endline ("_____________Entro nella moduleTree e sto lavorando sul modulo padre: " ^ String.concat "" minfo.modulePath.modulePathToList);
  (*prerr_endline ("state: " ^ String.concat "" state);*)
  StringMap.iter (fun k v -> prerr_endline ("Chiave: " ^ k ^ ". Valore :" ^ dotModulePath v.modulePath)) minfo.submodules;
  let mapElems = List.map (fun (_, v) -> v) (StringMap.bindings minfo.submodules) in
  (*prerr_endline ("La lista di figli pre filtro è lunga: " ^ string_of_int (List.length mapElems));
  let submodulesName = List.map (fun x -> dotModulePath x.modulePath) mapElems in
  prerr_endline ("Ed è composta da: \n" ^ String. concat "\n" submodulesName);
  
  let mapElems = List.filteri (fun i _ -> i < 6) (List.map (fun (_, v) -> v) (StringMap.bindings minfo.submodules)) in
  prerr_endline ("La lista di figli è lunga: " ^ string_of_int (List.length mapElems));
  let submodulesName = List.map (fun x -> dotModulePath x.modulePath) mapElems in
  prerr_endline ("Ed è composta da: \n" ^ String. concat "\n" submodulesName);*)
  let state, subModulePaths = 
    List.fold_left (fun (state, l) minfo -> 
      prerr_endline ("DENTRO LA FOLD");
      prerr_endline ("APPLICATA AL MODULO: " ^ (dotModulePath minfo.modulePath));
      let t = writeModuleTree' state verbose_ dirPrefix dependencies minfo
      in fst t, l @ (snd t)) 
      (state,[]) mapElems
  in
  prerr_endline ("ESCO DALLA FOLD");
  prerr_endline ("state: " ^ String.concat "" state);
  prerr_endline ("subModulePaths: " ^ String.concat " " subModulePaths);
  let state = writeModuleInfo state verbose_ dirPrefix dependencies minfo in
  (*prerr_endline ("Resituisco: " ^ String.concat " " (dotWithPrefix minfo.modulePath::subModulePaths));*)
  state, (dotWithPrefix minfo.modulePath) :: subModulePaths


let genDuneFile libName outputDir cFiles deps =
  (*FIXME da capire se questa join ci va messo il dir_sep o no*)
  let duneFilePath = String.concat dir_sep [outputDir; "dune"] in
  let libs = List.map (fun x -> "GI" ^ x) deps in
  let pkgConfName =
    match libName with
    | "GtkSource" -> "gtksourceview-3.0"
    | _ -> "gtk+-3.0"
  in let commonPart =
    [ 
      "(library";
      " (name GI" ^ libName ^ ")";
      " (public_name GI" ^ libName ^ ")";
      " (libraries gilablgtk3 " ^ String.concat " " libs ^ ")";
      ]
  

  in 
  prerr_endline ("CFILESSSSSS DENTRO A GENDUNE È: " ^ String.concat " " cFiles);
  let fileContent = 
    match cFiles with
    | [] -> String.concat "\n" (commonPart @ [")"])
    | _ ->
      String.concat "\n" (
        [ "(rule";
          " (targets";
          "  cflag-" ^ pkgConfName ^ ".sexp";
          "  clink-" ^ pkgConfName ^ ".sexp)";
          " (action (run dune_config -pkg " ^ pkgConfName ^ " -version 3.18)))";
          ] @
        commonPart @
        [ " (flags :standard -w -6-7-9-10-27-32-33-34-35-36-50-52 -no-strict-sequence)";
          " (c_library_flags (:include clink-" ^ pkgConfName ^ ".sexp))";
          " (foreign_stubs";
          "  (language c)";
          "  (names " ^ String.concat " " cFiles ^ ")";
          "  (include_dirs %{project_root}/include)";
          "  (flags (:include cflag-" ^ pkgConfName 
           ^ ".sexp) -I$BASE_OCAML_C $GI_INCLUDES -Wno-deprecated-declarations)))";
           ]

      )
  in writeFile duneFilePath fileContent

let writeModuleTree verbose_ dirPrefix minfo dependencies =
  let cFiles, modules =
    writeModuleTree' [] verbose_ dirPrefix dependencies minfo in
  prerr_endline ("FINITA LA WRITE MODULE TREE_________________________________________");
  prerr_endline ("I cFiles sono: " ^ String.concat "" cFiles);
  let prefix' = Option.value dirPrefix ~default:"" in
  prerr_endline ("prefix': " ^ prefix');
  let libName = getLibName dirPrefix in
  let modules' = List.filter (fun m -> 
  List.length (String.split_on_char '.' m) <3 ) modules in
  prerr_endline ("Modules' è lungo: " ^ string_of_int(List.length modules'));
  prerr_endline (String.concat " " modules');
  let regexp = Str.regexp "[.]" in
  prerr_endline ("DIRNAME: " ^ Filename.dirname "GLib/AsyncQueue");
  prerr_endline ("REPLACED: " ^ (Str.global_replace regexp "/"  "GLib.AsyncQueue"));
  let modulePaths = List.map (
      fun x -> (Str.global_replace regexp "/" x) |> Filename.dirname |> fun y -> prefix' ^ dir_sep ^ y)
      modules' |> List.to_seq |> StringSet.of_seq
  in let dirFileTuple = List.map (fun cFile -> (Filename.dirname cFile, Filename.basename cFile |> Filename.remove_extension)) cFiles in
  let cFilesMap = List.fold_left (fun fMap (key, file) -> 
    let el = StringMap.find_opt key fMap in
    match el with
    | None -> StringMap.add key [file] fMap
    | Some el -> StringMap.add key (file::el) fMap) 
    StringMap.empty dirFileTuple
  in 
  StringMap.iter (fun k v -> prerr_endline("Chiave: " ^ k ^ ". Valore: " ^ String.concat " " v)) cFilesMap;
  StringSet.iter (fun el -> prerr_endline("Valore modulePaths: " ^ el)) modulePaths;
  let _ =  StringSet.iter (
    fun path ->
      prerr_endline ("Path: " ^ path);
      let _ = Sys.command ("mkdir -p " ^ path) in
      let cFileNames = Option.value (StringMap.find_opt path cFilesMap) ~default:[] in
      genDuneFile libName path cFileNames dependencies 
      ) modulePaths
  in modules, StringSet.to_seq modulePaths |> List.of_seq
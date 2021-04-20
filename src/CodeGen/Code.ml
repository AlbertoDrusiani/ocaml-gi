open API
open ModulePath
(*open Naming*)
(*open Util*)
open ModulePath
open GIR.BasicTypes
open Config

type code = Code of (code_token list)

and code_token =
      | Line of string
      | Indent of code
      | Group of code
      | IncreaseIndent


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
  | CGErrorNonImplemented of string
  | CGErrorBadIntrospectionInfo of string
  | CGErrorMissingInfo of string


type named_tyvar =
  | SingleCharTyvar of char
  | IndexedTyvar of string*int

type cg_state = {
  cgsNextAvailableTyvar: named_tyvar;
}

let emptyCGState =
  {cgsNextAvailableTyvar = SingleCharTyvar 'b'}


(*let evalCodeGen cfg apis mPath cg =
  let initialInfo = emptyModule mPath in
  let cfg' = {hConfig = cfg; loadedAPIs = apis} in
  unwrapCodeGen cg cfg' (emptyCGState, initialInfo)
*)

 (*TODO questa è la funzione che mi faccio per creare il contesto per poi passarlo in giro
   * la semantica dovrebbe essere simile allo stato della monade. Sta roba viene fatta
   * nell'evalCodegen in Haskell, potevo chiamarla così ma qua non valuto niente*)
  (* config -> Map (name, api) -> module_path -> (codegen_config, cgstate, module_info)*)
  
let setContext cfg apis mPath =
    let initialInfo = emptyModule mPath in
    let cfg' = { hConfig = cfg; loadedAPIs = apis; (*c2hMap = cToHaskellMap (NameMap.to_seq apis |> List.of_seq)*)} in
     cfg', emptyCGState, initialInfo 


let rec submodule cfg cgstate minfo mPath =
  match mPath.modulePathToList with
  | [] -> cfg cgstate minfo
  | m::ms -> submodule' m (submodule cfg cgstate minfo {modulePathToList = ms}) 



let tellHCode minfo c =
  match minfo.hCode with
  | Code cd -> { minfo with hCode = Code (cd @  [c])}



let hline minfo s =
  tellHCode minfo (Line s)





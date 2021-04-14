open API
open ModulePath
open Naming
open Util
open ModulePath

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
  Code t


type named_section =
  | MethodSection
  | PropertySection
  | SignalSection
  | EnumSection
  | FlagSection


type haddock_section =
  | ToplevelSection
  | NamedSubsection of named_section*string


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
    tCode: code;
    (*submodules *TODO mappe e insiemi *)
    (*moduleDeps:
     *cDeps:*)
    moduleExports: export list;
    (*qualifiedImports:
    sectionDocs*)
  }


type module_flag



(*TODO la parte su Haddock ha senso farla per OCaml?*)
(*type HaddockSection =
  | ToplevelSection
  | NamesSub

*)





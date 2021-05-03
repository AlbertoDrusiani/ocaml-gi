open Filename
open Sys
open Util
open API
open GIR.BasicTypes
open Config
open ModulePath
open CodeGen
open Code

type library = {
    name: string;
    version: string;
    overridesFile: string option;
}


type tagged_override = {
  overrideTag: string;
  overrideText: string;
}

let union_f _ v1 _ =
  Some v1





(*per ora non considero gli overrides*)
(*string -> string -> bool -> ModuleInfo*)
let genLibraryCode name version verbosity (*_overrides*) =
  (*TODO dummy value, da implementare questa parte*)
  (*let ovs = defaultOverrides in*)
  (*let gir(*, girDeps*) = loadRawGIRInfo verbosity name (Some version) (*[]*) (*(ovs.girFixups)*) in*)
  let gir, girDeps = loadGIRInfo verbosity name (Some version) [] [] in
  let dependencies = List.map (fun x -> x.girNSName ) girDeps in 
  (*let apis, deps = filterAPIsAndDeps (*ovs*) gir (*girDeps*) in*)
  let apis = gir.girAPIs |> List.to_seq |> NameMap.of_seq in
  let deps = List.map (fun x -> x.girAPIs |> List.to_seq |> NameMap.of_seq) girDeps in
  let deps = List.fold_left (fun acc m -> NameMap.union union_f acc m) NameMap.empty deps in
  let allAPIs = NameMap.union union_f apis deps in

  let cfg = {modName = name; verbose = verbosity; (*overrides = ovs*)} in
  genCode cfg allAPIs (toModulePath name) (fun (cfg, cgstate, minfo) -> genModule (cfg, cgstate, minfo) apis), dependencies


  
  let genConfigFiles outputDir modName _maybeGiven =
    let baseDir = String.concat dir_sep [Option.value outputDir ~default:""] in
    let dunePrj = String.concat dir_sep [baseDir; "dune-project"] in
    let duneBindings = String.concat dir_sep [baseDir; "dune"] in
    let dirname = Filename.dirname baseDir in
    let _ = Sys.command ("mkdir -p " ^ dirname) in
    let _ = writeFile dunePrj (String.concat "\n" ["(lang dune 2.0)"; "(package"; " (name GI" ^ modName ^ "))"]) in
    writeFile duneBindings (String.concat "\n" ["(env"; " (_"; "  (binaries"; "   ./tools/dune_config.exe)))"])


(* bool -> library -> unit *)
let genBindings verbosity library =
  (*let inheritedOverrides = [] in*)
  let outputDir = "bindings" ^ dir_sep ^ library.name in
  let dirExists = file_exists outputDir in
  if dirExists
  then 
  removeDirectoryContents outputDir;
  (*let givenOvs = 
    match library.overridesFile with
    | Some x ->  Some {overrideTag = x; overrideText = readFile (x)}
    | None -> None
  in let ovs =
    match givenOvs with
      | Some x -> [x] @ inheritedOverrides
      | None -> inheritedOverrides*)
  let m, deps' = genLibraryCode library.name library.version verbosity (*ovs*) in
  let deps = List.filter (fun x -> not (List.mem x ["xlib"; "GModule"])) deps' in
  let _ = writeModuleTree verbosity (Some outputDir) m deps in
  genConfigFiles (Some outputDir) library.name [];
  let _ = Sys.command ("cp -rf " ^ "base-ocaml/tools " ^ (outputDir ^ dir_sep ^ "tools")) in
  prerr_endline ("Compiling " ^ outputDir ^ " using Dune...");
  chdir outputDir;
  Sys.command "dune build" 
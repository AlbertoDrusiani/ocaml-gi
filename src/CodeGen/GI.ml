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


(*let genCode cfg apis mPath cg =
  evalCodeGen cfg apis mPath cg
*)

(*per ora non considero gli overrides e le dipendenze*)
(*string -> string -> bool -> ModuleInfo*)
let genLibraryCode name version verbosity (*_overrides*) =
  (*TODO dummy value, da implementare questa parte*)
  (*let ovs = defaultOverrides in*)
  (*let gir(*, girDeps*) = loadRawGIRInfo verbosity name (Some version) (*[]*) (*(ovs.girFixups)*) in*)
  let gir, _ = loadGIRInfo verbosity name (Some version) [] in
  prerr_endline ("Parsing di " ^ name ^ " completato!");
  (*let dependencies = List.map (fun x -> x.girNSName ) girDeps in*)
  (*let apis, deps = filterAPIsAndDeps (*ovs*) gir (*girDeps*) in*)
  let apis = gir.girAPIs |> List.to_seq |> NameMap.of_seq in
  (*let allAPIs = NameMap.union union_f apis deps in*)
  let cfg = {modName = name; verbose = verbosity; (*overrides = ovs*)} in
  (*genCode cfg allAPIs (toModulePath name) (genModule apis), dependencies*)
  let cfg, cgstate, minfo = setContext cfg apis (toModulePath name) in
  let _, _, minfo = genModule (cfg, cgstate, minfo) apis in
  minfo

  
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
  begin
  if dirExists
  then removeDirectoryContents outputDir 
  end;
  (*let givenOvs = 
    match library.overridesFile with
    | Some x ->  Some {overrideTag = x; overrideText = readFile (x)}
    | None -> None
  in let ovs =
    match givenOvs with
      | Some x -> [x] @ inheritedOverrides
      | None -> inheritedOverrides*)
  let m(*  deps'*) = genLibraryCode library.name library.version verbosity (*ovs*) in
  let _ = writeModuleTree verbosity (Some outputDir) m [] in
  (*let _ = genConfigFiles (Some outputDir) library.name givenOvs in*)
  Sys.command ("cp -rf " ^ "base-ocaml/tools " ^ (outputDir ^ dir_sep ^ "tools"))




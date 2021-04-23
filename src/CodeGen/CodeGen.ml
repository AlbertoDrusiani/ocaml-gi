open Code
open Naming
open QualifiedNaming
open GIR.BasicTypes
open API
open GIR.Struct
open GIR.Union
open Naming
open Struct
open EnumFlags

(*let genFunction n f =*)

let genUnionCasts minfo n u =
  let mbCType = u.unionCType in
  match mbCType with
  | Some cType -> hline minfo ("#define" ^ (structVal n) ^ "(val) ((" ^ cType ^ ") MLPointer_val(val))") 
  | None -> minfo
  (*TODO da sistemare asterisco da escapare e da capire come restituire unit*)


let genUnion cfg minfo n u =
  let uinfo = addTypeFile cfg minfo n in
  genUnionCasts uinfo n u


let genStructCasts minfo n s =
  let mbCType = s.structCType in
  match mbCType with
  | Some "GdkAtom" -> hline minfo ("#define " ^ (structVal n) ^ "(val) ((GdkAtom) MLPointer_val(val))")
  | Some cType -> hline minfo ("define " ^ (structVal n) ^ "(val) ((" ^ cType ^ ") MLPointer_val(val))")
  | None -> minfo

let genStruct cfg minfo n s =
  if not(ignoreStruct n s)
  then
    let uinfo = addTypeFile cfg minfo n in
    genStructCasts uinfo n s
  else
    minfo


(* code_gen_config*cgstate*module_info -> name -> api -> code_gen_config*cgstate*module_info *) 
let genAPI (cfg, cgstate, minfo) n api =
  match api with
  | APIConst _ -> cfg, cgstate, minfo
  | APIFunction _ -> cfg, cgstate, minfo
  | APIEnum e -> genEnum cfg cgstate minfo n e
  | APIFlags f -> genFlags cfg cgstate minfo n f
  | APICallback _ -> cfg, cgstate, minfo
  | APIStruct s -> cfg, cgstate, genStruct cfg minfo n s
  | APIUnion u -> cfg, cgstate, genUnion cfg minfo n u
  | APIObject o -> genObject cfg cgstate minfo n o
  | APIInterface i -> genInterface cfg cgstate minfo n i

(* code_gen_config*cgstate*module_info -> name -> api -> code_gen_config*cgstate*module_info  *)
let genAPIModule (cfg, cgstate, minfo) n api =
  submodule (genAPI (cfg, cgstate, minfo) n api) (submoduleLocation n api)

    
(* code_gen_config*cgstate*module_info -> Map (name, api) -> code_gen_config*cgstate*module_info *)
let genModule' (cfg, cgstate, minfo) apis =
  let listapi = NameMap.to_seq apis |> List.of_seq in
  let tmp1 = List.fold_left (fun (cfg, cgstate, minfo) (nm, api) -> genAPIModule (cfg, cgstate, minfo) nm api) (cfg, cgstate, minfo) listapi in
  let tmp2 = submodule tmp1 {modulePathToList = ["Callbacks"]} in
  cfg, cgstate, tmp2



(*code_gen_config*cgstate*module_info -> Map (name, api) -> code_gen_config*cgstate*module_info *)
let genModule state apis =
  (*let embeddedAPIs = NameMap.to_seq api |> c
  recurseWithAPIs all_apis (genModule' apis)*)
  (*let allAPIs = minfo.loadedAPIs in*)
  genModule' state apis






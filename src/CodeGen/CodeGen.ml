open GIR.BasicTypes
open GIR.Struct
open GIR.Union

open Code
open Naming
open QualifiedNaming
open API
open EnumFlags
open Object
open Struct
open Fixups
open ModulePath


let genUnionCasts minfo n u =
  let mbCType = u.unionCType in
  match mbCType with
  | Some cType -> 
  hline ("#define" ^ (structVal n) ^ "(val) ((" ^ cType ^ "*) MLPointer_val(val))") minfo
  | None -> minfo


let genUnion cfg minfo n u =
  let uinfo = addTypeFile cfg minfo n in
  genUnionCasts uinfo n u


let genStructCasts minfo n s =
  let mbCType = s.structCType in
  match mbCType with
  | Some "GdkAtom" -> hline ("#define " ^ (structVal n) ^ "(val) ((GdkAtom) MLPointer_val(val))") minfo
      (*commento per ricordarmi dov'è AsyncQueue DEBUG*)
  | Some cType -> 
  hline ("#define " ^ (structVal n) ^ "(val) ((" ^ cType ^ "*) MLPointer_val(val))") minfo
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
  | APIEnum e -> 
    let cgstate, minfo = genEnum cgstate minfo n e
    in cfg, cgstate, minfo
  | APIFlags f -> 
    let cgstate, minfo = genFlags cgstate minfo n f
    in cfg, cgstate, minfo
  | APICallback _ -> cfg, cgstate, minfo
  | APIStruct s -> cfg, cgstate, genStruct cfg minfo n s
  | APIUnion u -> cfg, cgstate, genUnion cfg minfo n u
  | APIObject o -> 
    let cgstate, minfo = genObject cfg cgstate minfo n o
    in cfg, cgstate, minfo
  | APIInterface i -> 
    let cgstate, minfo = genInterface cfg cgstate minfo n i
    in cfg, cgstate, minfo

(* code_gen_config*cgstate*module_info -> name -> api -> code_gen_config*cgstate*module_info  *)
let genAPIModule (cfg, cgstate, minfo) n api =
  submodule (cfg, cgstate, minfo) (submoduleLocation n api) (fun (cfg, cgstate, minfo) -> genAPI (cfg, cgstate, minfo) n api) 
    
(* code_gen_config*cgstate*module_info -> Map (name, api) -> code_gen_config*cgstate*module_info *)
let genModule' (cfg, cgstate, minfo) apis =
  let listapi = NameMap.to_seq apis |> List.of_seq 
    |> List.map checkClosureDestructors 
    |> List.map dropDuplicatedFields
    |> List.map detectGObject
    |> List.map guessPropertyNullability
    |> List.map fixAPIStructs
  in let listApiOption = List.map (fun (x, y) -> x, dropMovedItems y) listapi in
  let listApiOption = List.map (fun (x, y) -> 
    match y with
    | None -> None
    | Some y -> Some (x, y)) listApiOption in
  let listapi = List.filter_map (fun x -> x) listApiOption in
  let listapi = List.filter (fun x -> 
  not (List.mem (fst x) [
    {namespace = "GLib"; name = "Array"};
    {namespace = "GLib"; name = "Error"};
    {namespace = "GLib"; name = "HashTable"};
    {namespace = "GLib"; name = "List"};
    {namespace = "GLib"; name = "SList"};
    {namespace = "GLib"; name = "Variant"};
    {namespace = "GObject"; name = "Value"};
    {namespace = "GObject"; name = "Closure"};
  ] )) listapi
  in 
  let cfg, cgstate, minfo = List.fold_left (fun (cfg, cgstate, minfo) (nm, api) -> 
    genAPIModule (cfg, cgstate, minfo) nm api) (cfg, cgstate, minfo) listapi in
  let cfg, cgstate, minfo = submodule (cfg, cgstate, minfo) {modulePathToList = ["Callbacks"]} (fun (cfg, cgstate, minfo) -> cfg, cgstate, minfo) in
  cfg, cgstate, minfo


(*code_gen_config*cgstate*module_info -> Map (name, api) -> code_gen_config*cgstate*module_info *)
let genModule (cfg, cgstate, minfo) apis =
  let embeddedAPIs = NameMap.to_seq apis |> List.of_seq |> 
      List.concat_map extractCallbacksInStruct |> List.to_seq |> NameMap.of_seq
  in let allAPIs = cfg.loadedAPIs in
  let f_union _ m1 _ = Some m1 in
  recurseWithAPIs 
    (cfg, cgstate, minfo) 
    (fun (cfg, cgstate, minfo) -> 
      genModule' (cfg, cgstate, minfo) (NameMap.union f_union apis embeddedAPIs)) 
    (NameMap.union f_union allAPIs embeddedAPIs)





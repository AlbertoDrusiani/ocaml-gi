open Code
open Naming
open QualifiedNaming
open GIR.BasicTypes
open API
(*open GIR.Struct
open GIR.Union
open Naming

(*let genFunction n f =*)
*)

let genUnionCasts cfg cgstate minfo n u =
  let mbCType = u.unionCType in
  (*TODO da sistemare asterisco da escapare e da capire come restituire unit*)
  Option.map (fun cType -> hline minfo ("#define" ^ (structVal n) ^ "(val) ((" ^ cType ^ ") MLPointer_val(val))")) mbCType


let genUnion cfg cgstate minfo n u =
  addTypeFile n;
  genUnionCasts cfg cgstate minfo n u


let genStructCasts n s =
  let mbCType = s.structCType in
  Option.map (
      fun cType -> match cType with
        | "GdkAtom" -> hline ("#define " ^ (structVal n) ^ "(val) ((GdkAtom) MLPointer_val(val))")
        | _ -> hline ("define " ^ (structVal n) ^ "(val) ((" ^ cType ^ ") MLPointer_val(val))")
    ) mbCType



let genStruct n s =
  if !(ignoreStruct n s)
  then
    addTypeFile n;
    genStructCasts n s


 


let genAPI cfg cgstate minfo n api =
  match api with
  | APIConst _ -> cfg cgstate minfo
  | APIFunction _ -> cfg cgstate minfo
  | APIEnum e -> genEnum cfg cgstate minfo n e
  | APIFlags f -> genFlags cfg cgstate minfo n f
  | APICallback _ -> cfg cgstate minfo
  | APIStruct s -> genStruct cfg cgstate minfo n s
  | APIUnion u -> genUnion cfg cgstate minfo n u
  | APIObject o -> genObject cfg cgstate minfo n o
  | APIInterface i -> genInterface cfg cgstate minfo n i


let genAPIModule cfg cgstate minfo n api =
  submodule (genAPI cfg cgstate minfo n api) (submoduleLocation n api)

    
let genModule' cfg cgstate minfo apis =
  let listapi = NameMap.to_seq apis |> List.of_seq in
  let _ = List.map (fun (x,y) -> genAPIModule cfg cgstate minfo x y) listapi in
  let _ = submodule cfg cgstate minfo "Callbacks" in
  cfg, cgstate, minfo



    


(*Map (name, api) -> Map (name, api) -> codegen *)
let genModule (cfg, cgstate, minfo) apis =
  (*let embeddedAPIs = NameMap.to_seq api |> c
  recurseWithAPIs all_apis (genModule' apis)*)
  (*let allAPIs = minfo.loadedAPIs in*)
  genModule' cfg, cgstate, minfo, apis






open API
open GObject
open GIR.BasicTypes
open Naming
open Code
open GIR.Object
open GIR.Interface
open Files
open QualifiedNaming
open Util

(* config*cgstate*module_info -> name -> string -> string -> config*cgstate*module_info)*)
let genObjectCasts (cfg, cgstate, minfo) n ctype checkMacro =
  match not (NameSet.mem n noCType) with
  | true -> (cfg, cgstate, minfo)
  | false -> 
    let minfo = 
      if NameSet.mem n noCheckMacro
        then 
         hline minfo ("#define " ^ objectVal n ^ "(val) ((" ^ ctype ^ "*) (*val)")
        else
         hline minfo ("#define " ^ objectVal n ^ "(val) check_cast(" ^ checkMacro ^ ", val)")
    in let minfo = hline minfo ("#define " ^ valObject n ^ " Val_GAnyObject") in 
    let minfo = cline minfo ("Make_Val_option(" ^ ctype ^ "," ^ valObject n ^ ")") in 
    let minfo = hline minfo ("value " ^ valOptObject n ^ " (" ^ ctype ^ "*);") in 
    cfg, cgstate, minfo
  

let genSignalClass (cfg, minfo) n o =
  let parents = instanceTree cfg n in
  let ocamlName = ocamlIdentifier n in
  let minfo = gline minfo ("class " ^ ocamlName ^ "_signals obj = object (self)") in
  match parents with
  | [] -> minfo
  | parent::_ ->
    let parentClass = nsOCamlClass minfo parent in
    let parentSignal =
      match List.hd parents with
      | {namespace = "Gtk"; name = "Widget"} -> "GObj.widget_signals_impl"
      | {namespace = "GObject"; name = "Object"} -> "[_] GObj.gobject_signals"
      | {namespace = "Gtk"; name = _} -> parentClass ^ "_signals"
      | {namespace = "GtkSource"; name = _} -> parentClass ^ "_signals"
      | _ -> "[_] GObj.gobject_signals"
    in let minfo = gline minfo ("  inherit " ^ parentSignal ^ " obj") in
    let minfo = List.fold_left (
        fun minfo iface -> 
            match (o.objSignals = []) || (NameSet.mem iface (NameSet.union buggedIfaces excludeFiles)) with
            | true -> minfo
            | false ->
            let ifaceClass = nsOCamlClass minfo iface in
            let api = findAPIByName cfg iface in
            match api with
            | APIInterface i ->
                begin
                match i.ifSignals with
                | [] -> minfo
                | _ -> gline minfo ("  inherit " ^ ifaceClass ^ "_signals obj")
                end
            | _ -> assert false 
    ) minfo o.objInterfaces in
    let minfo = List.fold_left (fun minfo s -> genGSignal minfo s n) minfo o.objSignals in
    let minfo = gline minfo "end" in
  gblank minfo



let cTypeInit cTypeName typeInit =
  String.concat "/n" [
      "CAMLprim value ml_gi" ^ cTypeName ^ "_init(value unit) {";
      "    GType t = " ^ typeInit ^ "();";
      "    return Val_GType(t);";
      "}"
  ]


let genCObjectTypeInit minfo o n =
  match o with
  | obj when obj.objTypeInit != "" -> 
      let minfo = cline minfo (cTypeInit (camelCaseToSnakeCase n.namespace ^ n.name) obj.objTypeInit) in
      minfo
  | _ -> minfo


let genMlTypeInit minfo nm =
  let namespaceOcamlName = camelCaseToSnakeCase (nm.namespace ^ nm.name) in
  let minfo = line minfo 
    ("external ml_gi" ^ namespaceOcamlName ^ 
    "_init : unit -> unit = \"ml_gi" ^ namespaceOcamlName ^ "_init\"") in
  line minfo ("let () = ml_gi" ^ namespaceOcamlName ^ "_init ()")
 

 let genObject' (cfg, minfo) n o ocamlName =
  let parents = instanceTree cfg n in 
  let name' = upperName n in 
  let nspace = n.namespace in
  let objectName = n.name in
  let minfo = genCObjectTypeInit minfo o n in
  let minfo = genSignalClass (cfg, minfo) n o in
  let minfo = gline minfo ("class " ^ ocamlName ^ "_skel obj = object (self)") in
  let minfo = 
  match parents with
  | [] -> minfo
  | parent::_ ->
    let parentClass = nsOCamlClass minfo parent in
    let parentSkelClass =
      begin
      match parent with
      | {namespace = "Gtk"; name = "Widget"} -> "['a] GObj.widget_impl"
      | {namespace = "GObject"; name = "Object"} -> "GObj.gtkobj"
      | {namespace = "Gtk"; name = _} -> parentClass ^ "_skel"
      | {namespace = "GtkSource"; _} -> parentClass ^ "_skel"
      | _ -> "GObj.gtkobj"
      end
    in let minfo = gline minfo ("  inherit " ^ parentSkelClass ^ " obj") in
    let minfo = List.fold_left (
        fun minfo iface ->
          match NameSet.mem iface (NameSet.union buggedIfaces excludeFiles) with
          | true -> minfo
          | false ->
            let ifaceClass = nsOCamlClass minfo iface in
            gline minfo ("  method i" ^ ocamlIdentifier iface ^ " = new " ^ ifaceClass ^ "_skel obj")
    ) minfo o.objInterfaces 
    in gline minfo ("  method as_" ^ ocamlName ^ " = (obj :> " ^ nsOCamlType n.namespace n ^ " Gobject.obj") in
  let minfo = genMlTypeInit minfo n in
  (*let minfo = group minfo in*)
  let





let getObjCheckMacro o =
  String.uppercase_ascii (breakOnFirst "_get_type" o.objTypeInit)

(* config*cgstate*module_info -> nListame -> object -> module_info)*)
let genObject (cfg, cgstate, minfo) n o =
  let isGO = isGObject cfg (TInterface n) in 
  if not isGO
  then (cfg, cgstate, minfo)
  else
    let objectName = n.name in 
    let ocamlName = escapeOCamlReserved (camelCaseToSnakeCase objectName) in 
    let minfo = addTypeFile cfg minfo n in 
    let minfo = addCDep minfo (n.namespace ^ n.name) in 
    let (cfg, cgstate, minfo) = 
      match o.objCType with
      | None -> cfg, cgstate, minfo
      | Some ctype -> genObjectCasts (cfg, cgstate, minfo) n ctype (getObjCheckMacro o)
    in if NameSet.mem n excludeFiles 
    then cfg, cgstate, minfo
    else genObject' (cfg, cgstate, minfo) n o ocamlName

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
open GIR.Property
open GIR.Method
open Filename
open Signal

(* config*cgstate*module_info -> name -> string -> string -> config*cgstate*module_info)*)
let genObjectCasts (cfg, cgstate, minfo) n ctype checkMacro =
  match not (NameSet.mem n noCType) with
  | true -> (cfg, cgstate, minfo)
  | false -> 
    let minfo = 
      if NameSet.mem n noCheckMacro
        then 
         hline ("#define " ^ objectVal n ^ "(val) ((" ^ ctype ^ "*) (*val)") minfo 
        else
         hline ("#define " ^ objectVal n ^ "(val) check_cast(" ^ checkMacro ^ ", val)") minfo 
    in let minfo = hline ("#define " ^ valObject n ^ " Val_GAnyObject") minfo  in 
    let minfo = cline ("Make_Val_option(" ^ ctype ^ "," ^ valObject n ^ ")")  minfo in 
    let minfo = hline ("value " ^ valOptObject n ^ " (" ^ ctype ^ "*);") minfo  in 
    cfg, cgstate, minfo
  

let genSignalClass (cfg, minfo) n o =
  let parents = instanceTree cfg n in
  let ocamlName = ocamlIdentifier n in
  let minfo = gline ("class " ^ ocamlName ^ "_signals obj = object (self)") minfo  in
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
    in let minfo = gline ("  inherit " ^ parentSignal ^ " obj") minfo  in
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
                | _ -> gline ("  inherit " ^ ifaceClass ^ "_signals obj") minfo 
                end
            | _ -> assert false 
    ) minfo o.objInterfaces in
    let minfo = List.fold_left (fun info s -> genGSignal s n cfg info) minfo o.objSignals in
    let minfo = gline "end" minfo  in
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
      let minfo = cline (cTypeInit (camelCaseToSnakeCase n.namespace ^ n.name) obj.objTypeInit) minfo  in
      minfo
  | _ -> minfo


let genMlTypeInit minfo nm =
  let namespaceOcamlName = camelCaseToSnakeCase (nm.namespace ^ nm.name) in
  let minfo = line ("external ml_gi" ^ namespaceOcamlName ^ 
    "_init : unit -> unit = \"ml_gi" ^ namespaceOcamlName ^ "_init\"") minfo 
  in line ("let () = ml_gi" ^ namespaceOcamlName ^ "_init ()") minfo
 

let isSetterOrGetter o m =
  let props = o.objProperties in
  let propNames = List.map (fun x -> x.propName |> hyphensToUnderscores) props in
  let mName = m.methodName.name in
  (isPrefixOf "get" mName) || (isPrefixOf "set" mName)
  && List.fold_left (fun acc pref -> (check_suffix mName pref) && acc) true propNames


let isMakeParamsParent ns nm =
  match ns, nm with
  | _, {namespace = "GObject"; name = "Object"} -> false
  | currNS, {namespace = ns; name = _} -> 
    if currNS != ns 
    then false
    else true



let genDefaultObjectConstructor n ocamlName cfg minfo =
  let currNS = currentNS minfo in
  let parents = instanceTree cfg n in
  let makeParamsParents = 
    List.filter (isMakeParamsParent currNS) (List.rev parents)
  in let mkParentsNum = List.length makeParamsParents in
  let creator = 
    indentBy (mkParentsNum + 3) ^ "new " ^ ocamlName ^ " (" ^ n.name ^ ".create pl" in
  genObjectConstructor' ocamlName creator n



let genObject' (cfg, minfo) n o ocamlName =
  let parents = instanceTree cfg n in 
  let name' = upperName n in 
  let nspace = n.namespace in
  let objectName = n.name in
  let minfo = genCObjectTypeInit minfo o n in
  let minfo = genSignalClass (cfg, minfo) n o in
  let minfo = gline ("class " ^ ocamlName ^ "_skel obj = object (self)") minfo in
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
    in let minfo = gline ("  inherit " ^ parentSkelClass ^ " obj") minfo in
    let minfo = List.fold_left (
        fun minfo iface ->
          match NameSet.mem iface (NameSet.union buggedIfaces excludeFiles) with
          | true -> minfo
          | false ->
            let ifaceClass = nsOCamlClass minfo iface in
            gline ("  method i" ^ ocamlIdentifier iface ^ " = new " ^ ifaceClass ^ "_skel obj") minfo 
    ) minfo o.objInterfaces 
    in gline ("  method as_" ^ ocamlName ^ " = (obj :> " ^ nsOCamlType n.namespace n ^ " Gobject.obj") minfo in
  let minfo = genMlTypeInit minfo n in
  (*let minfo = group minfo in*)
  let minfo = 
    match o.objSignals = [] with
    | true -> minfo
    | false -> group 
              (fun m -> indent 
                        (fun i -> 
                        let acc = line "open GtkSignal" i 
                        |> line "openGobject"
                        |> line "open Data"
                        in List.fold_left (fun info s -> genSignal s n cfg info) acc o.objSignals) 
                        (line "module S = struct" m) |> line "end")
              minfo
  in let minfo = group (line ("let cast w : " ^ 
                        nsOCamlType n.namespace n ^
                        " Gobject.obj = Gobject.try_cast w \"" ^
                        nspace ^ objectName ^ "\"")) minfo
  in let minfo = group (line ("let create pl : " ^
                        nsOCamlType n.namespace n ^
                        " Gobject.obj = GtkObject.make \" pl" ^
                        nspace ^ objectName ^ "\"")) minfo
  in let minfo = gline ("  (* Methods *)") minfo in
  let methods = o.objMethods in
  let methods' = List.filter (fun x -> not(isSetterOrGetter o x)) methods in
  (*TODO qua probabilmente ci andrà una try perché lui chiama a handleCGexc,
  forse la genMethod può fallire*)
  let minfo = List.fold_left (fun info f -> genMethod info n f) minfo methods' in
  let minfo = gline "end" minfo in
  let minfo = gblank minfo in
  let minfo = gline (" and" ^ ocamlName ^ " obj = object (self)") minfo in
  let minfo = gline ("  inherit " ^ ocamlName ^ "_skel obj") minfo in
  let minfo = gline ("  method connect = new " ^ ocamlName ^ "_signals obj") minfo in
  let minfo = gline "end" minfo in
  let minfo = gblank minfo in
  let minfo = genDefaultObjectConstructor n ocamlName cfg minfo in
  let constructors = 
    List.filter (fun m -> m.methodType = Constructor && m.methodName.name != "new") o.objMethods in
  let minfo =
    List.fold_left (fun info m -> 
                    let canGenerate = canGenerateCallable m.methodCallable in
                    if canGenerate
                    then genAdditionalObjectConstructor n ocamlName m info 
                    else gblank info)
                    minfo constructors in
  minfo




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
    else cfg, cgstate, genObject' (cfg, minfo) n o ocamlName

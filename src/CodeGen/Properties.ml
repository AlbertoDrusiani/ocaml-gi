open Inheritance
open Naming
open GIR.Property
open GIR.BasicTypes
open GIR.Method
open QualifiedNaming
open Util
open Code
open TypeRep
open Conversions
open GObject

let infoType cfg minfo owner prop =
  let infoType =
    upperName owner ^ (hyphensToCamelCase prop.propName) ^ "PropertyInfo"
  in qualifiedSymbol cfg minfo infoType owner


let hPropName p =
  p.propName |> hyphensToCamelCase |> lcFirst


let accessorOrUndefined cfg minfo available owner cName =
  if not available
  then minfo, "undefined"
  else 
    let minfo, qn = qualifiedSymbol cfg minfo cName owner in
    minfo, escapeOCamlReserved qn


let propTypeConverter cfg minfo prop =
  let isNullable = Option.value prop.propReadNullable ~default:false in
  ocamlDataConv cfg minfo isNullable prop.propType


let genPropertyOCaml cfg minfo classe prop =
  let currNS = currentNS minfo in
  let pName = prop.propName in
  let uScoresName = hyphensToUnderscores pName |> escapeOCamlReserved in
  let classType = typeShow currNS (RowCon (More, (PolyCon (NameCon classe)))) in
  let ocamlConverter = propTypeConverter cfg minfo prop in
  let minfo = line ("let " ^ uScoresName ^ " : " ^ "(" ^ classType ^ ",_) property =") minfo in
  let _, minfo = indent
    (fun minfo -> 
      (0, line ("{" ^ "name=\"" ^ pName ^ "\"; " ^ "conv=" ^ ocamlConverter ^ "}") minfo)) minfo
  in minfo



let isPropertyInParents cfg cn pName =
  let pName' = hyphensToUnderscores pName in
  let parents = instanceTree cfg cn in
  let parentsHasProp =
    List.map (
        fun parentName ->
          let api = findAPIByName cfg parentName in
          match api with
          | APIObject o ->
            let parentPropNames = List.map (fun p -> p.propName |> hyphensToUnderscores) o.objProperties in
            let parentPropGetters = List.map (fun p -> "get_" ^ p) parentPropNames in
            let parentPropSetters = List.map (fun p -> "set_" ^ p) parentPropNames in
            let parentPropNames' = parentPropGetters @ parentPropSetters in
            let parentMethodNames = List.map (fun m -> m.methodName.name) o.objMethods in
            let parentNames = parentPropNames' @ parentMethodNames in
            List.mem pName' parentNames
          | _ -> false
    ) parents
  in List.mem true parentsHasProp



let genPropertySetter cfg minfo setter classe prop =
  let alreadyDefProp = isPropertyInParents cfg classe ("set_" ^ prop.propName) in
  let setterDecl =
    if alreadyDefProp
    then setter ^ "_" ^ ocamlIdentifier classe
    else setter
  in let minfo = gline ("  method set_" ^ setterDecl ^ " = Gobject.set " ^ classe.name ^ ".P." ^ setter ^ " obj") minfo in
  minfo 



let genPropertyGetter cfg minfo getter classe prop =
  let alreadyDefProp = isPropertyInParents cfg classe ("get_" ^ prop.propName) in
  let getterDecl =
    if alreadyDefProp
    then getter ^ "_" ^ ocamlIdentifier classe
    else getter
  in let minfo = gline ("  method " ^ getterDecl ^ " = Gobject.get " ^ classe.name ^ ".P." ^ getter ^ " obj") minfo in
  minfo 




let genOneProperty cfg minfo owner prop =
  let name = upperName owner in
  let cName = prop.propName |> hyphensToCamelCase in
  let pName = name ^ cName in
  let flags = prop.propFlags in
  let writable =
    (List.mem PropertyWritable flags) && (not (List.mem PropertyConstructOnly flags))
  in let readable = List.mem PropertyReadable flags in
  let constructOnly = List.mem PropertyConstructOnly flags in
    prerr_endline("property ");

  if prop.propTransfer != TransferNothing
  then
    notImplementedError ("Property " ^ pName ^ " has unsupported transfer type");
  if not (readable || writable || constructOnly)
  then 
    notImplementedError ("Property is not readable, writable, or constructible: " ^ pName);
  
  let minfo, getter = accessorOrUndefined cfg minfo readable owner cName in
  let minfo, setter = accessorOrUndefined cfg minfo writable owner cName in
  let minfo = genPropertyOCaml cfg minfo owner prop in
  let minfo = 
  if getter != "undefined"
  then genPropertyGetter cfg minfo getter owner prop
  else minfo
  in let minfo =
  if setter != "undefined"
  then genPropertySetter cfg minfo setter owner prop
  else minfo

  in minfo 
  

let genMakeParams cfg minfo className props =
  let mayCons constrName = "may_cons P." ^ constrName ^ " " ^ constrName in
  let emptyMake = "let make_params ~cont pl = cont pl" in
  let inheritedMake parent = "let make_params = " ^ parent.name ^ ".make_params" in
  let isConstructor prop = List.mem PropertyWritable prop.propFlags in
  let constructors = List.filter isConstructor props in
  if constructors != []
  then
    let constructorNames' =
      List.map (fun prop -> 
          try
            let _ = propTypeConverter cfg minfo prop in
            Some (escapeOCamlReserved prop.propName)
          with CGError (CGErrorNotImplemented _) -> None
          
      ) constructors
    in let constructorNames = List.filter_map Fun.id constructorNames' in
    let underlinedConstrNames = List.map hyphensToUnderscores constructorNames in
    let optionalArgs = "?" ^ String.concat " ?" underlinedConstrNames in
    let minfo = blank minfo in
    let minfo = line ("let make_params ~cont pl " ^ optionalArgs ^ " =") minfo in
    let _, minfo = 
      indent (
        fun minfo -> 
          let numConstructors = List.length underlinedConstrNames in
          (*if className.name = "BufferedInputStream"
          then let _ = prerr_endline (string_of_int(numConstructors)) in assert false
          else*)
          let firstConstrs = take (numConstructors - 1) underlinedConstrNames in
          let lastConstr = List.rev underlinedConstrNames |> List.hd in
          let minfo = line "let pl = (" minfo in
          let cgstate, minfo =
            indent (
              fun minfo ->
                let minfo = List.fold_left (fun minfo cName -> line (mayCons cName ^ " (") minfo) minfo firstConstrs in
                let minfo = line (mayCons lastConstr ^ " pl" ^ (String.make numConstructors ')') ^ " in") minfo in
                0, minfo) minfo
          in let minfo = line "cont pl" minfo
          in cgstate, minfo
            ) minfo
    in minfo
  else
    let parents = instanceTree cfg className in
    let currNS = currentNS minfo in
    let minfo = 
      match parents with
      | [] -> line emptyMake minfo
      | parent::_ -> 
        line
          (match (currNS, parent) with
          | ("Gtk", {namespace = "GObject"; name = "Object"}) -> emptyMake
          | ("Gtk", {namespace = "Gtk"; name = "Widget"}) -> emptyMake
          | ("Gtk", {namespace = "Gtk"; name = _}) -> inheritedMake parent
          | (_, _) -> emptyMake
          ) minfo
    in minfo
    


  let genProperties cfg cgstate minfo n ownedProps _allProps =
  let minfo = line "let may_cons = Gobject.Property.may_cons" minfo in
  let minfo = line "let may_cons_opt = Gobject.Property.may_cons_opt" minfo in
  let minfo = blank minfo in
  let minfo = gline "  (* Properties *)" minfo in
  let minfo = line "module P = struct" minfo in
  let _, minfo = indent
    (fun minfo -> 
      let minfo = line "open Gobject" minfo in
      let minfo = line "open Data" minfo in
      let name = upperName n in
      let cgstate, minfo = List.fold_left (
        fun (cgstate, minfo) prop ->
          let action = fun cgstate minfo -> cgstate, genOneProperty cfg minfo n prop in
          let fallback = fun cgstate minfo err -> cgstate, 
            commentLine minfo ("XXX Generation of property \"" ^ prop.propName ^ "\" of object \""
                         ^ name ^ "\" failed: " ^ describeCGError err)
          in let cgstate, minfo = handleCGExc (cgstate, minfo) fallback action
          in cgstate, minfo
      ) (cgstate, minfo) ownedProps
      in cgstate, minfo
      ) minfo
  in let minfo = line "end" minfo in
  genMakeParams cfg minfo  n ownedProps


let genObjectProperties cfg cgstate minfo n o =
  let isGO = apiIsGObject cfg n (APIObject o) in
  if isGO then
    let allProps = 
    List.fold_left_map 
      (fun minfo (owner, prop) ->
        let minfo, pi = infoType cfg minfo owner prop in
        minfo, ("'(\"" ^ hPropName prop ^ "\", " ^ pi ^ ")")
      ) minfo (fullObjectPropertyList cfg n o)
    in genProperties cfg cgstate minfo n o.objProperties allProps
  else
    minfo


let genInterfaceProperties cfg cgstate minfo n iface =
  let allProps = fullInterfacePropertyList cfg n iface in
  let minfo, allProps = 
    List.fold_left_map (fun minfo (owner, prop) -> 
      let minfo, pi = infoType cfg minfo owner prop in
      minfo, ("'(\"" ^ hPropName prop ^ "\", " ^ pi ^ ")")) minfo allProps
  in genProperties cfg cgstate minfo n (iface.ifProperties) allProps
open API 
open Code
open GIR.Interface
open GIR.BasicTypes


let rec apiDoParentSearch cfg parent n api =
  if parent = n
  then true
  else
    match api with
    | APIObject o ->
      begin
      match o.objParent with
      | Some p -> typeDoParentSearch cfg parent (TInterface p)
      | None -> false
      end
    | APIInterface iface ->
      let prs = iface.ifPrerequisites in 
      let prereqs = List.combine prs (List.map (findAPIByName cfg) prs) in
   (* if n.name = "ActionGroup" 
      then prerr_endline (string_of_int(List.length (prereqs)));
      if n.name = "ActionGroup"  then assert false;*)
      let flag = List.exists Fun.id (List.map (fun (x, y) -> apiDoParentSearch cfg parent x y) prereqs) in
      if flag && n.name = "ActionGroup" then let _ = prerr_endline("CORRETTO") in assert false else flag
      (*List.fold_left (fun x y -> x || y) false (List.map (fun (x, y) -> apiDoParentSearch cfg parent x y) prereqs)*)
    | _ -> false

and typeDoParentSearch cfg nm t =
  match nm, t with
  | parent, TInterface n -> findAPIByName cfg n |> apiDoParentSearch cfg parent n
  | _, _ -> false 


let isGObject cfg t =
  typeDoParentSearch cfg ({namespace = "GObject"; name = "Object"}) t


let apiIsGObject cfg n api =
  apiDoParentSearch cfg {namespace = "GObject"; name = "Object"} n api
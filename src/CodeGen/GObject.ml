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
      List.fold_left (fun x y -> x || y) false (List.map (fun (x, y) -> apiDoParentSearch cfg parent x y) prereqs)
    | _ -> false

and typeDoParentSearch cfg nm t =
  match nm, t with
  | parent, TInterface n -> findAPIByName cfg n |> apiDoParentSearch cfg parent n
  | _, _ -> false 



let isGObject cfg t =
  typeDoParentSearch cfg ({namespace = "GObject"; name = "Object"}) t
open GIR.Interface
open GIR.Object
open GIR.Property
open GIR.Signal
open GIR.Method
open GIR.BasicTypes
open API
open Code


module type Inheritable = sig
  type t
  val ifInheritables : interface -> t list
  val objInheritables : object_ml -> t list
  val iName : t -> string
end



module Inheritable_property : Inheritable with type t = property = struct
  type t = property
  let ifInheritables p = p.ifProperties
  let objInheritables p = p.objProperties
  let iName p = p.propName
end


module Inheritable_signal : Inheritable with type t = signal = struct
  type t = signal
  let ifInheritables s = s.ifSignals
  let objInheritables s = s.objSignals
  let iName s = s.sigName
end


module Inheritable_method : Inheritable with type t = method_ml = struct
  type t = method_ml
  let ifInheritables s = s.ifMethods
  let objInheritables s = s.objMethods
  let iName s = s.methodName.name
end


module MakeInheritableUtils (I: Inheritable) = struct

  let apiInheritables cfg n =
   let api = findAPIByName cfg n in
    match api with
    | APIInterface iface -> List.map (fun x -> (n ,x)) (I.ifInheritables iface)
    | APIObject obj -> List.map (fun x -> (n, x)) (I.objInheritables obj)
    | _ -> assert false


  let fullObjectInheritableList cfg n obj =
    let iT = instanceTree cfg n in
    let _ = List.flatten (List.map (apiInheritables cfg) (n::iT)) in
    (*let l2 = *)List.flatten (List.map (apiInheritables cfg) obj.objInterfaces)
    (*let acc = ref [] in
    for i = (List.length l1-1) downto 0 do
      for j = (List.length l2-1) downto 0 do
        acc := (List.nth l1 i*)
    (*TODO*)

  
  let rec fullInterfaceInheritableList cfg n iface =
    let _ = List.map (fun x -> (n, x)) (I.ifInheritables iface) in
    let _ = List.map (fullAPIInheritablesList cfg) iface.ifPrerequisites |> List.flatten in
    assert false
    (*TODO*)
  
  and fullAPIInheritablesList cfg n =
    let api = findAPIByName cfg n in
    match api with
    | APIInterface iface -> fullInterfaceInheritableList cfg n iface
    | APIObject obj -> fullObjectInheritableList cfg n obj
    | _ -> assert false


  let fullAPIInheritables cfg n =
    let api = findAPIByName cfg n in
    match api with
    | APIInterface iface -> fullInterfaceInheritableList cfg n iface
    | APIObject obj -> fullObjectInheritableList cfg n obj
    | _ -> assert false


  let removeDuplicates _verbose inheritables =
    
    let filterDups m (name, prop) =
      match StringMap.find_opt (I.iName prop) m with
      | Some (tainted, _n, _p) when tainted -> m
      | Some (_tainted, _n, p) when p = prop -> m
      | Some (_tainted, n, p) ->
        (*let minfo = 
        if verbose
        then 
          let minfo = commentLine minfo "XXX Duplicated object with different types:" in
          let minfo = commentLine minfo ("  " ^ n ^ " -> " ^ p) in
          commentLine minfo ("  " ^ name ^ " -> " ^ prop)
        else minfo*)
        StringMap.add (I.iName prop) (true, n, p) m
      | None -> StringMap.add (I.iName prop) (false, name, prop) m

    in let filterTainted xs =
      match xs with
      | [(_, (_, name, prop))] -> [(name, prop)] 
      | _ -> assert false

    in 
    let l = List.fold_left filterDups StringMap.empty inheritables
    |> StringMap.to_seq |> List.of_seq
    in filterTainted l

end


module PropertyUtils = MakeInheritableUtils(Inheritable_property)
module SignalUtils = MakeInheritableUtils(Inheritable_signal)
module MethodUtils = MakeInheritableUtils(Inheritable_method)



let fullObjectPropertyList cfg n o =
  PropertyUtils.fullObjectInheritableList cfg n o |> PropertyUtils.removeDuplicates true


let fullInterfacePropertyList cfg n i =
  PropertyUtils.fullInterfaceInheritableList cfg n i |> PropertyUtils.removeDuplicates true


let fullObjectSignalList cfg n o =
  SignalUtils.fullObjectInheritableList cfg n o |> SignalUtils.removeDuplicates true


let fullInterfaceSignalList cfg n i =
  SignalUtils.fullInterfaceInheritableList cfg n i |> SignalUtils.removeDuplicates true


let fullObjectMethodList cfg n o =
  MethodUtils.fullObjectInheritableList cfg n o |> MethodUtils.removeDuplicates true


let fullInterfaceMethodList cfg n i =
  MethodUtils.fullInterfaceInheritableList cfg n i |> MethodUtils.removeDuplicates true
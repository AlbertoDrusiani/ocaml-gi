open GIR.BasicTypes
open GIR.Allocation
open API
open Util

type overrides = {
  ignoredElems: StringSet.t  NameMap.t;
  ignoredAPIs: NameSet.t;
  sealedStructs: NameSet.t;
  allocInfo: allocation_info NameMap.t;
  pkgConfigMap: string StringMap.t;
  cabalPkgVersion: string option;
  nsChooseVersion: string StringMap.t;
  girFixups: gir_rule list;
  onlineDocsMap: string StringMap.t;
  }

let defaultOverrides = 
  {
   ignoredElems = NameMap.empty;
   ignoredAPIs = NameSet.empty;
   sealedStructs = NameSet.empty;
   allocInfo = NameMap.empty;
   pkgConfigMap = StringMap.empty;
   cabalPkgVersion = None;
   nsChooseVersion = StringMap.empty;
   girFixups = [];
   onlineDocsMap = StringMap.empty;
  }

let withFlags ns (state, flags) =
  if (List.fold_left (fun acc x -> acc && x) true flags)
  then Some (String.trim ns)
  else state


(*let parseOneLine (state, flags) t =
  match stripPrefix t "namespace" with
  | Some ns -> withFlags ns (state, flags)
  | None -> 
  if stripPrefix t "#" = Some _
  then state

  else if stripPrefix t "namespace " = Some ns
  then withFlags ns (state, flags)

  else if stripPrefix t "ignore " = Some ns
  then
  else if *)


(*let parseOverrides overrides =*)
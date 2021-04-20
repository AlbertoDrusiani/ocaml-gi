open GIR.BasicTypes
open GIR.Allocation
open API

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




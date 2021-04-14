open BasicTypes
open Method
open Property
open Signal
open Parser
open Deprecation
open Documentation
open Type

type object_ml = {
  objParent: name option;
  objTypeInit: string;
  objTypeName: string;
  objCType: string option;
  objRefFunc: string option;
  objUnrefFunc: string option;
  objSetValueFunc: string option;
  objGetValueFunc: string option;
  objInterfaces: name list;
  objDeprecated: deprecation_info option;
  objDocumentation: documentation;
  objMethods: method_ml list;
  objProperties: property list;
  objSignals: signal list;
    }



let pspec_type_init p = "haskell_gi_pspec_type_init_" ^ p

let resolveInternalType nm =
  match nm with
  | {namespace = "GObject"; name = "ParamSpec" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecBoolean" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecBoxed" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecChar" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecDouble" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecEnum" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecFlags" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecFloat" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecGType" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecInt" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecInt64" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecLong" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecObject" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecOverride" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecParam" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecPointer" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecString" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecUChar" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecUInt" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecUInt64" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecULong" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecUnichar" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecVariant" as p} -> pspec_type_init p
  | {namespace = "GObject"; name = "ParamSpecValueArray" as p} -> pspec_type_init p
  | _ -> assert false



let parseObject el ns =
  let name = parseName ns el in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let methods = List.map (parseMethod ns OrdinaryMethod) (parseChildrenWithLocalName "method" el) in
  let constructor = List.map (parseMethod ns Constructor) (parseChildrenWithLocalName "constructor" el) in
  let functions =  List.map (parseMethod ns MemberFunction) (parseChildrenWithLocalName "function" el) in
  let parent =  optionalAttr "parent" None el (fun x -> Some (qualifyName x ns)) in
  let interfaces =  List.map (parseName ns) (parseChildrenWithLocalName "implements" el) in
  let props =  List.map (parseProperty ns) (parseChildrenWithLocalName "property" el) in
  let typeInitFn = getAttrWithNamespace GLibGIRNS "get-type" el in
  let typeInit = 
    match typeInitFn with
    | "intern" -> resolveInternalType name
    | fn -> fn
  in let typeName = getAttrWithNamespace GLibGIRNS "type-name" el in
  let signals =  List.map (parseSignal ns) (parseChildrenWithNSName GLibGIRNS "signal" el) in
  let refFunc = queryAttrWithNamespace GLibGIRNS "ref-func" el in
  let unrefFunc = queryAttrWithNamespace GLibGIRNS "unref-func" el in
  let setValueFunc = queryAttrWithNamespace GLibGIRNS "set-value-func" el in
  let getValueFunc = queryAttrWithNamespace GLibGIRNS "get-value-func" el in
  let ctype = queryCType el in
  name,
  { objParent = parent;
    objTypeInit = typeInit;
    objCType = ctype;
    objRefFunc = refFunc;
    objUnrefFunc = unrefFunc;
    objSetValueFunc = setValueFunc;
    objGetValueFunc = getValueFunc;
    objTypeName = typeName;
    objInterfaces = interfaces;
    objDeprecated = deprecated;
    objDocumentation = doc;
    objMethods = constructor @ methods @ functions;
    objProperties = props;
    objSignals = signals;
  }


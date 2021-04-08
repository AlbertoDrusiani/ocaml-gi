open BasicTypes
(*open Method
open Property
open Signal*)
open Parser
open Deprecation
open Documentation
open Type

type object_ml = {
  objParent: name option;
  (*objTypeInit: string;*)
  objTypeName: string;
  objCType: string option;
  objRefFunc: string option;
  objUnrefFunc: string option;
  objSetValueFunc: string option;
  objGetValueFunc: string option;
 (* objInterfaces: name list;*)
  objDeprecated: deprecation_info option;
  objDocumentation: documentation;
 (* objMethods: method_ml list;*)
 (* objProperties: property list;*)
 (*  objSignals: signal list; *)
    }


(*let resolveInternalType nm el ns*)

(*TODO da capire come gestire, sembra fatta su misura per haskell-gi*)
(*let pspec_type_init p =*)


let parseObject el ns =
  let name = parseName el ns in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
 (* let methods = par
  let constructor =
  let functions =*)
  let parent =  optionalAttr "parent" None el (fun x -> Some (qualifyName x ns)) in
  (*let interfaces =
  let props =*)
  (*let typeInitFn = getAttrWithNamespace GLibGIRNS "get-type" el in*)
  (*let typeInit =*)
  let typeName = getAttrWithNamespace GLibGIRNS "type-name" el in
  (*let signals =*)
  let refFunc = queryAttrWithNamespace GLibGIRNS "ref-func" el in
  let unrefFunc = queryAttrWithNamespace GLibGIRNS "unref-func" el in
  let setValueFunc = queryAttrWithNamespace GLibGIRNS "set-value-func" el in
  let getValueFunc = queryAttrWithNamespace GLibGIRNS "get-value-func" el in
  let ctype = queryCType el in
  name,
  { objParent = parent;
    (*objTypeInit = typeinit;*)
    objCType = ctype;
    objRefFunc = refFunc;
    objUnrefFunc = unrefFunc;
    objSetValueFunc = setValueFunc;
    objGetValueFunc = getValueFunc;
    objTypeName = typeName;
   (* objInterfaces = interfaces;*)
    objDeprecated = deprecated;
    objDocumentation = doc;
   (* objMethods = constructor @ methods @ functions;*)
   (* objProperties = props;*)
   (* objSignals = signals;*)
  }


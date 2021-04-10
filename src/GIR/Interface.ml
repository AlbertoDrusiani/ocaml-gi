open Allocation
(*open Method*)
open Property
open Signal
open Parser
open Type
open Documentation
open Deprecation
open BasicTypes


type interface ={ 
    ifTypeInit: string option;
    ifCType: string option;
    ifDocumentation: documentation;
    ifPrerequisites: name list;
    ifProperties: property list;
    ifSignals: signal list;
    (*ifMethods: method_ml list;*)
    ifAllocationInfo: allocation_info;
    ifDeprecated: deprecation_info option;
    }


let parseInterface el ns =
  let name = parseName el ns in
  let props = List.map (fun x -> x ns) (List.map parseProperty (parseChildrenWithLocalName "property" el)) in
  let signals = List.map (fun x -> x ns) (List.map parseSignal (parseChildrenWithNSName GLibGIRNS "signal" el)) in
  let typeInit = queryAttrWithNamespace GLibGIRNS "get-type" el in
  (*let methods =
  let functions =
  let constructors =*)
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let ctype = queryCType el in
  name,
  { ifProperties = props;
    ifPrerequisites = [{name="prova"; namespace="prova"}]; (*TODO mette error*)
    ifSignals = signals;
    ifTypeInit = typeInit;
    ifCType = ctype;
    ifDocumentation = doc;
    (*ifMethods = constructors @ methods @ functions;*)
    ifAllocationInfo = unknownAllocationInfo;
    ifDeprecated = deprecated;
  }

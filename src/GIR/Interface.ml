open Allocation
open Method
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
    ifMethods: method_ml list;
    ifAllocationInfo: allocation_info;
    ifDeprecated: deprecation_info option;
    }


let parseInterface el ns =
  let name = parseName ns el in
  let props = List.map (parseProperty ns) (parseChildrenWithLocalName "property" el) in
  let signals = List.map (parseSignal ns) (parseChildrenWithNSName GLibGIRNS "signal" el) in
  let typeInit = queryAttrWithNamespace GLibGIRNS "get-type" el in
  let methods = List.map (parseMethod ns OrdinaryMethod) (parseChildrenWithLocalName "method" el) in
  let functions = List.map (parseMethod ns MemberFunction) (parseChildrenWithLocalName "function" el) in
  let constructors = List.map (parseMethod ns Constructor) (parseChildrenWithLocalName "constructor" el) in
  let deprecated = parseDeprecation el in
  let doc = parseDocumentation el in
  let ctype = queryCType el in
  name,
  { ifProperties = props;
    ifPrerequisites = [{name="dummy"; namespace="dummy"}]; (*TODO bomba, dummy value*)
    ifSignals = signals;
    ifTypeInit = typeInit;
    ifCType = ctype;
    ifDocumentation = doc;
    ifMethods = constructors @ methods @ functions;
    ifAllocationInfo = unknownAllocationInfo;
    ifDeprecated = deprecated;
  }

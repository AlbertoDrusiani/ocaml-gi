open Callable
open Parser
open Documentation
open Deprecation

type signal = { 
    sigName: string;
    sigCallable: callable;
    sigDeprecated: deprecation_info option;
    sigDetailed: bool;
    sigDoc: documentation;
    }


let parseSignal el ns =
  let n = getAttr "name" el in
  let detailed = optionalAttr "detailed" false el parseBool in
  let deprecated = parseDeprecation el in
  let callable = parseCallable el ns in
  let doc = parseDocumentation el in
  { sigName = n;
    sigCallable = callable;
    sigDeprecated = deprecated;
    sigDetailed = detailed;
    sigDoc = doc;
  }

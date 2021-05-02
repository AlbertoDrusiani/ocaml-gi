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


let parseSignal ns aliases el =
  let n = getAttr "name" el in
  let detailed = optionalAttr "detailed" false el parseBool in
  let deprecated = parseDeprecation el in
  let callable = parseCallable ns aliases el in
  let doc = parseDocumentation el in
  { sigName = n;
    sigCallable = callable;
    sigDeprecated = deprecated;
    sigDetailed = detailed;
    sigDoc = doc;
  }

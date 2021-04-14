open Callable
open Parser
open Type
open Documentation

type callback = { 
    cbCallable: callable;
    cbCType: string option;
    cbDocumentation: documentation;
    }


let parseCallback ns aliases el =
  prerr_endline ("Inizio il parse Callback");
  let name = parseName ns el in
  let callable = parseCallable ns aliases el in
  let ctype = queryCType el in
  let doc = parseDocumentation el in
  name,
  { cbCallable = callable;
    cbCType = ctype;
    cbDocumentation = doc;
  }


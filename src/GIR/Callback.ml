open Callable
open Parser
open Type
open Documentation

type callback = { 
    cbCallable: callable;
    cbCType: string option;
    cbDocumentation: documentation;
    }


let parseCallback el ns =
  let name = parseName el ns in
  let callable = parseCallable el ns in
  let ctype = queryCType el in
  let doc = parseDocumentation el in
  name,
  { cbCallable = callable;
    cbCType = ctype;
    cbDocumentation = doc;
  }


(*module GI = GObject_introspection

open Callable
open BasicTypes

type callback = { 
    cbCallable: callable;
   (* cbCType: string option;*)
   (* cbDocumentation: documentation;*)
    }


let parseCallback c =
    prerr_endline("pppppppppp CALLBACK PPPPPPPPPPPP");
    let name = GI.Callable_info.to_baseinfo c |> getName in
    (name, 
    { cbCallable = parseCallable c;})

*)

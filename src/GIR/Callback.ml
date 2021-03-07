module GI = GObject_introspection

open Callable

type callback = { 
    cbCallback: callable;
   (* cbCType: string option;*)
   (* cbDocumentation: documentation;*)
    }


let parseCallback c =
    { cbCallback = parseCallable c;}



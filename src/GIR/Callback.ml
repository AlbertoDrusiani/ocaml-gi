module GI = GObject_introspection

open Callable
open BasicTypes

type callback = { 
    cbCallable: callable;
   (* cbCType: string option;*)
   (* cbDocumentation: documentation;*)
    }


let parseCallback c =
    let name = GI.Callable_info.cast_to_baseinfo c |> getName in
    (name, 
    { cbCallable = parseCallable c;})



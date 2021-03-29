module GI = GObject_introspection


open Callable
open BasicTypes

type signal = { 
    sigName: string option;
    sigCallable: callable;
    sigDeprecated: bool;
     (* sigDetailed: bool;*)
     (* sigDoc: documentation;*)
    }

(*passo un signal_info*)
let parseSignal s =
    prerr_endline("pppppppppppp SIGNAL pppppppppp");
    let name =  GI.Signal_info.to_baseinfo s |> getOnlyName in
    { 
        sigName = name;
        sigCallable = GI.Signal_info.to_callableinfo s |> parseCallable;
        sigDeprecated = GI.Signal_info.to_baseinfo s |> GI.Base_info.is_deprecated;
    }

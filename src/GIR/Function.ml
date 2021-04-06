(*module GI = GObject_introspection

open Callable
open BasicTypes

type function_ml = { 
    fnSymbol: string;
    (*fnMovedTo: string option;*)
    fnCallable: callable;
    }

(*passo un Function_info*)    
let parseFunction f =
    prerr_endline("pppppppppppp FUNCTION pppppppppppppp");
    let name = GI.Function_info.to_baseinfo f |> getName in
    (name,
    { fnSymbol = GI.Function_info.get_symbol f;
      fnCallable = GI.Function_info.to_callableinfo f |> parseCallable
    })
      
*)

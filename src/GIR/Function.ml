module GI = GObject_introspection

open Callable
open BasicTypes

type function_ml = { 
    fnSymbol: string;
    (*fnMovedTo: string option;*)
    fnCallable: callable;
    }

(*passo un Function_info*)    
let parseFunction f =
    let name = GI.Function_info.cast_to_baseinfo f |> getName in
    (name,
    { fnSymbol = GI.Function_info.get_symbol f;
      fnCallable = GI.Function_info.cast_to_callableinfo f |> parseCallable
    })
      


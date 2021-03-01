module GI = GObject_introspection

open Callable

type function_ml =
    { fnSymbol: string;
      (*fnMovedTo: string option;*)
      fnCallable: callable;
    }

(*passo un Function_info*)    
let parseFunction f =
    { fnSymbol = GI.Function_info.get_symbol f;
      fnCallable = GI.Function_info.cast_to_callableinfo f |> parseCallable
    }
      


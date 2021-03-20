module GI = GObject_introspection
module B = Bindings

open BasicTypes
open Callable


type method_type =
    | Constructor
    | MemberFunction
    | OrdinaryMethod



type method_ml ={ 
    methodName: name;
    methodSymbol: string;
   (* methodType: method_type;*) (*L'API MI RITORNA UNA LISTA, COME GESTISCO?*)
   (* methodMovedTo: string option;*) (*NON SO COSA SIA*)
    methodCallable: callable;
    }

(*prende un B.Function_info.flags*)
let parseMethodType (l : B.Function_info.flags) =
    match l with
    | Is_method -> OrdinaryMethod
    | Is_constructor -> Constructor
    | _ -> MemberFunction

(*passo un Method_info*)    
let parseMethod m =
    print_endline("pppppppppppp METHOD pppppppppp");
    let method_name = 
        { name = GI.Function_info.cast_to_baseinfo m |> GI.Base_info.get_name;
          namespace = GI.Function_info.cast_to_baseinfo m |> GI.Base_info.get_namespace;
        } in
    { methodName = method_name;
      methodSymbol = GI.Function_info.get_symbol m;
     (* methodType = GI.Function_info.*)
      methodCallable = GI.Function_info.cast_to_callableinfo m |> parseCallable;
    }

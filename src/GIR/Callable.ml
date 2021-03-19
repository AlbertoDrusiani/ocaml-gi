module GI = GObject_introspection
module B = Bindings

open Arg
open BasicTypes

type callable ={ 
    returnType: type_ml option;
    returnMayBeNull: bool;
    returnTransfer: transfer;
   (* returnDocumentation: documentation;*)
    args: arg list ref;
    skipReturn: bool;
    callableThrows: bool;
    (*callableDeprecated: deprecation_info option;*)
    (*callableDOcumentation: documentation;*)
    (*callableResolvable: bool option;*)
    }


(*helper per wrappare type_ml in type_ml option*)
let wrap_callable_return_type a = 
    if GI.Callable_info.may_return_null a then
        Some (GI.Callable_info.get_return_type a |> cast_to_type_ml)
    else
        None

    
(*passo un Callable_info*)
let parseCallable a =
    let l = ref [] in
    print_endline("Primo for callable");
    for i = (GI.Callable_info.get_n_args a) - 1 downto 0 do
        l := (GI.Callable_info.get_arg a i |> parseArg) :: !l
    done;
    { returnType = wrap_callable_return_type a;
      returnMayBeNull = GI.Callable_info.may_return_null a;
      returnTransfer = GI.Callable_info.get_caller_owns a |> parseTransfer;
      args = l;
      skipReturn = GI.Callable_info.skip_return a;
      callableThrows = GI.Callable_info.can_throw_gerror a; 
    }

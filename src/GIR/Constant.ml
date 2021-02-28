
open BasicTypes

(*open GObject_introspection*)

module GI = GObject_introspection
module B = Bindings

(*da sistemare i due campi mancanti e value in stringa *)
type constant = 
    { constantType: type_ml;
      constantValue: GI.Types.argument_t Ctypes.union Ctypes.ptr;
      (*constantCType: string;
      constantDocumentation: string;*)
      constantIsDeprecated: bool;
    }
   

(*passo alla funzione la constant_info da fuori perché sto già facendo pattern matching*)
let parseConstant constant_info  =     
    {   constantValue = GI.Constant_info.get_value constant_info;
        constantType = cast_to_type_ml @@ GI.Constant_info.get_type constant_info;
        constantIsDeprecated = GI.Constant_info.cast_to_baseinfo constant_info |> GI.Base_info.is_deprecated;
    }
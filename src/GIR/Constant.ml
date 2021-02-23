
open BasicTypes
(*open GObject_introspection*)

type constant = 
    { constantType: type_ml;
      constantValue: string;
      constantCType: string;
      constantDocumentation: string;
      constantDeprecated: string option;
    }
   

(*passo la a alla funzione*)
(*let parseConstant namespace name_info = 
    let info = GObject_introspection.Repository.find_by_name namespace name_info in
    match info with 
    | Some a ->     
            {(*constantValue = GObject_introspection.Constant_info.get_value a;*)
             constantType = GObject_introspection.Type_info.get_tag @@ GObject_introspection.Type_info.cast_from_baseinfo a}
    | None -> None
        | None -> None*)

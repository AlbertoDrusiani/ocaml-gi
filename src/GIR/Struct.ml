module GI = GObject_introspection

open Field
open Method
open BasicTypes

type struct_ml = { 
    (*structIsBoxed: bool;*)
     (* structAllocationInfo: allocation_info; *)
    structTypeInit: string option; (*OK*)
    structCType: string option; (*OK*)
    structSize: int; (*OK*)
   (* gtypeStructFor: string option;*)
   (* structIsDisguised: bool;*)(*inserito per un bug trovato in gir, non presente nella libreria C*)
   (* structForceVisible: bool;*)
    structFields: field list ref; (*OK*)
    structMethods: method_ml list ref; (*OK*)
    structDeprecated: bool; (*OK*)
     (* structDocumentation: documentation*)
}

(*passo una Struct_info*)
let parseStruct s =
    prerr_endline("pppppppppp STRUCT ppppppppppp");
    let l_fields = ref [] in
   (*print_endline("Primo for");*)
    for i = (GI.Struct_info.get_n_fields s) - 1 downto 0 do
        l_fields := (GI.Struct_info.get_field s i |> parseField) :: !l_fields
    done;
     
    let l_methods = ref [] in
    (*print_endline("Secondo_for");*)
    for i = (GI.Struct_info.get_n_methods s) - 1 downto 0 do
        l_methods := (GI.Struct_info.get_method s i |> parseMethod) :: !l_methods
    done;
    let name = GI.Struct_info.to_baseinfo s |> getName in
    (*print_endline("Terzo for");*)
    (name,
    {
        structTypeInit = GI.Struct_info.to_registeredtypeinfo s |> GI.Registered_type_info.get_type_init;
        structCType = GI.Struct_info.to_registeredtypeinfo s |> GI.Registered_type_info.get_type_name;
        structSize = GI.Struct_info.get_size s;
        structFields = l_fields;
        structMethods = l_methods;
        structDeprecated = GI.Struct_info.to_baseinfo s |> GI.Base_info.is_deprecated;
    })

    

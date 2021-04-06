(*module GI = GObject_introspection

open Field
open Method
open BasicTypes

type union = {
   (* unionIsBoxed: bool;*) (*faccio come haskell*)
   (* unionAllocationInfo: allocation_info;*)
   (* unionDocumentation: documentation;*)
    unionSize: int; (*OK*)
    unionTypeInit: string option; (*OK*)
    unionFields: field list ref; (*OK*)
    unionMethods: method_ml list ref; (*OK*)
    unionCType: string option; (*OK*)
    unionDeprecated: bool; (*in haskell derprecation_info option*)
}

(*passo un Union_info*)
let parseUnion u =
   prerr_endline("pppppppppppp UNION ppppppppppp");
   let l_fields = ref [] in
    for i = (GI.Union_info.get_n_fields u) - 1 downto 0 do
        l_fields := (GI.Union_info.get_field u i |> parseField) :: !l_fields
    done;
     
    let l_methods = ref [] in
    for i = (GI.Union_info.get_n_methods u) - 1 downto 0 do
        l_methods := (GI.Union_info.get_method u i |> parseMethod) :: !l_methods
    done;
    let name = GI.Union_info.to_baseinfo u |> getName in
    (name,
    {
        unionSize = GI.Union_info.get_size u;
        unionTypeInit = GI.Union_info.to_registeredtypeinfo u |> GI.Registered_type_info.get_type_init;
        unionFields = l_fields;
        unionMethods = l_methods;
        unionCType = GI.Union_info.to_registeredtypeinfo u |> GI.Registered_type_info.get_type_name;
        unionDeprecated = GI.Union_info.to_baseinfo u |> GI.Base_info.is_deprecated;
    })*)

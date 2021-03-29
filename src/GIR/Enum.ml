module GI = GObject_introspection
module B = Bindings

open BasicTypes

type enumeration_member = { 
    enumMemberName: string;
    enumMemberValue: int64 option;
   (* enumMemberCId: string;
    enumMemberDoc: string; (*in haskell è Documentation*)*)
    }



type enumeration = { 
    enumMembers: enumeration_member list ref; (*TODO da chiedere al prof*)
    enumErrorDomain: string option;
    enumTypeInit: string option;
    (*enumDocumentation: string; (*in haskell è Documentation*)
    enumCType: string; forse è la get_type_name in registered_info
    enumStorageBytes: int;  c'è qualcosa, da guardare meglio*)
    enumDeprecated: bool; (*in haskell è DeprecationInfo*)
    }

(*passo alla funzione una enum_info e l'indice corrispondente*)
let parseEnumMember e i =
    let value =
        match GI.Enum_info.get_value e i with
        | Some x -> Some (GI.Value_info.get_value x)
        | None -> None
    in
    { enumMemberName = GI.Enum_info.get_method e i |> GI.Function_info.get_symbol;
      enumMemberValue = value;
      (*
      enumMemberCId =
      enumMemberDoc = *)
    }


(*passo una enum_info*)
let parseEnum e = 
    prerr_endline("ppppppppppp ENUM pppppppppppp");
    let l = ref [] in
    for i = (GI.Enum_info.get_n_methods e) - 1 downto 0 do
        l := parseEnumMember e i :: !l
    done;
    let name = GI.Enum_info.to_baseinfo e |> getName in
    (name,
    { enumMembers = l;
      enumErrorDomain = GI.Enum_info.get_error_domain e;
      enumTypeInit = GI.Enum_info.to_registeredtypeinfo e |> GI.Registered_type_info.get_type_init;
     (* enumDocumentation = ;
      enumCType = ;
      enumStorageBytes = ; *)
      enumDeprecated = GI.Enum_info.to_baseinfo e |> GI.Base_info.is_deprecated;
    })


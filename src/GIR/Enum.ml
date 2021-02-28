module GI = GObject_introspection
module B = Bindings

type enumeration_member = 
    { enumMemberName: string;
      enumMemberValue: int64;
     (* enumMemberCId: string;
      enumMemberDoc: string; (*in haskell è Documentation*)*)
    }



type enumeration = 
    { enumMembers: enumeration_member list ref; (*TODO da chiedere al prof*)
      enumErrorDomain: string option;
     (* enumTypeInit: string option;
      enumDocumentation: string; (*in haskell è Documentation*)
      enumCType: string;
      enumStorageBytes: int;*)
      enumDeprecated: bool; (*in haskell è DeprecationInfo*)
    }

(*passo alla funzione una enum_info e l'indice corrispondente*)
let parseEnumMember e i =
    let value = 
        match GI.Enum_info.get_value e i with
        | Some x -> GI.Value_info.get_value x
        | None -> 32L (*TODO sistemare errore con valore che abbia senso*)
    in 
    { enumMemberName = GI.Enum_info.get_method e i |> GI.Function_info.get_symbol;
      enumMemberValue = value;
      (*
      enumMemberCId =
      enumMemberDoc = *)
    }


(*passo una enum_info*)
let parseEnum e = 
    let l = ref [] in
    for i = GI.Enum_info.get_n_methods e downto 0 do
        l := parseEnumMember e i :: !l
    done;
    { enumMembers = l;
      enumErrorDomain = GI.Enum_info.get_error_domain e;
      (*
      enumTypeInit = ;
      enumDocumentation = ;
      enumCType = ;
      enumStorageBytes = ; *)
      enumDeprecated = GI.Enum_info.cast_to_baseinfo e |> GI.Base_info.is_deprecated;
    }


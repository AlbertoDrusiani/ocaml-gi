module GI = GObject_introspection
module B = Bindings

open BasicTypes

type property_flag =
    | PropertyReadable
    | PropertyWritable
    | PropertyConstruct
    | PropertyConstructOnly
    | NotHandled


type property = { 
    propName: string option;
    propType: type_ml option;
    propFlags: property_flag list;
   (* propReadNullable: bool option;
    propWriteNullable: bool option;*)
    propTransfer: transfer;
   (* propDoc: documentation;*)
    propDeprecated: bool; (*deprecation_info option in haskell*)
    }


(*passo una Bindings.GParam.flags list*)
let rec parsePropertyFlag a =
    match a with
    | [] -> []
    | x::xs -> 
       let parsed (f : B.GParam.flags) = 
            match f with
            | Readable -> PropertyReadable
            | Writable -> PropertyWritable
            | Construct -> PropertyConstruct;
            | Construct_only -> PropertyConstructOnly
            | _ -> NotHandled
        in (parsed x) :: (parsePropertyFlag xs)


(*passo una Property_info*)
let parseProperty a =
    prerr_endline("ppppppppp PROPERTY ppppppppp");
    let name =  GI.Property_info.to_baseinfo a |> getOnlyName in
    { 
        propName = name;
        propType = GI.Property_info.get_type a |> cast_to_type_ml;
        propFlags = GI.Property_info.get_flags a |> parsePropertyFlag;
        (* propReadNullable = ;
         propWriteNullable = ;*)
         propTransfer = GI.Property_info.get_ownership_transfer a |> parseTransfer;
         propDeprecated = GI.Property_info.to_baseinfo a |> GI.Base_info.is_deprecated;
    }
        

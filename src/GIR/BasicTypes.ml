module GI = GObject_introspection
module B = Bindings

type name = 
    { namespace : string;
      name : string option;
    }

type transfer =
    | TransferNothing
    | TransferContainer
    | TransferEverything


type alias = name

type basic_type =
    | TBoolean
    | Tint
    | TUInt
    | TLong
    | TUlong
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt64
    | TUInt64
    | TFloat
    | TDouble
    | TUniChar
    | TGType
    | TUTF8
    | TFileName
    | TPtr
    | TIntPtr
    | TUIntPtr


type type_ml = 
    | TBasicType of basic_type 
    | TError
    | TVariant
    | TGValue
    | TParamSpec
    | TCArray of bool * int * int * type_ml option
    | TGArray of type_ml option
    | TPtrArray of type_ml option
    | TByteArray
    | TGList of type_ml option
    | TGSList of type_ml option
    | TGHash of type_ml option * type_ml option
    | TGClosure of (type_ml option)
    | TInterface of name

(*prendo un type_info*)
let rec cast_to_type_ml a =
   (* let type_info = GI.Type_info.cast_from_baseinfo a in*)
    print_endline("Basic types: "^(GI.Type_info.get_tag a |> B.Types.string_of_tag));
    match GI.Type_info.get_tag a with
    | Void -> None
    | Boolean -> Some (TBasicType TBoolean)
    | Int8 -> Some (TBasicType TInt8)
    | Uint8 -> Some (TBasicType TUInt8)
    | Int16 -> Some (TBasicType TInt16)
    | Uint16 -> Some (TBasicType TUInt16)
    | Int32 -> Some (TBasicType TInt32)
    | Uint32 -> Some (TBasicType TUInt32)
    | Int64 -> Some (TBasicType TInt64)
    | Uint64 -> Some (TBasicType TUInt64)
    | Float -> Some (TBasicType TFloat)
    | Double -> Some (TBasicType TDouble)
    | GType -> Some (TBasicType TGType)
    | Utf8 -> Some (TBasicType TUTF8)
    | Filename -> Some (TBasicType TFileName)
    | Array -> 
            begin
            let array_type = GI.Type_info.get_array_type a in
            let array_param_type = GI.Type_info.get_param_type a in
            print_endline("Param type: " ^ GI.Type_info.to_string array_param_type);
            match array_type with
            | Some C ->
                let is_zero_terminated = GI.Type_info.is_zero_terminated a in
                let array_fixed_size = GI.Type_info.get_array_fixed_size a in
                let array_length = GI.Type_info.get_array_length a in
                Some (TCArray (is_zero_terminated, array_fixed_size, array_length, cast_to_type_ml array_param_type))
            | Some Array -> Some (TGArray (cast_to_type_ml array_param_type))
            | Some Ptr_array -> Some (TPtrArray (cast_to_type_ml array_param_type))
            | Some Byte_array -> Some TByteArray
            | None -> Some TError
            end
    | Interface ->
            let name = GI.Type_info.cast_to_baseinfo a |> GI.Base_info.get_name in
            let namespace = GI.Type_info.cast_to_baseinfo a |> GI.Base_info.get_namespace in
            Some (TInterface {name = name; namespace = namespace;})
    | GList ->  Some (TGList (GI.Type_info.get_param_type a |> cast_to_type_ml))
    | GSList -> Some (TGSList (GI.Type_info.get_param_type a |> cast_to_type_ml))
    | GHash -> Some (TGHash (GI.Type_info.unsafe_get_param_type a 0 |> cast_to_type_ml, GI.Type_info.unsafe_get_param_type a 1 |> cast_to_type_ml))
    | Error -> Some TError
    | Unichar -> Some (TBasicType TUniChar)


let parseTransfer (t : B.Arg_info.transfer) =
    match t with
    | Nothing -> TransferNothing
    | Container -> TransferContainer
    | Everything -> TransferEverything

(*prende un base_info*)
let getName b =
    {name =GI.Base_info.get_name b;
     namespace = GI.Base_info.get_namespace b;
    }

(* prende un base_info*)    
let getOnlyName b =
    GI.Base_info.get_name b 



(*let print_info ns =
    let _ = Result.get_ok (GI.Repository.require ns ()) in
    for i=0 to GI.Repository.get_n_infos ns do
        let nome = match GI.Repository.get_info ns i |> GI.Base_info.get_name with
            | Some a -> a
            | None -> "Errore"
        in print_endline ("Nome: " ^ nome);
        let tipo = GI.Repository.get_info ns i |> GI.Base_info.get_type |> B.Base_info.string_of_info_type in
        print_endline ("Tipo: " ^ tipo);
        if GI.Repository.get_info ns i |> GI.Base_info.get_type == Type then
            let tag = GI.Repository.get_info ns i |> GI.Type_info.cast_from_baseinfo |> GI.Type_info.get_tag |> B.Types.string_of_tag in
            print_endline ("Tag: " ^ tag)
        else
            print_endline "Non è un tipo"
    done

let _ = print_info "Gtk"*)
    

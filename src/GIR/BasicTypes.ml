module GI = GObject_introspection
module B = Bindings

type name = 
    { namespace : string;
      name : string;
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
    | TCArray of bool * int * int * type_ml
    | TGArray of type_ml
    | TPtrArray of type_ml
    | TByteArray
    | TGList of type_ml
    | TGSList of type_ml
    | TGHash of type_ml * type_ml
    | TGClosure of (type_ml option)
    | TInterface of name

(*prendo un type_info*)
let rec cast_to_type_ml a =
   (* let type_info = GI.Type_info.cast_from_baseinfo a in*)
    match GI.Type_info.get_tag a with
    | Void -> assert false
    | Boolean -> TBasicType TBoolean
    | Int8 -> TBasicType TInt8
    | Uint8 -> TBasicType TUInt8
    | Int16 -> TBasicType TInt16
    | Uint16 -> TBasicType TUInt16
    | Int32 -> TBasicType TInt32
    | Uint32 -> TBasicType TUInt32
    | Int64 -> TBasicType TInt64
    | Uint64 -> TBasicType TUInt64
    | Float -> TBasicType TFloat
    | Double -> TBasicType TDouble
    | GType -> TBasicType TGType
    | Utf8 -> TBasicType TUTF8
    | Filename -> TBasicType TFileName
    | Array -> 
            begin
            let array_type = GI.Type_info.get_array_type a in
            let array_param_type = GI.Type_info.get_param_type a in
            match array_type with
            | Some C ->
                let is_zero_terminated = GI.Type_info.is_zero_terminated a in
                let array_fixed_size = GI.Type_info.get_array_fixed_size a in
                let array_length = GI.Type_info.get_array_length a in
                TCArray (is_zero_terminated, array_fixed_size, array_length, cast_to_type_ml array_param_type)
            | Some Array -> TGArray (cast_to_type_ml array_param_type)
            | Some Ptr_array -> TPtrArray (cast_to_type_ml array_param_type)
            | Some Byte_array -> TByteArray
            | None -> TError
            end
    | Interface ->
            let name = 
                match GI.Type_info.cast_to_baseinfo a |> GI.Base_info.get_name with
                | Some x -> x
                | None -> "Errore"
            in let namespace = GI.Type_info.cast_to_baseinfo a |> GI.Base_info.get_namespace in
            TInterface {name = name; namespace = namespace}
    | GList -> TGList (cast_to_type_ml a)
    | GSList -> TGSList (cast_to_type_ml a)
    | GHash -> TGHash (cast_to_type_ml a, cast_to_type_ml a)
    | Error -> TError
    | Unichar -> TBasicType TUniChar


let parseTransfer (t : B.Arg_info.transfer) =
    match t with
    | Nothing -> TransferNothing
    | Container -> TransferContainer
    | Everything -> TransferEverything

(*prende un base_info*)
let getName b =
    {name =
        begin 
        match GI.Base_info.get_name b with 
        | Some x -> x
        | None -> "Error"
        end;
     namespace = GI.Base_info.get_namespace b;
    }

(* prende un base_info*)    
let getOnlyName b =
    match GI.Base_info.get_name b with
    | Some x -> x
    | None -> "Error"



let print_info ns =
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

let _ = print_info "Gtk"
    

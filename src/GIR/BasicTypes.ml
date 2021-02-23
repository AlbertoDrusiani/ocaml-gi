module GI = GObject_introspection
module B = Bindings

type name = 
    { namespace : string;
      name : string;
    }

type transfer =
    | TrasferNothing
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

(*prendo un base_info*)
let rec cast_to_type_ml a =
    let type_info = GI.Type_info.cast_from_baseinfo a in
    match GI.Type_info.get_tag type_info with
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
            let array_type = GI.Type_info.get_array_type type_info in
            let array_param_type = GI.Type_info.get_param_type type_info in
            match array_type with
            | Some C ->
                let is_zero_terminated = GI.Type_info.is_zero_terminated type_info in
                let array_fixed_size = GI.Type_info.get_array_fixed_size type_info in
                let array_length = GI.Type_info.get_array_length type_info in
                TCArray (is_zero_terminated, array_fixed_size, array_length, cast_to_type_ml @@ GI.Type_info.cast_to_baseinfo array_param_type)
            | Some Array -> TGArray (cast_to_type_ml @@ GI.Type_info.cast_to_baseinfo array_param_type)
            | Some Ptr_array -> TPtrArray (cast_to_type_ml @@ GI.Type_info.cast_to_baseinfo array_param_type)
            | Some Byte_array -> TByteArray
            | None -> TError
            end
    | Interface ->
            let name = 
                match GI.Base_info.get_name a with
                | Some x -> x
                | None -> "Errore"
            in let namespace = GI.Base_info.get_namespace a in
            TInterface {name = name; namespace = namespace}
    | GList -> TGList (cast_to_type_ml a)
    | GSList -> TGSList (cast_to_type_ml a)
    | GHash -> TGHash (cast_to_type_ml a, cast_to_type_ml a)
    | Error -> TError
    | Unichar -> TBasicType TUniChar



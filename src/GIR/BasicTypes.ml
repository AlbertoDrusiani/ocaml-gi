

type name = 
    { namespace : string;
      name : string;
    }

type transfer =
    | TransferNothing
    | TransferContainer
    | TransferEverything


type alias = Alias of name


module Alias = struct 
  type t = alias
  let compare (Alias {namespace=ns1; name=nm1}) (Alias {namespace=ns2; name=nm2}) =
    match Stdlib.compare ns1 ns2 with
    | 0 -> Stdlib.compare nm1 nm2
    | c -> c
end


module AliasMap = Map.Make(Alias)

type basic_type =
    | TBoolean
    | TInt
    | TUInt
    | TLong
    | TULong
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


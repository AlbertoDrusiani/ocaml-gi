open Code
open GIR.BasicTypes
open Naming

type expose_closures =
  | WithClosures
  | WithoutClosures


let conversionError converter case =
  raise (CGErrorNotImplemented ("This " ^ converter ^ " (" ^ case ^ ") isn't implemented yet"))


let ocamlDataConvErr err =
  conversionError "ocamlDataConv" err
  
let getModuleType minfo n =
  let currNs = currentNS minfo in
  nsOCamlType currNs n

let objConverter b conv =
  match b with
  | false -> "(gobject : " ^ conv ^ " Gobject.obj data_conv)"
  | true -> "(gobject_option : " ^ conv ^ " Gobject.obj option data_conv)"


let enumResolver minfo n =
  let currNS = currentNS minfo in
  if n.namespace = currNS
  then "Enums"
  else "GI" ^ n.namespace ^ "." ^ "Enums"


let ocamlDataConv cfg minfo isNullable tp =
  match tp with
  | TBasicType t ->
    begin
    match t with
    | TBoolean -> "boolean"
    | TInt -> "int"
    | TUInt -> "uint"
    | TLong -> "long"
    | TULong -> "ulong"
    | TInt8 -> ocamlDataConvErr "TInt8"
    | TUInt8 -> ocamlDataConvErr "TUInt8"
    | TInt16 -> ocamlDataConvErr "TInt16"
    | TUInt16 -> ocamlDataConvErr "TUInt16"
    | TInt32 -> "int32"
    | TUInt32 -> "uint32"
    | TInt64 -> "int64"
    | TUInt64 -> "uint64"
    | TFloat -> "float"
    | TDouble -> "double"
    | TUniChar -> "char"
    | TGType -> ocamlDataConvErr "TGType"
    | TUTF8 -> "string"
    | TFileName -> "string"
    | TPtr -> "int"
    | TIntPtr -> ocamlDataConvErr "TIntPtr"
    | TUIntPtr -> ocamlDataConvErr "TUIntPtr"
    end
  | TError -> ocamlDataConvErr "TError"
  | TVariant -> ocamlDataConvErr "TVariant"
  | TGValue -> ocamlDataConvErr "TGValue"
  | TParamSpec -> ocamlDataConvErr "TPAramSpec"
  | TCArray _ -> ocamlDataConvErr "TCArray"
  | TGArray _ -> ocamlDataConvErr "TGArray"
  | TPtrArray _ -> ocamlDataConvErr "TPtrArray"
  | TByteArray -> ocamlDataConvErr "TByteArray"
  | TGList _ -> ocamlDataConvErr "TGList"
  | TGSList _-> ocamlDataConvErr "TGSList"
  | TGHash _ -> ocamlDataConvErr "TGHash"
  | TGClosure _ -> ocamlDataConvErr "TGClosure"
  | TInterface n ->
    let handleObject =
      let convType = getModuleType minfo n in
      objConverter isNullable convType 
    in let enumFlagConv n =
      let enumRes = enumResolver minfo n in
      enumRes ^ "." ^ ocamlIdentifier n
    in
    begin
    match findAPIByName cfg n with
    | APIConst _ -> ocamlDataConvErr "APIConst"
    | APIFunction _ -> ocamlDataConvErr "APIFunction"
    | APICallback _ -> ocamlDataConvErr "APICallback"
    | APIEnum _ -> enumFlagConv n
    | APIFlags _ -> enumFlagConv n
    | APIInterface _ -> handleObject
    | APIObject _ -> handleObject
    | APIStruct _ ->
      let moduleT = getModuleType minfo n in
      "(unsafe_pointer _ " ^ moduleT ^ " data_conv"
    | APIUnion _ ->
      let moduleT = getModuleType minfo n in
      "(unsafe_pointer _ " ^ moduleT ^ " data_conv"
    end



open Code
open GIR.BasicTypes
open Naming
open GIR.Interface
open TypeRep
open Ctypes
open API

type expose_closures =
  | WithClosures
  | WithoutClosures


let conversionError converter case =
  raise (CGErrorNotImplemented ("This " ^ converter ^ " (" ^ case ^ ") isn't implemented yet"))


let ocamlDataConvErr err =
  conversionError "ocamlDataConv" err

let cTypeErr err =
  conversionError "cType" err

let ocamlValueToCErr err =
  conversionError "ocamlValueToC" err

let cToOCamlValueErr err =
  conversionError "cToOCamlValue" err
  
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



let ocamlValueToC cfg minfo tp =
    match tp with
  | TBasicType t ->
    begin
    match t with
    | TBoolean -> minfo, "Bool_val"
    | TInt -> minfo, "Int_val"
    | TUInt -> minfo, "Int_val"
    | TLong -> minfo, "Long_val"
    | TULong -> minfo, "Long_val"
    | TInt8 -> minfo, ocamlValueToCErr "TInt8"
    | TUInt8 -> minfo, ocamlValueToCErr "TUInt8"
    | TInt16 -> minfo, ocamlValueToCErr "TInt16"
    | TUInt16 -> minfo, ocamlValueToCErr "TUInt16"
    | TInt32 -> minfo, ocamlValueToCErr "TInt32"
    | TUInt32 -> minfo, ocamlValueToCErr "TUInt32"
    | TInt64 -> minfo, ocamlValueToCErr "TInt64"
    | TUInt64 -> minfo, ocamlValueToCErr "TUInt64"
    | TFloat -> minfo, "Float_val"
    | TDouble -> minfo, "Double_val"
    | TUniChar -> minfo, "Char_val"
    | TGType -> minfo, ocamlValueToCErr "TGType"
    | TUTF8 -> minfo, "String_val"
    | TFileName -> minfo, "String_val"
    | TPtr -> minfo, ocamlValueToCErr "TPtr"
    | TIntPtr -> minfo, ocamlValueToCErr "TIntPtr"
    | TUIntPtr -> minfo, ocamlValueToCErr "TUIntPtr"
    end
  | TError -> minfo, ocamlValueToCErr "TError"
  | TVariant -> minfo, ocamlValueToCErr "TVariant"
  | TGValue -> minfo, ocamlValueToCErr "TGValue"
  | TParamSpec -> minfo, ocamlValueToCErr "TPAramSpec"
  | TCArray _ -> minfo, ocamlValueToCErr "TCArray"
  | TGArray _ -> minfo, ocamlValueToCErr "TGArray"
  | TPtrArray _ -> minfo, ocamlValueToCErr "TPtrArray"
  | TByteArray -> minfo, ocamlValueToCErr "TByteArray"
  | TGList _ -> minfo, ocamlValueToCErr "TGList"
  | TGSList _-> minfo, ocamlValueToCErr "TGSList"
  | TGHash _ -> minfo, ocamlValueToCErr "TGHash"
  | TGClosure _ -> minfo, ocamlValueToCErr "TGClosure"
  | TInterface n ->
    let apiConverter =
      match n with
      | {namespace = "GObject"; name = "Value"} -> minfo, "GValue_val"
      | _ -> let minfo = addCDep minfo (n.namespace ^ n.name) in
             minfo, interfaceVal n
    in
    begin
    match findAPIByName cfg n with
    | APIConst _ -> minfo, ocamlValueToCErr "APIConst"
    | APIFunction _ -> minfo, ocamlValueToCErr "APIFunction"
    | APICallback _ -> minfo, ocamlValueToCErr "APICallback"
    | APIEnum _ -> addCDep minfo (n.namespace ^ "Enums"), flagsVal n
    | APIFlags _ -> addCDep minfo (n.namespace ^ "Enums"), flagsVal n
    | APIInterface i when Option.is_some(i.ifCType)  -> apiConverter
    | APIInterface _ -> minfo, "(ocamlValueToC) Can't convert a APIInterface with no ctype"
    | APIObject o when Option.is_some(o.objCType) -> apiConverter
    | APIObject _ -> minfo, "(ocamlValueToC) Can't convert a APIObject with no ctype"
    | APIStruct s when Option.is_some(s.structCType) -> apiConverter
    | APIStruct _ -> minfo, "(ocamlValueToC) Can't convert a APIStruct with no ctype"
    | APIUnion _ -> minfo, ocamlValueToCErr "APIUnion"
    end


let ocamlBasicType t =
  match t with
  | TPtr -> TextCon "()"
  | TBoolean -> TextCon "bool"
  | TInt ->
    begin
    match sizeof int with
    | 4 -> TextCon "int"
    | _ -> assert false
    end
  | TUInt ->
    begin
    match sizeof uint with
    | 4 -> TextCon "int"
    | _ -> assert false
    end
  | TLong -> TextCon "int"
  | TULong -> TextCon "int"
  | TInt8 -> TextCon "int"
  | TUInt8 -> TextCon "int"
  | TInt16 -> TextCon "int"
  | TUInt16 -> TextCon "int"
  | TInt32 -> TextCon "int"
  | TUInt32 -> TextCon "int"
  | TInt64 -> TextCon "int"
  | TUInt64 -> TextCon "int"
  | TGType -> TextCon "GType"
  | TUTF8 -> TextCon "string"
  | TFloat -> TextCon "float"
  | TDouble -> TextCon "double"
  | TUniChar -> TextCon "char"
  | TFileName -> TextCon "string"
  | TIntPtr -> TextCon "error"
  | TUIntPtr  -> TextCon "error"


let rec ocamlType cfg cgstate minfo t =
  match t with
  | TBasicType bt -> cgstate, ocamlBasicType bt
  | TCArray (_, _, _, TBasicType TUInt8) -> cgstate, TextCon "ByteString"
  | TCArray (_, _, _, a) -> 
     let s, t = ocamlType cfg cgstate minfo a in
     s, ListCon t
  | TGArray a -> 
     let s, t = ocamlType cfg cgstate minfo a in
     s, ListCon t
  | TPtrArray a -> 
    let s, t = ocamlType cfg cgstate minfo a in
     s, ListCon t
  | TByteArray -> cgstate, TextCon "ByteString"
  | TGList a -> 
    let s, t = ocamlType cfg cgstate minfo a in
     s, ListCon t
  | TGSList a -> 
    let s, t = ocamlType cfg cgstate minfo a in
     s, ListCon t
  | TGHash (a, b) -> 
    let currNS = currentNS minfo in
    let s, t = ocamlType cfg cgstate minfo a in
    let innerA = typeShow currNS t in
    let s, t = ocamlType cfg s minfo b in
    let innerB = typeShow currNS t in
    s, TextCon ("(" ^ innerA ^ ", " ^ innerB ^ ") Hashtbl.t")
  | TError -> cgstate, TextCon "GError"
  | TVariant -> cgstate, TextCon "GVariant"
  | TParamSpec -> cgstate, TextCon "GParamSpec"
  | TGClosure _ ->
    let cgstate, _ = getFreshTypeVariable cgstate in
    cgstate, TextCon "error"
  | TInterface {namespace = "GObject"; name = "Value"} ->
    cgstate, TextCon "Gobject.g_value"
  | TInterface n ->
    let handleObj =
    let cgstate, freshVar = getFreshTypeVariable cgstate in
    cgstate, ObjCon (TypeVarCon (freshVar, (RowCon (More, (PolyCon (NameCon n))))))

    in let ocamlName = ocamlIdentifier n in
    (*let tname = lowerName n in c'è in haskell ma non viene utilizzato..*)
    let api = getAPI cfg t in
    begin
    match api with
    | APIFlags _ ->
      let flagRes = enumResolver minfo n in
      cgstate, ListCon (TextCon (flagRes ^ "." ^ ocamlName))
    | APIEnum _ ->
      let enumRes = enumResolver minfo n in
      cgstate, TextCon (enumRes ^ "." ^ ocamlName)
    | APIObject _ -> handleObj
    | APIInterface _ -> handleObj
    | APIStruct _ -> cgstate, TextCon (getModuleType minfo n)
    | APIConst _ -> cgstate, TextCon "const"
    | APIFunction _ -> cgstate, TextCon "function"
    | APICallback _ -> cgstate, TextCon "callback"
    | APIUnion _ -> cgstate, TextCon "union"
    end
  | TGValue -> assert false (*FIXME nel codice haskell non c'è, com'è possibile?*)


let outParamOcamlType cfg cgstate minfo t =
  match t with
  | TBasicType bt -> cgstate, ocamlBasicType bt
  | TCArray (_, _, _, TBasicType (TUInt8)) -> ocamlType cfg cgstate minfo t
  | TCArray (_, _, _, _) -> ocamlType cfg cgstate minfo t
  | TGArray _ -> ocamlType cfg cgstate minfo t
  | TPtrArray _ -> ocamlType cfg cgstate minfo t
  | TByteArray -> ocamlType cfg cgstate minfo t
  | TGList _ -> ocamlType cfg cgstate minfo t
  | TGSList _ -> ocamlType cfg cgstate minfo t
  | TGHash (_, _) -> ocamlType cfg cgstate minfo t
  | TError -> ocamlType cfg cgstate minfo t
  | TVariant -> ocamlType cfg cgstate minfo t
  | TParamSpec -> ocamlType cfg cgstate minfo t
  | TGClosure _ -> ocamlType cfg cgstate minfo t
  | TInterface {namespace = "GObject"; name = "Value"} -> ocamlType cfg cgstate minfo t
  | TInterface n ->

    let handleEnum ocamlName =
      let enumRes = enumResolver minfo n in
      TextCon (enumRes ^ "." ^ ocamlName)

    in let handleObj n =
      let cgstate, freshVar = getFreshTypeVariable cgstate in
      cgstate, (ObjCon (TypeVarCon (freshVar, (RowCon (Less, (PolyCon (NameCon n)))))))
    in let ocamlName = ocamlIdentifier n in
    (*let tname = lowerName n in*)
    let api = getAPI cfg t in
    begin
    match api with
    | APIFlags _ -> cgstate, handleEnum ocamlName
    | APIEnum _ -> cgstate, handleEnum ocamlName
    | APIInterface _ -> handleObj n
    | APIObject _ -> handleObj n
    | _ -> raise (CGErrorNotImplemented "(outParamOcamlType) can't handle this type")
    end
  | TGValue -> assert false (*in haskell non c'è nel pattern matching, com'è possibile?*)




let cType cfg tp =
  match tp with
  | TBasicType t ->
    begin
    match t with
    | TBoolean -> "gboolean"
    | TInt -> "gint"
    | TUInt -> "guint"
    | TLong -> "glong"
    | TULong -> "gulong"
    | TInt8 -> "gint8"
    | TUInt8 -> "guint8"
    | TInt16 -> "gint16"
    | TUInt16 -> "guint16"
    | TInt32 -> "gint32"
    | TUInt32 -> "guint32"
    | TInt64 -> "gint64"
    | TUInt64 -> "guint64"
    | TFloat -> "gfloat"
    | TDouble -> "gdouble"
    | TUniChar -> "gchar"
    | TGType -> cTypeErr "TGType"
    | TUTF8 -> "gchar*"
    | TFileName -> "gchar*"
    | TPtr -> "gpointer"
    | TIntPtr -> "gintptr"
    | TUIntPtr -> "guintptr"
    end
  | TError -> cTypeErr "TError"
  | TVariant -> cTypeErr "TVariant"
  | TParamSpec -> cTypeErr "TParamSpec"
  | TCArray (_, _, _, _) -> cTypeErr "TCArray"
  | TGArray _ -> cTypeErr "TGArray"
  | TPtrArray _ -> cTypeErr "TPtrArray"
  | TByteArray -> cTypeErr "TByteArray"
  | TGList _ -> cTypeErr "TGList"
  | TGSList _ -> cTypeErr "TGSList"
  | TGHash (_, _) -> cTypeErr "TGHash"
  | TGClosure _ -> cTypeErr "TGClosure"
  | TInterface n ->
    let api = findAPIByName cfg n in
    begin
    match api with
    | APIConst c -> c.constantCType
    | APIFunction f -> f.fnSymbol
    | APICallback c ->
        (match c.cbCType with
        | Some ctype -> ctype
        | None -> cTypeErr "Callback without cType")
    | APIEnum e -> e.enumCType
    | APIFlags (Flags e) -> e.enumCType
    | APIInterface i ->
        (match i.ifCType with
        | Some ctype -> ctype
        | None -> cTypeErr "Interface without cType")
    | APIObject o ->
        (match o.objCType with
        | Some ctype -> ctype
        | None -> cTypeErr "Object without cType") 
    | APIStruct s ->
        (match s.structCType with
        | Some ctype -> ctype
        | None -> cTypeErr "Struct without cType")
    | APIUnion u ->
        (match u.unionCType with
        | Some ctype -> ctype
        | None -> cTypeErr "Union without cType")
    end
  | TGValue -> assert false (*in haskell non c'è nel pattern matching, com'è possibile?*)



  let cToOCamlValue cfg minfo nullable tp =
    match nullable, tp with
    | _, None -> minfo, "Unit"
    | false, Some (TBasicType t) ->
      begin
      match t with
      | TBoolean -> minfo, "Val_bool"
      | TInt -> minfo, "Val_int"
      | TUInt -> minfo, "Val_int"
      | TLong -> minfo, "Val_long"
      | TULong -> minfo, "Val_long"
      | TInt8 -> cToOCamlValueErr "TInt8"
      | TUInt8 -> cToOCamlValueErr "TUInt8"
      | TInt16 -> cToOCamlValueErr "TInt16"
      | TUInt16 -> cToOCamlValueErr "TUInt16"
      | TInt32 -> minfo, "caml_copy_int32"
      | TUInt32 -> minfo, "caml_copy_int32"
      | TInt64 -> minfo, "caml_copy_int64"
      | TUInt64 -> minfo, "caml_copy_int64"
      | TFloat -> minfo, "caml_copy_double"
      | TDouble -> minfo, "caml_copy_double"
      | TUniChar -> minfo, "Val_char"
      | TGType -> cToOCamlValueErr "TGType"
      | TUTF8 -> minfo, "Val_string"
      | TFileName -> minfo, "Val_string"
      | TPtr -> cToOCamlValueErr "TPtr"
      | TIntPtr -> cToOCamlValueErr "TIntPtr"
      | TUIntPtr -> cToOCamlValueErr "TUIntPtr"
      end
    | true, Some (TBasicType t) ->
      begin
      match t with
      | TUTF8 -> minfo, "Val_option_string"
      | TFileName -> minfo, "Val_option_string"
      | TPtr -> cToOCamlValueErr "TPtr"
      | TIntPtr -> cToOCamlValueErr "TIntPtr"
      | TUIntPtr -> cToOCamlValueErr "TUIntPtr"
      | _ -> raise (CGErrorNotImplemented "(cToOCamlValue) BasicType isn't implemented because this type should not be nullable" )
      end
    | _, Some TError -> cToOCamlValueErr "TError"
    | _, Some TVariant -> cToOCamlValueErr "TVariant"
    | _, Some TParamSpec -> cToOCamlValueErr "TParamSpec"
    | _, Some (TCArray (_, _, _, _)) -> cToOCamlValueErr "TCArray"
    | _, Some (TGArray _) -> cToOCamlValueErr "TGArray"
    | _, Some (TPtrArray _) -> cToOCamlValueErr "TPtrArray"
    | _, Some TByteArray -> cToOCamlValueErr "TByteArray"
    | _, Some (TGList _) -> cToOCamlValueErr "TGList"
    | _, Some (TGSList _) -> cToOCamlValueErr "TGSList"
    | _, Some (TGHash (_, _)) -> cToOCamlValueErr "TGHash"
    | _, Some (TGClosure _) -> cToOCamlValueErr "TGClosure"
    | false, Some (TInterface n) ->
      let api = findAPIByName cfg n in
      begin
      match api with
      | APIConst _ -> cToOCamlValueErr "APIConst"
      | APIFunction _ -> cToOCamlValueErr "APIFunction"
      | APICallback _ -> cToOCamlValueErr "APICallback"
      | APIEnum _ ->
        let minfo = addCDep minfo (n.namespace ^ "Enums") in
        minfo, valEnum n
      | APIFlags _ -> cToOCamlValueErr "APIFlags"
      | APIInterface _ ->
        let minfo = addCDep minfo (n.namespace ^ n.name) in
        minfo, valInterface n
      | APIObject _ ->
        let minfo = addCDep minfo (n.namespace ^ n.name) in
        minfo, valObject n
      | APIStruct _ -> cToOCamlValueErr "APIStruct"
      | APIUnion _ -> cToOCamlValueErr "APIUnion"
      end
    | true, Some (TInterface n) ->
      let api = findAPIByName cfg n in
      begin
      match api with
      | APIConst _ -> cToOCamlValueErr "APIConst"
      | APIFunction _ -> cToOCamlValueErr "APIFunction"
      | APICallback _ -> cToOCamlValueErr "APICallback"
      | APIEnum _ -> cToOCamlValueErr "APIEnum"
      | APIFlags _ -> cToOCamlValueErr "APIFlags"
      | APIInterface _ ->
        let minfo = addCDep minfo (n.namespace ^ n.name) in
        minfo, valOptInterface n
      | APIObject _ -> 
        let minfo = addCDep minfo (n.namespace ^ n.name) in
        minfo, valOptObject n
      | APIStruct _ -> cToOCamlValueErr "APIStruct"
      | APIUnion _ -> cToOCamlValueErr "APIUnion"
      end
    | _, Some TGValue -> assert false (*in haskell non c'è nel pattern matching..*)
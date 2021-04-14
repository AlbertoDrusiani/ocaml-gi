open BasicTypes
open Ctypes
open Parser
open Utils

(* string -> basic_type option *)
let nameToBasicType str =
    match str with
    | "gpointer" -> Some TPtr
    | "gboolean" -> Some TBoolean
    | "gchar" -> Some TInt8
    | "gint" -> Some TInt
    | "guint" -> Some TUInt
    | "glong" -> Some TLong
    | "gulong" -> Some TULong
    | "gint8" -> Some TInt8
    | "guint8" -> Some TUInt8
    | "gint16" -> Some TInt16
    | "guint16" -> Some TUInt16
    | "gint32" -> Some TInt32
    | "guint32" -> Some TUInt32
    | "gint64" -> Some TInt64
    | "guint64" -> Some TUInt64
    | "gfloat" -> Some TFloat
    | "gdouble" -> Some TDouble
    | "gunichar" -> Some TUniChar
    | "GType" -> Some TGType
    | "utf8" -> Some TUTF8
    | "filename" -> Some TFileName
    | "gintptr" -> Some TIntPtr
    | "guintptr" -> Some TUIntPtr
    | "gshort" -> 
            begin
                match sizeof short with
                  | 2 -> Some TInt16
                  | 4 -> Some TInt32
                  | 8 -> Some TInt64
                  | _ -> assert false
            end
    | "gushort" ->
            begin
                match sizeof ushort with
                  | 2 -> Some TUInt16
                  | 4 -> Some TUInt32
                  | 8 -> Some TInt64
                  | _ -> assert false
            end
    | "gssize" ->
            begin
                match sizeof PosixTypes.ssize_t with
                  | 4 -> Some TInt32
                  | 8 -> Some TInt64
                  | _ -> assert false
            end
    | "gsize" ->
            begin
                match sizeof size_t with
                  | 4 -> Some TUInt32
                  | 8 -> Some TUInt64
                  | _ -> assert false
            end
    | _ -> None
    
(* xml -> string -> type_ml *)
let rec parseArrayInfo ns aliases typ =
    match queryAttr "name" typ with
    | Some "GLib.Array" -> TGArray (parseType typ ns aliases)
    | Some "GLib.PtrArray" -> TPtrArray (parseType typ ns aliases)
    | Some "GLib.ByteArray" -> TByteArray
    | Some _ -> assert false
    | None -> parseCArrayType typ ns aliases

(* xml -> string -> type_ml option*)
(* TODO da dover gestire i vari assert false, li gestiamo con option, result, monadi?*)
and parseType typ ns aliases =
    match parseTypeElements typ ns aliases with
    | [Some e] ->  e
    | [] -> assert false
    | [None] -> assert false
    | _ -> assert false

(* xml -> string -> type_ml *)    
and parseCArrayType typ ns aliases =
    let zeroTerminated = match queryAttr "zero-terminated" typ with
                         | Some b -> parseBool b
                         | None -> true
    in let length = match queryAttr "length" typ with
                    | Some l -> parseIntegral l
                    | None -> -1
    in let fixedSize = match queryAttr "fixed-size" typ with
                       | Some s -> parseIntegral s
                       | None -> -1
    in let elementType = parseType typ ns aliases in
    TCArray (zeroTerminated, fixedSize, length, elementType)

(* xml -> string -> type_ml list option *)    
and parseTypeElements typ ns aliases =
    let types =  List.map (parseTypeInfo ns aliases) (parseChildrenWithLocalName "type" typ) in
    let arrays = List.map (parseArrayInfo ns aliases) (parseChildrenWithLocalName "array" typ) in
    types @ applyOption arrays

(* string -> xml -> type_ml option *)
and parseTypeInfo ns aliases typ =
    let typeName = getAttr "name" typ in
    if typeName = "none"
    then None
    else Some (parseTypeName typeName typ ns aliases)

(* string -> xml -> string -> type_ml *)
and parseTypeName typename typ ns aliases =
    match nameToBasicType typename with
    | Some b -> TBasicType b
    | None -> match String.split_on_char '.' typename with
              | xs::x::[] -> parseFundamentalType xs x typ ns aliases
              | x::[] -> parseFundamentalType ns x typ ns aliases
              | _ -> assert false

(* string -> string -> xml -> string -> type_ml *)
and parseFundamentalType str1 str2 typ ns aliases =
    match str1, str2 with
    | "GLib", "List" -> TGList (parseListType typ ns aliases)
    | "GLib", "SList" -> TGSList (parseListType typ ns aliases)
    | "GLib", "HashTable" -> parseHashTable typ ns aliases
    | "GLib", "Error" -> TError
    | "GLib", "Variant" -> TVariant
    | "GObject", "ParamSpec" -> TParamSpec
    | "GObject", "Value" -> TGValue
    | "GObject", "Closure" -> parseClosure typ ns aliases
    | namespace, n -> resolveQualifiedTypeName {namespace = namespace; name = n} aliases

(* xml -> string -> type_ml *)
and parseListType typ ns aliases = 
    match queryType typ ns aliases with
    | Some t -> t
    | None -> TBasicType TPtr

(* xml -> string -> type_ml *)
and parseHashTable typ ns aliases =
    match parseTypeElements typ ns aliases with
    | (Some key)::(Some value)::[] -> TGHash (key, value)
    | _ -> assert false

(* xml -> string -> type_ml *)
and parseClosure typ ns aliases =
    match queryAttr "closure-type" typ with
    | Some t -> TGClosure (Some (parseTypeName t typ ns aliases))
    | None -> TGClosure None

(* xml -> string -> type_ml option *)
and queryType typ ns aliases =
    match parseTypeElements typ ns aliases with
    | [Some e] -> Some e
    | [] -> None (*TODO* in Haksell [] cosa significa? *)
    | [None] -> assert false
    | _ -> assert false

(* xml -> string option *)
let queryCType typ =
    queryAttrWithNamespace CGIRNS "type" typ

(* xml -> string *)
let parseCType typ =
    getAttrWithNamespace CGIRNS "type" typ

(* xml -> string list *)
let parseCTypeNameElements typ =
    let types = List.map queryCType (parseChildrenWithLocalName "type" typ) in
    let arrays = List.map queryCType (parseChildrenWithLocalName "array" typ) in
    let f x = x in
    List.filter_map f (types @ arrays)


(* xml -> string -> type_ml option TODO *)
let parseOptionalType typ ns aliases =
    match parseTypeElements typ ns aliases with
    | [e] -> e
    | [] -> assert false
    | _ -> assert false

(* xml -> string option *)
let queryElementCType typ =
    match parseCTypeNameElements typ with
    | [ctype] -> Some ctype
    | [] -> None
    | _ -> assert false


open BasicTypes
open CTypes
open Parser

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
    | "filename" -> Some TFilenName
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
    

let queryType typ ns =
    match parseTypeElements typ ns with
    | [Some e] -> Some e
    | [] -> None (*TODO* in Haksell [] cosa significa? *)
    | [None] -> assert false


let parseHashTable typ ns =
    match parseTypeElements typ ns with
    | [Some key, Some value] -> TGHash (key value)
    | _ -> assert false


let parseTypeName typename typ ns =
    match nameToBasicType typename with
    | Some b -> TBasicType b
    | None -> match String.split_on_char '.' typename with
              | ns::n -> parseFundamentalType ns n
              | n::[] -> parseFundamentalType ns n
              | _ -> assert false

let parseClosure typ ns =
    match queryAttr "closure-type" typ with
    | Some t -> TGClosure (Some (parseTypeName t typ ns))
    | None -> TGClosure None

let parseListType typ ns = 
    match queryType typ ns with
    | Some t -> t
    | None -> TBasicType TPtr


let parseFundamentalType str1 str2 typ ns =
    match str1, str2 with
    | "GLib", "List" -> TGList (parseListType typ ns)
    | "GLib", "SList" -> TGSList (parseListType typ ns)
    | "GLib", "HashTable" -> parseHashTable typ ns
    | "GLib", "Error" -> TError
    | "GLib", "Variant" -> TVariant
    | "GObject", "ParamSpec" -> TParamSpec
    | "GObject", "Value" -> TGValue
    | "GObject", "Closure" -> parseClosure typ ns
    | ns, n -> assert false (*TODO resolveQualifiedTypeName {namespace = ns; name = n}*)

let parseTypeInfo typ ns =
    let typeName = getAttr "name" typ in
    if typeName == "none"
    then None
    else Some (parseTypeName typeName ns)


let parseTypeElements typ ns =
    let types = parseChildrenWithLocalName "type" (parseTypeInfo typ ns) in
    let arrays = parseChildrenWithLocalName "array" (parseArrayInfo typ ns)
    in types (*TODO, si può applicare map a Some?*)


let parseType typ ns =
    match parseTypeElements typ ns with
    | [Some e] -> e
    | [] -> assert false;
    | [None] -> assert false;
    | _ -> assert false;


let parseArrayInfo typ ns =
    match queryAttr "name" typ with
    | Some "GLib.Array" -> TGArray (parseType typ ns)
    | Some "GLib.PtrArray" -> TPtrArray (parseType typ ns)
    | Some "GLib.ByteArray" -> TByteArray
    | Some _ -> assert false
    | None -> parseCArrayType typ ns


let parseCArrayType typ ns =
    let zeroTerminated = match queryAttr "zero-terminated" typ with
                         | Some b -> parseBool b
                         | None -> true
    in let length = match queryAttr "length" typ with
                    | Some l -> parseIntegral l
                    | None -> -1
    in let fixedSize = match queryAttr "fixed-size" typ with
                       | Some s -> parseIntegral s
                       | None -> -1
    in let elementType = parseType typ ns in
    TCArray zeroTerminated fixedSize length elementType



let queryCType typ =
    queryAttrWithNamespace CGIRNS "type" typ


let parseCType typ =
    getAttrWithNamespace CGIRNS "type" typ



let parseCTypeNameElements typ =
    let types = parseChildrenWithLocalName "type" queryCType in
    let arrays = parseChildrenWithLocalName "array" queryCType in
    let f x = x in
    List.filter_map f (types @@ arrays)


let parseOptionalType typ ns =
    match parseTypeElements typ ns with
    | [e] -> e
    | [] -> assert false
    | _ -> assert false


let queryElementCType typ =
    match parseCTypeNameElements typ with
    | [ctype] -> Some ctype
    | [] -> None
    | _ -> assert false





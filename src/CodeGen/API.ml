
open GObject_introspection


type gir_info = {
    girPCPackages: string list;
    girNSName: string;
    girNSVersion: string;
    girAPIs: name*api list;
   (* girCTypes: *)
}

type gir_namespace = {
    nsName: string;
    nsVersion: string;
    nsAPIs: name*api list;
   (* nsCTypes*)
}

type gir_info_parse = {
    girIPPackage: string option list;
    girIPIncludes: string*string option list;
    girIPNamespaces: gir_namespace option;


type api = 
    | APIConst of constant;
    | APIFunction of function_ml
    | APICallback of callback
    | APIEnum of enumeration
    | APIFlags of flags
    | APIInterface of interface
    | APIObject of object_ml
    | APIStruct of struct_ml
    | APIUnioni of union







open GObject_introspection
open ../GIR/BasicTypes



type api = 
    | APIConst of Bindings.Base_info.info_type
    | APIFunction of function_ml
    | APICallback of callback
    | APIEnum of enumeration
    | APIFlags of flags
    | APIInterface of interface
    | APIObject of object_ml
    | APIStruct of struct_ml
    | APIUnioni of union



type gIRInfo
   (* { girPCPackages : string list;
      girNSName : string;
      girNSVersion : string;
      girAPIs : (name) 
    }*)



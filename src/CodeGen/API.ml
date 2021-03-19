
module GI = GObject_introspection
module B = Bindings

open GIR.BasicTypes
open GIR.Constant
open GIR.Function
open GIR.Callback
open GIR.Enum
open GIR.Interface
open GIR.Object
open GIR.Struct
open GIR.Union
open GIR.APITypes
open GIR.Flags

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
    nsAPIs: (name*api) list ref;
   (* nsCTypes, utilizzati in Haskell per fare non si sa cosa, CGIRNS*)
}

type gir_info_parse = {
    girIPPackage: string option list;
    girIPIncludes: string*string option list;
    girIPNamespaces: gir_namespace option list;
}


(*passo il namespace, gli alias, element, il wrapper api)*)
    

let parseInfo namespace index =
    let info = GI.Repository.get_info namespace index in
    let info_type = GI.Base_info.get_type info in
    print_endline(B.Base_info.string_of_info_type info_type);
    let name, api = match info_type with
    | Invalid -> assert false
    | Function -> 
            begin 
                match parseFunction @@ GI.Function_info.cast_from_baseinfo info with
                 | (name, api) -> name, APIFunction api
            end
    | Callback -> 
            begin
                match parseCallback @@ GI.Callable_info.cast_from_baseinfo info with
                | (name, api) -> name, APICallback api
            end
    | Struct -> 
            begin
                match parseStruct @@ GI.Struct_info.from_baseinfo info with
                | (name, api) -> name, APIStruct api
            end
    | Boxed -> assert false
    | Enum -> begin 
                match parseEnum @@ GI.Enum_info.cast_from_baseinfo info with
                | (name, api) -> name, APIEnum api
              end
    | Flags -> begin
                match parseFlags @@ GI.Enum_info.cast_from_baseinfo info with
                | (name, api) -> name, APIFlags api
                end
    | Object -> begin 
                    match parseObject @@ GI.Object_info.cast_from_baseinfo info with
                    | (name, api) -> name, APIObject api
                end
    | Interface -> begin 
                        match parseInterface @@ GI.Interface_info.cast_from_baseinfo info with
                        | (name, api) -> name, APIInterface api
                   end
    | Constant -> begin
                        match parseConstant @@ GI.Constant_info.cast_from_baseinfo info with
                        | (name, api) -> name, APIConst api
                  end

    | Invalid_0 -> assert false
    | Union -> begin 
                 match parseUnion @@ GI.Union_info.cast_from_baseinfo info with
                 | (name, api) -> name, APIUnion api
               end
    | Value -> assert false (*dentro ad enum*)
    | Signal -> assert false (*all'interno di object*)
    | Vfunc -> assert false
    | Property -> assert false (*all'interno di object*)
    | Field -> assert false (*all'interno di struct*)
    | Arg -> assert false (*all'interno di callable*)
    | Type -> assert false (*all'interno di tutto*)
    | Unresolved -> assert false
    in name, api

let parseNamespace namespace =
    let _ = Result.get_ok (GI.Repository.require namespace()) in
    let version = GI.Repository.get_version namespace in
    let name = namespace in
    let n_apis = ref [] in
    for i = 0 to (GI.Repository.get_n_infos namespace) - 1  do
        n_apis := parseInfo namespace i :: !n_apis; 
    done;
    { 
      nsName = name;
      nsVersion = version;
      nsAPIs = n_apis;
    }


let run =
    parseNamespace "Gtk";;

run;;




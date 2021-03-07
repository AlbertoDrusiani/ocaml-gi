module GI = GObject_introspection

open BasicTypes
open Method
open Signal
open Property

type interface ={ 
    ifTypeInit: string option;
    ifCType: string option;
   (* ifDocumentation: documentation;*)
    ifPrerequisites: name list ref;
    ifProperties: property list ref;
    ifSignals: signal list ref;
    ifMethods: method_ml list ref;
   (* ifAllocationInfo: allocation_info;*)
    ifDeprecated: bool;
    }

(*prende un base_info*)    
let parsePrerequisite p =
    { namespace = GI.Base_info.get_namespace p;
      name = match GI.Base_info.get_name p with
      |Some x -> x
      |None -> "Errore"
    }
             
 
let parseInterface j =
    let l_prerequisite = ref [] in
    for i = GI.Interface_info.get_n_prerequisites j downto 0 do
        l_prerequisite := (GI.Interface_info.get_prerequisite j i |> parsePrerequisite) :: !l_prerequisite
    done; 
    let l_properties = ref [] in
    for i = GI.Interface_info.get_n_properties j downto 0 do
        l_properties := (GI.Interface_info.get_property j i |> parseProperty) :: !l_properties
    done;
    let l_signals = ref [] in
    for i = GI.Interface_info.get_n_signals j downto 0 do
        l_signals := (GI.Interface_info.get_signal j i |> parseSignal) :: !l_signals
    done;
    let l_methods = ref [] in
    for i = GI.Interface_info.get_n_methods j downto 0 do
        l_methods := (GI.Interface_info.get_method j i |> parseMethod) :: !l_methods
    done;
    let name = GI.Interface_info.cast_to_baseinfo j |> getOnlyName in
    (name,
    { ifTypeInit = GI.Interface_info.cast_to_registeredtypeinfo j |> GI.Registered_type_info.get_type_init;
      ifCType = GI.Interface_info.cast_to_registeredtypeinfo j |> GI.Registered_type_info.get_type_name;
       ifPrerequisites = l_prerequisite;
       ifProperties = l_properties;
       ifSignals = l_signals;
       ifMethods = l_methods;
       ifDeprecated = GI.Interface_info.cast_to_baseinfo j |> GI.Base_info.is_deprecated;
    })





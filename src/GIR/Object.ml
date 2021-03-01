module GI = GObject_introspection

open BasicTypes
open Method
open Property
open Signal
open Interface

type object_ml =
    { objParent: name; (*in haskell è name_option*)
      objTypeInit: string;
      objTypeName: string;
      objCType: string option;
      objRefFunc: string option;
      objUnrefFunc: string option;
      objSetValueFunc: string option;
      objGetValueFunc: string option;
      objInterfaces: name list ref;
      objDeprecated: bool;
      (*objDocumentation: documentation *)
      objMethods: method_ml list ref;
      objProperties: property list ref;
      objSignals: signal list ref;
    }


(*passo un object_info*)
let parseObject o =
    let l_interfaces = ref [] in
    for i = GI.Object_info.get_n_interfaces o downto 0 do
        l_interfaces := (GI.Object_info.get_interface o i |> GI.Interface_info.cast_to_baseinfo |> getName) :: !l_interfaces
    done;
    let l_methods = ref [] in
    for i = GI.Object_info.get_n_methods o downto 0 do
        l_methods := (GI.Object_info.get_method o i |> parseMethod) :: !l_methods
    done;
    let l_properties = ref [] in
    for i = GI.Object_info.get_n_properties o downto 0 do
        l_properties := (GI.Object_info.get_property o i |> parseProperty) :: !l_properties
    done;
    let l_signals = ref [] in
    for i = GI.Object_info.get_n_signals o downto 0 do
        l_signals := (GI.Object_info.get_signal o i |> parseSignal) :: !l_signals
    done;
    { objParent = GI.Object_info.get_parent o |> getName;
      objTypeInit = GI.Object_info.get_type_init o;
      objTypeName = GI.Object_info.get_type_name o;
      objCType = GI.Object_info.cast_to_registeredtypeinfo o |> GI.Registered_type_info.get_type_name;
      objRefFunc = GI.Object_info.get_ref_function o;
      objUnrefFunc = GI.Object_info.get_unref_function o;
      objSetValueFunc = GI.Object_info.get_set_value_function o;
      objGetValueFunc = GI.Object_info.get_get_value_function o;
      objInterfaces = l_interfaces;
      objDeprecated = GI.Object_info.cast_to_baseinfo o |> GI.Base_info.is_deprecated;
      objMethods = l_methods;
      objProperties = l_properties;
      objSignals = l_signals;
    }




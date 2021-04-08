open Arg
open BasicTypes
open Parser
open Type
open Documentation
open Deprecation

type callable ={ 
    returnType: type_ml option;
    returnMayBeNull: bool;
    returnTransfer: transfer;
    returnDocumentation: documentation;
   (* args: arg list;*)
    skipReturn: bool;
    callableThrows: bool;
    callableDeprecated: deprecation_info option;
    callableDocumentation: documentation;
    callableResolvable: bool option;
    }


(*let parseArgs ns el =
  let parseArgSet = parseChildrenWithLocalName "parameter" el in
  let paramSets = parseChildrenWithLocalName "parameters" el 
TODO non capisco come gestirla, troppi parser..*)

(* xml -> string -> (type_ml option * bool * transfer * bool * documentation) *)
let parseOneReturn el ns =
  let returnType = parseOptionalType el ns in
  let allowNone = optionalAttr "allow-none" false el parseBool in
  let nullable = optionalAttr "nullable" false el parseBool in
  let transfer = parseTransfer el in
  let doc = parseDocumentation el in
  let skip = optionalAttr "skip" false el parseBool in
  returnType, allowNone || nullable, transfer, skip, doc


(* xml -> string -> (type_ml option * bool * transfer * bool * documentation) *)
let parseReturn el ns =
  (*let return_set_list = parseChildrenWithLocalName "return-value" el in
  let length = List.length return_set_list in
  let returnSets = List.map2 parseOneReturn return_set_list (List.init length (fun _ -> ns)) in*)
  let returnSets = List.map (fun x -> x ns) (List.map parseOneReturn (parseChildrenWithLocalName "return-value" el)) in
  match returnSets with
  | r::[] -> r
  | [] -> assert false
  | _ -> assert false


let parseCallable el ns =
 (* let args = parseArgs el ns in*)
  let returnType, mayBeNull, transfer, skip, returnDoc = parseReturn el ns in
  let deprecated = parseDeprecation el in
  let docs = parseDocumentation el in
  let throws = optionalAttr "throws" false el parseBool in
  { returnType = returnType;
    returnMayBeNull = mayBeNull;
    returnTransfer = transfer;
    returnDocumentation = returnDoc;
   (* args = args;*)
    skipReturn = skip;
    callableThrows = throws;
    callableDeprecated = deprecated;
    callableDocumentation = docs;
    callableResolvable = None;
  }




(*module GI = GObject_introspection
module B = Bindings

open Arg
open BasicTypes

type callable ={ 
    returnType: type_ml option;
    returnMayBeNull: bool;
    returnTransfer: transfer;
   (* returnDocumentation: documentation;*)
    args: arg list ref;
    skipReturn: bool;
    callableThrows: bool;
    (*callableDeprecated: deprecation_info option;*)
    (*callableDOcumentation: documentation;*)
    (*callableResolvable: bool option;*)
    }


(*helper per wrappare type_ml in type_ml option*)
let wrap_callable_return_type a = 
    if GI.Callable_info.may_return_null a then
        Some (GI.Callable_info.get_return_type a |> cast_to_type_ml)
    else
        None

    
(*passo un Callable_info*)
let parseCallable a =
    let l = ref [] in
    prerr_endline("ppppppppp CALLABLE pppppppppppp");
    for i = (GI.Callable_info.get_n_args a) - 1 downto 0 do
        l := (GI.Callable_info.get_arg a i |> parseArg) :: !l
    done;
    prerr_endline("FINE FOR ARGS CALLABLE");
    { returnType = GI.Callable_info.get_return_type a |> cast_to_type_ml;
      returnMayBeNull = GI.Callable_info.may_return_null a;
      returnTransfer = GI.Callable_info.get_caller_owns a |> parseTransfer;
      args = l;
      skipReturn = GI.Callable_info.skip_return a;
      callableThrows = GI.Callable_info.can_throw_gerror a; 
    }*)

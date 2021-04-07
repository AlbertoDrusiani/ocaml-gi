open BasicTypes
open Parser
open Type
open Documentation

type direction =
    | DirectionIn
    | DirectionOut
    | DirectionInout


type scope =
    | ScopeTypeInvalid
    | ScopeTypeCall
    | ScopeTypeAsync
    | ScopeTypeNotified


type arg = { 
    argCName: string;
    argType: type_ml;
    direction: direction;
    mayBeNull: bool;
    argDoc: documentation;
    argScope: scope;
    argClosure: int;
    argDestroy: int;
    argCallerAllocates: bool;
    transfer: transfer;
    }

(* xml -> transfer *) 
let parseTransfer el =
  match getAttr "transfer-ownership" el with
  | "none" -> TransferNothing
  | "container" -> TransferContainer
  | "full" -> TransferEverything
  | _ -> assert false


(* string -> scope *)
let parseScope str =
  match str with
  | "call" -> ScopeTypeCall
  | "async" -> ScopeTypeAsync
  | "notified" -> ScopeTypeNotified
  | _ -> assert false


(* string -> direction *)
let parseDirection str =
  match str with
  | "in" -> DirectionIn
  | "out" -> DirectionOut
  | "inout" -> DirectionInout
  | _ -> assert false

(* xml -> string -> arg *)
let parseArg el ns =
  let name = getAttr "name" el in
  let ownership = parseTransfer el in
  let scope = optionalAttr "scope" ScopeTypeInvalid el parseScope in
  let d = optionalAttr "direction" DirectionIn el parseDirection in
  let closure = optionalAttr "closure" (-1) el parseIntegral in
  let destroy = optionalAttr "destroy" (-1) el parseIntegral in
  let nullable = optionalAttr "nullable" false el parseBool in
  let allowNone = optionalAttr "allow-none" false el parseBool in
  let mayBeNull = if d == DirectionIn
                  then nullable || allowNone
                  else nullable
  in let callerAllocates = optionalAttr "caller-allocates" false el parseBool in
  let t = parseType el ns in
  let doc = parseDocumentation el in
  { argCName = name;
    argType = t;
    argDoc = doc;
    direction = d;
    mayBeNull = mayBeNull;
    argScope = scope;
    argClosure = closure;
    argDestroy = destroy;
    argCallerAllocates = callerAllocates;
    transfer = ownership;
  }


(*module GI = GObject_introspection
module B = Bindings

open BasicTypes

type direction =
    | DirectionIn
    | DirectionOut
    | DirectionInout


type scope =
    | ScopeTypeInvalid
    | ScopeTypeCall
    | ScopeTypeAsync
    | ScopeTypeNotified


type arg = { 
    argCName: string option;
    argType: type_ml option;
    direction: direction;
    mayBeNull: bool;
   (* argDoc: string;*) (*in haskell Documentation*)
    argScope: scope;
    argClosure: int;
    argDestroy: int;
    argCallerAllocates: bool;
    transfer: transfer;
    }

            
(*passo un Bindings.Arg_info.scope_type*)
let parseScope (s : B.Arg_info.scope_type) =
    match s with
    | Invalid -> ScopeTypeInvalid
    | Call -> ScopeTypeCall
    | Async -> ScopeTypeAsync
    | Notified -> ScopeTypeNotified

(*passo un Bindings.Arg_info.direction*)
let parseDirection (d : B.Arg_info.direction) =
    match d with
    | In -> DirectionIn
    | Out -> DirectionOut
    | InOut -> DirectionInout

(*passo un Arg_info*)
let parseArg a =
    prerr_endline("ppppppppppp ARG ppppppppppppp");
    { argCName = GI.Arg_info.to_baseinfo a |> GI.Base_info.get_name;
      argType = GI.Arg_info.get_type a |> cast_to_type_ml;
     (* argDoc = ;*)
      direction = GI.Arg_info.get_direction a |> parseDirection;
      mayBeNull = GI.Arg_info.may_be_null a;
      argScope = GI.Arg_info.get_scope a |> parseScope;
      argClosure = GI.Arg_info.get_closure a;
      argDestroy = GI.Arg_info.get_destroy a;
      argCallerAllocates = GI.Arg_info.is_caller_allocates a;
      transfer = GI.Arg_info.get_ownership_transfer a |> parseTransfer;
    }*)

module GI = GObject_introspection
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
    argCName: string;
    argType: type_ml;
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
    let name =
        match GI.Arg_info.cast_to_baseinfo a |> GI.Base_info.get_name with
        | Some x -> x
        | None -> "Errore"
    in 
    { argCName = name;
      argType = GI.Arg_info.get_type a |> cast_to_type_ml;
     (* argDoc = ;*)
      direction = GI.Arg_info.get_direction a |> parseDirection;
      mayBeNull = GI.Arg_info.may_be_null a;
      argScope = GI.Arg_info.get_scope a |> parseScope;
      argClosure = GI.Arg_info.get_closure a;
      argDestroy = GI.Arg_info.get_destroy a;
      argCallerAllocates = GI.Arg_info.is_caller_allocates a;
      transfer = GI.Arg_info.get_ownership_transfer a |> parseTransfer;
    }

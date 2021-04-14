open BasicTypes
open Parser
open Type
open Documentation
open XMLUtils

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
  (*TODO debugging*)
  let str = match lookupAttr "name" el with
    | Some c -> c
    | None -> "NONE"
  in
  prerr_endline ("L'elemento è un: " ^ (Xml.tag el) ^ " con name = " ^ str);
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
let parseArg ns aliases el =
  prerr_endline ("Inizio il parse Arg");
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
  let t = parseType el ns aliases in
  let doc = parseDocumentation el in
  prerr_endline "FINITO IL PARSE ARG";
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

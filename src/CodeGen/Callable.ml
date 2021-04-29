
open GIR.Arg
open GIR.Callable
open GIR.BasicTypes
open Conversions
open Util
open Code
open Naming
open TypeRep


(* config -> module_info -> int -> arg -> module_info*string*)
let foreignArgConverter cfg minfo i a =
  let optionVal argNum justConv nothingVal =
    let argNumStr = string_of_int argNum in
    let args = String.concat ", " [argNumStr; justConv; nothingVal] in
    "Option_val(arg" ^ args ^ ") Ignore"

  in let minfo, conv = ocamlValueToC cfg minfo (a.argType) in
  if a.mayBeNull
  then minfo, optionVal i conv "NULL"
  else minfo, conv





let arrayLenghtsMap callable =
  let rec go p acc =
    match p with
    | [] -> acc
    | x::xs -> 
        match x.argType with
        |TCArray (false, fixedSize, length, _) -> 
            if fixedSize > -1 || length = -1
            then go xs acc
            else go xs ((x, List.nth callable.args length) :: acc)
        | _ -> go xs acc
    in go callable.args []



let arrayLengths callable =
  List.map snd (arrayLenghtsMap callable) @
  match callable.returnType with
  | Some (TCArray (false, (-1), length, _)) ->
    if length > 1 then [List.nth callable.args length] else []
  | _ -> []


let callableHInArgs callable expose =
  let inArgs = List.filter (fun x -> x.direction != DirectionOut) callable.args in
  let closures = List.map (fun x -> x.argClosure) inArgs 
                |> List.filter (fun x -> x!= 1)
                |> List.map (fun x -> List.nth callable.args x) in
  let destroyers = List.map (fun x -> x.argDestroy) inArgs 
                |> List.filter (fun x -> x!= 1)
                |> List.map (fun x -> List.nth callable.args x) in
  let omitted = 
    match expose with
    | WithoutClosures -> arrayLengths callable @ closures @ destroyers
    | WithClosures -> arrayLengths callable
  in List.filter (fun x -> not (List.mem x omitted)) inArgs, omitted


let callableHInArgs' c =
  List.filter (fun a -> a.direction != DirectionOut) c.args


let callableHOutArgs c =
  let outArgs = List.filter (fun a -> a.direction != DirectionIn) c.args in
  List.filter (fun x -> not (List.mem x (arrayLengths c))) outArgs


let arrayLenghtsMap callable =
  
  let rec go l acc =
    match l with
    | [] -> acc
    | x::xs ->
      match x.argType with
      | TCArray (false, fixedSize, length, _) ->
        if fixedSize > -1 || length = -1
        then go xs acc
        else go xs ((x, List.nth callable.args length)::acc)
      | _ -> go xs acc
  
  in go callable.args []

let fixupCallerAllocates c =
  let fixupDir a =
    match a.argType with
    | TCArray (_, _, l, _) ->
      if a.argCallerAllocates && l > -1
      then { a with direction = DirectionInout; transfer = TransferEverything}
      else a
    | _ -> a

  in let lengthsMap = (List.map swap (arrayLenghtsMap c)) |> List.to_seq |> ArgMap.of_seq

  in let fixupLength a =
    match ArgMap.find_opt a lengthsMap with
    | None -> a
    | Some array -> 
      if array.argCallerAllocates
      then { a with direction = DirectionIn}
      else a

  in { c with args = List.map (fun x -> fixupDir x |> fixupLength) c.args}



let callableOCamlTypes cfg cgstate minfo c =
  let argToTyperep converter a =
    let cgstate, ocamlType = converter a.argType in
    cgstate, if a.mayBeNull then OptionCon (ocamlType) else ocamlType

  in let cgstate, inArgs' = 
    List.fold_left_map (fun cgstate arg -> argToTyperep (ocamlType cfg cgstate minfo) arg) cgstate (callableHInArgs' c) in
  let cgstate, outArgs = 
    List.fold_left_map (fun cgstate arg -> argToTyperep (outParamOcamlType cfg cgstate minfo) arg) cgstate (callableHOutArgs c) in
  let inArgs =
    match inArgs' @ outArgs with
    | [] -> [TextCon "unit"]
    | _ -> inArgs'
  in let cgstate, retType =
    match c.returnType with
    | None -> cgstate, TextCon "unit"
    | Some t -> outParamOcamlType cfg cgstate minfo t
  in let optionalRetType =
    if c.returnMayBeNull then OptionCon retType else retType
  in let outArgs' =
    match outArgs, c.returnType with
    | [], _ -> optionalRetType
    | _, None -> TupleCon outArgs
    | _, _ -> TupleCon (optionalRetType::outArgs)
  in cgstate, inArgs @ [outArgs']



let canGenerateCallable cfg cgstate minfo cb =
  (*TODO forse try catch*)
  let cgstate, _ = callableOCamlTypes cfg cgstate minfo cb in
  let minfo = List.fold_left (fun info x -> fst (foreignArgConverter cfg info 0 x)) minfo cb.args in
  cgstate, minfo, cb.callableThrows
  


let genOCamlExternal cfg cgstate minfo mn cSymbol callable =
  let addQuote s =
    match s with
    | "" -> ""
    | s -> "\"" ^ s ^ "\""

  in let currNS = currentNS minfo in
  let minfo = line ("external " ^ ocamlIdentifier mn ^ " : ") minfo in
  let cgstate, minfo = 
    indent (fun minfo ->
      let cgstate, argTypes = callableOCamlTypes cfg cgstate minfo callable in
      let argTypesStr = List.map (typeShow currNS) argTypes in
      let inArgs = List.rev argTypesStr |> List.tl in
      let outArg = List.rev argTypesStr |> List.hd in
      let minfo = 
        List.fold_left (fun info x -> line (x ^ " -> ") info) minfo inArgs in
      let minfo = line outArg minfo in
      let fn = mlGiPrefix mn cSymbol in
      let nativeFn = if List.length inArgs > 5 then fn ^ "_bc" else "" in
      if callable.callableThrows 
      then cgstate, minfo (*raise (CGErrorNotImplemented "Methods throwing exceptions are not implemented yet")*)
      else cgstate, line ("= " ^ addQuote nativeFn ^ " " ^ addQuote fn) minfo
    ) minfo in
    cgstate, minfo



let genMlMacro cfg cgstate minfo mn cSymbol callable =
  let inArgs = callableHInArgs' callable in
  let nArgs = List.length callable.args in
  let outArgs = callableHOutArgs callable in
  if List.for_all (fun a -> a.direction = DirectionInout) callable.args
  then raise (CGError (CGErrorNotImplemented "genMlMacro: inout parameters are not implemented yet"))
  else 
    if outArgs != []
    then 
      let numOutArgs = string_of_int (List.length outArgs) in
      let numInArgs = string_of_int (List.length inArgs) in
      let outCTypes = List.map (fun x -> cType cfg (x.argType)) outArgs in
      let minfo, outConvTypes = 
        List.fold_left_map (fun minfo outArg -> 
            cToOCamlValue cfg minfo outArg.mayBeNull (Some outArg.argType)) minfo outArgs
      (*sta roba dovrebbe funzionare come una fold_map_2, con due accumulatori perché ritorno una coppia *)
      in let minfo, inArgTypes = List.fold_left2 (fun (minfo, l) idx arg -> 
        let u = foreignArgConverter cfg minfo idx arg in
        fst u, (snd u) :: l) (minfo,[]) (List.init (List.length inArgs) (fun x -> x+1)) inArgs in
      let minfo, retTypeName = cToOCamlValue cfg minfo callable.returnMayBeNull callable.returnType in
      let outArgTypes = List.map2 (fun x y -> x ^ ", " ^ y) outCTypes outConvTypes in
      let macroName =
        match callable.returnType with
        | None -> "ML_" ^ numInArgs ^ "in_" ^ numOutArgs ^ "out_discard_ret ("
        | Some _ -> "ML_" ^ numInArgs ^ "in_" ^ numOutArgs ^ "out ("
      in cline
       (macroName ^ (String.lowercase_ascii mn.namespace) ^ ", " ^ cSymbol ^
        ", " ^ (String.concat ", " inArgTypes) ^ ", " ^ (String.concat ", " outArgTypes) ^
        ", " ^ retTypeName ^ ")") minfo cgstate
    else
      let macroName = "ML_" ^ string_of_int nArgs ^ " (" in
      let minfo, retTypeName = cToOCamlValue cfg minfo callable.returnMayBeNull callable.returnType in
      let minfo, argTypes = List.fold_left2 (fun (minfo, l) idx arg -> 
        let u = foreignArgConverter cfg minfo idx arg in
        fst u, (snd u) :: l) (minfo,[]) (List.init (List.length inArgs) (fun x -> x+1)) callable.args in
      let macroArgs =
        String.concat ", " ([String.lowercase_ascii mn.namespace; cSymbol] @ argTypes @ [retTypeName])
      in cline (macroName ^ macroArgs ^ ")") minfo cgstate
 
    


let genCCallableWrapper cfg cgstate minfo mn cSymbol callable =
  let callable' = fixupCallerAllocates callable in
  let cgstate, minfo = genOCamlExternal cfg cgstate minfo mn cSymbol callable' in
  let minfo = blank minfo in
  genMlMacro cfg cgstate minfo mn cSymbol callable'
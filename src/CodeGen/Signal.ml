(*open API*)
open Code
open Util
(*open QualifiedNaming*)
open Naming
open GIR.Signal
open GIR.Arg
open GIR.Callable
open Conversions
open Callable


let argsTypeRep cfg minfo args =
  List.map (fun arg -> ocamlDataConv cfg minfo (arg.mayBeNull) arg.argType) args


let ocamlMarshaller args out sigName onName cfg minfo =
prerr_endline ("Signal 18");
  match args with
  | [] -> "marshal_unit"
  | _ ->
    let args' = List.filter (fun arg -> arg.direction = DirectionIn) args in
    let sigName' = "\"" ^ ucFirst onName ^ "::" ^ sigName ^ "\"" in
    let len = List.length args' in
    match out with
    | None ->
        let marsh = "fun f -> marshal" ^ string_of_int len in
        prerr_endline ("Signal 28");
        let argTypes = argsTypeRep cfg minfo args' in
         prerr_endline ("Signal 30");   
        String.concat " " (marsh::(argTypes @ [sigName'; "f"]))
    | Some ret ->
        let marsh = "fun f -> marshal" ^ string_of_int len ^ "_ret" in
        let argTypes = argsTypeRep cfg minfo args' in
        let retty = ocamlDataConv cfg minfo false ret in
        String.concat " " (marsh:: ("~ret:" ^ retty) :: (argTypes @ [sigName'; "f"]))



let genOCamlCallbackPrototype subsec cb _htype classe expose _doc cfg minfo =
prerr_endline ("Signal 39");
  let currNS = currentNS minfo in
  let (hInArgs, _) = callableHInArgs cb expose in
  let ret = cb.returnType in
  prerr_endline ("Signal 43");
  let classType = "`" ^ escapeOCamlReserved (camelCaseToSnakeCase (currNS ^ "_" ^ classe)) in
  prerr_endline ("Siganl 45");
  let marshaller = ocamlMarshaller hInArgs ret subsec classe cfg minfo in
  line ("let " ^ subsec ^ " = {" ^ "name=\"" ^ subsec ^ "\"; " ^ "classe=" ^
        classType ^ "; " ^ "marshaller=" ^ marshaller ^ "}") minfo



let processSignalError minfo isG signal owner err =
  let qualifiedSignalName = upperName owner ^ "::" ^ signal.sigName in
  let errText = String.concat "" [
    "(* Could not generate signal ";
    qualifiedSignalName;
    " *)\n";
    "(* Error was : ";
    err;
    " *)"
  ]
  in if isG then
    gline errText minfo
  else  
    line errText minfo


let genGSignal s on cfg minfo =
  let sn' = signalOCamlName s.sigName in 
  let on' = lowerName on |> ucFirst in
  try
    let _ = argsTypeRep cfg minfo (s.sigCallable.args) in
    gline ("  method " ^ sn' ^ " = self#connect " ^ on' ^ ".S." ^ sn') minfo
  with CGError (CGErrorNotImplemented err) -> processSignalError minfo true s on err


let genSignal s on cfg minfo =
prerr_endline ("Signal 75");
  try
    let classe = lowerName on in
    let sn' = signalOCamlName s.sigName in
    let signalConnectorName = classe ^ ucFirst sn' in
    let cbType = signalConnectorName ^ "Callback" in
    genOCamlCallbackPrototype sn' s.sigCallable cbType classe WithoutClosures s.sigDoc cfg minfo
  with CGError (CGErrorNotImplemented err) -> processSignalError minfo false s on err
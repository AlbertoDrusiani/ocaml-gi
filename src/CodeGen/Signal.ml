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

  match args with
  | [] -> "marshal_unit"
  | _ ->
    let args' = List.filter (fun arg -> arg.direction = DirectionIn) args in
    let sigName' = "\"" ^ ucFirst onName ^ "::" ^ sigName ^ "\"" in
    let len = List.length args' in
    match out with
    | None ->
        let marsh = "fun f -> marshal" ^ string_of_int len in
        let argTypes = argsTypeRep cfg minfo args' in
        String.concat " " (marsh::(argTypes @ [sigName'; "f"]))
    | Some ret ->
        let marsh = "fun f -> marshal" ^ string_of_int len ^ "_ret" in
        let argTypes = argsTypeRep cfg minfo args' in
        let retty = ocamlDataConv cfg minfo false ret in
        String.concat " " (marsh:: ("~ret:" ^ retty) :: (argTypes @ [sigName'; "f"]))



let genOCamlCallbackPrototype subsec cb _htype classe expose _doc cfg minfo =
  let currNS = currentNS minfo in
  let (hInArgs, _) = callableHInArgs cb expose in
  let ret = cb.returnType in
  let classType = "`" ^ escapeOCamlReserved (camelCaseToSnakeCase (currNS ^ "_" ^ classe)) in
  let marshaller = ocamlMarshaller hInArgs ret subsec classe cfg minfo in
  line ("let " ^ subsec ^ " = {" ^ "name=\"" ^ subsec ^ "\"; " ^ "classe=" ^
        classType ^ "; " ^ "marshaller=" ^ marshaller ^ "}") minfo




let genGSignal s on cfg minfo =
  let sn' = signalOCamlName s.sigName in 
  let on' = lowerName on |> ucFirst in
  (*TODO da inserire un try catch*)
  let _ = argsTypeRep cfg minfo (s.sigCallable.args) in
  gline ("  method " ^ sn' ^ " = self#connect " ^ on' ^ ".S." ^ sn') minfo


let genSignal s on cfg minfo =
  let classe = lowerName on in
  let sn' = signalOCamlName s.sigName in
  let signalConnectorName = classe ^ ucFirst sn' in
  let cbType = signalConnectorName ^ "Callback" in
  genOCamlCallbackPrototype sn' s.sigCallable cbType classe WithoutClosures s.sigDoc cfg minfo

open GIR.Arg
open GIR.Callable
open Conversions


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



let rec applyOption l =
    match l with
    | [] -> []
    | x::xs -> Some x :: applyOption xs

let rec list_max l max =
  match l with
  | [] -> max
  | x::xs -> 
    if x > max then list_max xs x
    else list_max xs max

let rec list_min l min =
  match l with
  | [] -> min
  | x::xs -> 
    if x < min then list_min xs x
    else list_min xs min

(* 'a list -> 'a -> 'a -> 'a option *)
let maximumMay l acc min =
    match list_max l acc with
    | c when c = min -> None
    | m -> Some m

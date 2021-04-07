let rec applyOption l =
    match l with
    | [] -> []
    | x::xs -> Some x :: applyOption xs

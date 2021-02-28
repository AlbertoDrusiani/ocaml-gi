(*implemento la take di libreria di Haskell*)
(*take :: Num -> [a] -> [a] *)
let rec take k xs = match xs with
    | [] -> []
    | x::xs -> if k = 1 then [x] else x::take (k-1) xs;;


(*prendo la lista senza l'ultimo elemento, la init di Haskell*)
(*noLast :: [a] -> [a]*)
let noLast xs = take (List.length xs -1) xs


(*paddo la stringa in input con degli spazi fino ad arrivare ad n*)
(*padTo :: Int -> String -> String*)
let padTo n s = String.concat "" [s; String.make (n - String.length s) ' ']

(*FIXME da adattare ad OCaml, prima capiamo cosa serve *)
let withComment a b = String.concat "" [padTo 40 a; "-- "; b]


(*FIXME da capire bene cosa faccia*)
let prime a = String.concat "" [a; "'"]

(* wrappa il valore tra parentesi *)
let parenthesize s = String.concat "" ["("; s; ")"]


(*tshow da fare*)

(*terror da fare*)

(*rende maiuscolo il primo carattere della stringa data*)
let ucFirst t = String.capitalize_ascii t

(*rende minusoclo il primo carattere della stringa data*)
let lcFirst t = String.uncapitalize_ascii t

let modifyQualified f s = 
    let rec modify x = 
        match x with
        | [] -> []
        | a :: [] -> f a :: []
        | a :: at -> a :: modify at
    in String.concat "."  (modify (String.split_on_char '.' s))


let splitOn x xs = 
    let rec go y acc =
        match y with
        | [] -> [List.rev acc]
        | y :: ys -> if x == y then List.rev acc :: go ys [] else go ys (y :: acc)
    in go xs []


(*let utf8ReadFile fname =*)

let range a b =
    List.init (b-a) ((+) a)

(*prende una stringa e capitalizza la prima lettera, non so se serve.
 * Per me è stata fatta per differenziare String e Text. Per ora la faccio
 * diventare un alias per ucFirst  *)
let upFirst s = ucFirst s
(*let upFirst s =
    let inner c i =
       match i with
       | 0 -> String.uppercase_ascii c
       | _ -> c 
    in List.map inner s (range 0 (List.length(s) -1))*)

let indentBy idx = String.make (2*idx) ' '
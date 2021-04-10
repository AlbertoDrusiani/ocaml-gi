open Filename
open Sys
open Utils

(* string -> string -> string -> string *)
let girFilePath name version path =
  path ^ dir_sep ^ name ^ "-" ^ version ^ ".gir"

(* string -> string option -> string -> string option*)
let girFile' name version path =
  match version with
  | Some v -> let filepath = girFilePath name v path in
    begin
     match file_exists filepath with
     | true -> Some filepath
     | false -> None
    end
  | None ->
     match is_directory path with
               (* creo la lista dei nomi dei file nella cartella con i file .gir, rimuovendo le estensioni e il path*)
     | true -> let repositories = List.map (fun x -> basename (remove_extension x)) (Array.to_list (readdir path)) in
               let r = Str.regexp (name ^ "-") in
               let f x = match x with
                         | _::xs::[] -> Some xs
                         | _ -> None
               (* cerco il file con il nome dato in input in modo da estrarre la versione.*)
               (* splitto tutti i nomi dei file e rendo None tutti quelli il cui prefisso non matcha con il nome e rimuovo i None *)
               (* rimangono solo le versioni del file target in formato stringa del tipo "1.0"*)
               in let splitted = List.filter_map (fun x -> x) (List.map f (List.map (Str.split_delim r) repositories)) in
               let version = maximumMay (List.map float_of_string splitted) neg_infinity neg_infinity in
               begin
                match version with
                | Some v -> Some (girFilePath name (string_of_float v) path)
                | None -> None
                end
     | false -> None
               
            
(* 'a -> 'a list -> 'a list list *)
let splitOn x xs =
  let rec go l acc =
    match l with
    | [] -> [List.rev acc]
    | y::ys -> if x == y 
               then List.rev (acc::(go ys []))
               else go ys (y::acc)
  in go xs []

(*TODO bacato, va messa la cartella di sistema in base al tipo di sistema, non so come fare per ora*)
let girDataDirs = "/usr/share/gir-1.0"

(* string list -> string list *)
let buildSearchPath (*extraPaths*) =
  (*TODO qua gestisce una variabile d'ambiente di Haskell-GI*)
  girDataDirs

(* string -> string option -> string list -> string option *)
let girFile name version searchPath =
  (*TODO fa roba per monadi, forse skippabile *)
  girFile' name version searchPath

(* bool -> string -> string option -> xml *)
let readGiRepository verbose name version (*extraPaths*) =
  let searchPath = buildSearchPath in
  match girFile name version searchPath with
  | Some path -> 
    if verbose then prerr_endline ("Loading GI repository: " ^ path);
    Xml.parse_file path;
  | None -> prerr_endline ("Did not find a GI repository for the path: " ^ searchPath); assert false

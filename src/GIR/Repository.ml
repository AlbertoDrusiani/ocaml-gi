open Filename
(*open Sys*)
(*open Utils*)

let girFilePath name version path =
  path ^ dir_sep ^ name ^ "-" ^ version ^ ".gir"


(*let girFile' name version path =
  match version with
  | Some v -> let filepath = girFilePath name version path in
    begin
     match file_exists filepath with
     | true -> Some filepath
     | false -> None
    end
  | None ->
     match is_directory filepath with
     | true -> let repositories = List.map (fun x -> basename (remove_extension x)) (readdir path) in
               let version = maximumMay
*)

let splitOn x xs =
  let rec go l acc =
    match l with
    | [] -> [List.rev acc]
    | y::ys -> if x == y 
               then List.rev (acc::(go ys []))
               else go ys (y::acc)
  in go xs []


(*let girDataDirs =*)


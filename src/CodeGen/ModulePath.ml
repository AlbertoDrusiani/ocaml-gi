open Util

type module_path = {
    modulePathToList: string list;
}

module ModulePath = struct
  type t = module_path
  let rec compare {modulePathToList = x1} {modulePathToList = x2} =
    match x1, x2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | x1::xs1, x2::xs2 ->
      match Stdlib.compare x1 x2 with
      | 0 -> compare {modulePathToList = xs1} {modulePathToList = xs2}
      | c -> c
end

module ModulePathSet = Set.Make(ModulePath)


let toModulePath p =
    { modulePathToList = List.map ucFirst (String.split_on_char '.' p)}


let dotModulePath p =
    match p with
    | {modulePathToList = mpl;} -> String.concat "." mpl


let concatModulePath mp p =
    {modulePathToList = mp.modulePathToList @ (toModulePath p).modulePathToList}

(*let (/.) mp p =
    match mp with
    | {modulePathToList = mpl;} -> {modulePathToList = mpl @ [p]}*)

(*TODO non ho capito come si usi il (<>) in haskell*)
(*let addNamePrefix prefix mp =
    match mp with
    | {modulePathList = mpl;} -> {modulePathList = mapNth (List.length mpl -1) prefix*)




let modulePathNS mp =
    match mp with
    | {modulePathToList = mpl;} -> List.hd mpl


let moduleName mp =
    match mp with
    | {modulePathToList = [];} -> "Errore in moduleName: module can't be empty"
    | {modulePathToList = mpl;} -> List.hd (List.rev mpl)






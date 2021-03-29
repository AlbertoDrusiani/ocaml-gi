open Util

type module_path = {
    modulePathToList: string list;
}



let toModulePath p =
    { modulePathToList = List.map ucFirst (String.split_on_char '.' p)}


let dotModulePath p =
    match p with
    | {modulePathToList = mpl;} -> String.concat "." mpl


let (/.) mp p =
    match mp with
    | {modulePathToList = mpl;} -> {modulePathToList = mpl @ [p]}

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






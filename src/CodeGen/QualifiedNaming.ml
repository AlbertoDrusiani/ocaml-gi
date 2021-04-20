open Util
open API
open ModulePath
open Naming

let signalHaskellName sn =
  match String.split_on_char '-' sn with
  | w::ws -> w ^ (String.concat "" (List.map ucFirst ws))
  | [] -> ""


let submoduleLocation n api =
  match n, api with
  | _, APIConst _ -> {modulePathToList = ["Constants"]}
  | _, APIFunction _ -> {modulePathToList = ["Functions"]}
  | _, APICallback _ -> {modulePathToList = ["Callbacks"]}
  | _, APIEnum _ -> {modulePathToList = ["Enums"]}
  | _, APIFlags _ -> {modulePathToList = ["Enums"]}
  | n, APIInterface _ -> {modulePathToList = [(upperName n)]}
  | n, APIObject _ -> {modulePathToList = [(upperName n)]}
  | n, APIStruct _ -> {modulePathToList = [(upperName n)]}
  | n, APIUnion _ -> {modulePathToList = [(upperName n)]}




  

(*open GIR.BasicTypes


module SetNames = Set.Make(struct type t = name let compare = compare end)
(*utilizzo: let myset = SetNames.add {name = _; namespace = _} SetNames.empty*)
module MapNamesText = Map.Make(struct type t = name let compare = compare end)

(*type overrides = {
    ignoredElems: MapNamesText
    ignoredAPIs: Set.*)
   

*)

open Enum

type flags = Flags of enumeration


let parseFlags ns el =
  let n, enum = parseEnum ns el in
  n, Flags enum


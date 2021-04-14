open Enum

type flags = Flags of enumeration


let parseFlags ns el =
  prerr_endline ("Inizio il parse Flags");
  let n, enum = parseEnum ns el in
  n, Flags enum


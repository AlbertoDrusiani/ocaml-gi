open Enum

type flags = Flags of enumeration


let parseFlags el ns =
  let n, enum = parseEnum el ns in
  n, Flags enum


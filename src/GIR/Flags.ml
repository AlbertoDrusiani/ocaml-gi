open Enum

type flags = Flags of enumeration


let parseFlags f =
    match parseEnum f with
    | (name, flag) -> name, Flags flag


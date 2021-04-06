(*open Enum

type flags = Flags of enumeration


let parseFlags f =
    prerr_endline("pppppppppppp FLAGS ppppppppppppp");
    match parseEnum f with
    | (name, flag) -> name, Flags flag
*)

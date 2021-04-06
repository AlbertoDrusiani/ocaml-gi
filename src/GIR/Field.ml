(*module GI = GObject_introspection
module B = Bindings

open BasicTypes
open Callback

type field_info_flag =
    | FieldIsReadable
    | FieldIsWritable

type field =
    { fieldName: string;
     (* fieldVisible: bool;*)
      fieldType: type_ml option;
      fieldIsPointer: bool; (*in haskell è un bool option*)
     (* fieldCallback: callback option;*)
      fieldOffset: int;
      fieldFlags: field_info_flag list;
     (* fieldDocumentation: documentation;
      fieldDeprecated: deprecation_info option;*)
    }

let rec parseFlags (f : B.Field_info.flags list)  =
    match f with
    | [] -> []
    | x::xs -> 
            let flag =
               match x with
               | Is_readable -> FieldIsReadable
               | Is_writable -> FieldIsWritable
            in flag :: (parseFlags xs)

(*passo un Field_info*)
let parseField f =
    prerr_endline("ppppppppppppp FIELD pppppppppp");
    let name = 
        match GI.Field_info.to_baseinfo f |> GI.Base_info.get_name with
        | Some x -> x
        | None -> "Error"
    in
    { fieldName = name;
      fieldType = GI.Field_info.get_type f |> cast_to_type_ml;
      fieldIsPointer = GI.Field_info.get_type f |> GI.Type_info.is_pointer;
      fieldOffset = GI.Field_info.get_offset f;
      fieldFlags = GI.Field_info.get_flags f |> parseFlags;
    }*)

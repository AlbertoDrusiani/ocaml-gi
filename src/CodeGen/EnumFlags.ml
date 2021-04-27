open Code
open Naming
open Ctypes
open GIR.Enum
open GIR.Flags

type enum_or_flag =
  | Enum
  | Flag


let hashVariant s =
  let variantHash' xs acc = List.fold_left (fun a c -> (223 * a) + Char.code c) acc (explode xs) in
  let variantHash xs = variantHash' xs 0 in
  let reduceTo31bits hash = hash land ((1 lsl 31) -1) in
  let toSigned64 hash = if hash > 0x3FFFFFFF then hash - (1 lsl 31) else hash in
  variantHash s |> reduceTo31bits |> toSigned64



let genEnumOrFlags minfo n e enumOrFlag =
  if (sizeof uint) != 4
  then raise (CGErrorNotImplemented ("Unsupported uint size: " ^ string_of_int (sizeof uint)));
  if e.enumStorageBytes != 4
  then raise (CGErrorNotImplemented ("Storage of size /= 4 not supported: " ^ string_of_int (e.enumStorageBytes)));
  let enumName = upperName n |> camelCaseToSnakeCase |> escapeOCamlReserved in
  let memberNames = List.map (fun x -> escapeOCamlReserved (String.uppercase_ascii (x.enumMemberName))) e.enumMembers in
  let variants = List.map (fun x -> "`" ^ x) memberNames in
  let cIds = List.map (fun x -> x.enumMemberCId) e.enumMembers in
  let mlTableName = mlGiPrefix n ("table_" ^ enumName) in
  let ocamlTbl = enumName ^ "_tbl" in
  let cGetterFn = mlGiPrefix n (String.lowercase_ascii n.namespace) ^ "_get_" ^ enumName ^ "_table" in
  let addHash (n, id) = (hashVariant n, (n,id)) in
  let sortByHash x = List.sort (fun x y -> compare (fst x) (fst y)) x in
  let discardHash x = List.map snd x in
  let zipped = List.combine memberNames cIds in
  let namesAndIds = discardHash (sortByHash (List.map addHash zipped)) in
  let minfo = List.fold_left (
    fun minfo memberName -> 
    let hashValue = hashVariant memberName in 
    hline ("#define MLTAG_" ^ memberName ^ " ((value)(" ^ string_of_int(hashValue) ^"*2+1))") minfo
    )  minfo memberNames in 
  let minfo = hline "" minfo in 
  let minfo = hline ("extern const lookup_info " ^ mlTableName ^ "[];") minfo in 
  let minfo = hline ("#define " ^ valEnum n ^ "(data) ml_lookup_from_c (" ^ mlTableName ^ ", data)") minfo in 
  let minfo = hline ("#define " ^ enumVal n ^ "(key) ml_lookup_to_c (" ^ mlTableName ^ ", key)") minfo in 
  let minfo = hline "" minfo in 
  let minfo = line ("type " ^ enumName ^ " = [ " ^ (String.concat " | " variants) ^ " ]") minfo in 
  let minfo = blank minfo in 
  let minfo = line ("external get_" ^ enumName ^ "_table : unit -> " ^ enumName ^ " GPointer.variant_table = \"" ^ cGetterFn ^ "\"" ) minfo in 
  let minfo = line ("let " ^ ocamlTbl ^ " = get_" ^ enumName ^ "_table ()") minfo in 
  let minfo = 
  if enumOrFlag = Enum
  then line ("let " ^ enumName ^ " = GObject.Data.enum " ^ ocamlTbl) minfo
  else line ("let " ^ enumName ^ " = GObject.Data.flags " ^ ocamlTbl) minfo
  in let minfo = blank minfo in
  let minfo = addCDep minfo (n.namespace ^ "Enums")  in 
  let minfo = cline ("const lookup_info " ^ mlTableName ^ "[] = {") minfo in
  let minfo = cline ("  { 0, " ^ string_of_int(List.length e.enumMembers) ^ " },") minfo in 
  let minfo = List.fold_left (
    fun minfo (memberName, memberCId) -> cline ("  { MLTAG_" ^ memberName ^ ", " ^ memberCId ^ " },") minfo )
    minfo namesAndIds in
  let minfo = cline "};" minfo in 
  let minfo = cline "" minfo in 
  let minfo = cline ("CAMLprim value " ^ cGetterFn ^ " () {") minfo in
  let minfo = cline ("  return (value) " ^ mlTableName ^ ";") minfo in 
  let minfo = cline "}" minfo in 
  let minfo =
    if (enumOrFlag = Flag)
    then 
      let minfo = cline ("Make_Flags_val(" ^ enumVal n ^ ")") minfo in 
      let minfo = cline ("Make_OptFlags_val(" ^ enumVal n ^ ")") minfo in 
      let minfo = hline ("CAMLprim int " ^ flagsVal n ^ " (value list);") minfo in 
      hline ("CAMLprim int " ^ optFlagsVal n ^ " (value list);") minfo
    else 
      minfo
  in cline "" minfo
 

let genEnum cfg cgstate minfo n e =
  try
    cfg, cgstate, genEnumOrFlags minfo n e Enum
  with Failure exc -> cfg, cgstate, (commentLine minfo ("Could not generate: " ^ exc))


let genFlags cfg cgstate minfo n f =
  match f with
  | Flags enum ->
  try
    cfg, cgstate, genEnumOrFlags minfo n enum Flag
  with Failure exc -> cfg, cgstate, (commentLine minfo ("Could not generate: " ^ exc))
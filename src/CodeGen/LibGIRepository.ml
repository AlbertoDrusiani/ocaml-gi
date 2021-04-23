module GOI = GObject_introspection
open GIR.BasicTypes

type field_info = {
    fieldInfoOffset: int;
}

let getFieldInfo field =
  let fname = Option.get(GOI.Field_info.to_baseinfo field |> GOI.Base_info.get_name) in
  let fOffset = GOI.Field_info.get_offset field in
  fname, {fieldInfoOffset = fOffset}


let girStructFieldInfo ns name =
  let baseinfo = GOI.Repository.find_by_name ns name in
  match baseinfo with
  | Some b -> 
    let struct_info = GOI.Struct_info.from_baseinfo b in
    let size = GOI.Struct_info.get_size struct_info in
    let nfields = GOI.Struct_info.get_n_fields struct_info in
    let fieldInfos = ref [] in
    for i = nfields-1 downto 0 do
      fieldInfos := getFieldInfo (GOI.Struct_info.get_field struct_info i) :: !fieldInfos
    done;
    let seq = List.to_seq !fieldInfos in
    size, StringMap.of_seq seq
  | None -> assert false


let girUnionFieldInfo ns name =
  let baseinfo = GOI.Repository.find_by_name ns name in
  match baseinfo with
  | Some b -> 
    let union_info = GOI.Union_info.from_baseinfo b in
    let size = GOI.Union_info.get_size union_info in
    let nfields = GOI.Union_info.get_n_fields union_info in
    let fieldInfos = ref [] in
    for i=nfields-1 downto 0 do
      fieldInfos := getFieldInfo (GOI.Union_info.get_field union_info i) :: !fieldInfos
    done;
    let seq = List.to_seq !fieldInfos in
    size, StringMap.of_seq seq
  | None -> assert false

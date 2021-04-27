(*open API*)
open GIR.BasicTypes
open GIR.Struct
open GIR.Field
open Naming

open API

(* name -> struct -> bool *)
let ignoreStruct nm st =
 ( Option.is_some st.gtypeStructFor || Filename.check_suffix nm.name "Private") && (not st.structForceVisible)

let fieldCallbackType structName field =
  structName ^ (field.fieldName |> underscoresToCamelCase) ^ "FieldCallback"


let extractCallbacksInStruct (n, s) =
  match s with
  | APIStruct s -> 
    let callbackInField field =
      let callback = field.fieldCallback in
      match callback with
      | Some callback ->
        let n' = fieldCallbackType n.name field in
        Some ({namespace = n.namespace; name = n'}, APICallback callback)
      | None -> None
    in 
    (match ignoreStruct n s with
    | true -> []
    | false -> List.filter_map callbackInField s.structFields)
  | _ -> []


let fixCallbackStructFields n s =
  
  let fixedField field =
    match field.fieldCallback with
    | None -> field
    | Some _ ->
      let n' = fieldCallbackType n.name field in
      { field with fieldType = TInterface {namespace = n.namespace; name = n'}}
  in let fixedFields = List.map fixedField s.structFields in
  { s with structFields = fixedFields}


let fixAPIStructs (n, api) =
  match api with
  | APIStruct s -> (n, APIStruct (fixCallbackStructFields n s))
  | api -> (n, api)
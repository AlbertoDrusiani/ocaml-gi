(*open API*)
open GIR.BasicTypes
open GIR.Struct

(* name -> struct -> bool *)
let ignoreStruct nm st =
 ( Option.is_some st.gtypeStructFor || Filename.check_suffix nm.name "Private") && (not st.structForceVisible)

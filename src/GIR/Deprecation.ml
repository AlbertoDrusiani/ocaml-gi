open XMLUtils

type _DeprecationInfo = {
    deprecatedSinceVersion: string option;
    deprecationMessage: string option;
}

(* xml -> _DeprecationInfo option *)
let queryDeprecated element =
    try 
        let _ = lookupAttr "deprecated" element in (*la lookup attr può restituire un'eccezione No_Attribute*)
        let version = lookupAttr "deprecated-version" element in
        let msg = firstChildWithLocalName "doc-deprecated" element in
        let m = match msg with
        | Some msg -> getElementContent msg
        | None -> None
        in Some { deprecatedSinceVersion = version; deprecationMessage = m;}        
    with Xml.No_attribute str -> prerr_endline str;  None
    

type name = {
    nameLocalName: string;
    nameNamespace: string option;
    namePrefix: string option;
}


type _GIRXMLNamespace =
    | GLibGIRNS
    | CGIRNS
    | CoreGIRNS

(* GIRXMLNamespace -> string *)
let girNamespace ns =
    match ns with
    | GLibGIRNS -> "http://www.gtk.org/introspection/glib/1.0"
    | CGIRNS -> "http://www.gtk.org/introspection/c/1.0"
    | CoreGIRNS -> "http://www.gtk.org/introspection/core/1.0"


(*xml-light non gestisce i nomi qualified, quindi non splitta nome locale da namespace*)
(*creo alcune funzioni helper per gestire le cose e rimanere aderente ad haskell-gi*)
(* string option -> GIRXMLNamespace *)
let prefixToGIRXMLNamespace p =
    match p with
    | Some "c" -> CGIRNS
    | Some "glib" -> GLibGIRNS
    | _ -> CoreGIRNS 

(* GIRXMLNamespace -> string *)
let girXMLNamespaceToPrefix ns =
    match ns with
    | CGIRNS -> "c"
    | GLibGIRNS -> "glib"
    | CoreGIRNS -> ""

(* string option -> string *)
let prefixToGIRNamespace p =
    prefixToGIRXMLNamespace p |> girNamespace

(*estrae il prefisso da un elemento o da un attributo*)
(* string -> string option *)
let get_prefix str =
    let l = String.split_on_char ':' str in
    match l with
    | _::[] -> None
    | x::_ -> Some x
    | _ -> None


(*estrae il localName dalla stringa nome di un elemento/attributo*)
(* string -> string *)
let localName str =
    let l = String.split_on_char ':' str in
    List.hd (List.rev l)

(*costruisce un name a partire da un local name*)
(* string -> name *)
let xmlLocalName n =
    { nameLocalName = n;
      nameNamespace = None;
      namePrefix = None;}

(*costruisce un name a partire da un local name e un namespace*)
(* string -> string -> name *)
let xmlNSName ns n =
    { nameLocalName = n;
      nameNamespace = Some (girNamespace ns);
      namePrefix = None;
    }

        
(*estrae il nome qualificato a partire da un element*)
(* xml -> name *)
let element_to_name el =
    {nameLocalName = localName (Xml.tag el);
     nameNamespace = Some (get_prefix (Xml.tag el) |> prefixToGIRNamespace);
     namePrefix = get_prefix (Xml.tag el);
    }

(*costruisce un name a partire da un attributo*)
(* string*string -> name *)
let attribute_to_name attr =
    match attr with
    | (key, _) ->  {nameLocalName = localName key; 
                    nameNamespace = Some (get_prefix key |> prefixToGIRNamespace);
                    namePrefix = get_prefix key;}

(*prende un xml e restituisce None se non è un elemento*)    
(* xml -> xml option*)
let nodeToElement node =
    match node with
    | Xml.Element e -> Some (Xml.Element e)
    | Xml.PCData _ -> None

(*rimuove dai figli di un elemento tutti quelli che non sono elementi*)    
(* xml -> xml list *)
let subelements el =
    List.filter_map nodeToElement (Xml.children el)


(* restituisce tutti i figli di un elemento che hanno come local name quello dato*)    
(* string -> xml -> xml list *) (*TODO come viene gestito il namespace? Devo togliere ciò che c'è prima dei due punti?*)
let childElemsWithLocalName n el =
  let localNameMatch e = 
    Xml.tag e = n in
  List.filter localNameMatch (subelements el)

(* come sopra ma specificando anche il namespace *)    
(* GIRXMLNamespace -> string -> xml -> xml list *)
let childElemsWithNSName ns n el =
    let name = {nameLocalName = n; nameNamespace = Some (girNamespace ns); namePrefix = None;} in
    let nameMatch e = element_to_name e |> (fun x -> x == name) in
    List.filter nameMatch (subelements el)

(* restituisce il primo figlio di un elemento con il nome locale specficato *)
(* string -> xml -> xml option *)
let firstChildWithLocalName n el =
    List.nth_opt (childElemsWithLocalName n el) 0

(* restituisce il contenuto di un elemento (nodo) *)    
(* xml -> string option *)
let getElementContent el =
    let getContent node =
        match node with
        | Xml.PCData str -> Some str
        | _ -> None
    in List.nth_opt (List.filter_map getContent (Xml.children el)) 0

(*in Haskell restituisce un option, qui solleva un'eccezione se non lo trova*)
(* restituisce il valore di un attributo, data la chiave*)
(* string -> xml -> string option *)
let lookupAttr attr element =
    try
        Some (Xml.attrib element attr) (*TODO da capire come viene poi chiamata, per vedere se devo lasciare il namespace prima dei due punti*)
    with Xml.No_attribute _ -> (*prerr_endline ("Errore No_attribute: " ^ str);*) None

(*prendo il local name dell'attributo e ci piazzo davanti il namespace dato, e poi cerco*)
(* GIRXMLNamespace -> string -> xml -> string option *)
let lookupAttrWithNamespace ns attr element =
    let attr_ns = girXMLNamespaceToPrefix ns ^ ":" ^  attr in (*TODO dovrebbe andare*)
    lookupAttr attr_ns element 






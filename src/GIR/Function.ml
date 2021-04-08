open Callable
open Parser
open BasicTypes

type function_ml = {
  fnSymbol: string;
  fnMovedTo: string option;
  fnCallable: callable;
}


let parseFunction el ns =
  let name_ = parseName el ns in
  let shadows = queryAttr "shadows" el in
  let exposedName = 
    begin
     match shadows with
     | Some n -> {name_ with name = n}
     | None -> name_
    end
  in let callable = parseCallable el ns in
  let symbol = getAttrWithNamespace CGIRNS "identifier" el in
  let movedTo = queryAttr "moved-to" el in
  exposedName,
  { fnSymbol = symbol;
    fnCallable = callable;
    fnMovedTo = movedTo;
  }


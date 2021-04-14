open Arg
open Callable
open Parser
open BasicTypes

type method_type =
    | Constructor
    | MemberFunction
    | OrdinaryMethod


type method_ml = { 
    methodName: name;
    methodSymbol: string;
    methodType: method_type;
    methodMovedTo: string option;
    methodCallable: callable;
    }

let parseInstanceArg ns el =
  let parseInstPars = parseChildrenWithLocalName "parameters" el in
  let instanceInfo = List.map (parseChildrenWithLocalName "instance-parameter") parseInstPars in
  match instanceInfo with
  | [[inst]] -> parseArg ns inst
  | [] -> assert false
  | _ -> assert false


let parseMethod ns mType el =
  let name_ = parseName ns el in
  let shadows = queryAttr "shadows" el in
  let exposedName = match shadows with
                    | Some n -> {name_ with name = n}
                    | None -> name_
  in let callable =
       if mType != OrdinaryMethod
       then parseCallable el ns
       else
         let c = parseCallable el ns in
         let instanceArg = parseInstanceArg ns el in
         { c with args = instanceArg::(c.args)}
  in let symbol = getAttrWithNamespace CGIRNS "identifier" el in
  let movedTo = queryAttr "moved-to" el in
  { methodName = exposedName;
    methodSymbol = symbol;
    methodType = mType;
    methodMovedTo = movedTo;
    methodCallable = callable;
  }




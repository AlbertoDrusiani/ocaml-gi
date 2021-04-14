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

let parseInstanceArg ns aliases el =
  let parseInstPars = parseChildrenWithLocalName "parameters" el in
  let instanceInfo = List.map (parseChildrenWithLocalName "instance-parameter") parseInstPars in
  match instanceInfo with
  | [[inst]] -> parseArg ns aliases inst
  | [] -> assert false
  | _ -> assert false


let parseMethod ns aliases mType el =
  prerr_endline ("Inizio il parse Method");
  let name_ = parseName ns el in
  let shadows = queryAttr "shadows" el in
  let exposedName = match shadows with
                    | Some n -> {name_ with name = n}
                    | None -> name_
  in let callable =
       if mType != OrdinaryMethod
       then parseCallable ns aliases el
       else
         let c = parseCallable ns aliases el in
         let instanceArg = parseInstanceArg ns aliases el in
         { c with args = instanceArg::(c.args)}
  in let symbol = getAttrWithNamespace CGIRNS "identifier" el in
  let movedTo = queryAttr "moved-to" el in
  { methodName = exposedName;
    methodSymbol = symbol;
    methodType = mType;
    methodMovedTo = movedTo;
    methodCallable = callable;
  }




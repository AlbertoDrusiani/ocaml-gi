(*open Arg*)
open Callable
(*open Parser*)
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

(*TODO da capire la solita questione di parseChildren con i parser vari*)
(*let parseInstanceArg el ns =
 (* let parseInstPars el ns =*)
    List.map ((List.map parseArg (parseChildrenWithLocalName "instance-parameters" el))) ns
  *)

(*
let parseMethod el ns mtype =
  let name_ = parseName el ns in
  let shadown = queryAttr "shadows" el in
  let exposedName = match shadows with
                    | Some n -> {name_ with name = n}
                    | None -> name_
  in let callable =
       if mType != OrdinaryMethod
       then parseCallable el ns
       else
         let c = parseCallable el ns in
         let instanceArg = parseInstanceArg el ns in
         { c with args = instanceArg::(c.args)}
  in let symbol = gettAttrWithNamespace CGIRNS "identifier" el in
  let movedTo = queryAttr "moved-to" in
  { methodName = exposedName;
    methodSymbol = symbol;
    methodType = mType;
    methodMovedTo = movedTo;
    methodCallable = callabe;
  }

*)



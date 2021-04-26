open GIR.BasicTypes
open GIR.Method
open GIR.Callable
open GObject
open Naming
open Code
open QualifiedNaming



type method_in_arg =
  | BasicIn of string * string
  | ClassType of bool * string * name
  | NonGtkClassType of bool * string * string

 
type method_out_arg =
  | BasicOut of string
  | Class of bool * name
  | NonGtkClass of bool * string * string
  | TupleOut of method_out_arg list


type method_args = method_in_arg * (method_in_arg list) * method_out_arg


let extractVars l =
  let inner p =
    match p with
    | ClassType (_, var, _) -> [var]
    | NonGtkClassType (_, var, _) -> [var]
    | BasicIn (var, _) -> [var]
  in List.concat_map inner l


let extractClassVars (_, inArgs, outArgs) =

  let inArgTVars m =
    match m with
    | ClassType (_, var, _) -> Some var
    | NonGtkClassType (_, var, _) -> Some var
    | BasicIn (_, _) -> None

  in let rec outArgTVars m =
    match m with
    | BasicOut _ -> []
    | Class (_, _) -> []
    | NonGtkClass (_, tvar, _) -> [tvar]
    | TupleOut outArgs' -> List.concat_map outArgTVars outArgs'

  in (List.filter_map inArgTVars inArgs) @ (outArgTVars outArgs)


let addOption b t =
  match b with
  | false -> t
  | true -> t ^ " option"



let showMethodInArg minfo m =
  match m with
  | BasicIn (_, t) -> t
  | NonGtkClassType (_, _, t) -> t
  | ClassType (isOption, tvar, n) ->
    let currNS = currentNS minfo in
    addOption isOption ("(" ^ nsOCamlO currNS n ^ " as '" ^ tvar ^ ")")



let rec showMethodOutArg cfg minfo m =
  match m with
  | BasicOut t -> t
  | NonGtkClass (_, _, t) -> t
  | Class (isOption, n) ->
    let api = findAPIByName cfg n in
    let ocamlClass' = nsOCamlClass minfo n in
    let ocamlClass =
      match api with
      | APIObject _ -> ocamlClass'
      | APIInterface _ -> ocamlClass' ^ "_skel"
      | _ -> assert false
    in addOption isOption ocamlClass
  | TupleOut mArgs ->
    let argsText = List.map (showMethodOutArg cfg minfo) mArgs in
    "(" ^ (String.concat " * " argsText) ^ ")"


let fixConstructorReturnType returnsGObject cn c =
  let returnType' =
    if returnsGObject then Some (TInterface cn) else c.returnType
  in { c with returnType = returnType'}


let genMethod cfg minfo cn m  =
  let getOCamlClass cfg n =
    let ocamlClass = nsOCamlClass minfo n in
    let api = findAPIByName cfg n in
    match api with
    | APIObject _ -> ocamlClass
    | APIInterface _ -> ocamlClass ^ "_skel"
    | _ -> assert false

  in let boundMethod mName (headArg, inArgs, outArg) =
    let mapInArg _ p =
      match p with
      | ClassType (false, tvar, n) -> tvar ^ "#as_" ^ ocamlIdentifier n
      | ClassType (true, tvar, n) -> 
        "(Option.map (fun z -> z#as_" ^ ocamlIdentifier n ^ ") " ^ tvar ^ ")"
      | BasicIn (tvar, _) -> tvar
      | NonGtkClassType (_, tvar, _) -> tvar
    in let obj = 
      match headArg with
      | ClassType (_, _, _) -> " obj "
      | _ -> " "
      (*FIXME non mi convince quella fmap, riga 321 haskell*)
    in cn.name ^ "." ^ mName ^ obj ^ String.concat " " (List.map (mapInArg outArg) inArgs)

  in let methodSignature argVars (_, inArgs, outArg) =
    let inArgsShow = List.map (showMethodInArg minfo) inArgs in
    let outShow = showMethodOutArg cfg minfo outArg in
    let argShow = inArgsShow @ [outShow] in
    " : " ^ argVars ^ (String.concat " -> " argShow)

  in let bodyPrefix (_, inArgs, _) =
    let vars = extractVars inArgs in
    match vars with
    | [] -> ""
    | _ -> "fun " ^ (String.concat " "vars) ^ " -> "

  in let methodBody _ mName mArgs =
    match mArgs with
    | _, _, Class (false, n) ->
      let ocamlClass = getOCamlClass cfg n in
      "new " ^ ocamlClass ^ " (" ^ boundMethod mName mArgs ^ ")"
    | _, _, Class (true, n) ->
      let ocamlClass = getOCamlClass cfg n in
      "Option.map (new" ^ ocamlClass ^ ") (" ^ boundMethod mName mArgs ^ ")"
    | mArgs -> boundMethod mName mArgs


  in match m.methodType with
  | Constructor ->
    if m.methodName.name != "new"
    then
      let returnsGObject = 
        match m.methodCallable.returnType with
        | None -> false
        | Some s -> isGObject cfg s
      in let c' = fixConstructorReturnType returnsGObject cn m.methodCallable in
      genCCallableWrapper m.methodName m.methodName c'
    else minfo
  | t ->
    let isAProp = isMethodAlsoAProp cn m.methodName.name in
    match isAProp with
    | true -> minfo
    | false -> 
      let alreadyDefMethod = isMethodInParents cn m.methodName.name in
      let mName = escapeOCamlReserved m.methodName.name in
      let mDeclName = if alreadyDefMethod then mName ^ "_" ^ ocamlIdentifier cn else mName in
      let c' = if t = OrdinaryMethod then fixMethodArgs m.methodCallable else m.methodCallable in
      let minfo = genCCallableWrapper m.methodName m.methodSymbol c' in
      let typeReps = callableOcamlTypes c' in
      let mbMethodArgs = typeRepsToMethodArgs typeReps in
      match mbMethodArgs with
      | None -> minfo
      | Some mArgs ->
        let tVars = extractClassVars mArgs in
        let tVarsText = 
          match tVars with
          | [] -> ""
          (*FIXME da capire la concatenazione se è corretta, riga 265 haskell*)
          | vars -> String.concat " " (("'"::vars) @ ["."])
        in let mSig = methodSignature tVarsText mArgs in
        let mBody = methodBody tVars mName mArgs in
        let minfo = gline ("  method " ^ mDeclName ^ mSig ^ " = ") minfo in
        let minfo = gline ("    " ^ bodyPrefix mArgs ^ mBody) minfo in
        gline "" minfo 

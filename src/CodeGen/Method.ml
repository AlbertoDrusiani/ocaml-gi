open GIR.BasicTypes
open GIR.Method
open GIR.Callable
open GIR.Property
open GIR.Arg
open GObject
open Naming
open Code
open QualifiedNaming
open Callable
open TypeRep
open Util



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



let methodInTypeShow cgstate ns trep =
  match ns, trep with
  | _, OptionCon (ObjCon (TypeVarCon (tvar, (RowCon (More, (PolyCon( NameCon n))))))) 
    when n.namespace = "Gtk" 
  -> cgstate, ClassType (true, tvar, n)
  | currNS, (OptionCon (ObjCon (TypeVarCon (tvar, (RowCon (More, (PolyCon( NameCon _))))))) as t)-> 
    cgstate, NonGtkClassType (true, tvar, (methodTypeShow currNS t))
  | _, ObjCon (TypeVarCon (tvar, RowCon (More, PolyCon (NameCon n)))) 
    when n.namespace = "Gtk" -> cgstate, ClassType (false, tvar, n)
  | currNS, ((ObjCon (TypeVarCon (tvar, (RowCon (More, PolyCon (NameCon _)))))) as t)
    -> cgstate, NonGtkClassType (false, tvar, (methodTypeShow currNS t))
  | currNS, t -> 
    let cgstate, tvar = getFreshTypeVariable cgstate in
    cgstate, BasicIn (tvar, (methodTypeShow currNS t))



let rec methodOutTypeShow ns trep =
  match ns, trep with
  | _, OptionCon (ObjCon (TypeVarCon (_, (RowCon (Less, (PolyCon( NameCon n))))))) 
    when n.namespace = "Gtk" -> Class (true, n)
  | currNS, (OptionCon (ObjCon (TypeVarCon (tvar, (RowCon (Less, (PolyCon( NameCon _))))))) as t)-> 
    NonGtkClass (true, tvar, (methodTypeShow currNS t))
  | _, ObjCon (TypeVarCon (_, RowCon (Less, PolyCon (NameCon n)))) 
    when n.namespace = "Gtk" -> Class (false, n)
  | currNS, ((ObjCon (TypeVarCon (tvar, (RowCon (Less, PolyCon (NameCon _)))))) as t)
    -> NonGtkClass (false, tvar, (methodTypeShow currNS t))
  | currNS, TupleCon treps ->
    let outArgs = List.map (methodOutTypeShow currNS) treps in
    TupleOut outArgs
  | currNS, t -> BasicOut (methodTypeShow currNS t)


let typeRepsToMethodArgs cgstate minfo l =
  match l with
  | [] -> cgstate, None
  | h::typeRepsTail ->
    let currNS = currentNS minfo in
    let cgstate, headArg = methodInTypeShow cgstate currNS h in
    let cgstate, inArgs' = 
      List.fold_left_map (fun state trep -> 
        methodInTypeShow state currNS trep) cgstate (noLast typeRepsTail)
    in let inArgs =
      match headArg with
      | ClassType (_, _, _) -> inArgs'
      | BasicIn (_, _) -> headArg :: inArgs'
      | NonGtkClassType (_, _, _) -> headArg :: inArgs'
    in let retTypeRep = List.rev l |> List.hd in
    let retArg = methodOutTypeShow currNS retTypeRep in
    cgstate, Some (headArg, inArgs, retArg)


let fixMethodArgs c =

  let fixCArrayLength t =
    match t with
    | TCArray (zt, fixed, len, t) ->
        if len > -1 then TCArray (zt, fixed, len+1, t) else TCArray (zt, fixed, len, t)
    | t -> t
  
  in let fixLengthArg arg =
    { arg with argType = fixCArrayLength arg.argType}

  in let fixDestroyers arg =
    let destroy = arg.argDestroy in
    if destroy > -1 then { arg with argDestroy = destroy + 1} else arg

  in let fixClosures arg =
    let closure = arg.argClosure in
    if closure > -1 then { arg with argClosure = closure + 1} else arg

  in let fixInstance arg =
    { arg with mayBeNull = false; direction = DirectionIn}

  in let returnType' = Option.map fixCArrayLength c.returnType in
  let args' = List.map (fun x -> fixLengthArg x |> fixClosures |> fixDestroyers) c.args in
  let args'' = fixInstance (List.hd args') :: List.tl args' in
  { c with args = args''; returnType = returnType'}





let fixConstructorReturnType returnsGObject cn c =
  let returnType' =
    if returnsGObject then Some (TInterface cn) else c.returnType
  in { c with returnType = returnType'}


let isMethodAlsoAProp cfg cn mName =
  let api = findAPIByName cfg cn in
  match api with
  | APIObject o ->
    let propName = List.map (fun x -> x.propName |> hyphensToUnderscores) o.objProperties in
    List.mem mName propName
  | _ -> false


let isMethodInParents cfg cn mName =
  let parents = instanceTree cfg cn in
  let parentsHasProp = 
  List.map (fun parentName ->
    let api = findAPIByName cfg parentName in
    match api with
    | APIObject o ->
      let parentPropNames = List.map (fun x -> x.propName |> hyphensToUnderscores) o.objProperties in
      let parentPropGetters = List.map (fun x -> "get_" ^ x) parentPropNames in
      let parentPropSetters = List.map (fun x -> "set_" ^ x) parentPropNames in
      let parentPropNames' = parentPropGetters @ parentPropSetters in
      let parentMethodNames = List.map (fun x -> x.methodName.name) o.objMethods in
      let parentNames = parentPropNames' @ parentMethodNames in
      List.mem mName parentNames
    | _ -> false
  ) parents
  in List.mem true parentsHasProp


let genMethod cfg cgstate minfo cn m  =
  let getOCamlClass cfg n =
    let ocamlClass = nsOCamlClass minfo n in
    let api = findAPIByName cfg n in
    match api with
    | APIObject _ -> ocamlClass
    | APIInterface _ -> ocamlClass ^ "_skel"
    | _ -> assert false

  in 
  let boundMethod mName (headArg, inArgs, outArg) =
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

  in 
  let methodSignature argVars (_, inArgs, outArg) =
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


  in 
  match m.methodType with
  | Constructor ->
    if m.methodName.name <> "new"
    then
      let returnsGObject = 
        match m.methodCallable.returnType with
        | None -> false
        | Some s -> isGObject cfg s
      in let c' = fixConstructorReturnType returnsGObject cn m.methodCallable in
      genCCallableWrapper cfg cgstate minfo m.methodName m.methodSymbol c'
    else cgstate, minfo
  | t ->
    let isAProp = isMethodAlsoAProp cfg cn m.methodName.name in
    match isAProp with
    | true -> cgstate, minfo
    | false -> 
      let alreadyDefMethod = isMethodInParents cfg cn m.methodName.name in
      let mName = escapeOCamlReserved m.methodName.name in
      let mDeclName = if alreadyDefMethod then mName ^ "_" ^ ocamlIdentifier cn else mName in
      let c' = if t = OrdinaryMethod then fixMethodArgs m.methodCallable else m.methodCallable in
      let cgstate, minfo = genCCallableWrapper cfg cgstate minfo m.methodName m.methodSymbol c' in
      let cgstate, typeReps = callableOCamlTypes cfg cgstate minfo c' in
      let cgstate, mbMethodArgs = typeRepsToMethodArgs cgstate minfo typeReps in
      match mbMethodArgs with
      | None -> cgstate, minfo
      | Some mArgs ->
        let tVars = extractClassVars mArgs in
        let tVarsText = 
          match tVars with
          | [] -> ""
          | vars -> 
            let l = List.map (fun x -> "'" ^ x) vars in
            (String.concat " " l) ^ "."
        in let mSig = methodSignature tVarsText mArgs in
        let mBody = methodBody tVars mName mArgs in
        let minfo = gline ("  method " ^ mDeclName ^ mSig ^ " = ") minfo in
        let minfo = gline ("    " ^ bodyPrefix mArgs ^ mBody) minfo in
        cgstate, gline "" minfo 

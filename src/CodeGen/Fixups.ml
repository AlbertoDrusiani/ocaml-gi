open GIR.Arg
open GIR.Callable
open GIR.Method
open GIR.Interface
open GIR.Struct
open GIR.BasicTypes
open GIR.Enum
open GIR.Property
open GIR.Object
open GIR.Function
open API


let checkCallableDestructors c =
  
  let checkArg arg =
    if arg.argDestroy >= 0 && arg.argClosure = -1
    then { arg with argDestroy = -1}
    else arg
  
  in { c with args = List.map checkArg c.args}


let checkMethodDestructors l =
  
  let checkMethod m =
    { m with methodCallable = checkCallableDestructors m.methodCallable}

  in List.map checkMethod l


let checkClosureDestructors (n, api) =
  match n, api with
  | n, APIObject o -> (n, APIObject { o with objMethods = checkMethodDestructors o.objMethods})
  | n, APIInterface i -> (n, APIInterface { i with ifMethods = checkMethodDestructors i.ifMethods})
  | n, APIStruct s -> (n, APIStruct { s with structMethods = checkMethodDestructors s.structMethods})
  | n, APIUnion u -> (n, APIUnion { u with unionMethods = checkMethodDestructors u.unionMethods})
  | n, APIFunction f -> (n, APIFunction { f with fnCallable = checkCallableDestructors f.fnCallable})
  | n, api -> n, api


let dropDuplicatedEnumFields enum =
  
  let rec dropDuplicates s l =
    match s, l with
    | _, [] -> []
    | previous, m::ms ->
      if StringSet.mem m.enumMemberName previous
      then dropDuplicates previous ms
      else m::dropDuplicates (StringSet.add m.enumMemberName previous) ms

  in { enum with enumMembers = dropDuplicates StringSet.empty enum.enumMembers}
 


let dropDuplicatedFields (n, api) =
  match (n, api) with
  | (n, APIFlags (Flags enum)) -> (n, APIFlags (Flags (dropDuplicatedEnumFields enum)))
  | (n, api) -> (n, api)



let detectGObject (n, api) =
  match (n, api) with
  | (n, APIInterface iface) -> 
    if not (iface.ifProperties = [] || iface.ifSignals = [])
    then 
      let gobject = {namespace = "GObject"; name = "Object"} in
      if List.mem gobject iface.ifPrerequisites
        then (n, APIInterface iface)
        else
          (n, APIInterface ({iface with ifPrerequisites = gobject::iface.ifPrerequisites}))
      else (n, APIInterface iface)
  | (n, api) -> (n, api)



let findMethod methods n =
  match List.filter (fun x -> x.methodName.name = n) methods with
  | [m] -> Some m
  | _ -> None


let guessWriteNullability methods p =
  
  let nullableSetter =
    let regexp = Str.regexp "-" in
    let prop_name = Str.global_replace regexp "_" p.propName in
    match findMethod methods ("set_" ^ prop_name) with
    | None -> None
    | Some m ->
      let c = m.methodCallable in
      if (List.length c.args = 2)
        && ((c.args |> List.rev |> List.hd).argType = p.propType)
        && (c.returnType = None)
        && ((c.args |> List.rev |> List.hd).transfer = TransferNothing)
        && ((c.args |> List.rev |> List.hd).direction = DirectionIn)
        && (m.methodMovedTo = None)
        && (m.methodType = OrdinaryMethod)
        && (c.callableThrows = false)
      then Some ((c.args |> List.rev |> List.hd).mayBeNull)
      else None

  in match p.propWriteNullable with
  | Some _ -> p
  | None -> {p with propWriteNullable = nullableSetter}

let guessReadNullability methods p =
  
  let nullableGetter =
    let regexp = Str.regexp "-" in
    let prop_name = Str.global_replace regexp "_" p.propName in
    match findMethod methods ("get_" ^ prop_name) with
    | None -> None
    | Some m ->
      let c = m.methodCallable in
      if (List.length c.args = 1)
        && (c.returnType = Some p.propType)
        && (c.returnTransfer = TransferNothing)
        && (c.skipReturn = false)
        && (c.callableThrows = false)
        && (m.methodType = OrdinaryMethod)
        && (m.methodMovedTo = None)
      then Some c.returnMayBeNull
      else None

  in match p.propReadNullable with
  | Some _ -> p
  | None -> {p with propReadNullable = nullableGetter}



let guessNullability methods p =
  guessWriteNullability methods p |> guessReadNullability methods


let guessInterfacePropertyNullability iface =
  { iface with ifProperties = List.map (guessNullability iface.ifMethods) iface.ifProperties}


let guessObjectPropertyNullability obj =
  { obj with objProperties = List.map (guessNullability obj.objMethods) obj.objProperties}


let guessPropertyNullability (n, api) =
  match api with
  | APIObject obj -> (n, APIObject (guessObjectPropertyNullability obj))
  | APIInterface iface -> (n, APIInterface (guessInterfacePropertyNullability iface))
  | api -> (n, api)



let filterMovedMethods methods =
  List.filter (fun x -> x.methodMovedTo |> Option.is_none) methods


let dropMovedItems api =
  match api with
  | APIFunction f -> if f.fnMovedTo = None then Some (APIFunction f) else None
  | APIInterface i -> Some (APIInterface { i with ifMethods = filterMovedMethods i.ifMethods})
  | APIObject o -> Some (APIObject { o with objMethods = filterMovedMethods o.objMethods})
  | APIStruct s -> Some (APIStruct { s with structMethods = filterMovedMethods s.structMethods})
  | APIUnion u -> Some (APIUnion { u with unionMethods = filterMovedMethods u.unionMethods})
  | a -> Some a
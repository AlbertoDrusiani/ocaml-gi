open Naming
open GIR.BasicTypes


type row_direction =
  | Less
  | More


type type_rep =
  | ListCon of type_rep
  | OptionCon of type_rep
  | ObjCon of type_rep
  | RowCon of row_direction*type_rep
  | TypeVarCon of string*type_rep
  | TupleCon of type_rep list
  | PolyCon of type_rep
  | TextCon of string
  | NameCon of name


(* string -> type_rep -> string *)
let rec typeShow currNS tr =
  match tr with
  | ListCon t -> (typeShow currNS t) ^ " list"
  | OptionCon t -> (typeShow currNS t) ^ " option"
  | ObjCon t -> (typeShow currNS t) ^ " Gobject.obj"
  | RowCon (Less, t) -> "[< " ^ (typeShow currNS t) ^ "]"
  | RowCon (More, t) -> "[> " ^ (typeShow currNS t) ^ "]"
  | TypeVarCon (_, t) -> typeShow currNS t
  | PolyCon NameCon n -> typeShow currNS (NameCon n)
  | PolyCon t -> "`" ^ (typeShow currNS t)
  | NameCon n -> nsOCamlType currNS n
  | TextCon text -> text
  | TupleCon treps -> "(" ^ (String.concat " * " (List.map (typeShow currNS) treps)) ^ ")"


(* string -> type_rep -> string *)
let rec methodTypeShow currNS tr =
  match tr with
  | ListCon t -> (methodTypeShow currNS t) ^ " list"
  | OptionCon t -> (methodTypeShow currNS t) ^ " option"
  | ObjCon t -> (methodTypeShow currNS t) ^ " Gobject.obj"
  | RowCon (Less, t) -> "[< " ^ (methodTypeShow currNS t) ^ "]"
  | RowCon (More, t) -> "[> " ^ (methodTypeShow currNS t) ^ "]"
  | TypeVarCon (var, t) -> "(" ^ (methodTypeShow currNS t) ^ " as '" ^ var ^ ")"
  | PolyCon NameCon n -> methodTypeShow currNS (NameCon n)
  | PolyCon t -> "`" ^ (methodTypeShow currNS t)
  | NameCon n -> nsOCamlType currNS n
  | TextCon text -> text
  | TupleCon treps -> "(" ^ (String.concat " * " (List.map (methodTypeShow currNS) treps)) ^ ")"


(* type_rep -> string list *)
let rec getVars tr =
  match tr with
  | OptionCon (ObjCon (TypeVarCon (_, (RowCon (Less, (PolyCon (NameCon {namespace = "Gtk"; name =  _}))))))) -> []
  | ObjCon (TypeVarCon (_, (RowCon (Less, (PolyCon (NameCon {namespace = "Gtk"; name = _})))))) -> []
  | TypeVarCon (var, t) -> var :: getVars t
  | ListCon t -> getVars t
  | OptionCon t -> getVars t
  | ObjCon t -> getVars t
  | RowCon (_, t) -> getVars t
  | PolyCon t -> getVars t
  | NameCon _ -> []
  | TextCon _ -> []
  | TupleCon treps -> List.concat_map getVars treps 


(* type_rep -> bool *)
let isOptional tr =
  match tr with
  | OptionCon _ -> true
  | _ -> false








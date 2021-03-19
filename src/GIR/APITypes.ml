open Constant
open Function
open Callback
open Enum
open Interface
open Object
open Struct
open Union
open Flags

type api =
    | APIConst of constant
    | APIFunction of function_ml
    | APICallback of callback
    | APIEnum of enumeration
    | APIFlags of flags
    | APIInterface of interface
    | APIObject of object_ml
    | APIStruct of struct_ml
    | APIUnion of union


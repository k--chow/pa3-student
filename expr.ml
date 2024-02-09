type prim1 =
  | Add1
  | Sub1
  | IsNum
  | IsBool

type prim2 =
  | Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal

type expr =
  | ELet of (string * expr) list * expr list
  | EWhile of expr * expr list
  | ESet of string * expr
  | EIf of expr * expr * expr
  | EId of string
  | ENumber of int64
  | EBool of bool
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr

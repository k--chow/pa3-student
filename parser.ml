open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Int64

let boa_max = int_of_float(2.**62.) - 1;;
let boa_min = -int_of_float(2.**62.);;
let valid_id_regex = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let number_regex = Str.regexp "^[-]?[0-9]+"
let reserved_words = ["let"; "add1"; "sub1"; "isNum"; "isBool"; "if"]
let reserved_constants = ["true"; "false"; ]
let int_of_string_opt s =
  try Some(of_string s) with
  | _ -> None

let boa_max = (sub (shift_left 1L 62) 1L) (* need - 1 *)
let boa_min = sub (mul boa_max (-1L)) 1L (* bullshit ocaml can't even subtract two numbers *)

let rec parse sexp =
  match sexp with
    | Atom a ->
      (match int_of_string_opt a with
        | Some i -> 
            if i < boa_min || i > boa_max then failwith "Non-representable Number" 
            else ENumber(i)
        | None -> 
          match a with
            | "true" -> EBool(true)
            | "false" -> EBool(false)
            | _ -> EId(a))
    | List l ->
      (match l with
        | [Atom("add1"); second] ->
          EPrim1(Add1, parse second)
        | [Atom("sub1"); second] ->
          EPrim1(Sub1, parse second)
        | [Atom("+"); second; third] ->
          EPrim2(Plus, parse second, parse third)
        | [Atom("-"); second; third] ->
          EPrim2(Minus, parse second, parse third)
        | [Atom("*"); second; third] ->
          EPrim2(Times, parse second, parse third)
        | [Atom("<"); second; third] ->
          EPrim2(Less, parse second, parse third)
        | [Atom(">"); second; third] ->
          EPrim2(Greater, parse second, parse third)
        | [Atom("=="); second; third] ->
          EPrim2(Equal, parse second, parse third)
        | Atom("let")::List(second)::third ->
            ELet(List.map parse_binding second, List.map parse third)
        | [Atom("if"); second; third; fourth] ->
          EIf(parse second, parse third, parse fourth)    
        | [Atom("isNum"); second] ->
          EPrim1(IsNum, parse second)
        | [Atom("isBool"); second] ->
          EPrim1(IsBool, parse second)
        | [Atom("set"); Atom second; third] ->
          ESet(second, parse third)
        | [Atom("while"); cond; List(third)] ->
          EWhile(parse cond, List.map parse third)
        | _ -> failwith ("Invalid syntax " ^ Sexplib.Sexp.to_string(sexp))
       )

and parse_binding binding =
  match binding with
    | List [Atom a; b] -> (a, parse b)
    | _ -> failwith "Invalid binding"


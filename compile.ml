open Printf
open Expr
open Asm

let rec find ls x =
  match ls with
  | [] -> None
  | (y,v)::rest ->
    if y = x then Some(v) else find rest x

let stackloc si = RegOffset(-8 * si, RSP)

let true_const  = HexConst(0x0000000000000002L)
let false_const = HexConst(0x0000000000000000L)

type typ =
  | TNumber
  | TBoolean

let rec tc_e (e : expr) (env : (string * typ) list) : typ =
  match e with
  | ENumber(_) -> TNumber
  | EBool(_) -> TBoolean
  | EPrim1(op, e1) ->
      (match op with
      | Add1 -> 
          let e1_t = tc_e e1 env in
          if e1_t == TNumber then TNumber
          else failwith ("Add1 Mistype expected TNumber but got ")
      | IsBool -> TBoolean
          
       )
  | EIf(e1, e2, e3) ->
      let e1_t = tc_e e1 env in
      let e2_t = tc_e e2 env in
      let e3_t = tc_e e3 env in
      if e1_t != TBoolean then failwith ("If condition not bool")
      else if e2_t != e3_t then failwith ("If values not the same")
      else e2_t
  | EPrim2(op, e1, e2) ->
      (match op with
      | Plus ->
          let e1_t = tc_e e1 env in
          let e2_t = tc_e e2 env in
          if e1_t == TNumber && e2_t == TNumber then TNumber
          else failwith ("Plus Mistype expected TNumber"))

  | _ -> failwith "Not yet implemented"

let rec precompile_bindings b env errs =
  match b with
  | (v, eb)::rest ->
      if List.exists (fun (k, _) -> k = v) env then
        precompile_bindings rest env (("Duplicate binding for " ^ v)::errs)
      else
        precompile_bindings rest ((v, 0)::env) errs
  | [] -> (env, errs)

let rec well_formed_e (e : expr) (env : (string * int) list) : string list =
  match e with
  | ELet(b, e1) ->
    let (b2, errs) = precompile_bindings b env [] in
    let apply_well_formed_e e_list = well_formed_e e_list b2 in 
    errs
    @ List.concat (List.map apply_well_formed_e e1)  (* should b2 move along though with each body? fix this for a list of expressions*)
  | EIf(a,b,c) ->
      well_formed_e a env
      @ well_formed_e b env
      @ well_formed_e c env
  | EId(s) ->
    (match find env s with
    | Some(x) -> []
    | None -> [("Variable identifier " ^ s ^ " unbound")])
  | ENumber(i) -> []
  | EBool(_) -> []
  | EPrim1(_, e1) -> well_formed_e e1 env
  | EPrim2(_, e1, e2) ->
      (well_formed_e e1 env
      @ well_formed_e e2 env)
  | _ -> ["Not well formed"]

let check (e : expr) : string list =
  match well_formed_e e [("input", -1)] with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | ELet(b, e) -> compile_let b e si [] (* reset when starting a new let! or clash *)
  | EIf(cond, e1, e2) ->
      let if_branch = gen_temp "if_branch" in
      let end_if = gen_temp "end_if" in
      let boolean = compile_expr cond si env in (* bool in rax *) (* TODO: check its a bool *)
      let e1_true = compile_expr e1 si env in
      let e2_false = compile_expr e2 si env in
      boolean
      @ check_bool
      @ [
          ICmp(Reg(RAX), true_const);
          IJe(if_branch);
        ]
      @ e2_false
      @ [
          IJmp(end_if);
          ILabel(if_branch);
        ]
      @ e1_true
      @ [ILabel(end_if)];
  | EId(s) ->
    (match find env s with
    | None -> failwith ("Unbound variable identifier " ^ s)
    | Some(x) -> [IMov(Reg(RAX), stackloc x)])
  | ENumber(i) ->
      [IMov(Reg(RAX), Const64(add (shift_left i 1) 1L))]
  | EBool(c) ->
    (match c with
    | true -> [IMov(Reg(RAX), true_const)]
    | false -> [IMov(Reg(RAX), false_const)])
  | EPrim1(op, e1) -> compile_prim1 op e1 si env
  | EPrim2(op, e1, e2) -> compile_prim2 op e1 e2 si env
  | _ -> failwith ("Not yet implemented expr: ")

and compile_let b e si env =
 (* Map b to association list *)
  match b with
  | (v, eb)::rest ->
      if List.exists (fun (k, _) -> k = v) env then
        failwith ("Multiple bindings for variable identifier " ^ v)
      else
        let ee = compile_expr eb si [] in (*env should be empty*)
        ee
        @ [IMov(stackloc si, Reg(RAX))]
        @ compile_let rest e (si+1) ((v, si)::env)
  | [] ->
      compile_expr e (si+1) env

and compile_prim1 op e si env =
  (* TODO *)
  failwith "Not yet implemented"

and compile_prim2 op e1 e2 si env =
  (* TODO *)
  failwith "Not yet implemented"

let compile_to_string prog =
  let _ = check prog in
  let _ = tc_e prog [("input", TNumber)] in
  let prelude = "  section .text\n" ^
                "  extern error\n" ^
                "  global our_code_starts_here\n" ^
                "our_code_starts_here:\n" ^
                "  mov [rsp - 8], rdi\n" in
  let postlude = [IRet]
    (* TODO *) in
  let compiled = (compile_expr prog 2 [("input", 1)]) in
  let as_assembly_string = (to_asm (compiled @ postlude)) in
  sprintf "%s%s\n" prelude as_assembly_string

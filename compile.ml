open Printf
open Expr
open Asm
open Int64

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

type type_env = (string * typ) list
type bind_list = (string * expr) list
     
let rec tc_e (e : expr) (env : (string * typ) list) : typ =
  match e with
  | ENumber(_) -> TNumber
  | EBool(_) -> TBoolean
  | ELet(b_list, e_list) -> 
      (* do bindings first and return new_env
       * do sequence and check if set used
       * *)
      let new_env = tc_b b_list env in
      tc_seq e_list new_env 
  | EPrim1(op, e1) ->
      let e1_t = tc_e e1 env in
      (match op with
      | Add1 
      | Sub1 -> 
          if e1_t == TNumber then TNumber
          else failwith ("Add1/Sub1 Mistype expected TNumber but got ")
      | IsBool 
      | IsNum -> TBoolean)
  | EIf(e1, e2, e3) ->
      let e1_t = tc_e e1 env in
      let e2_t = tc_e e2 env in
      let e3_t = tc_e e3 env in
      if e1_t != TBoolean then failwith ("If condition not bool")
      else if e2_t != e3_t then failwith ("If values not the same")
      else e2_t
  | EPrim2(op, e1, e2) ->
      let e1_t = tc_e e1 env in
      let e2_t = tc_e e2 env in
      (match op with
      | Plus 
      | Minus
      | Times ->
          if e1_t == TNumber && e2_t == TNumber then TNumber
          else failwith ("Plus/Minus/Times Mistype expected TNumber")
      | Less 
      | Greater
      | Equal ->
          if e1_t == TNumber && e2_t == TNumber then TBoolean
          else failwith ("Less/Greater/Equal Mistype expected TNumber"))
  | ESet(s, e1) ->
      let e1_t = tc_e e1 env in e1_t
  | EWhile(c, b) ->
      let c_t = tc_e c env in
      let apply_tc_e e_i = tc_e e_i env in
      let body_tc_e = (List.map apply_tc_e b) in  
      if c_t == TBoolean then TBoolean
      else failwith ("While Mistype expected TBoolean")
  | EId(s) ->
    (match find env s with
    | Some(t) -> t
    | None -> failwith ("Variable identifier " ^ s ^ " unbound type"))

  | _ -> failwith "Not yet implemented type check"

and tc_b (b_list : bind_list) (env : type_env) : type_env =
  match b_list with
  | (b, e)::rest ->
      let t_e = (tc_e e env) in
      tc_b rest ((b, t_e)::env)
  | [] -> env

and tc_seq (e_list: expr list) (env : type_env) : typ =
  match e_list with
  | [e] ->
      tc_e e env 
  | e::rest ->
      let e_t = tc_e e env in
      (match e with
      | ESet(s, _) ->
          let replace_env = List.map (fun (k, v) -> if k = s then (k, e_t) else (k, v)) env in
          tc_seq rest replace_env 
      | _ ->
          tc_seq rest env)
      

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
  | ESet(_, e1) ->
      well_formed_e e1 env
  | EWhile(cond, body) ->
      let apply_well_formed_e e_list = well_formed_e e_list env in
      (well_formed_e cond env
      @ List.concat (List.map apply_well_formed_e body))
  | _ -> ["Not well formed"]

let check (e : expr) : string list =
  match well_formed_e e [("input", -1)] with
  | [] -> []
  | errs -> failwith (String.concat "\n" errs)

let rec compile_expr (e : expr) (si : int) (env : (string * int) list) : instruction list =
  match e with
  | ELet(b, e) -> (compile_let b e si []) 
  | EIf(cond, e1, e2) ->
      let if_branch = gen_temp "if_branch" in
      let end_if = gen_temp "end_if" in
      let boolean = compile_expr cond si env in (* bool in rax *) (* TODO: check its a bool *)
      let e1_true = compile_expr e1 si env in
      let e2_false = compile_expr e2 si env in
      boolean
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
  | ESet(s, e1) ->
      let e1_eval = compile_expr e1 si env in
      (match find env s with
      | None -> failwith ("Fail set")
      | Some(x) -> 
          e1_eval
          @ [IMov(stackloc x, Reg(RAX))]
      )
  | EWhile(c, b) ->
      let cond = compile_expr c si env in
      let apply_compile_expr e1 = compile_expr e1 si env in
      let start_while = gen_temp "start_while" in
      let end_while = gen_temp "end_while" in
      [ILabel("start_while")]
      @ cond
      @ [
          ICmp(Reg(RAX), false_const);
          IJe("end_while");
        ]
      @ List.concat (List.map apply_compile_expr b)
      @ [
          IJmp("start_while");
          ILabel("end_while");
        ]
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
      match e with
      | e1::rest ->
          compile_expr e1 (si+1) env
          @ compile_let [] rest (si+1) env
      | [] -> []
          
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

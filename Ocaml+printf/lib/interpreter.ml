(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `Division_by_zero
  | `Matching_failure
  | `Invalid_recursion
  | `Invalid_argument of string
  | `Type_mismatch (* this error is unreachable after type check *)
  | `No_variable (* this error is unreachable after type check *)
  ]

(* TODO: add pretty prints of  error *)

module MONAD_WRITE_ERROR : sig
  type 'a t

  val return : 'a -> 'a t
  val write : string -> unit t
  val fail : error -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> string list * ('a, error) Base.Result.t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end = struct
  open Base

  type 'a t = string list -> string list * ('a, error) Result.t

  let return x : 'a t = fun list -> list, Result.return x
  let write str : unit t = fun list -> str :: list, Result.return ()
  let fail err : 'a t = fun list -> list, Result.fail err

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun list ->
    match m list with
    | list, Result.Error e -> list, Result.fail e
    | list, Result.Ok x -> f x list
  ;;

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun list ->
    match m list with
    | list, Result.Error e -> list, Result.fail e
    | list, Result.Ok x -> list, Result.return (f x)
  ;;

  let bind x f = x >>= f
  let run m = m []

  module Syntax = struct
    let ( let* ) x f = bind x f
  end
end

type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VFormat_string of Ast.fstring
  | VFun of string option * Ast.pattern * Ast.expr * env_values
  | VTuple of value list
  | VList of value list

and env_values = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf = function
  | VInt i -> Format.fprintf ppf "%d" i
  | VBool b -> Format.fprintf ppf "%B" b
  | VChar c -> Format.fprintf ppf {|'%c'|} c
  | VString s -> Format.fprintf ppf {|"%s"|} s
  | VFun _ -> Format.fprintf ppf "<fun>"
  | VTuple lst ->
    Format.printf "(";
    let rec helper = function
      | [] -> Format.printf ")"
      | h :: tl ->
        let fmt = if tl = [] then format_of_string "%a" else format_of_string "%a, " in
        Format.fprintf ppf fmt pp_value h;
        helper tl
    in
    helper lst
  | VList lst ->
    Format.printf "[";
    let rec helper = function
      | [] -> Format.printf "]"
      | h :: tl ->
        let fmt = if tl = [] then format_of_string "%a" else format_of_string "%a; " in
        Format.fprintf ppf fmt pp_value h;
        helper tl
    in
    helper lst
    (*TODO: maybe change *)
  | VFormat_string _ -> Format.printf "format string"
;;

module EnvValues : sig
  val std : env_values
  val empty : env_values
  val find : env_values -> string -> value option
  val update : env_values -> string -> value -> env_values
  val pp_env_values : Stdlib.Format.formatter -> env_values -> unit
end = struct
  open Base

  let empty = Map.empty (module String)

  (* TODO : add *)
  let std = Map.empty (module String)
  let find mp key = Map.find mp key
  let update mp key data = Map.update mp key ~f:(function _ -> data)

  let pp_env_values ppf env_values =
    Map.iteri env_values ~f:(fun ~key ~data ->
      match find std key with
      | None ->
        Stdlib.Format.printf "val %s = " key;
        pp_value ppf data;
        Stdlib.Format.printf "\n"
      | Some _ -> Stdlib.Format.printf "")
  ;;

  let std =
    let open Ast in
    let val_get =
      VFun (None, Pat_val (LCIdent "i"), Expr_fun (Pat_val (LCIdent "s"), Expr_get), empty)
    in
    let val_format_of_str =
      VFun (None, Pat_val (LCIdent "i"), Expr_format_of_str, empty)
    in
    let init_env = empty in
    let init_env = update init_env "get" val_get in
    let init_env = update init_env "format_of_string" val_format_of_str in
    init_env
  ;;
end

open MONAD_WRITE_ERROR
open MONAD_WRITE_ERROR.Syntax

let return_int i = return (VInt i)
let return_bool b = return (VBool b)
let return_char c = return (VChar c)
let return_string s = return (VString s)

let eval_binop p v1 v2 =
  let open Ast in
  match p, v1, v2 with
  | Add, VInt i1, VInt i2 -> return_int (i1 + i2)
  | Sub, VInt i1, VInt i2 -> return_int (i1 - i2)
  | Mul, VInt i1, VInt i2 -> return_int (i1 * i2)
  | Div, VInt i1, VInt i2 ->
    if i2 = 0 then fail `Division_by_zero else return_int (i1 / i2)
  | Eq, VInt i1, VInt i2 -> return_bool (i1 = i2)
  | Neq, VInt i1, VInt i2 -> return_bool (i1 <> i2)
  | Leq, VInt i1, VInt i2 -> return_bool (i1 <= i2)
  | Geq, VInt i1, VInt i2 -> return_bool (i1 >= i2)
  | Gre, VInt i1, VInt i2 -> return_bool (i1 > i2)
  | Less, VInt i1, VInt i2 -> return_bool (i1 < i2)
  | And, VBool b1, VBool b2 -> return_bool (b1 && b2)
  | Or, VBool b1, VBool b2 -> return_bool (b1 || b2)
  | Concat, VString s1, VString s2 -> return_string (s1 ^ s2)
  | Concat_format, VFormat_string s1, VFormat_string s2 ->
    return (VFormat_string (List.append s1 s2))
  | _, _, _ -> fail `Type_mismatch
;;

let rec match_pattern env = function
  | Ast.Pat_any, _ -> Some env
  | Ast.Pat_empty_list, VList [] -> Some env
  | Ast.Pat_const (Int i1), VInt i2 when i1 = i2 -> Some env
  | Ast.Pat_const (Bool b1), VBool b2 when b1 = b2 -> Some env
  | Ast.Pat_const (Char c1), VChar c2 when c1 = c2 -> Some env
  | Ast.Pat_const (String s1), VString s2 when s1 = s2 -> Some env
  | Ast.Pat_val (LCIdent name), v -> Some (EnvValues.update env name v)
  | Ast.Pat_tuple pat_list, VTuple val_tuple ->
    let f1 env p v =
      match env with
      | Some env -> match_pattern env (p, v)
      | None -> None
    in
    List.fold_left2 f1 (Some env) pat_list val_tuple
  | Ast.Pat_cons_list (p1, p2), VList (h :: tl) ->
    (match match_pattern env (p1, h) with
     | Some env -> match_pattern env (p2, VList tl)
     | None -> None)
  | _ -> None
;;

let eval_expr =
  let rec helper env = function
    | Ast.Expr_const e ->
      (match e with
       | Ast.Int i -> return_int i
       | Ast.Bool b -> return_bool b
       | Ast.Char c -> return_char c
       | Ast.String s -> return_string s)
    | Ast.Expr_val (Ast.LCIdent name) ->
      (match EnvValues.find env name with
       | None ->
         (match EnvValues.find env name with
          | Some x -> return x
          | None -> fail `No_variable)
       | Some x -> return x)
    | Ast.Expr_empty_list -> return @@ VList []
    | Ast.Expr_fstring fmt -> return @@ VFormat_string fmt
    | Ast.Un_op (op, e) ->
      let* v = helper env e in
      (match op, v with
       | Ast.Un_plus, VInt i -> return_int i
       | Ast.Un_minus, VInt i -> return_int (-i)
       | _, _ -> fail `Type_mismatch)
    | Ast.Bin_op (op, e1, e2) ->
      let* e1 = helper env e1 in
      let* e2 = helper env e2 in
      eval_binop op e1 e2
    | Ast.Expr_ite (e1, e2, e3) ->
      let* e1 = helper env e1 in
      (match e1 with
       | VBool true ->
         let* e2 = helper env e2 in
         return e2
       | VBool false ->
         let* e3 = helper env e3 in
         return e3
       | _ -> fail `Type_mismatch)
    | Ast.Expr_let ((false, Ast.LCIdent name, e1), e2) ->
      let* v1 = helper env e1 in
      let env = EnvValues.update env name v1 in
      let* v2 = helper env e2 in
      return v2
    | Ast.Expr_let ((true, Ast.LCIdent name, e1), e2) ->
      let* v1 = helper env e1 in
      let v1 =
        match v1 with
        (* If add patterns add error here for non-valname *)
        | VFun (None, p, e, env) -> VFun (Some name, p, e, env)
        | other -> other
      in
      let env = EnvValues.update env name v1 in
      let* v2 = helper env e2 in
      return v2
    | Ast.Expr_fun (p, e) -> return (VFun (None, p, e, env))
    | Ast.Expr_app (e1, e2) ->
      let* f = helper env e1 in
      let* arg = helper env e2 in
      (match f with
       | VFun (fun_name, pat, e, fun_env) ->
         let* new_env =
           match match_pattern fun_env (pat, arg) with
           | Some env -> return env
           | None -> fail `Matching_failure
         in
         let new_env =
           match fun_name with
           | Some fun_name -> EnvValues.update new_env fun_name f
           | None -> new_env
         in
         helper new_env e
       | _ -> fail `Type_mismatch)
    | Ast.Expr_tuple list ->
      let* value_list =
        List.fold_right
          (fun expr acc ->
            let* acc = acc in
            let* value = helper env expr in
            return (value :: acc))
          list
          (return [])
      in
      return @@ VTuple value_list
    | Ast.Expr_cons_list (h, tl) ->
      let* h = helper env h in
      let* tl = helper env tl in
      (match tl with
       | VList tl -> return @@ VList (h :: tl)
       | _ -> fail `Type_mismatch)
    | Ast.Expr_match (e, list) ->
      let* value = helper env e in
      eval_match env value list
    (* hardcoded functions *)
    | Ast.Expr_format_of_str ->
      let s = EnvValues.find env "s" in
      (match s with
       | Some (VFormat_string s) -> return @@ VFormat_string s
       | _ -> fail `No_variable)
    | Ast.Expr_get ->
      let s = EnvValues.find env "s" in
      let i = EnvValues.find env "i" in
      (match s, i with
       | Some (VString s), Some (VInt i) ->
         if i < String.length s
         then return @@ VChar s.[i]
         else fail @@ `Invalid_argument "Index out of bounds"
       | _, _ -> fail `No_variable)
    | _ -> return @@ VInt 1
  and eval_match env value = function
    | [] -> fail `Matching_failure
    | (pat, expr) :: tl ->
      let new_env = match_pattern env (pat, value) in
      (match new_env with
       | Some env -> helper env expr
       | None -> eval_match env value tl)
  in
  helper
;;

let eval_toplevel env = function
  | Ast.Let_decl (false, Ast.LCIdent name, expr) ->
    let* v = eval_expr env expr in
    let env = EnvValues.update env name v in
    return (env, v)
  | Ast.Let_decl (true, Ast.LCIdent name, expr) ->
    let* v = eval_expr env expr in
    let v =
      match v with
      | VFun (None, p, e, env) -> VFun (Some name, p, e, env)
      | other -> other
    in
    let env = EnvValues.update env name v in
    return (env, v)
  | Ast.Expr expr ->
    let* v = eval_expr env expr in
    return (env, v)
;;

let eval_program prog =
  List.fold_left
    (fun acc toplevel ->
      let* acc = acc in
      let* env, _ = eval_toplevel acc toplevel in
      return env)
    (return @@ EnvValues.std)
    prog
;;

let run_eval_expr expr =
  let out, value = run (eval_expr EnvValues.std expr) in
  List.iter (fun s -> Format.printf "%s\n" s) out;
  value
;;

let run_eval_program prog =
  let out, env = run (eval_program prog) in
  List.iter (fun s -> Format.printf "%s\n" s) out;
  env
;;

(* TODO: remove tests *)

let eval_expr_and_print str =
  let res = Result.get_ok (Parser.run_parser_expr str) in
  let res = run_eval_expr res in
  match res with
  | Base.Result.Ok v -> Format.printf "%a" pp_value v
  | Base.Result.Error _ -> Format.printf "ERROR <- TODO, change"
;;

let eval_program_and_print str =
  let res = Result.get_ok (Parser.run_parser_program str) in
  let res = run_eval_program res in
  match res with
  | Base.Result.Ok v -> Format.printf "%a" EnvValues.pp_env_values v
  | Base.Result.Error _ -> Format.printf "ERROR <- TODO, change"
;;

(* well formed *)

let%expect_test _ =
  let _ =
    eval_program_and_print
      {|
  let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  let a = fac 6
  let rev list =
    let rec helper acc list =
      match list with
      | [] -> acc
      | h :: tl -> helper (h :: acc) tl
    in
    helper [] list 
  let reversed1 = rev [1;2;3;4;5]
  let reversed2 = rev [true;false;false;false]
  |}
  in
  [%expect {|
    val a = 720
    val fac = <fun>
    val format_of_string = <fun>
    val get = <fun>
    val rev = <fun>
    val reversed1 = [5; 4; 3; 2; 1]
    val reversed2 = [false; false; false; true] |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = 0 in let b = 1 in b / a|} in
  [%expect {| ERROR <- TODO, change |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let f = 2 in f|} in
  [%expect {| 2 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = 1 in let b = 2 in let c = 3 in a + b * c|} in
  [%expect {| 7 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let id x = x in let b a = id a in b 5|} in
  [%expect {| 5 |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print {|let rec f n = if n <= 1 then 1 else n * f (n - 1) in f 5|}
  in
  [%expect {| 120 |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let rec f a b = if a + b > 100 then a + b else f (a + 3) (b * 2) in f 1 5 |}
  in
  [%expect {| 176 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let rec x = 1 in x|} in
  [%expect {| 1 |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let rec x = x + 1 in x|} in
  [%expect {| ERROR <- TODO, change |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let a = (1+3*4, 'c', "ab" ^ "cd", strue) in a|} in
  [%expect {| ERROR <- TODO, change |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let str = "abc" in str.[0]|} in
  [%expect {| 'a' |}]
;;

let%expect_test _ =
  let _ = eval_expr_and_print {|let str = "a\nc" in str.[3]|} in
  [%expect {| ERROR <- TODO, change |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let a = 1::2::[3;4] in match a with | h::tl -> (h, tl) | _ -> (1,[2])|}
  in
  [%expect {| (1, [2; 3; 4]) |}]
;;

let%expect_test _ =
  let _ =
    eval_expr_and_print
      {|let str = "abc" in let a = (str, 'c') in match a with | ("abc", _) -> "yes" | _ -> "no"|}
  in
  [%expect {| "yes" |}]
;;

(* TODO: test for tuple *)
(* let%expect_test _ =
  let _ = eval_expr_and_print {|let fst (a, b) = a in let b a = id a in b 5|} in
  [%expect {| 7 |}]
;; *)

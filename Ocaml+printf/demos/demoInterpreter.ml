(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

(* match typed variables to letters *)
let assign_names typ =
  let calc_name letter num =
    "\'"
    ^
    if num = 0
    then Base.Char.to_string letter
    else Base.Char.to_string letter ^ Int.to_string num
  in
  let next_letter c = Base.Char.of_int_exn (Base.Char.to_int c + 1) in
  let next letter num =
    if Char.equal letter 'z' then 'a', num + 1 else next_letter letter, num
  in
  let rec helper names letter num = function
    | Typedtree.TVar n ->
      (match Base.Map.add names ~key:n ~data:(calc_name letter num) with
       | `Ok new_names -> new_names, next letter num
       | `Duplicate -> names, (letter, num))
    | TArr (l, r) ->
      let names, (letter, num) = helper names letter num l in
      helper names letter num r
    | TList t -> helper names letter num t
    | TTuple list ->
      List.fold_left
        (fun (names, (letter, num)) -> helper names letter num)
        (names, (letter, num))
        list
    | _ -> names, (letter, num)
  in
  let names, (_, _) = helper (Base.Map.empty (module Base.Int)) 'a' 0 typ in
  names
;;

(* pretty print types with letters *)
let pp_typ ppf typ =
  let open Typedtree in
  let names = assign_names typ in
  let rec helper ppf = function
    | TVar n -> Format.fprintf ppf "%s" (Base.Map.find_exn names n)
    | TPrim s -> Format.fprintf ppf "%s" s
    | TArr (l, r) ->
      (match l, r with
       | TArr (_, _), _ -> Format.fprintf ppf "(%a) -> %a" helper l helper r
       | _ -> Format.fprintf ppf "%a -> %a" helper l helper r)
    | TUnit -> Format.fprintf ppf "unit"
    | TTuple l ->
      (match l with
       | h :: tl ->
         List.fold_left
           (fun _ item -> Format.fprintf ppf " * %a" pp_typ item)
           (Format.fprintf ppf "(%a" pp_typ h)
           tl;
         Format.fprintf ppf ")"
       | _ -> Format.fprintf ppf "Impossible state")
    | TList t -> Format.fprintf ppf "%a list" helper t
    | TFString t -> Format.fprintf ppf "%a format_string" helper t
  in
  helper ppf typ
;;

(* print scheme without binded vars *)
let pp_scheme ppf = function
  | Typedtree.Scheme (_, t) -> Format.fprintf ppf "%a" pp_typ t
;;

let print_res typed_env env_values =
  let open Interpreter in
  let open Inferencer in
  let std = EnvValues.std in
  let without_std =
    Base.Map.filter_keys env_values ~f:(fun name ->
      match EnvValues.find std name with
      | None -> true
      | Some _ -> false)
  in
  Base.Map.iteri without_std ~f:(fun ~key ~data ->
    match TypeEnv.find typed_env key with
    | Some sch -> Format.printf "val %s : %a = %a\n" key pp_scheme sch pp_value data
    | None -> Format.printf "Value %s is not in typed environment\n" key)
;;

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error. %a" Inferencer.pp_error err
     | Ok (typed_env, new_ast) ->
       (match Interpreter.run_eval_program new_ast with
        | Error err -> Format.printf "Interpretation error. %a" Interpreter.pp_error err
        | Ok env_values -> print_res typed_env env_values))
;;

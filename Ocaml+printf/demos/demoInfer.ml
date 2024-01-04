(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

(* TODO : delete this *)
(* it is assumed that the parsing is successful *)
let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Result.get_ok (Parser.parse_program str) in
  match Inferencer.run_infer_program parsed with
  | Ok (env, _) -> Format.printf "%a" Inferencer.TypeEnv.pp_env env
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

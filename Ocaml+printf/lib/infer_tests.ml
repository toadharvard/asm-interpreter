(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let infer_expr_and_print str =
  let parsed = Result.get_ok (Parser.parse_expression str) in
  match Inferencer.run_infer_expr parsed with
  | Ok ty -> Format.printf "%a" Inferencer.pp_typ ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let infer_program_and_print str =
  let parsed = Result.get_ok (Parser.parse_program str) in
  match Inferencer.run_infer_program parsed with
  | Ok ty -> Format.printf "%a" Inferencer.TypeEnv.pp_env ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f x g = g x in f|} in
  [%expect {| ('_5 -> (('_5 -> '_6) -> '_6)) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print
      {|let f x g = g x in let id x = x in let fst x y = x in fst (f id)|}
  in
  [%expect {| ('_10 -> ((('_14 -> '_14) -> '_13) -> '_13)) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {| fun f -> fun x -> f x |} in
  [%expect {| (('_3 -> '_4) -> ('_3 -> '_4)) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print {|let id x = x in if (id 2 < 3) then id else (fun t -> t)|}
  in
  [%expect {| ('_7 -> '_7) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let t _ a = a in 1|} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|fun (a,b,c,d) -> a + d|} in
  [%expect {| ((int * '_4 * '_3 * int) -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|fun (a,b,(2::t),d) -> a + d|} in
  [%expect {| ((int * '_4 * int list * int) -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f ((1,2)::y) = 0 in f |} in
  [%expect {| ((int * int) list -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f (1,_,y) = y in f |} in
  [%expect {| ((int * '_5 * '_4) -> '_4) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let a = [] in a |} in
  [%expect {| '_3 list |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f a = a::['c';'d'] in f |} in
  [%expect {| (char -> char list) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print {|let rec f n = if n <= 1 then 0 else n * f (n - 1) in f|}
  in
  [%expect {| (int -> int) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print
      {|let rec f (a, b) = if a + b < 10 then a + b else f (a-1,b-1) in f|}
  in
  [%expect {| ((int * int) -> int) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print {|let f x = match x with | [] -> "end" | h::tl -> h in f |}
  in
  [%expect {| (string list -> string) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {| let x = 1 in match x with | a -> 3 |} in
  [%expect {| int |}]
;;

(* Incorrect *)

let%expect_test _ =
  let _ = infer_expr_and_print {|let f x = x + 1; x ^ "str" in f |} in
  [%expect {| Unification failed on int and string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {| let x = 1 in match x with | 0 -> 'c' | _ -> "abc"  |} in
  [%expect {| Unification failed on string and char |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f (a::1) = 0 in f |} in
  [%expect {| Incorrect pattern mathing, expected '_0 list, found int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f x = a in f|} in
  [%expect {| Undefined variable 'a' |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f x = "sdfsdf" || true in f |} in
  [%expect {| Unification failed on string and bool |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let b = 'c' in let a = 1::b::2::[] in a|} in
  [%expect {| Unification failed on char and int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print {|let f x = f  1 in f |} in
  [%expect {| Undefined variable 'f' |}]
;;

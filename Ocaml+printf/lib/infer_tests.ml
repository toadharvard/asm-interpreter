(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let infer_expr_and_print_typ str =
  let parsed = Result.get_ok (Parser.parse_expression str) in
  match Inferencer.run_infer_expr parsed with
  | Ok (ty, _) -> Format.printf "%a" Inferencer.pp_typ ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let infer_program_and_print_ast str =
  let parsed = Result.get_ok (Parser.parse_program str) in
  match Inferencer.run_infer_program parsed with
  | Ok (ty, _) -> Format.printf "%a" Inferencer.TypeEnv.pp_env ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x g = g x in f|} in
  [%expect {| ('_6 -> (('_6 -> '_7) -> '_7)) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {|let f x g = g x in let id x = x in let fst x y = x in fst (f id)|}
  in
  [%expect {| ('_11 -> ((('_15 -> '_15) -> '_14) -> '_14)) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| fun f -> fun x -> f x |} in
  [%expect {| (('_4 -> '_5) -> ('_4 -> '_5)) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let id x = x in if (id 2 < 3) then id else (fun t -> t)|}
  in
  [%expect {| ('_8 -> '_8) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let t _ a = a in 1|} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun (a,b,c,d) -> a + d|} in
  [%expect {| ((int * '_5 * '_4 * int) -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun (a,b,(2::t),d) -> a + d|} in
  [%expect {| ((int * '_5 * int list * int) -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f ((1,2)::y) = 0 in f |} in
  [%expect {| ((int * int) list -> int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f (1,_,y) = y in f |} in
  [%expect {| ((int * '_6 * '_5) -> '_5) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let a = [] in a |} in
  [%expect {| '_4 list |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f a = a::['c';'d'] in f |} in
  [%expect {| (char -> char list) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let rec f n = if n <= 1 then 0 else n * f (n - 1) in f|}
  in
  [%expect {| (int -> int) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {|let rec f (a, b) = if a + b < 10 then a + b else f (a-1,b-1) in f|}
  in
  [%expect {| ((int * int) -> int) |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let f x = match x with | [] -> "end" | h::tl -> h in f |}
  in
  [%expect {| (string list -> string) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let x = 1 in match x with | a -> 3 |} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| "as%dad" ^^ "sfs%c" |} in
  [%expect {|
    (int -> (char -> unit)) format_string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let str = format_of_string "%d%d%ddfs" in str|} in
  [%expect {|
    (int -> (int -> (int -> unit))) format_string |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {| let str = format_of_string "%c" in (str ^^ "%d%s")|}
  in
  [%expect {|
    (char -> (int -> (string -> unit))) format_string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let fmt = format_of_string in fmt "%c" |} in
  [%expect {|
    (char -> unit) format_string |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {| let c = 'c' in let my_printf = printf "%c" in my_printf c |}
  in
  [%expect {|
    unit |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {| let fs = format_of_string "%d%Babc" in printf (fs ^^ "%s") |}
  in
  [%expect {|
    (int -> (bool -> (string -> unit))) |}]
;;

(* Incorrect *)

(* TODO: printf*)
(* let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x = x + 1; x ^ "str" in f |} in
  [%expect {| Unification failed on int and string |}]
;; *)

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let s = "123" in (s ^^ "%d") |} in
  [%expect
    {| Invalid format concatination of "string" and "(int -> unit) format_string" |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let rec f x = f in f |} in
  [%expect {| Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f (a,b) = a + b in f (1,2,3)|} in
  [%expect {| Unification failed on (int * int) and (int * int * int) |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let rec f x = f in f |} in
  [%expect {| Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let f x = x + 1; x || true; "asdad" in x |} in
  [%expect {| Unification failed on int and unit |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {| let x = 1 in match x with | 0 -> 'c' | _ -> "abc"  |}
  in
  [%expect {| Unification failed on char and string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f (a::1) = 0 in f |} in
  [%expect {| Incorrect pattern matching, expected '_0 list, found int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x = a in f|} in
  [%expect {| Undefined variable 'a' |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x = "sdfsdf" || true in f |} in
  [%expect {| Unification failed on string and bool |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let b = 'c' in let a   = 1::b::2::[] in a|} in
  [%expect {| Unification failed on char and int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x = f  1 in f |} in
  [%expect {| Undefined variable 'f' |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun f x -> x x|} in
  [%expect {| Occurs check failed |}]
;;

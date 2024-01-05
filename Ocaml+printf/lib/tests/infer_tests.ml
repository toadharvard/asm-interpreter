(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let infer_expr_and_print_typ str =
  let parsed = Result.get_ok (Parser.run_parser_expr str) in
  match Inferencer.run_infer_expr parsed with
  | Ok (ty, _) -> Format.printf "%a" Inferencer.pp_typ ty
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let infer_program_and_print_env str =
  let parsed = Result.get_ok (Parser.run_parser_program str) in
  match Inferencer.run_infer_program parsed with
  | Ok (env, _) -> Format.printf "%a" Inferencer.TypeEnv.pp_env env
  | Error err -> Format.printf "%a" Inferencer.pp_error err
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x g = g x in f|} in
  [%expect {| '_5 -> ('_5 -> '_6) -> '_6 |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {|let f x g = g x in let id x = x in let fst x y = x in fst (f id)|}
  in
  [%expect {| '_10 -> (('_14 -> '_14) -> '_13) -> '_13 |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| fun f -> fun x -> f x |} in
  [%expect {| ('_3 -> '_4) -> '_3 -> '_4 |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let id x = x in if (id 2 < 3) then id else (fun t -> t)|}
  in
  [%expect {| '_7 -> '_7 |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let t _ a = a in 1|} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun (a,b,c,d) -> a + d|} in
  [%expect {| (int * '_4 * '_3 * int) -> int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun (a,b,(2::t),d) -> a + d|} in
  [%expect {| (int * '_5 * int list * int) -> int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f ((1,2)::y) = 0 in f |} in
  [%expect {| (int * int) list -> int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f (1,_,y) = y in f |} in
  [%expect {| (int * '_5 * '_4) -> '_4 |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let a = [] in a |} in
  [%expect {| '_3 list |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f a = a::['c';'d'] in f |} in
  [%expect {| char -> char list |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let rec f n = if n <= 1 then 0 else n * f (n - 1) in f|}
  in
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ
      {|let rec f (a, b) = if a + b < 10 then a + b else f (a-1,b-1) in f|}
  in
  [%expect {| (int * int) -> int |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {|let f x = match x with | [] -> "end" | h::tl -> h in f |}
  in
  [%expect {| string list -> string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let x = 1 in match x with | a -> 3 |} in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| "as%dad" ^^ "sfs%c" |} in
  [%expect {|
    int -> char -> unit format_string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let str = format_of_string "%d%d%ddfs" in str|} in
  [%expect {|
    int -> int -> int -> unit format_string |}]
;;

let%expect_test _ =
  let _ =
    infer_expr_and_print_typ {| let str = format_of_string "%c" in (str ^^ "%d%s")|}
  in
  [%expect {|
    char -> int -> string -> unit format_string |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let fmt = format_of_string in fmt "%c" |} in
  [%expect {|
    char -> unit format_string |}]
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
    int -> bool -> string -> unit |}]
;;

let%expect_test _ =
  let _ =
    infer_program_and_print_env
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
  [%expect
    {|
    val a : forall [ ] . int
    val fac : forall [ ] . int -> int
    val rev : forall [ 22; ] . '_22 list -> '_22 list
    val reversed1 : forall [ ] . int list
    val reversed2 : forall [ ] . bool list |}]
;;

let%expect_test _ =
  let _ =
    infer_program_and_print_env
      {|let fmt = "%d%B" ^^ "%c%s";;
        printf fmt 1 true 'a' "abc"|}
  in
  [%expect
    {|
    val fmt : forall [ ] . int -> bool -> char -> string -> unit format_string |}]
;;

(* Errors *)

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| let s = "123" in (s ^^ "%d") |} in
  [%expect {| Invalid format concatination of "string" and "int -> unit format_string" |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| printf "%" |} in
  [%expect {| Invalid format string "%" |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {| fun (a,a) -> a + a |} in
  [%expect {| Variable a is bound several times in matching |}]
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
  [%expect {| Unification failed on int and '_3 list |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|let f x = a in f|} in
  [%expect {| Undefined variable "a" |}]
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
  [%expect {| Undefined variable "f" |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|fun f x -> x x|} in
  [%expect {| Occurs check failed |}]
;;

let%expect_test _ =
  let _ = infer_expr_and_print_typ {|
  let rec fix f x = f (fix f) x in 
  fix 
  |} in
  [%expect {| (('_8 -> '_9) -> '_8 -> '_9) -> '_8 -> '_9 |}]
;;

let%expect_test "Formatted Logging" =
  let _ =
    infer_expr_and_print_typ
      {| 
      let log ppf  = 
        if true then printf ppf
        else  printf ppf
      in 
      log "%s" "asdf"
        |}
  in
  [%expect {|
    Occurs check failed |}]
;;

(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_printf_lib

let parse_and_print str =
  Format.printf "%a" Ast.pp_program (Result.get_ok (Parser.parse_program str))
;;

let%expect_test _ =
  parse_and_print {|  ;; ;;;;   ;; ;;let _a=1;; ;;;; let   _b =1+2  |};
  [%expect
    {|
    [(Let_decl (false, (LCIdent "_a"), (Expr_const (Int 1))));
      (Let_decl
         (false, (LCIdent "_b"),
          (Bin_op (Add, (Expr_const (Int 1)), (Expr_const (Int 2))))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let abcde a  b  c  d  e   =1     |};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "abcde"),
         (Expr_fun ((LCIdent "a"),
            (Expr_fun ((LCIdent "b"),
               (Expr_fun ((LCIdent "c"),
                  (Expr_fun ((LCIdent "d"),
                     (Expr_fun ((LCIdent "e"), (Expr_const (Int 1))))))
                  ))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let a =  10/( 2+(+ +5) )*( 7 + - 1) - ( 1 + 2*  3 ) |};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "a"),
         (Bin_op (Sub,
            (Bin_op (Mul,
               (Bin_op (Div, (Expr_const (Int 10)),
                  (Bin_op (Add, (Expr_const (Int 2)),
                     (Un_op (Un_plus, (Un_op (Un_plus, (Expr_const (Int 5))))))))
                  )),
               (Bin_op (Add, (Expr_const (Int 7)),
                  (Un_op (Un_minus, (Expr_const (Int 1))))))
               )),
            (Bin_op (Add, (Expr_const (Int 1)),
               (Bin_op (Mul, (Expr_const (Int 2)), (Expr_const (Int 3))))))
            ))))
      ] |}]
;;

(* factorial *)
let%expect_test _ =
  parse_and_print {|let rec fac n = if n <= 1 then 1 else n * fac (n - 1)|};
  [%expect
    {|
    [(Let_decl
        (true, (LCIdent "fac"),
         (Expr_fun ((LCIdent "n"),
            (Expr_ite (
               (Bin_op (Leq, (Expr_val (LCIdent "n")), (Expr_const (Int 1)))),
               (Expr_const (Int 1)),
               (Bin_op (Mul, (Expr_val (LCIdent "n")),
                  (Expr_app ((Expr_val (LCIdent "fac")),
                     (Bin_op (Sub, (Expr_val (LCIdent "n")), (Expr_const (Int 1))
                        ))
                     ))
                  ))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print {|let f = fun a b -> a + b|};
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_fun ((LCIdent "a"),
            (Expr_fun ((LCIdent "b"),
               (Bin_op (Add, (Expr_val (LCIdent "a")), (Expr_val (LCIdent "b"))))
               ))
            ))))
      ] |}]
;;

let%expect_test _ =
  parse_and_print "let f a = let rec add b c = \"asdasd\" + \"ad\na\" + c in add a 3";
  [%expect
    {|
    [(Let_decl
        (false, (LCIdent "f"),
         (Expr_fun ((LCIdent "a"),
            (Expr_let (
               (true, (LCIdent "add"),
                (Expr_fun ((LCIdent "b"),
                   (Expr_fun ((LCIdent "c"),
                      (Bin_op (Add,
                         (Bin_op (Add, (Expr_const (String "asdasd")),
                            (Expr_const (String "ad\na")))),
                         (Expr_val (LCIdent "c"))))
                      ))
                   ))),
               (Expr_app (
                  (Expr_app ((Expr_val (LCIdent "add")), (Expr_val (LCIdent "a"))
                     )),
                  (Expr_const (Int 3))))
               ))
            ))))
      ]
   |}]
;;

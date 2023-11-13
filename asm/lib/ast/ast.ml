(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type i32
type i64

type _ register =
  | Eax : i32 register
  | Ebx : i32 register
  | Ecx : i32 register
  | Edx : i32 register
  | Esi : i32 register
  | Edi : i32 register
  | Esp : i32 register
  | Ebp : i32 register
  | Rax : i64 register
  | Rbx : i64 register
  | Rcx : i64 register
  | Rdx : i64 register
  | Rsp : i64 register
  | Rbp : i64 register
  | Rsi : i64 register
  | Rdi : i64 register
[@@deriving variants]

type _ immediate = ImmInt of int [@@deriving variants]
type 'a register_ref = RegisterRef of 'a register [@@deriving variants]
type label_ref = LabelRef of string [@@deriving variants]

type _ operand =
  (* Double register operands *)
  | Reg_32_Reg_32 : i32 register * i32 register -> [> `Reg_32_Reg_32 ] operand
  | Reg_64_Reg_64 : i64 register * i64 register -> [> `Reg_64_Reg_64 ] operand
  (* Double memory operands *)
  | Mem_32_Mem_32 : i32 register_ref * i32 register_ref -> [> `Mem_32_Mem_32 ] operand
  | Mem_64_Mem_64 : i64 register_ref * i64 register_ref -> [> `Mem_64_Mem_64 ] operand
  (* Mixed double operands *)
  | Reg_32_Imm_a : i32 register * _ immediate -> [> `Reg_32_Imm_a ] operand
  | Reg_64_Imm_a : i64 register * _ immediate -> [> `Reg_64_Imm_a ] operand
  (* Single immidiate operand *)
  | Imm_a : _ immediate -> [> `Imm_a ] operand
  (* Single register operand*)
  | Reg_32 : i32 register -> [> `Reg_32 ] operand
  | Reg_64 : i64 register -> [> `Reg_64 ] operand
  (* Single memory operand*)
  | Mem_32 : i32 register_ref -> [> `Mem_32 ] operand
  | Mem_64 : i64 register_ref -> [> `Mem_64 ] operand
  (* Mnemonic without operand (e.g Ret)*)
  | Nothing : [> `Nothing ] operand
  (* Label operand (e.g Jmp lbl) *)
  | Label : label_ref -> [> `Label ] operand
[@@deriving variants]

(* Some mnemonic variants are not supported, but it's easy to add them later *)
type _ mnemonic =
  | Mov : [< `Reg_64_Imm_a ] mnemonic
  | Ret : [< `Nothing ] mnemonic
  | Push : [< `Reg_64 ] mnemonic
  | Add : [< `Reg_64_Reg_64 ] mnemonic
  | Xor : [< `Reg_64_Reg_64 ] mnemonic
  | Syscall : [< `Nothing ] mnemonic
  | Pop : [< `Reg_64 ] mnemonic
  | Jmp : [< `Label ] mnemonic
  | Sub : [< `Reg_64_Imm_a ] mnemonic
  | Cmp : [< `Reg_64_Imm_a ] mnemonic
  | Je : [< `Label ] mnemonic
[@@deriving variants]

type directive =
  | Section of string
  | Global of string
[@@deriving variants]

type statement =
  | Directive of directive
  | LabelDecl of string
  | Instruction : 'kind_of mnemonic * 'kind_of operand -> statement
[@@deriving variants]

type ast = statement list

let show_register = Variants_of_register.to_name
let show_mnemonic = Variants_of_mnemonic.to_name

let show_immediate = function
  | ImmInt x -> Printf.sprintf {|(ImmInt %d)|} x
;;

let show_register_ref = function
  | RegisterRef x -> Printf.sprintf {|(RegisterRef %s)|} (show_register x)
;;

let show_label_ref = function
  | LabelRef x -> Printf.sprintf {|(LabelRef %s)|} x
;;

let show_directive = function
  | Section x -> Printf.sprintf {|(Section %s)|} x
  | Global x -> Printf.sprintf {|(Global %s)|} x
;;

type show_operand_wrapper = W : 'kind_of operand -> show_operand_wrapper

let show_operand (W op) =
  match op with
  | Reg_32 x -> Printf.sprintf {|(Reg_32 %s)|} (show_register x)
  | Reg_64 x -> Printf.sprintf {|(Reg_64 %s)|} (show_register x)
  | Mem_32 x -> Printf.sprintf {|(Mem_32 %s)|} (show_register_ref x)
  | Mem_64 x -> Printf.sprintf {|(Mem_64 %s)|} (show_register_ref x)
  | Mem_32_Mem_32 (x, y) ->
    Printf.sprintf
      {|(Mem_32_Mem_32 (%s, %s))|}
      (show_register_ref x)
      (show_register_ref y)
  | Mem_64_Mem_64 (x, y) ->
    Printf.sprintf
      {|(Mem_64_Mem_64 (%s, %s))|}
      (show_register_ref x)
      (show_register_ref y)
  | Reg_32_Reg_32 (x, y) ->
    Printf.sprintf {|(Reg_32_Reg_32 (%s, %s))|} (show_register x) (show_register y)
  | Reg_64_Reg_64 (x, y) ->
    Printf.sprintf {|(Reg_64_Reg_64 (%s, %s))|} (show_register x) (show_register y)
  | Imm_a x -> Printf.sprintf {|(Imm_a %s)|} (show_immediate x)
  | Label x -> Printf.sprintf {|(Label %s)|} (show_label_ref x)
  | Nothing -> "(Nothing)"
  | Reg_32_Imm_a (x, y) ->
    Printf.sprintf {|(Reg_32_Imm_a (%s, %s))|} (show_register x) (show_immediate y)
  | Reg_64_Imm_a (x, y) ->
    Printf.sprintf {|(Reg_64_Imm_a (%s, %s))|} (show_register x) (show_immediate y)
;;

let show_statement = function
  | Directive x -> Printf.sprintf {|(Directive %s)|} (show_directive x)
  | LabelDecl x -> Printf.sprintf {|(LabelDecl %s)|} x
  | Instruction (mnemonic, args) ->
    Printf.sprintf
      {|(Instruction (%s %s))|}
      (show_mnemonic mnemonic)
      (show_operand (W args))
;;

let show_ast ast = String.concat "\n" (List.map show_statement ast)

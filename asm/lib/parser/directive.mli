(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
val parse_section : Ast.directive Angstrom.t

val parse_global : Ast.directive Angstrom.t
val parse_directive : Ast.directive Angstrom.t

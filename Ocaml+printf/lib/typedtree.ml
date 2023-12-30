(** Copyright 2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type typ =
  | TVar of int
  | TPrim of string
  | TArr of typ * typ
  | TUnit
  | TTuple of typ list
  | TList of typ
  | TFString of typ
[@@deriving eq, show { with_path = false }]

module TypeVarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = TypeVarSet.t [@@deriving eq, show { with_path = false }]
type scheme = Scheme of binder_set * typ [@@deriving eq, show { with_path = false }]

let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let char_typ = TPrim "char"
let string_typ = TPrim "string"
let arrow l r = TArr (l, r)
let ( @-> ) = arrow

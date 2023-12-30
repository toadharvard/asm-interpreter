val pp_typ : Format.formatter -> Typedtree.typ -> unit

type error =
  [ `Impossible_state of string
  | `Multiple_bound of string
  | `No_variable of string
  | `Occurs_check
  | `Pattern_matching of Typedtree.typ * Typedtree.typ
  | `Unification_failed of Typedtree.typ * Typedtree.typ
  ]

val pp_error : Format.formatter -> error -> unit

module TypeEnv : sig
  type t

  val pp_env : Format.formatter -> t -> unit
end

val run_infer_expr : Ast.expr -> (Typedtree.typ, error) result
val run_infer_program : Ast.let_decl list -> (TypeEnv.t, error) result

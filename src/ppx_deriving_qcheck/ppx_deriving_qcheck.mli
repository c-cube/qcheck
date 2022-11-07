open Ppxlib

val derive_gens :
  version:[`QCheck | `QCheck2] ->
  loc:location ->
  rec_flag * type_declaration list ->
  structure
(** [derive_gens ~version ~loc xs] creates generators for type declaration in [xs].

    The generators can either be [QCheck.Gen.t] or [QCheck2.Gen.t] based on
    [version]. *)

val derive_arbs :
  loc:location ->
  rec_flag * type_declaration list ->
  structure
(** [derive_arbs ~loc xs] creates generators for type declaration in [xs] and
    use these lasts to build [QCheck.arbitrary]. *)

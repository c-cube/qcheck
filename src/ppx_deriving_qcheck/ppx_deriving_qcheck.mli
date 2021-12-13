open Ppxlib

val derive_gen :
  version:[`QCheck | `QCheck2] ->
  loc:location ->
  rec_flag * type_declaration list ->
  structure
(** [derive_gen ~version ~loc xs] creates generators for type declaration in [xs].

    The generator can either be a [QCheck.Gen.t] or a [QCheck2.Gen.t] based on
    [version]. *)

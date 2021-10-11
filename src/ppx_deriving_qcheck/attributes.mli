open Ppxlib
(** This file handles every attributes to be found in a core_type definition *)

val gen : core_type -> expression option
(** [gen loc ct] look for an attribute "gen" in [ct]

    example:
    {[
    type t =
    | A of int
    | B of (int [@gen QCheck.int32])
    ]}

    It allows the user to specify which generator he wants for a specific type.
    Returns the generator as an expression and returns None if no attribute
    is present *)

val weight : attributes -> expression option
(** [weight loc ct] look for an attribute "weight" in [ct]

    example:
    {[
    type t =
    | A [@weight 5]
    | B [@weight 6]
    | C
    ]}
    It allows the user to specify the weight of a type case. *)

open Ppxlib

val derive_gen : loc:location -> rec_flag * type_declaration list -> structure
(** [derive_gen loc xs] derives a generator for each type_declaration in [xs] *)

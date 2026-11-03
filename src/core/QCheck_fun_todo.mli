(** [Fun.Todo] (OCaml 5.6) as a signal of incompleteness. The source is
    selected by compiler version in [dune]. *)

val reason : exn -> string option
(** [reason e] is the location of the [Fun.todo ()] call if [e] is
    [Fun.Todo], as [Fun]'s printer shows it, and [None] otherwise. *)

val shrinks : ('a -> string) -> ('a -> bool) -> 'a -> bool
(** a generic printer for laws *)

val shrinks_int : (int -> bool) -> int -> bool
(** a printer which prints ints for int laws *)

val shrinks_char : (char -> bool) -> char -> bool
(** a printer which prints chars for char laws *)

val shrinks_string : (string -> bool) -> string -> bool
(** a printer which prints strings for string laws *)

val shrinks_list_length : ('a list -> bool) -> 'a list -> bool
(** a printer which prints list length for list laws *)

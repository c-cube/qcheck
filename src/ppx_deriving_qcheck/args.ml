open Ppxlib

(** [curry_args args body] adds parameter to [body]

    e.g.:
    curry_args [gen_a; gen_b] () => fun gen_a -> fun gen_b -> ()
*)
let rec curry_args ~loc args body =
  match args with
  | [] -> body
  | x :: xs -> [%expr fun [%p x] -> [%e curry_args ~loc xs body]]

(** [apply_args args body] applies parameters to [body]

    e.g.:
    apply_args [gen_a; gen_b] f => f gen_a gen_b
*)
let apply_args ~loc args body =
  let rec aux acc = function
    | [] -> acc
    | [arg] -> [%expr [%e acc] [%e arg]]
    | arg :: args -> aux [%expr [%e acc] [%e arg]] args
  in
  aux body args

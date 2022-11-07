open Ppxlib
module G = QCheck_generators
module O = G.Observable

(** {1. Tuple } *)

(** This module implements nested tuples based on QCheck tuples generators (or observables):
    - [Gen.pair]
    - [Gen.triple]
    - [Gen.quad]

    It can be used to nest large tuples in a generator.
    - e.g.
    {[
    type t = int * int * int
    ]}

    Lets say QCheck does not have combinator to generate a triple. One has to write:

    {[
    let gen = QCheck.Gen.(map (fun ((x, y), z) -> (x, y, z) (pair (pair int int) int))
    ]}

    We copy this nesting mechanism with this module.
 *)

type 'a t =
  | Pair of 'a t * 'a t
  | Triple of 'a * 'a * 'a
  | Quad of 'a * 'a * 'a * 'a
  | Elem of 'a

(** [from_list l] builds an {!'a t}, if len of [l] is greater than 4, the list
    is split into a [Pair] of generators. *)
let rec from_list = function
  | [ a; b; c; d ] -> Quad (a, b, c, d)
  | [ a; b; c ] -> Triple (a, b, c)
  | [ a; b ] -> Pair (Elem a, Elem b)
  | [ a ] -> Elem a
  | l ->
     let n = List.length l / 2 in
     let i = ref 0 in
     let l1 =
       List.filter
         (fun _ ->
           let x = !i in
           i := x + 1;
           x < n)
            l
     in
     i := 0;
     let l2 =
       List.filter
         (fun _ ->
           let x = !i in
           i := x + 1;
           x >= n)
         l
     in
     Pair (from_list l1, from_list l2)

let rec to_list = function
  | Quad (a, b, c, d) -> [ a; b; c; d ]
  | Triple (a, b, c) -> [ a; b; c ]
  | Pair (a, b) -> to_list a @ to_list b
  | Elem a -> [ a ]

(** [to_expr ~loc t] creates a tuple expression based on [t].
    [t] is transformed to a list, and each element from the list becomes
    a variable referencing a generator.

    - e.g.
    to_expr (Pair (_, _)) => (gen0, gen1)
 *)
let to_expr ~loc t =
  let l = to_list t in
  let (module A) = Ast_builder.make loc in
  List.mapi
    (fun i _ ->
      let s = Printf.sprintf "gen%d" i in
      A.evar s)
    l
  |> A.pexp_tuple

(** [nest pair triple quad t] creates a generator expression for [t] using

    - [pair] to combine Pair (_, _)
    - [triple] to combine Triple (_, _, )
    - [quad] to combine Quad (_, _, _, _)
*)
let rec nest ~pair ~triple ~quad = function
  | Quad (a, b, c, d) -> quad a b c d
  | Triple (a, b, c) -> triple a b c
  | Pair (a, b) ->
     pair
       (nest ~pair ~triple ~quad a)
       (nest ~pair ~triple ~quad b)
  | Elem a -> a

(** [to_gen t] creates a Gen.t with generators' combinators *)
let to_gen ~loc ~version t =
  nest
    ~pair:(G.pair ~loc ~version)
    ~triple:(G.triple ~loc ~version)
    ~quad:(G.quad ~loc ~version) t

(** [to_obs t] creates a Obs.t with obsersvables' combinators *)
let to_obs ~loc ~version t =
  nest
    ~pair:(O.pair ~loc ~version)
    ~triple:(O.triple ~loc ~version)
    ~quad:(O.quad ~loc ~version) t

let to_pat ~loc t =
  let fresh_id =
    let id = ref 0 in
    fun () ->
    let x = !id in
    let () = id := x + 1 in
    Printf.sprintf "gen%d" x
  in
  let (module A) = Ast_builder.make loc in
  let rec aux = function
    | Quad (_, _, _, _) ->
       let a = A.pvar @@ fresh_id () in
       let b = A.pvar @@ fresh_id () in
       let c = A.pvar @@ fresh_id () in
       let d = A.pvar @@ fresh_id () in
       [%pat? [%p a], [%p b], [%p c], [%p d]]
    | Triple (_, _, _) ->
       let a = A.pvar @@ fresh_id () in
       let b = A.pvar @@ fresh_id () in
       let c = A.pvar @@ fresh_id () in
       [%pat? [%p a], [%p b], [%p c]]
    | Pair (a, b) ->
       let a = aux a in
       let b = aux b in
       [%pat? [%p a], [%p b]]
    | Elem _ -> A.pvar @@ fresh_id ()
  in
  aux t

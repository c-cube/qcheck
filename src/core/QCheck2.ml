(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot,
Jan Midtgaard, Julien Debon, Valentin Chaboche
all rights reserved.
*)

(** {1 Quickcheck inspired property-based testing} *)

let poly_compare=compare

module RS = struct
  (* Poor man's splitter for version < 5.0                       *)
  (* This definition is shadowed by the [include] on OCaml >=5.0 *)
  let split rs =
    let bits = Random.State.bits rs in
    let rs' = Random.State.make [|bits|] in
    rs'
  include Random.State
  (* This is how OCaml 5.0 splits:       *)
  (* Split a new PRNG off the given PRNG *)
  (*
  let split s =
    let i1 = bits64 s in let i2 = bits64 s in
    let i3 = bits64 s in let i4 = bits64 s in
    mk i1 i2 i3 i4
  *)
end

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let _opt_map_2 ~f a b = match a, b with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let _opt_map_3 ~f a b c = match a, b, c with
  | Some x, Some y, Some z -> Some (f x y z)
  | _ -> None

let _opt_map_4 ~f a b c d = match a, b, c, d with
  | Some x, Some y, Some z, Some w -> Some (f x y z w)
  | _ -> None

let _opt_sum a b = match a, b with
  | Some _, _ -> a
  | None, _ -> b

let sum_int = List.fold_left (+) 0

exception Failed_precondition
(* raised if precondition is false *)

exception No_example_found of string
(* raised if an example failed to be found *)

let assume b = if not b then raise Failed_precondition

let assume_fail () = raise Failed_precondition

let (==>) b1 b2 = if b1 then b2 else raise Failed_precondition

(** Enhancement of Stdlib [Seq] to backport some recent functions, and add a few useful others. *)
module Seq = struct

  include Seq

  (* The following functions are copied from https://github.com/ocaml/ocaml/blob/trunk/stdlib/seq.ml to support older OCaml versions. *)

  let rec unfold f u () =
    match f u with
    | None -> Nil
    | Some (x, u') -> Cons (x, unfold f u')

  let rec append seq1 seq2 () =
    match seq1() with
    | Nil -> seq2()
    | Cons (x, next) -> Cons (x, append next seq2)

  let cons x next () = Cons (x, next)

  (* End of copy of old functions. *)

  let is_empty (seq : _ t) : bool = match seq () with
    | Nil -> true
    | _ -> false

  (** Take at most [n] values. *)
  let rec take (n : int) (seq : _ t) : _ t = fun () -> match (n, seq ()) with
    | (0, _) | (_, Nil) -> Nil
    | (n, Cons (a, rest)) -> Cons (a, take (n - 1) rest)


  let hd (l : 'a t) : 'a option =
    match l () with
    | Nil -> None
    | Cons (hd, _) -> Some hd

  (** Useful to improve [Seq] code perf when chaining functions *)
  let apply (l : 'a t) : 'a node = l ()
end

module Shrink = struct

  module type Number = sig
    type t
    val equal : t -> t -> bool
    val div : t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val of_int : int -> t
  end

  let number_towards (type a) (module Number : Number with type t = a) ~(destination : a) (x : a) : a Seq.t = fun () ->
    Seq.unfold (fun current_shrink ->
        if Number.equal current_shrink x
        then None
        else (
          (* Halve the operands before subtracting them so they don't overflow.
             Consider [number_towards min_int max_int] *)
          let half_diff =  Number.sub (Number.div x (Number.of_int 2)) (Number.div current_shrink (Number.of_int 2)) in
          if half_diff = Number.of_int 0
          (* [current_shrink] is the last valid shrink candidate, put [x] as next step to make sure we stop *)
          then Some (current_shrink, x)
          else Some (current_shrink, Number.add current_shrink half_diff)
      )) destination ()

  let int_towards destination x = fun () ->
    let module Int : Number with type t = int = struct
      include Int
      let of_int = Fun.id
    end in
    number_towards (module Int) ~destination x ()

  let int32_towards destination x = fun () ->
    number_towards (module Int32) ~destination x ()

  let int64_towards destination x = fun () ->
    number_towards (module Int64) ~destination x ()

  (** Arbitrarily limit to 15 elements as dividing a [float] by 2 doesn't converge quickly
      towards the destination. *)
  let float_towards destination x = fun () ->
    number_towards (module Float) ~destination x |> Seq.take 15 |> Seq.apply

  let int_aggressive_towards (destination : int) (n : int) : int Seq.t = fun () ->
    Seq.unfold (fun current ->
        if current = n then None
        else if current < n then let next = succ current in Some (next, next)
        else let next = pred current in Some (next, next)
      ) destination ()

  let int_aggressive n = fun () -> int_aggressive_towards 0 n ()

end

module Tree = struct
  type 'a t = Tree of 'a * ('a t) Seq.t

  let root (Tree (root, _) : 'a t) : 'a = root

  let children (Tree (_, children) : 'a t) : ('a t) Seq.t = children

  let rec pp ?(depth : int option) (inner_pp : Format.formatter -> 'a -> unit) (ppf : Format.formatter) (t : 'a t) : unit =
    let Tree (x, xs) = t in
    let wrapper_box ppf inner =
      Format.fprintf ppf "@[<hv2>Tree(@,%a@]@,)" inner ()
    in
    let inner ppf () =
      Format.fprintf ppf "@[<hv2>Node(@,%a@]@,),@ @[<hv>Shrinks(" inner_pp x;
      if Option.fold depth ~none:false ~some:(fun depth -> depth <= 0) then (
        Format.fprintf ppf "<max depth reached>@])")
      else if Seq.is_empty xs then Format.fprintf ppf "@])"
      else (
        Format.fprintf ppf "@,%a@]@,)"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
             (pp ?depth:(Option.map pred depth) inner_pp))
          (List.of_seq xs);
      )
    in
    wrapper_box ppf inner

  let rec map (f : 'a -> 'b) (a : 'a t) : 'b t =
    let Tree (x, xs) = a in
    let y = f x in
    let ys = fun () -> Seq.map (fun smaller_x -> map f smaller_x) xs () in
    Tree (y, ys)

  (** Note that parameter order is reversed. *)
  let (>|=) a f = map f a

  let rec ap (f : ('a -> 'b) t) (a : 'a t) : 'b t =
    let Tree (x0, xs) = a in
    let Tree (f0, fs) = f in
    let y = f0 x0 in
    let ys = fun () -> Seq.append (Seq.map (fun f' -> ap f' a) fs) (Seq.map (fun x' -> ap f x') xs) () in
    Tree (y, ys)

  let (<*>) = ap

  let liftA2 (f : 'a -> 'b -> 'c) (a : 'a t) (b : 'b t) : 'c t =
    (a >|= f) <*> b

  let rec bind (a : 'a t) (f : 'a -> 'b t) : 'b t =
    let Tree (x, xs) = a in
    let Tree (y, ys_of_x) = f x in
    let ys_of_xs = fun () -> Seq.map (fun smaller_x -> bind smaller_x f) xs () in
    let ys = fun () -> Seq.append ys_of_xs ys_of_x () in
    Tree (y, ys)

  let (>>=) = bind

  let pure x = Tree (x, Seq.empty)

  let rec make_primitive (shrink : 'a -> 'a Seq.t) (x : 'a) : 'a t =
    let shrink_trees = fun () -> shrink x |> Seq.map (make_primitive shrink) |> Seq.apply in
    Tree (x, shrink_trees)

  let rec opt (a : 'a t) : 'a option t =
    let Tree (x, xs) = a in
    let shrinks = fun () -> Seq.cons (pure None) (Seq.map opt xs) () in
    Tree (Some x, shrinks)

  let rec sequence_list (l : 'a t list) : 'a list t = match l with
    | [] -> pure []
    | hd :: tl -> liftA2 List.cons hd (sequence_list tl)

  let rec add_shrink_invariant (p : 'a -> bool) (a : 'a t) : 'a t =
    let Tree (x, xs) = a in
    let xs' = fun () -> Seq.filter_map (fun (Tree (x', _) as t) -> if p x' then Some (add_shrink_invariant p t) else None) xs () in
    Tree (x, xs')

  (** [applicative_take n trees] returns a tree of lists with at most the [n] first elements of the input list. *)
  let rec applicative_take (n : int) (l : 'a t list) : 'a list t = match (n, l) with
    | (0, _) | (_, []) -> pure []
    | (n, (tree :: trees)) -> liftA2 List.cons tree (applicative_take (pred n) trees)
end

module Gen = struct

  type 'a t = RS.t -> 'a Tree.t

  type 'a sized = int -> RS.t -> 'a Tree.t

  let map f x = fun st -> Tree.map f (x st)

  (** Note that parameter order is reversed. *)
  let (>|=) x f = map f x

  let (<$>) = map

  let pure (a : 'a) : 'a t = fun _ -> Tree.pure a

  let ap (f : ('a -> 'b) t) (x : 'a t) : 'b t = fun st -> Tree.ap (f st)  (x st)

  let (<*>) = ap

  let liftA2 (f : 'a -> 'b -> 'c) (a : 'a t) (b : 'b t) : 'c t =
    (a >|= f) <*> b

  let liftA3 (f : 'a -> 'b -> 'c -> 'd) (a : 'a t) (b : 'b t) (c : 'c t) : 'd t =
    (a >|= f) <*> b <*> c

  let map2 = liftA2

  let map3 = liftA3

  let return = pure

  let bind (gen : 'a t) (f : 'a -> ('b t)) : 'b t = fun st -> Tree.bind (gen st) (fun a -> f a st)

  let (>>=) = bind

  let sequence_list (l : 'a t list) : 'a list t = fun st -> List.map (fun gen -> gen st) l |> Tree.sequence_list

  let make_primitive ~(gen : RS.t -> 'a) ~(shrink : 'a -> 'a Seq.t) : 'a t = fun st ->
    Tree.make_primitive shrink (gen st)

  let parse_origin (loc : string) (pp : Format.formatter -> 'a -> unit) ~(origin : 'a) ~(low : 'a) ~(high : 'a) : 'a =
    if origin < low then invalid_arg Format.(asprintf "%s: origin value %a is lower than low value %a" loc pp origin pp low)
    else if origin > high then invalid_arg Format.(asprintf "%s: origin value %a is greater than high value %a" loc pp origin pp high)
    else origin

  let small_nat : int t = fun st ->
    let p = RS.float st 1. in
    let x = if p < 0.75 then RS.int st 10 else RS.int st 100 in
    let shrink a = fun () -> Shrink.int_towards 0 a () in
    Tree.make_primitive shrink x

  (** Natural number generator *)
  let nat : int t = fun st ->
    let p = RS.float st 1. in
    let x =
      if p < 0.5 then RS.int st 10
      else if p < 0.75 then RS.int st 100
      else if p < 0.95 then RS.int st 1_000
      else RS.int st 10_000
    in
    let shrink a = fun () -> Shrink.int_towards 0 a () in
    Tree.make_primitive shrink x

  let big_nat : int t = fun st ->
    let p = RS.float st 1. in
    if p < 0.75
    then nat st
    else
      let shrink a = fun () -> Shrink.int_towards 0 a () in
      Tree.make_primitive shrink (RS.int st 1_000_000)

  let unit : unit t = fun _st -> Tree.pure ()

  let bool : bool t = fun st ->
    let false_gen = Tree.pure false in
    if RS.bool st
    then Tree.Tree (true, Seq.return false_gen)
    else false_gen

  let float : float t = fun st ->
    let x = exp (RS.float st 15. *. (if RS.bool st then 1. else -1.))
            *. (if RS.bool st then 1. else -1.)
    in
    let shrink a = fun () -> Shrink.float_towards 0. a () in
    Tree.make_primitive shrink x

  let pfloat : float t = float >|= abs_float

  let nfloat : float t = pfloat >|= Float.neg

  let float_bound_inclusive ?(origin : float = 0.) (bound : float) : float t = fun st ->
    let (low, high) = Float.min_max_num 0. bound in
    let shrink a = fun () ->
      let origin = parse_origin "Gen.float_bound_inclusive" Format.pp_print_float ~origin ~low ~high in
      Shrink.float_towards origin a ()
    in
    let x = RS.float st bound in
    Tree.make_primitive shrink x

  let float_bound_exclusive ?(origin : float = 0.) (bound : float) : float t =
    if bound = 0. then invalid_arg "Gen.float_bound_exclusive";
    fun st ->
      let (low, high) = Float.min_max_num 0. bound in
      let shrink a = fun () ->
        let origin = parse_origin "Gen.float_bound_exclusive" Format.pp_print_float ~origin ~low ~high in
        Shrink.float_towards origin a ()
      in
      let bound =
        if bound > 0.
        then bound -. epsilon_float
        else bound +. epsilon_float
      in
      let x = RS.float st bound in
      Tree.make_primitive shrink x

  let pick_origin_within_range ~low ~high ~goal =
    if low > goal then low
    else if high < goal then high
    else goal

  let float_range ?(origin : float option) (low : float) (high : float) : float t =
    if high < low then invalid_arg "Gen.float_range: high < low"
    else if high -. low > max_float then invalid_arg "Gen.float_range: high -. low > max_float";
    let origin = parse_origin "Gen.float_range" Format.pp_print_float
                   ~origin:(Option.value ~default:(pick_origin_within_range ~low ~high ~goal:0.) origin)
                   ~low
                   ~high in
    (float_bound_inclusive ~origin (high -. low))
    >|= (fun x -> low +. x)

  let (--.) low high = float_range ?origin:None low high

  let neg_int : int t = nat >|= Int.neg

  (** [option gen] shrinks towards [None] then towards shrinks of [gen]. *)
  let option ?(ratio : float = 0.85) (gen : 'a t) : 'a option t = fun st ->
    let p = RS.float st 1. in
    if p < (1. -. ratio)
    then Tree.pure None
    else Tree.opt (gen st)

  (** [opt] is an alias of {!val:option} for backward compatibility. *)
  let opt = option

  (* Uniform positive random int generator.

     We can't use {!RS.int} because the upper bound must be positive and is excluded,
     so {!Int.max_int} would never be reached. We have to manipulate bits directly.

     Note that the leftmost bit is used for negative numbers, so it must be [0].

     {!RS.bits} only generates 30 bits, which is exactly enough on
     32-bits architectures (i.e. {!Sys.int_size} = 31, i.e. 30 bits for positive numbers)
     but not on 64-bits ones.

     That's why for 64-bits, 3 30-bits segments are generated and shifted to craft a
     62-bits number (i.e. {!Sys.int_size} = 63). The leftmost segment is masked to keep
     only the last 2 bits.

     The current implementation hard-codes 30/32/62/64 values, but technically we should
     rely on {!Sys.int_size} to find the number of bits.

     Note that we could also further generalize this function to merge it with [random_binary_string].
     Technically this function is a special case of [random_binary_string] where the size is
     {!Sys.int_size}.
  *)
  let pint_raw : RS.t -> int =
    if Sys.word_size = 32
    then fun st -> RS.bits st
    else (* word size = 64 *)
      fun st ->
      (* Technically we could write [3] but this is clearer *)
      let two_bits_mask = 0b11 in
      (* Top 2 bits *)
      let left = ((RS.bits st land two_bits_mask) lsl 60) in
      (* Middle 30 bits *)
      let middle = (RS.bits st lsl 30) in
      (* Bottom 30 bits *)
      let right = RS.bits st in
      left lor middle lor right

  let pint ?(origin : int = 0) : int t = fun st ->
    let x = pint_raw st in
    let shrink a = fun () ->
      let origin = parse_origin "Gen.pint" Format.pp_print_int ~origin ~low:0 ~high:max_int in
      Shrink.int_towards origin a ()
    in
    Tree.make_primitive shrink x

  let number_towards = Shrink.number_towards

  let int_towards = Shrink.int_towards

  let int64_towards = Shrink.int64_towards

  let int32_towards = Shrink.int32_towards

  let float_towards = Shrink.float_towards

  let int : int t =
    bool >>= fun b ->
    if b
    then pint ~origin:0 >|= (fun n -> - n - 1)
    else pint ~origin:0

  let int_bound (n : int) : int t =
    if n < 0 then invalid_arg "Gen.int_bound";
    fun st ->
      if n <= (1 lsl 30) - 2
      then Tree.make_primitive (fun a () -> Shrink.int_towards 0 a ()) (RS.int st (n + 1))
      else Tree.map (fun r -> r mod (n + 1)) (pint st)

  (** To support ranges wider than [Int.max_int], the general idea is to find the center,
      and generate a random half-difference number as well as whether we add or
      subtract that number from the center. *)
  let int_range ?(origin : int option) (low : int) (high : int) : int t =
    if high < low then invalid_arg "Gen.int_range: high < low";
    fun st ->
      let Tree.Tree(n, _shrinks) = if low >= 0 || high < 0 then (
          (* range smaller than max_int *)
          Tree.map (fun n -> low + n) (int_bound (high - low) st)
        ) else (
          (* range potentially bigger than max_int: we split on 0 and
             choose the interval with regard to their size ratio *)
          let f_low = float_of_int low in
          let f_high = float_of_int high in
          let ratio = (-.f_low) /. (1. +. f_high -. f_low) in
          if RS.float st 1. <= ratio
          then Tree.map (fun n -> -n - 1) (int_bound (- (low + 1)) st)
          else int_bound high st
        ) in
      let shrink a = fun () ->
        let origin = match origin with
          | None -> pick_origin_within_range ~low ~high ~goal:0
          | Some origin ->
         if origin < low
         then invalid_arg "Gen.int_range: origin < low"
         else if origin > high then invalid_arg "Gen.int_range: origin > high"
         else origin
        in
        Shrink.int_towards origin a ()
      in
      Tree.make_primitive shrink n

  let (--) low high = int_range ?origin:None low high

  let oneof (l : 'a t list) : 'a t =
    int_range 0 (List.length l - 1) >>= List.nth l

  let oneofl (l : 'a list) : 'a t =
    int_range 0 (List.length l - 1) >|= List.nth l

  let oneofa (a : 'a array) : 'a t =
    int_range 0 (Array.length a - 1) >|= Array.get a

  (* NOTE: we keep this alias to not break code that uses [small_int]
     for sizes of strings, arrays, etc. *)
  let small_int = small_nat

  let small_signed_int : int t = fun st ->
    if RS.bool st
    then small_nat st
    else (small_nat >|= Int.neg) st

  (** Shrink towards the first element of the list *)
  let frequency (l : (int * 'a t) list) : 'a t =
    if l = [] then failwith "QCheck2.frequency called with an empty list";
    let sums = sum_int (List.map fst l) in
    if sums < 1 then failwith "QCheck2.frequency called with weight sum < 1";
    int_bound (sums - 1)
    >>= fun i ->
    let rec aux acc = function
      | ((x, g) :: xs) -> if i < acc + x then g else aux (acc + x) xs
      | _ -> assert false
    in
    aux 0 l

  let frequencyl (l : (int * 'a) list) : 'a t =
    List.map (fun (weight, value) -> (weight, pure value)) l
    |> frequency

  let frequencya a = frequencyl (Array.to_list a)

  let char_range ?(origin : char option) (a : char) (b : char) : char t =
    (int_range ~origin:(Char.code (Option.value ~default:a origin)) (Char.code a) (Char.code b)) >|= Char.chr

  let random_binary_string (length : int) (st : RS.t) : string =
    (* 0b011101... *)
    let s = Bytes.create (length + 2) in
    Bytes.set s 0 '0';
    Bytes.set s 1 'b';
    for i = 0 to length - 1 do
      Bytes.set s (i+2) (if RS.bool st then '0' else '1')
    done;
    Bytes.unsafe_to_string s

  let int32 : int32 t = fun st ->
    let x = random_binary_string 32 st |> Int32.of_string in
    let shrink a = fun () -> Shrink.int32_towards 0l a () in
    Tree.make_primitive shrink x

  let ui32 : int32 t = map Int32.abs int32

  let int64 : int64 t = fun st ->
    let x = random_binary_string 64 st |> Int64.of_string in
    let shrink a = fun () -> Shrink.int64_towards 0L a () in
    Tree.make_primitive shrink x

  let ui64 : int64 t = map Int64.abs int64

  (* A tail-recursive implementation over Tree.t *)
  let list_size (size : int t) (gen : 'a t) : 'a list t =
    fun st ->
    Tree.bind (size st) @@ fun size ->
    let rec loop n acc =
      if n <= 0
      then acc
      else (loop [@tailcall]) (n - 1) (Tree.liftA2 List.cons (gen st) acc)
    in
    loop size (Tree.pure [])

  let list (gen : 'a t) : 'a list t = list_size nat gen

  let list_repeat (n : int) (gen : 'a t) : 'a list t = list_size (pure n) gen

  let array_size (size : int t) (gen : 'a t) : 'a array t =
    (list_size size gen) >|= Array.of_list

  let array (gen : 'a t) : 'a array t = list gen >|= Array.of_list

  let array_repeat (n : int) (gen : 'a t) : 'a array t = list_repeat n gen >|= Array.of_list

  let rec flatten_l (l : 'a t list) : 'a list t =
    match l with
    | [] -> pure []
    | gen :: gens -> liftA2 List.cons gen (flatten_l gens)

  let flatten_a (a : 'a t array) : 'a array t =
    Array.to_list a |> flatten_l >|= Array.of_list

  let flatten_opt (o : 'a t option) : 'a option t =
    match o with
    | None -> pure None
    | Some gen -> option gen

  let flatten_res (res : ('a t, 'e) result) : ('a, 'e) result t =
    match res with
    | Ok gen -> gen >|= Result.ok
    | Error e -> pure (Error e)

  let shuffle_a (a : 'a array) : 'a array t = fun st ->
    let a = Array.copy a in
    for i = Array.length a - 1 downto 1 do
      let j = RS.int st (i + 1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
    done;
    Tree.pure a

  let shuffle_l (l : 'a list) : 'a list t =
    Array.of_list l |> shuffle_a >|= Array.to_list

  let shuffle_w_l (l : ((int * 'a) list)) : 'a list t = fun st ->
    let sample (w, v) =
      let Tree.Tree (p, _) = float_bound_inclusive 1. st in
      let fl_w = float_of_int w in
      (p ** (1. /. fl_w), v)
    in
    let samples = List.rev_map sample l in
    samples
    |> List.sort (fun (w1, _) (w2, _) -> poly_compare w1 w2)
    |> List.rev_map snd
    |> Tree.pure

  let pair (g1 : 'a t) (g2 : 'b t) : ('a * 'b) t = liftA2 (fun a b -> (a, b)) g1 g2

  let triple (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) : ('a * 'b * 'c) t = (fun a b c -> (a, b, c)) <$> g1 <*> g2 <*> g3

  let quad (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) : ('a * 'b * 'c * 'd) t =
    (fun a b c d -> (a, b, c, d)) <$> g1 <*> g2 <*> g3 <*> g4

  let tup2 = pair

  let tup3 = triple

  let tup4 = quad

  let tup5 (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) (g5 : 'e t) : ('a * 'b * 'c * 'd * 'e) t =
    (fun a b c d e -> (a, b, c, d, e)) <$> g1 <*> g2 <*> g3 <*> g4 <*> g5

  let tup6 (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) (g5 : 'e t) (g6 : 'f t) : ('a * 'b * 'c * 'd * 'e * 'f) t =
    (fun a b c d e f -> (a, b, c, d, e, f)) <$> g1 <*> g2 <*> g3 <*> g4 <*> g5 <*> g6

  let tup7 (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) (g5 : 'e t) (g6 : 'f t) (g7 : 'g t) : ('a * 'b * 'c * 'd * 'e * 'f * 'g) t =
    (fun a b c d e f g -> (a, b, c, d, e, f, g)) <$> g1 <*> g2 <*> g3 <*> g4 <*> g5 <*> g6 <*> g7

  let tup8 (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) (g5 : 'e t) (g6 : 'f t) (g7 : 'g t) (g8 : 'h t) : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t =
    (fun a b c d e f g h -> (a, b, c, d, e, f, g, h)) <$> g1 <*> g2 <*> g3 <*> g4 <*> g5 <*> g6 <*> g7 <*> g8

  let tup9 (g1 : 'a t) (g2 : 'b t) (g3 : 'c t) (g4 : 'd t) (g5 : 'e t) (g6 : 'f t) (g7 : 'g t) (g8 : 'h t) (g9 : 'i t) : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t =
    (fun a b c d e f g h i -> (a, b, c, d, e, f, g, h, i)) <$> g1 <*> g2 <*> g3 <*> g4 <*> g5 <*> g6 <*> g7 <*> g8 <*> g9

  (** Don't reuse {!int_range} which is much less performant (many more checks because of the possible range and origins). As a [string] generator may call this hundreds or even thousands of times for a single value, it's worth optimizing. *)
  let char : char t = fun st ->
    let c = RS.int st 256 in
    let shrink a = fun () -> Shrink.int_towards (int_of_char 'a') a |> Seq.apply in
    Tree.map char_of_int (Tree.make_primitive shrink c)

  (** The first characters are the usual lower case alphabetical letters to help shrinking. *)
  let printable_chars : char list =
    (* Left and right inclusive *)
    let range min max = List.init (max - min + 1) (fun i -> char_of_int (i + min)) in
    let a = 97 in
    let z = 122 in
    let lower_alphabet = range a z in
    (* ' ' *)
    let first_printable_char = 32 in
    let before_lower_alphabet = range first_printable_char (a - 1) in
    (* '~' *)
    let last_printable_char = 126 in
    let after_lower_alphabet = range (z + 1) last_printable_char in
    let newline = ['\n'] in
    (* Put alphabet first for shrinking *)
    List.flatten [lower_alphabet; before_lower_alphabet; after_lower_alphabet; newline]

  let printable : char t =
    int_range ~origin:0 0 (List.length printable_chars - 1)
    >|= List.nth printable_chars

  let numeral : char t =
    let zero = 48 in
    let nine = 57 in
    int_range ~origin:zero zero nine >|= char_of_int

  let bytes_size ?(gen = char) (size : int t) : bytes t = fun st ->
    let open Tree in
    size st >>= fun size ->
    (* Adding char shrinks to a mutable list is expensive: ~20-30% cost increase *)
    (* Adding char shrinks to a mutable lazy list is less expensive: ~15% cost increase *)
    let char_trees_rev = ref [] in
    let bytes = Bytes.init size (fun _ ->
                    let char_tree = gen st in
                    char_trees_rev := char_tree :: !char_trees_rev ;
                    (* Performance: return the root right now, the heavy processing of shrinks can wait until/if there is a need to shrink *)
                    root char_tree) in
    let shrink = fun () ->
      let char_trees = List.rev !char_trees_rev in
      let char_list_tree = sequence_list char_trees in
      let bytes_tree = char_list_tree >|= (fun char_list ->
          let bytes = Bytes.create size in
          List.iteri (Bytes.set bytes) char_list ;
          bytes) in
      (* Technically [bytes_tree] is the whole tree, but for perf reasons we eagerly created the root above *)
      children bytes_tree ()
    in
    Tree (bytes, shrink)

  let string_size ?(gen = char) (size : int t) : string t =
    bytes_size ~gen size >|= Bytes.unsafe_to_string

  let bytes : bytes t = bytes_size nat

  let bytes_of gen = bytes_size ~gen nat

  let bytes_printable = bytes_size ~gen:printable nat

  let bytes_small st = bytes_size small_nat st

  let bytes_small_of gen st = bytes_size ~gen small_nat st

  let string : string t = string_size nat

  let string_of gen = string_size ~gen nat

  let string_printable = string_size ~gen:printable nat

  let string_small st = string_size small_nat st

  let string_small_of gen st = string_size ~gen small_nat st

  let small_string ?(gen=char) = string_small_of gen

  let small_list gen = list_size small_nat gen

  let small_array gen = array_size small_nat gen

  let join (gen : 'a t t) : 'a t = gen >>= Fun.id

  (* corner cases *)

  let graft_corners (gen : 'a t) (corners : 'a list) () : 'a t =
    let cors = ref corners in fun st ->
      match !cors with [] -> gen st
                     | e::l -> cors := l; Tree.pure e

  let int_pos_corners = [0; 1; 2; max_int]

  let int_corners = int_pos_corners @ [min_int]

  let small_int_corners () : int t = graft_corners nat int_pos_corners ()

  (* sized, fix *)

  let sized_size (size : int t) (gen : 'a sized) : 'a t =
    size >>= gen

  let sized (gen : 'a sized) : 'a t = sized_size nat gen

  let fix f =
    let rec f' n st = f f' n st in
    f'

  let generate ?(rand=RS.make_self_init()) ~(n : int) (gen : 'a t) : 'a list =
    list_repeat n gen rand |> Tree.root

  let generate1 ?(rand=RS.make_self_init()) (gen : 'a t) : 'a =
    gen rand |> Tree.root

  let generate_tree ?(rand=RS.make_self_init()) (gen : 'a t) : 'a Tree.t =
    gen rand

  let delay (f : unit -> 'a t) : 'a t = fun st -> f () st

  let add_shrink_invariant (p : 'a -> bool) (gen : 'a t) : 'a t =
    fun st -> gen st |> Tree.add_shrink_invariant p

  let set_shrink shrink gen =
    make_primitive
      ~gen:(fun st -> gen st |> Tree.root)
      ~shrink
  
  let no_shrink (gen: 'a t) : 'a t = set_shrink (fun _ -> Seq.empty) gen

  let (let+) = (>|=)

  let (and+) = pair

  let (let*) = (>>=)

  let (and*) = pair
end

module Print = struct
  type 'a t = 'a -> string

  let unit _ = "()"

  let int = string_of_int

  let bool = string_of_bool

  let float = string_of_float

  let bytes = Bytes.to_string

  let string s = Printf.sprintf "%S" s

  let char c = Printf.sprintf "%C" c

  let option f = function
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"

  let pair a b (x,y) = Printf.sprintf "(%s, %s)" (a x) (b y)

  let triple a b c (x,y,z) = Printf.sprintf "(%s, %s, %s)" (a x) (b y) (c z)

  let quad a b c d (x,y,z,w) =
    Printf.sprintf "(%s, %s, %s, %s)" (a x) (b y) (c z) (d w)

  let list pp l =
    let b = Buffer.create 25 in
    Buffer.add_char b '[';
    List.iteri (fun i x ->
        if i > 0 then Buffer.add_string b "; ";
        Buffer.add_string b (pp x))
      l;
    Buffer.add_char b ']';
    Buffer.contents b

  let array pp a =
    let b = Buffer.create 25 in
    Buffer.add_string b "[|";
    Array.iteri (fun i x ->
        if i > 0 then Buffer.add_string b "; ";
        Buffer.add_string b (pp x))
      a;
    Buffer.add_string b "|]";
    Buffer.contents b

  let contramap f p x = p (f x)

  let comap = contramap

  let default = fun _ -> "<no printer>"

  let tup2 p_a p_b (a, b) =
    Printf.sprintf "(%s, %s)" (p_a a) (p_b b)

  let tup2_opt p_a p_b (a, b) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    tup2 p_a p_b (a, b)

  let tup3 p_a p_b (p_c) (a, b, c) =
    Printf.sprintf "(%s, %s, %s)" (p_a a) (p_b b) (p_c c)

  let tup3_opt p_a p_b p_c (a, b, c) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    tup3 p_a p_b p_c (a, b, c)

  let tup4 p_a p_b p_c p_d (a, b, c, d) =
    Printf.sprintf "(%s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)

  let tup4_opt p_a p_b p_c p_d (a, b, c, d) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    tup4 p_a p_b p_c p_d (a, b, c, d)

  let tup5 p_a p_b p_c p_d p_e (a, b, c, d, e) =
    Printf.sprintf "(%s, %s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)
      (p_e e)

  let tup5_opt p_a p_b p_c p_d p_e (a, b, c, d, e) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    let p_e = Option.value ~default p_e in
    tup5 p_a p_b p_c p_d p_e (a, b, c, d, e)

  let tup6 p_a p_b p_c p_d p_e p_f (a, b, c, d, e, f) =
    Printf.sprintf "(%s, %s, %s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)
      (p_e e) (p_f f)

  let tup6_opt p_a p_b p_c p_d p_e p_f (a, b, c, d, e, f) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    let p_e = Option.value ~default p_e in
    let p_f = Option.value ~default p_f in
    tup6 p_a p_b p_c p_d p_e p_f (a, b, c, d, e, f)

  let tup7 p_a p_b p_c p_d p_e p_f p_g (a, b, c, d, e, f, g) =
    Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)
      (p_e e) (p_f f)
      (p_g g)

  let tup7_opt p_a p_b p_c p_d p_e p_f p_g (a, b, c, d, e, f, g) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    let p_e = Option.value ~default p_e in
    let p_f = Option.value ~default p_f in
    let p_g = Option.value ~default p_g in
    tup7 p_a p_b p_c p_d p_e p_f p_g (a, b, c, d, e, f, g)

  let tup8 p_a p_b p_c p_d p_e p_f p_g p_h (a, b, c, d, e, f, g, h) =
    Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)
      (p_e e) (p_f f)
      (p_g g) (p_h h)

  let tup8_opt p_a p_b p_c p_d p_e p_f p_g p_h (a, b, c, d, e, f, g, h) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    let p_e = Option.value ~default p_e in
    let p_f = Option.value ~default p_f in
    let p_g = Option.value ~default p_g in
    let p_h = Option.value ~default p_h in
    tup8 p_a p_b p_c p_d p_e p_f p_g p_h (a, b, c, d, e, f, g, h)

  let tup9 p_a p_b p_c p_d p_e p_f p_g p_h p_i (a, b, c, d, e, f, g, h, i) =
    Printf.sprintf "(%s, %s, %s, %s, %s, %s, %s, %s, %s)"
      (p_a a) (p_b b)
      (p_c c) (p_d d)
      (p_e e) (p_f f)
      (p_g g) (p_h h)
      (p_i i)

  let tup9_opt p_a p_b p_c p_d p_e p_f p_g p_h p_i (a, b, c, d, e, f, g, h, i) =
    let p_a = Option.value ~default p_a in
    let p_b = Option.value ~default p_b in
    let p_c = Option.value ~default p_c in
    let p_d = Option.value ~default p_d in
    let p_e = Option.value ~default p_e in
    let p_f = Option.value ~default p_f in
    let p_g = Option.value ~default p_g in
    let p_h = Option.value ~default p_h in
    let p_i = Option.value ~default p_i in
    tup9 p_a p_b p_c p_d p_e p_f p_g p_h p_i (a, b, c, d, e, f, g, h, i)
end

(** {2 Observe Values} *)

module Observable = struct
  (** An observable is a (random) predicate on ['a] *)
  type -'a t = {
    print: 'a Print.t;
    eq: ('a -> 'a -> bool);
    hash: ('a -> int);
  }

  let hash o x = o.hash x

  let equal o x y = o.eq x y

  let print o x = o.print x

  let make ?(eq=(=)) ?(hash=Hashtbl.hash) print =
    {print; eq; hash; }

  module H = struct
    let combine a b = Hashtbl.seeded_hash a b

    let combine_f f s x = Hashtbl.seeded_hash s (f x)

    let int i = i land max_int

    let bool b = if b then 1 else 2

    let char x = Char.code x

    let bytes (x:bytes) = Hashtbl.hash x

    let string (x:string) = Hashtbl.hash x

    let option f = function
      | None -> 42
      | Some x -> combine 43 (f x)
    let list f l = List.fold_left (combine_f f) 0x42 l

    let array f l = Array.fold_left (combine_f f) 0x42 l

    let pair f g (x,y) = combine (f x) (g y)
  end

  module Eq = struct
    type 'a t = 'a -> 'a -> bool

    let int : int t = (=)

    let bytes : bytes t = (=)

    let string : string t = (=)

    let bool : bool t = (=)

    let float = Float.equal

    let unit () () = true

    let char : char t = (=)

    let rec list f l1 l2 = match l1, l2 with
      | [], [] -> true
      | [], _ | _, [] -> false
      | x1::l1', x2::l2' -> f x1 x2 && list f l1' l2'

    let array eq a b =
      let rec aux i =
        if i = Array.length a then true
        else eq a.(i) b.(i) && aux (i+1)
      in
      Array.length a = Array.length b
      &&
      aux 0

    let option f o1 o2 = match o1, o2 with
      | None, None -> true
      | Some _, None
      | None, Some _ -> false
      | Some x, Some y -> f x y

    let pair f g (x1,y1)(x2,y2) = f x1 x2 && g y1 y2
  end

  let unit : unit t = make ~hash:(fun _ -> 1) ~eq:Eq.unit Print.unit

  let bool : bool t = make ~hash:H.bool ~eq:Eq.bool Print.bool

  let int : int t = make ~hash:H.int ~eq:Eq.int Print.int

  let float : float t = make ~eq:Eq.float Print.float

  let bytes = make ~hash:H.bytes ~eq:Eq.bytes Print.bytes

  let string = make ~hash:H.string ~eq:Eq.string Print.string

  let char = make ~hash:H.char ~eq:Eq.char Print.char

  let option p =
    make ~hash:(H.option p.hash) ~eq:(Eq.option p.eq)
      (Print.option p.print)

  let array p =
    make ~hash:(H.array p.hash) ~eq:(Eq.array p.eq) (Print.array p.print)

  let list p =
    make ~hash:(H.list p.hash) ~eq:(Eq.list p.eq) (Print.list p.print)

  let contramap f p =
    make ~hash:(fun x -> p.hash (f x)) ~eq:(fun x y -> p.eq (f x)(f y))
      (fun x -> p.print (f x))

  let map = contramap

  let pair a b =
    make ~hash:(H.pair a.hash b.hash) ~eq:(Eq.pair a.eq b.eq) (Print.pair a.print b.print)

  let triple a b c =
    contramap (fun (x,y,z) -> x,(y,z)) (pair a (pair b c))

  let quad a b c d =
    contramap (fun (x,y,z,u) -> x,(y,z,u)) (pair a (triple b c d))
end

type 'a stat = string * ('a -> int)
(** A statistic on a distribution of values of type ['a] *)

(** Internal module taking care of storing generated function bindings.

    In essence, a generated function of type ['a -> 'b] is a map (table) where
    keys are input values of type ['a] and values are output values of
    type ['b], plus a default value of type ['b].

    This module provides the "map of input/output" part.
 *)
module Poly_tbl : sig
  type ('key, 'value) t

  val create: 'key Observable.t -> ?v_print:'value Print.t -> 'value Gen.t -> int -> ('key, 'value) t Gen.t

  val get : ('key, 'value) t -> 'key -> 'value option

  val size : ('value -> int) -> ('key, 'value) t -> int

  val print : ('key, 'value) t Print.t
end = struct
  type ('key, 'value) t = {
    get : 'key -> 'value option; (** Don't be fooled by its name and signature: this function mutates the table during test execution by adding entries (key is the value on which the function is applied in the test, and the value is generated on the fly). *)
    p_size: ('value -> int) -> int;
    p_print: unit -> string;
    p_tree_bindings_rev : ('key * 'value Tree.t) list ref;
  }

  let create (type k) (type v) (k_obs : k Observable.t) ?(v_print: v Print.t option) (v_gen : v Gen.t) (size : int) : (k, v) t Gen.t =
    fun st ->
    let module T = Hashtbl.Make(struct
                       type t = k
                       let equal = k_obs.Observable.eq
                       let hash = k_obs.Observable.hash
                     end) in
    (* split random state to avoid later failed [get]s to side-effect the current [st] *)
    let st' = RS.split st in
    (* make a table
       @param extend if [true], extend table [tbl] on the fly (during test execution, to "record" input values and generate an associated output value). [false] during shrinking (use the default value if the input value is not in the table). *)
    let make ~extend tbl =
      let initial_tree_bindings_rev = T.to_seq tbl |> List.of_seq |> List.rev_map (fun (k, v) -> k, Tree.pure v) in
      let p_tree_bindings_rev = ref initial_tree_bindings_rev in
      let get = (fun key ->
          try Some (T.find tbl key)
          with Not_found ->
            if extend then (
              (* Generate a new value and "record" the binding for potential future display/shrinking *)
              let value_tree = v_gen st' in
              p_tree_bindings_rev := (key, value_tree) :: !p_tree_bindings_rev;
              let v = Tree.root value_tree in
              T.add tbl key v;
              Some v
            ) else None)
      in
      let p_print = (fun () ->
          let pp_v = Option.value ~default:(fun _ -> "<opaque>") v_print in
          let b = Buffer.create 64 in
          let to_b = Format.formatter_of_buffer b in
          T.iter
            (fun key value ->
              Format.fprintf to_b "%s -> %s; "
                (k_obs.Observable.print key) (pp_v value))
            tbl;
          Format.pp_print_flush to_b ();
          Buffer.contents b)
      in
      let p_size=(fun size_v -> T.fold (fun _ v n -> n + size_v v) tbl 0) in
      {get; p_print; p_size; p_tree_bindings_rev}
    in
    let root_tbl = T.create size in
    (* During initial running of the test, record bindings, hence [~extend:true]. *)
    let root = make ~extend:true root_tbl in
    (* Build the (lazy!) shrink tree of tables here *)
    let shrinks : (k, v) t Tree.t Seq.t = fun () ->
      (* This only gets evaluated *after* the test was run for [tbl], meaning it is correctly
         populated with bindings recorded during the test already *)
      let current_bindings : (k * v Tree.t) list = List.rev !(root.p_tree_bindings_rev) in
      let take_at_most_tree : int Tree.t = Tree.make_primitive (Shrink.int_towards 0) (List.length current_bindings) in
      let current_tree_bindings : (k * v) Tree.t list = List.map (fun (k, tree) -> Tree.map (fun v -> (k, v)) tree) current_bindings in
      let shrunk_bindings_tree : (k * v) list Tree.t = Tree.bind take_at_most_tree (fun take_at_most -> Tree.applicative_take take_at_most current_tree_bindings) in
      (* During shrinking, we don't want to record/add bindings, so [~extend:false]. *)
      let shrunk_poly_tbl_tree : (k, v) t Tree.t = Tree.map (fun bindings -> List.to_seq bindings |> T.of_seq |> make ~extend:false) shrunk_bindings_tree in
      (* [shrunk_poly_tbl_tree] is a bit misleading: its root *should* be the same as [root] but because of the required laziness
         induced by the mutation of bindings, we don't use it, only graft its children to the original [root]. *)
      Tree.children shrunk_poly_tbl_tree ()
    in
    Tree.Tree (root, shrinks)

  let get t x = t.get x
  let print t = t.p_print ()
  let size p t = t.p_size p
end

(** Internal representation of functions, used for shrinking and printing (in case of error). *)
type ('a, 'b) fun_repr_tbl = {
  fun_tbl: ('a, 'b) Poly_tbl.t; (** Input-output bindings *)
  fun_gen: 'b Gen.t; (** How to generate output values *)
  fun_print: 'b Print.t option; (** How to print output values *)
  fun_default: 'b; (** Default value for all inputs not explicitly mapped in {!fun_tbl} *)
}

type 'f fun_repr =
  | Fun_tbl : ('a, 'ret) fun_repr_tbl -> ('a -> 'ret) fun_repr (** Input-output list of bindings *)
  | Fun_map : ('f1 -> 'f2) * 'f1 fun_repr -> 'f2 fun_repr (** Mapped from another function (typically used for currying) *)

(** A QCheck function, as in Koen Claessen's paper "Shrinking and showing functions".
    Such a function is a pair of the function representation (used for shrinking and
    printing the function) and a "real" function, which can be seen as an input-output
    map + a default value for all other inputs.

    - Test developers will only use the "real" function inside their tests (and ignore the function representation).
    - During shrinking/printing, QCheck will ignore the "real" function and only use its representation.
 *)
type 'f fun_ = Fun of 'f fun_repr * 'f

(** Reifying functions *)
module Fn = struct
  let apply (Fun (_repr, real_function)) = real_function

  (** [function_of_repr repr] creates the "real" function (that will be used in tests)
      from its representation. *)
  let rec function_of_repr : type f. f fun_repr -> f = function
    | Fun_tbl {fun_tbl; fun_default; _} ->
       (fun x -> match Poly_tbl.get fun_tbl x with
                      | None -> fun_default
                      | Some y -> y)
    | Fun_map (g, sub_repr) -> g (function_of_repr sub_repr)

  let make_ (r : 'a fun_repr) : 'a fun_ = Fun (r, function_of_repr r)

  let mk_repr tbl gen ?print def =
    Fun_tbl { fun_tbl=tbl; fun_gen=gen; fun_print=print; fun_default=def; }

  let map_repr f repr = Fun_map (f, repr)

  let map_fun f (Fun (repr, _real_function)) = make_ (map_repr f repr)

  (** [print_rep repr] returns a string representation of [repr]. *)
  let print_repr r =
    let buf = Buffer.create 32 in
    let rec aux
      : type f. Buffer.t -> f fun_repr -> unit
      = fun buf r -> match r with
        | Fun_map (_, sub_repr) -> aux buf sub_repr
        | Fun_tbl r ->
           Buffer.add_string buf (Poly_tbl.print r.fun_tbl);
           Printf.bprintf buf "_ -> %s" (match r.fun_print with
                                         | None -> "<opaque>"
                                         | Some print -> print r.fun_default);
    in
    Printf.bprintf buf "{";
    aux buf r;
    Printf.bprintf buf "}";
    Buffer.contents buf

  let print (Fun (repr, _real_function)) = print_repr repr

  (** [gen_rep obs gen] creates a function generator. Input values are observed with [obs] and
      output values are generated with [gen]. *)
  let gen_rep (obs : 'a Observable.t) ?(print : 'b Print.t option) (gen : 'b Gen.t)  : ('a -> 'b) fun_repr Gen.t =
    Gen.liftA2 (fun default_value poly_tbl -> mk_repr poly_tbl gen ?print default_value) gen (Poly_tbl.create ?v_print:print obs gen 8)

  let gen (obs : 'a Observable.t) ?(print : 'b Print.t option) (gen : 'b Gen.t)  : ('a -> 'b) fun_ Gen.t =
    Gen.map make_ (gen_rep obs gen ?print)
end

let fun1 obs ?print gen = Fn.gen obs ?print gen

module Tuple = struct
  (** heterogeneous list (generic tuple) used to uncurry functions *)
  type 'a t =
    | Nil : unit t
    | Cons : 'a * 'b t -> ('a * 'b) t

  let nil = Nil

  let cons x tail = Cons (x,tail)

  type 'a obs =
    | O_nil : unit obs
    | O_cons : 'a Observable.t * 'b obs -> ('a * 'b) obs

  let o_nil = O_nil

  let o_cons x tail = O_cons (x,tail)

  let rec hash
    : type a. a obs -> a t -> int
    = fun o t -> match o, t with
      | O_nil, Nil -> 42
      | O_cons (o,tail_o), Cons (x, tail) ->
        Observable.H.combine (Observable.hash o x) (hash tail_o tail)

  let rec equal
    : type a. a obs -> a t -> a t -> bool
    = fun o a b -> match o, a, b with
      | O_nil, Nil, Nil -> true
      | O_cons (o, tail_o), Cons (x1, tail1), Cons (x2,tail2) ->
        Observable.equal o x1 x2 &&
        equal tail_o tail1 tail2

  let print o tup =
    let rec aux
      : type a. a obs -> Buffer.t -> a t  -> unit
      = fun o buf t -> match o, t with
        | O_nil, Nil -> Printf.bprintf buf "()"
        | O_cons (o, O_nil), Cons (x,Nil) ->
          Printf.bprintf buf "%s" (Observable.print o x)
        | O_cons (o, tail_o), Cons (x,tail) ->
          Printf.bprintf buf "%s, %a"
            (Observable.print o x) (aux tail_o) tail
    in
    let buf = Buffer.create 64 in
    Buffer.add_string buf "(";
    aux o buf tup;
    Buffer.add_string buf ")";
    Buffer.contents buf

  let observable (o:'a obs) : 'a t Observable.t =
    Observable.make
      ~eq:(equal o)
      ~hash:(hash o)
      (print o)

  let gen (o:'a obs) ?(print:'b Print.t option) (ret:'b Gen.t) : ('a t -> 'b) fun_ Gen.t =
    Fn.gen (observable o) ?print ret

  module Infix = struct
    let (@::) x tail = cons x tail
    let (@->) o tail = o_cons o tail
  end
  include Infix
end

let fun_nary (o:_ Tuple.obs) ?print ret : _ Gen.t = Tuple.gen o ?print ret

let fun2 o1 o2 ?print ret =
  Gen.map
    (Fn.map_fun (fun g x y -> g Tuple.(x @:: y @:: nil)))
    (fun_nary Tuple.(o1 @-> o2 @-> o_nil) ?print ret)

let fun3 o1 o2 o3 ?print ret =
  Gen.map
    (Fn.map_fun (fun g x y z -> g Tuple.(x @:: y @:: z @:: nil)))
    (fun_nary Tuple.(o1 @-> o2 @-> o3 @-> o_nil) ?print ret)

let fun4 o1 o2 o3 o4 ?print ret =
  Gen.map
    (Fn.map_fun (fun g x y z w -> g Tuple.(x @:: y @:: z @:: w @:: nil)))
    (fun_nary Tuple.(o1 @-> o2 @-> o3 @-> o4 @-> o_nil) ?print ret)

module TestResult = struct
  type 'a counter_ex = {
    instance: 'a; (** The counter-example(s) *)
    shrink_steps: int; (** How many shrinking steps for this counterex *)
    msg_l: string list; (** messages. @since 0.7 *)
  }

  (** Result state.
      changed in 0.10 (move to inline records) *)
  type 'a state =
    | Success
    | Failed of {
        instances: 'a counter_ex list; (** Failed instance(s) *)
      }
    | Failed_other of {msg: string}
    | Error of {
        instance: 'a counter_ex;
        exn: exn;
        backtrace: string;
      } (** Error, backtrace, and instance that triggered it *)


  (* result returned by running a test *)
  type 'a t = {
    mutable state : 'a state;
    mutable count: int;  (* number of tests *)
    mutable count_gen: int; (* number of generated cases *)
    collect_tbl: (string, int) Hashtbl.t lazy_t;
    stats_tbl: ('a stat * (int, int) Hashtbl.t) list;
    mutable warnings: string list;
  }

  let get_state {state; _} = state

  let get_count {count; _} = count

  let get_count_gen {count_gen; _} = count_gen

  (* indicate failure on the given [instance] *)
  let fail ~msg_l ~steps:shrink_steps res instance =
    let c_ex = {instance; shrink_steps; msg_l; } in
    match res.state with
    | Success -> res.state <- Failed {instances=[ c_ex ]}
    | Error _
    | Failed_other _ -> ()
    | Failed {instances=[]} -> assert false
    | Failed {instances=l} -> res.state <- Failed {instances=c_ex :: l}

  let error ~msg_l ~steps res instance exn backtrace =
    res.state <- Error {instance={instance; shrink_steps=steps; msg_l; }; exn; backtrace}

  let get_collect r =
    if Lazy.is_val r.collect_tbl then Some (Lazy.force r.collect_tbl) else None

  let collect = get_collect

  let get_stats r = r.stats_tbl

  let stats = get_stats

  let get_warnings r = r.warnings

  let warnings = get_warnings

  let is_success r = match r.state with
    | Success -> true
    | Failed _ | Error _ | Failed_other _ -> false

  let is_failed r = match r.state with
    | Failed _ -> true
    | Success | Error _ | Failed_other _ -> false
end

module Test_exceptions = struct

  exception Test_fail of string * string list
  exception Test_error of string * string * exn * string
  exception Test_unexpected_success of string
end

module Test = struct

  type 'a cell = {
    count : int; (* number of tests to do *)
    long_factor : int; (* multiplicative factor for long test count *)
    positive : bool; (* indicates whether test is considered positive or negative *)
    max_gen : int; (* max number of instances to generate (>= count) *)
    max_fail : int; (* max number of failures *)
    retries : int; (* max number of retries during shrinking *)
    law : 'a -> bool; (* the law to check *)
    gen : 'a Gen.t; (* how to generate/shrink instances *)
    print : 'a Print.t option; (* how to print values *)
    collect : ('a -> string) option; (* collect values by tag, useful to display distribution of generated *)
    stats : 'a stat list; (* distribution of values of type 'a *)
    qcheck1_shrink : ('a -> ('a -> unit) -> unit) option; (* QCheck1-backward-compatible shrinking *)
    if_assumptions_fail: [`Fatal | `Warning] * float;
    mutable name : string; (* name of the law *)
  }

  type t = | Test : 'a cell -> t

  let get_name {name; _} = name

  let set_name c name = c.name <- name

  let get_law {law; _} = law

  let get_gen {gen; _} = gen

  let get_print_opt {print; _} = print

  let get_collect_opt {collect; _} = collect

  let get_stats {stats; _} = stats

  let get_count {count; _ } = count

  let get_long_factor {long_factor; _} = long_factor

  let get_positive {positive; _} = positive

  let default_count = 100

  let default_long_factor = 1

  let global_nonnegative_var default env_var var =
    let var = match (var, Sys.getenv_opt env_var) with
    | (Some x, _) -> x
    | (_, Some x) -> int_of_string x
    | (None, None) -> default
  in
  if var < 0 then invalid_arg (env_var ^ " must be >= 0 but value is " ^ string_of_int var) else var

  let global_count count = global_nonnegative_var default_count "QCHECK_COUNT" count

  let global_long_factor long_factor = global_nonnegative_var default_long_factor "QCHECK_LONG_FACTOR" long_factor

  let fresh_name =
    let r = ref 0 in
    (fun () -> incr r; Printf.sprintf "anon_test_%d" !r)

  let default_if_assumptions_fail = `Warning, 0.05

  let make_cell ?(if_assumptions_fail=default_if_assumptions_fail)
      ?(count) ?long_factor ?(negative=false) ?max_gen
      ?(max_fail=1) ?(retries=1) ?(name=fresh_name()) ?print ?collect ?(stats=[]) gen law
    =
    let count = global_count count in
    let long_factor = global_long_factor long_factor in
    let positive = not negative in
    let max_gen = match max_gen with None -> count + 200 | Some x->x in
    {
      law;
      gen;
      collect;
      print;
      stats;
      max_gen;
      max_fail;
      retries;
      name;
      count;
      long_factor;
      positive;
      if_assumptions_fail;
      qcheck1_shrink = None;
    }

  let make_cell_from_QCheck1 ?(if_assumptions_fail=default_if_assumptions_fail)
      ?(count) ?long_factor ?(negative=false) ?max_gen
      ?(max_fail=1) ?(retries=1) ?(name=fresh_name()) ~gen ?shrink ?print ?collect ~stats law
    =
    let count = global_count count in
    let long_factor = global_long_factor long_factor in
    let positive = not negative in
    (* Make a "fake" QCheck2 arbitrary with no shrinking *)
    let fake_gen = Gen.make_primitive ~gen ~shrink:(fun _ -> Seq.empty) in
    let max_gen = match max_gen with None -> count + 200 | Some x->x in
    {
      law;
      gen = fake_gen;
      print;
      collect;
      stats;
      max_gen;
      max_fail;
      retries;
      name;
      count;
      long_factor;
      positive;
      if_assumptions_fail;
      qcheck1_shrink = shrink;
    }

  let make' ?if_assumptions_fail ?count ?long_factor ?max_gen ?max_fail ?retries ?name ?print ?collect ?stats ~negative arb law =
    Test (make_cell ?if_assumptions_fail ?count ?long_factor ?max_gen ?max_fail ?retries ?name ?print ?collect ?stats ~negative arb law)

  let make = make' ~negative:false
  let make_neg = make' ~negative:true

  let test_get_count (Test cell) = get_count cell
  let test_get_long_factor (Test cell) = get_long_factor cell

  (** {6 Running the test} *)

  module R = TestResult

  (* Result of an instance run *)
  type res =
    | Success
    | Failure
    | FalseAssumption
    | Error of exn * string

  (* Step function, called after each instance test *)
  type 'a step = string -> 'a cell -> 'a -> res -> unit

  let step_nil_ _ _ _ _ = ()

  (* Events of a test *)
  type 'a event =
    | Generating
    | Collecting of 'a
    | Testing of 'a
    | Shrunk of int * 'a
    | Shrinking of int * int * 'a

  type 'a handler = string -> 'a cell -> 'a event -> unit

  let handler_nil_ _ _ _ = ()

  (* state required by {!check} to execute *)
  type 'a state = {
    test: 'a cell;
    step: 'a step;
    handler : 'a handler;
    rand: RS.t;
    res: 'a TestResult.t;
    mutable cur_count: int;  (** number of iterations remaining to do *)
    mutable cur_max_gen: int; (** maximum number of generations allowed *)
    mutable cur_max_fail: int; (** maximum number of counter-examples allowed *)
  }

  let is_done state = state.cur_count <= 0 || state.cur_max_gen <= 0

  let decr_count state =
    state.res.R.count <- state.res.R.count + 1;
    state.cur_count <- state.cur_count - 1

  let new_input_tree state =
    state.res.R.count_gen <- state.res.R.count_gen + 1;
    state.cur_max_gen <- state.cur_max_gen - 1;
    state.test.gen state.rand

  (* statistics on inputs *)
  let collect st i = match st.test.collect with
    | None -> ()
    | Some f ->
      let key = f i in
      let (lazy tbl) = st.res.R.collect_tbl in
      let n = try Hashtbl.find tbl key with Not_found -> 0 in
      Hashtbl.replace tbl key (n+1)

  let update_stats st i =
    List.iter
      (fun ((_,f), tbl) ->
         let key = f i in
         let n = try Hashtbl.find tbl key with Not_found -> 0 in
         Hashtbl.replace tbl key (n+1))
      st.res.R.stats_tbl

  type res_or_exn =
    | Shrink_fail
    | Shrink_exn of exn

  (* triggered by user to fail with a message *)
  exception User_fail of string

  let fail_report m = raise (User_fail m)

  let fail_reportf m =
    let buf = Buffer.create 64 in
    Format.kfprintf
      (fun out -> Format.fprintf out "@?"; fail_report (Buffer.contents buf))
      (Format.formatter_of_buffer buf) m

  type 'a run_res =
    | Run_ok
    | Run_fail of string list

  (* run_law is a helper function for testing a property [law] on a
     generated input [x].

     When passed a ~retries number n>1, the tested property is checked
     n times for each shrunk input candidate. The default value is 1,
     thus causing no change in behaviour.

     Retrying a property can be useful when testing non-deterministic
     code with QCheck, e.g., for multicore execution. The idea is
     described in
        'Testing a Database for Race Conditions with QuickCheck'
        Hughes and Bolinder, Erlang 2011, Sec.6:

     "As we explained in section 4, we ensure that tests fail when
     races are present simply by repeating each test a large number of
     times, and by running on a dual core machine. We obtained the
     minimal failing cases in the previous section by repeating each
     test 100 times during shrinking: thus we stopped shrinking a test
     case only when all of its candidate shrinkings passed 100 tests
     in a row."  *)
  let run_law ~retries law x =
    let rec loop i = match law x with
      | false -> Run_fail []
      | true ->
        if i<=1 then Run_ok else loop (i-1) in
    try
      loop retries
    with User_fail msg -> Run_fail [msg]

  (* QCheck1-compatibility code *)
  exception Iter_exit
  let iter_find_map p iter =
    let r = ref None in
    (try iter (fun x -> match p x with Some _ as y -> r := y; raise Iter_exit | None -> ())
     with Iter_exit -> ()
    );
    !r

  (* try to shrink counter-ex [i] into a smaller one. Returns
     shrinked value and number of steps *)
  let shrink st (i_tree : 'a Tree.t) (r : res_or_exn) m : 'a * res_or_exn * string list * int =
    let is_err = match r with
      | Shrink_exn _ -> true | _ -> false
    in
    let rec shrink_ st i_tree r m ~steps =
      let Tree.Tree (i, shrinks) = i_tree in
      st.handler st.test.name st.test (Shrunk (steps, i));
      let count = ref 0 in
      let i' = match st.test.qcheck1_shrink with
        | Some f -> (* QCheck1-compatibility, copied almost verbatim from QCheck.ml old code *)
           iter_find_map
             (fun x ->
               (* let Tree.Tree (x, _) = x_tree in *)
               try
                 incr count;
                 st.handler st.test.name st.test (Shrinking (steps, !count, x));
                 begin match run_law ~retries:st.test.retries st.test.law x with
                 | Run_fail m when not is_err -> Some (Tree.pure x, Shrink_fail, m)
                 | _ -> None
                 end
               with
               | Failed_precondition | No_example_found _ -> None
               | e when is_err -> Some (Tree.pure x, Shrink_exn e, []) (* fail test (by error) *)
             ) (f i)
        | None -> (* QCheck2 (or QCheck1 with a shrinkless tree): use the shrink tree *)
        Seq.filter_map
          (fun x_tree ->
             let Tree.Tree (x, _) = x_tree in
             try
               incr count;
               st.handler st.test.name st.test (Shrinking (steps, !count, x));
               begin match run_law ~retries:st.test.retries st.test.law x with
                 | Run_fail m when not is_err -> Some (x_tree, Shrink_fail, m)
                 | _ -> None
               end
             with
             | Failed_precondition | No_example_found _ -> None
             | e when is_err -> Some (x_tree, Shrink_exn e, []) (* fail test (by error) *)
          ) shrinks
               |> Seq.hd
      in
      match i' with
      | None -> i, r, m, steps
      | Some (i_tree',r',m') -> shrink_ st i_tree' r' m' ~steps:(steps + 1) (* shrink further *)
    in
    shrink_ ~steps:0 st i_tree r m

  type 'a check_result =
    | CR_continue
    | CR_yield of 'a TestResult.t

  (* test raised [e] on [input]; try to shrink then fail *)
  let handle_exn state input e bt : _ check_result =
    (* first, shrink
       TODO: shall we shrink differently (i.e. expected only an error)? *)
    let input, r, msg_l, steps = shrink state input (Shrink_exn e) [] in
    (* recover exception of shrunk input *)
    let e = match r with
      | Shrink_fail -> e
      | Shrink_exn e' -> e'
    in
    state.step state.test.name state.test input (Error (e, bt));
    R.error state.res ~steps ~msg_l input e bt;
    CR_yield state.res

  (* test failed on [input], which means the law is wrong. Continue if
     we should. *)
  let handle_fail state input msg_l : _ check_result =
    (* first, shrink *)
    let input, _, msg_l, steps = shrink state input Shrink_fail msg_l in
    (* fail *)
    decr_count state;
    state.step state.test.name state.test input Failure;
    state.cur_max_fail <- state.cur_max_fail - 1;
    R.fail state.res ~steps ~msg_l input;
    CR_yield state.res

  (* [check_state state] applies [state.test] repeatedly ([iter] times)
      on output of [test.rand], and if [state.test] ever returns false,
      then the input that caused the failure is returned in [Failed].
      If [func input] raises [Failed_precondition] then  the input is discarded, unless
         max_gen is 0. *)
  let rec check_state state : _ R.t =
    if is_done state then state.res
    else (
      state.handler state.test.name state.test Generating;
      match new_input_tree state with
      | i_tree ->
        check_state_input state i_tree
      | exception e ->
        (* turn it into an error *)
        let bt = Printexc.get_backtrace() in
        let msg =
          Printf.sprintf
            "ERROR: uncaught exception in generator for test %s after %d steps:\nException: %s\nBacktrace: %s"
            state.test.name state.test.count (Printexc.to_string e) bt
        in
        state.res.R.state <- R.Failed_other {msg};
        state.res
    )
  and check_state_input state input_tree =
    let Tree.Tree (input, _) = input_tree in
    state.handler state.test.name state.test (Collecting input);
    collect state input;
    update_stats state input;
    let res =
      try
        state.handler state.test.name state.test (Testing input);
        begin match run_law ~retries:1 state.test.law input with
          | Run_ok ->
            (* one test ok *)
            decr_count state;
            state.step state.test.name state.test input Success;
            CR_continue
          | Run_fail msg_l ->
            handle_fail state input_tree msg_l
        end
      with
      | Failed_precondition | No_example_found _ ->
        state.step state.test.name state.test input FalseAssumption;
        CR_continue
      | e ->
        let bt = Printexc.get_backtrace () in
        handle_exn state input_tree e bt
    in
    match res with
    | CR_continue -> check_state state
    | CR_yield x -> x

  type 'a callback = string -> 'a cell -> 'a TestResult.t -> unit

  let callback_nil_ : _ callback = fun _ _ _ -> ()

  (* check that there are sufficiently many tests which passed, to avoid
     the case where they all passed by failed precondition *)
  let check_if_assumptions target_count cell res : unit =
    let percentage_of_count = float_of_int res.R.count /. float_of_int target_count in
    let assm_flag, assm_frac = cell.if_assumptions_fail in
    if R.is_success res && percentage_of_count < assm_frac then (
      let msg =
        format_of_string "%s: \
                          only %.1f%% tests (of %d) passed precondition for %S\n\n\
                          NOTE: it is likely that the precondition is too strong, or that \
                          the generator is buggy.\n%!"
      in
      match assm_flag with
      | `Warning ->
        let msg = Printf.sprintf
            msg "WARNING"
            (percentage_of_count *. 100.) cell.count cell.name in
        res.R.warnings <- msg :: res.R.warnings
      | `Fatal ->
        (* turn it into an error *)
        let msg = Printf.sprintf
            msg "ERROR"
            (percentage_of_count *. 100.) cell.count cell.name in
        res.R.state <- R.Failed_other {msg}
    )

  (* main checking function *)
  let check_cell ?(long=false) ?(call=callback_nil_)
      ?(step=step_nil_) ?(handler=handler_nil_)
      ?(rand=RS.make [| 0 |]) cell =
    let factor = if long then cell.long_factor else 1 in
    let target_count = factor*cell.count in
    let state = {
      test=cell; rand;
      step; handler;
      cur_count=target_count;
      cur_max_gen=factor*cell.max_gen;
      cur_max_fail=factor*cell.max_fail;
      res = {R.
              state=R.Success; count=0; count_gen=0;
              collect_tbl=lazy (Hashtbl.create 10);
              warnings=[];
              stats_tbl= List.map (fun stat -> stat, Hashtbl.create 10) cell.stats;
            };
    } in
    let res = check_state state in
    check_if_assumptions target_count cell res;
    call cell.name cell res;
    res

  include Test_exceptions

  (* print instance using [arb] *)
  let print_instance arb i = match arb.print with
    | None -> "<instance>"
    | Some pp -> pp i

  let print_c_ex arb c : string =
    let buf = Buffer.create 64 in
    begin
      if c.R.shrink_steps > 0
      then Printf.bprintf buf "%s (after %d shrink steps)"
          (print_instance arb c.R.instance) c.R.shrink_steps
      else Buffer.add_string buf (print_instance arb c.R.instance)
    end;
    List.iter
      (fun msg ->
         Buffer.add_char buf '\n';
         Buffer.add_string buf msg;
         Buffer.add_char buf '\n')
      c.R.msg_l;
    Buffer.contents buf

  let pp_print_test_fail name out l =
    let rec pp_list out = function
      | [] -> ()
      | [x] -> Format.fprintf out "%s@," x
      | x :: y -> Format.fprintf out "%s@,%a" x pp_list y
    in
    Format.fprintf out "@[test `%s`@ failed on ≥ %d cases:@ @[<v>%a@]@]"
      name (List.length l) pp_list l

  let asprintf fmt =
    let buf = Buffer.create 128 in
    let out = Format.formatter_of_buffer buf in
    Format.kfprintf (fun _ -> Buffer.contents buf) out fmt

  let print_test_fail name l = asprintf "@[%a@]@?" (pp_print_test_fail name) l

  let print_unexpected_success name = Format.sprintf "@[negative test `%s`@ succeeded unexpectedly@]" name

  let print_test_error name i e stack =
    Format.sprintf "@[test `%s`@ raised exception `%s`@ on `%s`@,%s@]"
      name (Printexc.to_string e) i stack

  let print_collect c =
    let out = Buffer.create 64 in
    Hashtbl.iter
      (fun case num -> Printf.bprintf out "%s: %d cases\n" case num) c;
    Buffer.contents out

  let stat_max_lines = 20 (* maximum number of lines for a histogram *)

  let print_stat ((name,_), tbl) =
    let avg = ref 0. in
    let num = ref 0 in
    let min_idx, max_idx =
      Hashtbl.fold
        (fun i res (m1,m2) ->
           avg := !avg +. float_of_int (i * res);
           num := !num + res;
           min i m1, max i m2)
        tbl (max_int,min_int)
    in
    (* compute average *)
    if !num > 0 then (
      avg := !avg /. float_of_int !num
    );
    (* compute std-dev: sqroot of sum of squared distance-to-average
       https://en.wikipedia.org/wiki/Standard_deviation *)
    let stddev =
      Hashtbl.fold
        (fun i res m -> m +. (float_of_int i -. !avg) ** 2. *. float_of_int res)
        tbl 0.
      |> (fun s -> if !num>0 then s /. float_of_int !num else s)
      |> sqrt
    in
    (* compute median *)
    let median = ref 0 in
    let median_num = ref 0 in (* how many values have we seen yet? once >= !n/2 we set median *)
    (Hashtbl.fold (fun i cnt acc -> (i,cnt)::acc) tbl [])
    |> List.sort (fun (i,_) (j,_) -> poly_compare i j)
    |> List.iter
      (fun (i,cnt) ->
         if !median_num < !num/2 then (
           median_num := !median_num + cnt;
           (* just went above median! *)
           if !median_num >= !num/2 then
             median := i));
    (* group by buckets, if there are too many entries: *)
    (* first compute histogram and bucket size *)
    let min_idx64, max_idx64 = Int64.(of_int min_idx, of_int max_idx) in
    let hist_size, bucket_size =
      let sample_width = Int64.sub max_idx64 min_idx64 in
      if sample_width > Int64.of_int stat_max_lines
      then stat_max_lines,
           int_of_float (ceil (Int64.to_float sample_width /. float_of_int stat_max_lines))
      else max_idx-min_idx, 1
    in
    let hist_size =
      if Int64.(add min_idx64 (mul (of_int bucket_size) (of_int hist_size))) <= max_idx64
      then 1+hist_size
      else hist_size in
    (* accumulate bucket counts *)
    let max_val = ref 0 in (* max value after grouping by buckets *)
    let bucket_count = Array.init hist_size (fun _ -> 0) in
    Hashtbl.iter
      (fun j count ->
         let bucket = Int64.(to_int (div (sub (of_int j) min_idx64) (of_int bucket_size))) in
         let new_count = bucket_count.(bucket) + count in
         bucket_count.(bucket) <- new_count;
         max_val := max !max_val new_count) tbl;
    (* print entries of the table, sorted by increasing index *)
    let out = Buffer.create 128 in
    Printf.bprintf out "stats %s:\n" name;
    Printf.bprintf out
      "  num: %d, avg: %.2f, stddev: %.2f, median %d, min %d, max %d\n"
      !num !avg stddev !median min_idx max_idx;
    let indwidth =
      let str_width i = String.length (Printf.sprintf "%d" i) in
      List.map str_width [min_idx; max_idx; min_idx + bucket_size * hist_size] |> List.fold_left max min_int in
    let labwidth = if bucket_size=1 then indwidth else 2+2*indwidth in
    for i = 0 to hist_size - 1 do
      let i' = min_idx + i * bucket_size in
      let blabel =
        if bucket_size=1
        then Printf.sprintf "%*d" indwidth i'
        else
          let bucket_bound = i'+bucket_size-1 in
          Printf.sprintf "%*d..%*d" indwidth i' indwidth (if bucket_bound < i' then max_int else bucket_bound) in
      let bcount = bucket_count.(i) in
      (* NOTE: keep in sync *)
      let bar_len = bcount * 55 / !max_val in
      Printf.bprintf out "  %*s: %-56s %10d\n" labwidth blabel (String.make bar_len '#') bcount
    done;
    Buffer.contents out

  let () = Printexc.register_printer
      (function
        | Test_fail (name,l) -> Some (print_test_fail name l)
        | Test_error (name,i,e,st) -> Some (print_test_error name i e st)
        | Test_unexpected_success name -> Some (print_unexpected_success name)
        | User_fail s -> Some ("qcheck: user fail:\n" ^ s)
        | _ -> None)

  let print_fail arb name l =
    print_test_fail name (List.map (print_c_ex arb) l)

  let print_fail_other name ~msg =
    print_test_fail name [msg]

  let print_expected_failure cell c_exs = match c_exs with
    | [] -> Format.sprintf "negative test `%s` failed as expected\n" (get_name cell)
    | c_ex::_ -> Format.sprintf "negative test `%s` failed as expected on: %s\n" (get_name cell) (print_c_ex cell c_ex)

  let print_error ?(st="") arb name (i,e) =
    print_test_error name (print_c_ex arb i) e st

  let check_result cell res = match res.R.state, cell.positive with
    | R.Success, true -> ()
    | R.Success, false ->
      raise (Test_unexpected_success cell.name)
    | R.Error {instance; exn; backtrace}, _ ->
      raise (Test_error (cell.name, print_c_ex cell instance, exn, backtrace))
    | R.Failed {instances=l}, true ->
      let l = List.map (print_c_ex cell) l in
      raise (Test_fail (cell.name, l))
    | R.Failed _, false -> ()
    | R.Failed_other {msg}, _ ->
      raise (Test_fail (cell.name, [msg]))

  let check_cell_exn ?long ?call ?step ?handler ?rand cell =
    let res = check_cell ?long ?call ?step ?handler ?rand cell in
    check_result cell res

  let check_exn ?long ?rand (Test cell) = check_cell_exn ?long ?rand cell
end

let find_example ?(name : string = "<example>") ?(count : int option) ~(f : 'a -> bool) (gen : 'a Gen.t) : 'a Gen.t =
  (* the random generator of examples satisfying [f]. To do that we
     test the property [fun x -> not (f x)]; any counter-example *)
  let gen st =
    let cell =
      Test.make_cell ~max_fail:1 ?count gen (fun x -> not (f x))
    in
    let res = Test.check_cell ~rand:st cell in
    begin match res.TestResult.state with
      | TestResult.Success -> raise (No_example_found name)
      | TestResult.Error _ -> raise (No_example_found name)
      | TestResult.Failed {instances=[]} -> assert false
      | TestResult.Failed {instances=failed::_} ->
        (* found counter-example! *)
        Tree.pure failed.TestResult.instance
      | TestResult.Failed_other {msg=_} ->
        raise (No_example_found name)

    end
  in
  gen

let find_example_gen ?(rand : RS.t option) ?(name : string option) ?(count : int option) ~(f : 'a -> bool) (gen : 'a Gen.t) : 'a =
  let g = find_example ?name ?count ~f gen in
  Gen.generate1 ?rand g

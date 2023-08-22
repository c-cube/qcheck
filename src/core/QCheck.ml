(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

(** {1 Quickcheck inspired property-based testing} *)

let poly_compare=compare
open Printf

module RS = struct
  (* Poor man's splitter for version < 5.0                       *)
  (* This definition is shadowed by the [include] on OCaml >=5.0 *)
  let split rs =
    let bits = Random.State.bits rs in
    let rs' = Random.State.make [|bits|] in
    rs'
  include Random.State
  (* This is how OCaml 5.0 splits:             *)
  (* Split a new PRNG off the given PRNG *)
  (*
  let split s =
    let i1 = bits64 s in let i2 = bits64 s in
    let i3 = bits64 s in let i4 = bits64 s in
    mk i1 i2 i3 i4
  *)
end

let (|>) x f = f x

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let _is_some = function Some _ -> true | None -> false

let _opt_map_or ~d ~f = function
  | None -> d
  | Some x -> f x

let _opt_or a b = match a with
  | None -> b
  | Some x -> x

let _opt_map ~f = function
  | None -> None
  | Some x -> Some (f x)

let _opt_map_2 ~f a b = match a, b with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let _opt_map_3 ~f a b c = match a, b, c with
  | Some x, Some y, Some z -> Some (f x y z)
  | _ -> None

let _opt_map_4 ~f a b c d = match a, b, c, d with
  | Some x, Some y, Some z, Some w -> Some (f x y z w)
  | _ -> None

let _opt_map_5 ~f a b c d e = match a, b, c, d, e with
  | Some x, Some y, Some z, Some u, Some v -> Some (f x y z u v)
  | _ -> None

let _opt_map_6 ~f a b c d e g = match a, b, c, d, e, g with
  | Some a, Some b, Some c, Some d, Some e, Some g -> Some (f a b c d e g)
  | _ -> None

let _opt_map_7 ~f a b c d e g h = match a, b, c, d, e, g, h with
  | Some a, Some b, Some c, Some d, Some e, Some g, Some h -> Some (f a b c d e g h)
  | _ -> None

let _opt_map_8 ~f a b c d e g h i = match a, b, c, d, e, g, h, i with
  | Some a, Some b, Some c, Some d, Some e, Some g, Some h, Some i ->
    Some (f a b c d e g h i)
  | _ -> None

let _opt_map_9 ~f a b c d e g h i j = match a, b, c, d, e, g, h, i, j with
  | Some a, Some b, Some c, Some d, Some e, Some g, Some h, Some i, Some j ->
    Some (f a b c d e g h i j)
  | _ -> None

let _opt_sum a b = match a, b with
  | Some _, _ -> a
  | None, _ -> b

let sum_int = List.fold_left (+) 0

(* Included for backwards compatibility, pre 4.13 *)
let string_fold_right f s acc =
  let len = String.length s in
  let rec loop i acc =
    if i<0
    then acc
    else loop (i-1) (f s.[i] acc) in
  loop (len-1) acc

exception No_example_found of string
(* raised if an example failed to be found *)

let assume = QCheck2.assume

let assume_fail = QCheck2.assume_fail

let (==>) = QCheck2.(==>)

module Gen = struct
  type 'a t = RS.t -> 'a
  type 'a sized = int -> Random.State.t -> 'a

  let return x _st = x
  let pure = return

  let (>>=) gen f st =
    f (gen st) st

  let (<*>) f x st = f st (x st)
  let map f x st = f (x st)
  let map2 f x y st = f (x st) (y st)
  let map3 f x y z st = f (x st) (y st) (z st)
  let map_keep_input f gen st = let x = gen st in x, f x
  let (>|=) x f st = f (x st)
  let (<$>) f x st = f (x st)

  let oneof l st = List.nth l (Random.State.int st (List.length l)) st
  let oneofl xs st = List.nth xs (Random.State.int st (List.length xs))
  let oneofa xs st = Array.get xs (Random.State.int st (Array.length xs))

  let frequencyl l st =
    let sums = sum_int (List.map fst l) in
    let i = Random.State.int st sums in
    let rec aux acc = function
      | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
      | _ -> failwith "frequency"
    in
    aux 0 l

  let frequencya a = frequencyl (Array.to_list a)

  let frequency l st = frequencyl l st st

  let small_nat st =
    let p = RS.float st 1. in
    if p < 0.75 then RS.int st 10 else RS.int st 100

  (* natural number generator *)
  let nat st =
    let p = RS.float st 1. in
    if p < 0.5 then RS.int st 10
    else if p < 0.75 then RS.int st 100
    else if p < 0.95 then RS.int st 1_000
    else RS.int st 10_000

  let big_nat st =
    let p = RS.float st 1. in
    if p < 0.75 then nat st
    else RS.int st 1_000_000

  let unit _st = ()

  let bool st = RS.bool st

  let float st =
    exp (RS.float st 15. *. (if RS.float st 1. < 0.5 then 1. else -1.))
    *. (if RS.float st 1. < 0.5 then 1. else -1.)

  let pfloat st = abs_float (float st)
  let nfloat st = -.(pfloat st)

  let float_bound_inclusive bound st = RS.float st bound

  let float_bound_exclusive bound st =
    match bound with
    | 0. -> raise (Invalid_argument "Gen.float_bound_exclusive")
    | b_pos when bound > 0. -> RS.float st (b_pos -. epsilon_float)
    | b_neg -> RS.float st (b_neg +. epsilon_float)

  let float_range low high =
    if high < low || high -. low > max_float then invalid_arg "Gen.float_range";
    fun st -> low +. (float_bound_inclusive (high -. low) st)

  let (--.) = float_range

  let neg_int st = -(nat st)

  let option ?(ratio = 0.85) f st =
    let p = RS.float st 1. in
    if p < (1.0 -. ratio) then None
    else Some (f st)

  let opt = option

  (* Uniform random int generator *)
  let pint =
    if Sys.word_size = 32 then
      fun st -> RS.bits st
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

  let int st = if RS.bool st then - (pint st) - 1 else pint st
  let int_bound n =
    if n < 0 then invalid_arg "Gen.int_bound";
    if n <= (1 lsl 30) - 2
    then fun st -> Random.State.int st (n + 1)
    else fun st -> let r = pint st in r mod (n + 1)
  let int_range a b =
    if b < a then invalid_arg "Gen.int_range";
    if a >= 0 || b < 0 then (
      (* range smaller than max_int *)
      assert (b-a >= 0);
      fun st -> a + (int_bound (b-a) st)
    ) else (
      (* range potentially bigger than max_int: we split on 0 and
         choose the itv wrt to their size ratio *)
      fun st ->
      let f_a = float_of_int a in
      let ratio = (-.f_a) /. (1. +. float_of_int b -. f_a) in
      if Random.State.float st 1. <= ratio then - (int_bound (- (a+1)) st) - 1
      else int_bound b st
    )

  let (--) = int_range

  (* NOTE: we keep this alias to not break code that uses [small_int]
     for sizes of strings, arrays, etc. *)
  let small_int = small_nat

  let small_signed_int st =
    if bool st
    then small_nat st
    else - (small_nat st)

  let char_range a b = map Char.chr (Char.code a -- Char.code b)

  let random_binary_string st length =
    (* 0b011101... *)
    let s = Bytes.create (length + 2) in
    Bytes.set s 0 '0';
    Bytes.set s 1 'b';
    for i = 0 to length - 1 do
      Bytes.set s (i+2) (if RS.bool st then '0' else '1')
    done;
    Bytes.unsafe_to_string s

  let ui32 st = Int32.of_string (random_binary_string st 32)
  let ui64 st = Int64.of_string (random_binary_string st 64)

  let list_size size gen st =
    foldn ~f:(fun acc _ -> (gen st)::acc) ~init:[] (size st)
  let list gen st = list_size nat gen st
  let list_repeat n g = list_size (return n) g

  let array_size size gen st =
    Array.init (size st) (fun _ -> gen st)
  let array gen st = array_size nat gen st
  let array_repeat n g = array_size (return n) g

  let flatten_l l st = List.map (fun f->f st) l
  let flatten_a a st = Array.map (fun f->f st) a
  let flatten_opt o st =
    match o with
    | None -> None
    | Some f -> Some (f st)
  let flatten_res r st =
    match r with
    | Ok f -> Ok (f st)
    | Error e -> Error e

  let shuffle_a a st =
    for i = Array.length a-1 downto 1 do
      let j = Random.State.int st (i+1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
    done

  let shuffle_l l st =
    let a = Array.of_list l in
    shuffle_a a st;
    Array.to_list a

  let shuffle_w_l l st =
    let sample (w, v) =
      let fl_w = float_of_int w in
      (float_bound_inclusive 1. st ** (1. /. fl_w), v)
    in
    let samples = List.rev_map sample l in
    List.sort (fun (w1, _) (w2, _) -> poly_compare w1 w2) samples |> List.rev_map snd

  let range_subset ~size low high st =
    let range_size = high - low + 1 in
    if not (0 <= size && size <= range_size) then
      invalid_arg "Gen.range_subset";
    (* The algorithm below is attributed to Floyd, see for example
       https://eyalsch.wordpress.com/2010/04/01/random-sample/
       https://math.stackexchange.com/questions/178690

       Note: the code is easier to read when drawing from [0..range_size-1]
       rather than [low..high]. We draw in [0..bound], and shift the
       results by adding [low] when writing them to the result array.
    *)
    let module ISet = Set.Make(Int) in
    let s = ref ISet.empty in
    for i = range_size - size to range_size - 1 do
      let pos = int_range 0 i st in
      let choice = if ISet.mem pos !s then i else pos in
      s := ISet.add choice !s;
    done;
    let arr = Array.make size 0 in
    let idx = ref 0 in
    ISet.iter (fun choice -> arr.(!idx) <- low + choice; incr idx) !s;
    arr

  let array_subset size arr st =
    range_subset ~size 0 (Array.length arr - 1) st
    |> Array.map (fun i -> arr.(i))

  let pair g1 g2 st = (g1 st, g2 st)

  let triple g1 g2 g3 st = (g1 st, g2 st, g3 st)

  let quad g1 g2 g3 g4 st = (g1 st, g2 st, g3 st, g4 st)

  let char st = char_of_int (RS.int st 256)

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

  let printable_chars =
    let l = 126-32+1 in
    let s = Bytes.create l in
    for i = 0 to l-2 do
      Bytes.set s i (char_of_int (32+i))
    done;
    Bytes.set s (l-1) '\n';
    Bytes.unsafe_to_string s

  let printable st = printable_chars.[RS.int st (String.length printable_chars)]
  let numeral st = char_of_int (48 + RS.int st 10)

  let bytes_size ?(gen = char) size st =
    let s = Bytes.create (size st) in
    for i = 0 to Bytes.length s - 1 do
      Bytes.set s i (gen st)
    done;
    s

  let string_size ?(gen = char) size st =
    let s = bytes_size ~gen size st in
    Bytes.unsafe_to_string s

  let bytes ?gen st = bytes_size ?gen nat st
  let string ?gen st = string_size ?gen nat st
  let bytes_of gen = bytes_size ~gen nat
  let string_of gen = string_size ~gen nat
  let bytes_printable = bytes_size ~gen:printable nat
  let string_printable = string_size ~gen:printable nat
  let string_readable = string_printable
  let bytes_small st = bytes_size small_nat st
  let bytes_small_of gen st = bytes_size ~gen small_nat st
  let small_string ?gen st = string_size ?gen small_nat st
  let small_list gen = list_size small_nat gen
  let small_array gen = array_size small_nat gen
  let string_small st = string_size small_nat st
  let string_small_of gen st = string_size ~gen small_nat st

  let join g st = (g st) st

  (* corner cases *)

  let graft_corners gen corners () =
    let cors = ref corners in fun st ->
      match !cors with [] -> gen st
      | e::l -> cors := l; e

  let int_pos_corners = [0;1;2;max_int]
  let int_corners = int_pos_corners @ [min_int]

  let nng_corners () = graft_corners nat int_pos_corners ()

  (* sized, fix *)

  let sized_size s f st = f (s st) st
  let sized f = sized_size nat f

  let fix f =
    let rec f' n st = f f' n st in
    f'

  (* nat splitting *)

  let pos_split2 n st =
    if (n < 2) then invalid_arg "pos_split2";
    let n1 = int_range 1 (n - 1) st in
    (n1, n - n1)

  let nat_split2 n st =
    if (n < 0) then invalid_arg "nat_split2";
    let n1 = int_range 0 n st in
    (n1, n - n1)

  let pos_split ~size:k n st =
    if (n < 0) then invalid_arg "pos_split";
    if 0 = k && 0 = n then [||]
    else begin
      if not (0 < k && k <= n) then invalid_arg "pos_split";
      (* To split n into n{0}+n{1}+..+n{k-1}, we draw distinct "boundaries"
         b{-1}..b{k-1}, with b{-1}=0 and b{k-1} = n
         and the k-1 intermediate boundaries b{0}..b{k-2}
         chosen randomly distinct in [1;n-1].

         Then each n{i} is defined as b{i}-b{i-1}. *)
      let b = range_subset ~size:(k-1) 1 (n - 1) st in
      if k = 1 then [|n|]
      else Array.init k (fun i ->
        if i = 0 then b.(0)
        else if i = k-1 then n - b.(i-1)
        else b.(i) - b.(i-1)
      )
    end

  let nat_split ~size:k n st =
    if not (0 <= k && 0 <= n) then invalid_arg "nat_split";
    pos_split ~size:k (n+k) st
    |> Array.map (fun v -> v - 1)

  let generate ?(rand=Random.State.make_self_init()) ~n g =
    list_repeat n g rand

  let generate1 ?(rand=Random.State.make_self_init()) g = g rand

  let delay f st = f () st

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
  let string s = s
  let char c = String.make 1 c

  let option f = function
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"

  let pair a b (x,y) = Printf.sprintf "(%s, %s)" (a x) (b y)
  let triple a b c (x,y,z) = Printf.sprintf "(%s, %s, %s)" (a x) (b y) (c z)
  let quad a b c d (x,y,z,w) =
    Printf.sprintf "(%s, %s, %s, %s)" (a x) (b y) (c z) (d w)

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

  let comap f p x = p (f x)
end

module Iter = struct
  type 'a t = ('a -> unit) -> unit
  let empty _ = ()
  let return x yield = yield x
  let (<*>) a b yield = a (fun f -> b (fun x ->  yield (f x)))
  let (>>=) a f yield = a (fun x -> f x yield)
  let map f a yield = a (fun x -> yield (f x))
  let map2 f a b yield = a (fun x -> b (fun y -> yield (f x y)))
  let (>|=) a f = map f a
  let append a b yield = a yield; b yield
  let append_l l yield = List.iter (fun s->s yield) l
  let flatten s yield = s (fun sub -> sub yield)
  let filter f s yield = s (fun x -> if f x then yield x)
  let (<+>) = append
  let of_list l yield = List.iter yield l
  let of_array a yield = Array.iter yield a
  let pair a b yield = a (fun x -> b(fun y -> yield (x,y)))
  let triple a b c yield = a (fun x -> b (fun y -> c (fun z -> yield (x,y,z))))
  let quad a b c d yield =
    a (fun x -> b (fun y -> c (fun z -> d (fun w -> yield (x,y,z,w)))))

  exception IterExit
  let find_map p iter =
    let r = ref None in
    (try iter (fun x -> match p x with Some _ as y -> r := y; raise IterExit | None -> ())
     with IterExit -> ()
    );
    !r

  let find p iter = find_map (fun x->if p x then Some x else None) iter

  let (let+) = (>|=)

  let (and+) = pair

  let (let*) = (>>=)

   let (and*) = pair
end

module Shrink = struct
  type 'a t = 'a -> 'a Iter.t

  let nil _ = Iter.empty

  let unit = nil

  (* balanced shrinker for integers (non-exhaustive) *)
  let int x yield =
    let y = ref x in
    (* try some divisors *)
    while !y < -2 || !y >2 do y := !y / 2; yield (x - !y); done; (* fast path *)
    if x = 1 || (x>0 && !y <> 1) then yield (x-1);
    if x = -1 || (x<0 && !y <> -1) then yield (x+1);
    ()

  let int32 x yield =
    let open Int32 in
    let y = ref x in
    (* try some divisors *)
    while !y < -2l || !y > 2l do y := div !y 2l; yield (sub x !y); done; (* fast path *)
    if x = 1l || (x>0l && !y <> 1l) then yield (pred x);
    if x = -1l || (x<0l && !y <> -1l) then yield (succ x);
    ()

  let int64 x yield =
    let open Int64 in
    let y = ref x in
    (* try some divisors *)
    while !y < -2L || !y > 2L do y := div !y 2L; yield (sub x !y); done; (* fast path *)
    if x = 1L || (x>0L && !y <> 1L) then yield (pred x);
    if x = -1L || (x<0L && !y <> -1L) then yield (succ x);
    ()

  (* aggressive shrinker for integers,
     get from 0 to x, by dichotomy or just enumerating smaller values *)
  let int_aggressive x yield =
    let y = ref x in
    while !y < -2 || !y >2 do y := !y / 2; yield (x - !y); done; (* fast path *)
    if x>0 then for i=x-1 downto 0 do yield i done;
    if x<0 then for i=x+1 to 0 do yield i done

  let filter f shrink x = Iter.filter f (shrink x)

  let char_generic target c =
    if c = target
    then Iter.empty
    else
      let c_code = Char.code c in
      let target_code = Char.code target in
      Iter.map (fun diff -> Char.chr (target_code + diff)) (int (c_code - target_code))
  let char = char_generic 'a'
  let char_numeral = char_generic '0'

  let char_printable = function
    | '\n' -> char '~' (* treat '\n' (10) as '~' (126) to ensure a non-trivial, printable output *)
    | c -> char c

  let option s x = match x with
    | None -> Iter.empty
    | Some x -> Iter.(return None <+> map (fun y->Some y) (s x))

  let array ?shrink a yield =
    let n = Array.length a in
    let chunk_size = ref n in
    while !chunk_size > 0 do
      for i=0 to n - !chunk_size do
        (* remove elements in [i .. i+!chunk_size] *)
        let a' = Array.init (n - !chunk_size)
          (fun j -> if j< i then a.(j) else a.(j + !chunk_size))
        in
        yield a'
      done;
      chunk_size := !chunk_size / 2;
    done;
    match shrink with
    | None -> ()
    | Some f ->
        (* try to shrink each element of the array *)
        for i = 0 to Array.length a - 1 do
          f a.(i) (fun x ->
            let b = Array.copy a in
            b.(i) <- x;
            yield b
          )
        done

  let rec list_spine l yield =
    let rec split l len acc = match len,l with
      | _,[]
      | 0,_ -> List.rev acc, l
      | _,x::xs -> split xs (len-1) (x::acc) in
    match l with
    | [] -> ()
    | [_] -> yield []
    | [x;y] -> yield []; yield [x]; if (try x <> y with Invalid_argument _ -> x != y) then yield [y]
    | _::_ ->
      let len = List.length l in
      let xs,ys = split l ((1 + len) / 2) [] in
      yield xs;
      list_spine xs (fun xs' -> yield (xs'@ys))

  let list_elems shrink l yield =
    (* try to shrink each element of the list *)
    let rec elem_loop rev_prefix suffix = match suffix with
      | [] -> ()
      | x::xs ->
         shrink x (fun x' -> yield (List.rev_append rev_prefix (x'::xs)));
         elem_loop (x::rev_prefix) xs
    in
    elem_loop [] l

  let list ?shrink l yield =
    list_spine l yield;
    match shrink with
    | None -> ()
    | Some shrink -> list_elems shrink l yield

  let string ?(shrink = char) s yield =
    let buf = Buffer.create 42 in
    list ~shrink
      (string_fold_right (fun c acc -> c::acc) s [])
      (fun cs ->
         List.iter (fun c -> Buffer.add_char buf c) cs;
         let s = Buffer.contents buf in
         Buffer.clear buf;
         yield s)

  let bytes ?(shrink = char) b = Iter.map Bytes.of_string (string ~shrink (Bytes.to_string b))

  let pair a b (x,y) yield =
    a x (fun x' -> yield (x',y));
    b y (fun y' -> yield (x,y'))

  let triple a b c (x,y,z) yield =
    a x (fun x' -> yield (x',y,z));
    b y (fun y' -> yield (x,y',z));
    c z (fun z' -> yield (x,y,z'))

  let quad a b c d (x,y,z,w) yield =
    a x (fun x' -> yield (x',y,z,w));
    b y (fun y' -> yield (x,y',z,w));
    c z (fun z' -> yield (x,y,z',w));
    d w (fun w' -> yield (x,y,z,w'))

  let default = nil

  let tup2 = pair

  let tup2_opt a b =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    tup2 a b

  let tup3 = triple

  let tup3_opt a b c =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    tup3 a b c

  let tup4 = quad

  let tup4_opt a b c d =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    tup4 a b c d

  let tup5 a b c d e (a', b', c', d', e') yield =
    a a' (fun x -> yield (x,b',c',d',e'));
    b b' (fun x -> yield (a',x,c',d',e'));
    c c' (fun x -> yield (a',b',x,d',e'));
    d d' (fun x -> yield (a',b',c',x,e'));
    e e' (fun x -> yield (a',b',c',d',x))

  let tup5_opt a b c d e =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    let e = Option.value ~default e in
    tup5 a b c d e

  let tup6 a b c d e f (a', b', c', d', e', f') yield =
    a a' (fun x -> yield (x,b',c',d',e',f'));
    b b' (fun x -> yield (a',x,c',d',e',f'));
    c c' (fun x -> yield (a',b',x,d',e',f'));
    d d' (fun x -> yield (a',b',c',x,e',f'));
    e e' (fun x -> yield (a',b',c',d',x,f'));
    f f' (fun x -> yield (a',b',c',d',e',x))

  let tup6_opt a b c d e f =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    let e = Option.value ~default e in
    let f = Option.value ~default f in
    tup6 a b c d e f

  let tup7 a b c d e f g (a', b', c', d', e', f', g') yield =
    a a' (fun x -> yield (x,b',c',d',e',f',g'));
    b b' (fun x -> yield (a',x,c',d',e',f',g'));
    c c' (fun x -> yield (a',b',x,d',e',f',g'));
    d d' (fun x -> yield (a',b',c',x,e',f',g'));
    e e' (fun x -> yield (a',b',c',d',x,f',g'));
    f f' (fun x -> yield (a',b',c',d',e',x,g'));
    g g' (fun x -> yield (a',b',c',d',e',f',x))

  let tup7_opt a b c d e f g =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    let e = Option.value ~default e in
    let f = Option.value ~default f in
    let g = Option.value ~default g in
    tup7 a b c d e f g

  let tup8 a b c d e f g h (a', b', c', d', e', f', g', h') yield =
    a a' (fun x -> yield (x,b',c',d',e',f',g',h'));
    b b' (fun x -> yield (a',x,c',d',e',f',g',h'));
    c c' (fun x -> yield (a',b',x,d',e',f',g',h'));
    d d' (fun x -> yield (a',b',c',x,e',f',g',h'));
    e e' (fun x -> yield (a',b',c',d',x,f',g',h'));
    f f' (fun x -> yield (a',b',c',d',e',x,g',h'));
    g g' (fun x -> yield (a',b',c',d',e',f',x,h'));
    h h' (fun x -> yield (a',b',c',d',e',f',g',x))

  let tup8_opt a b c d e f g h =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    let e = Option.value ~default e in
    let f = Option.value ~default f in
    let g = Option.value ~default g in
    let h = Option.value ~default h in
    tup8 a b c d e f g h

  let tup9 a b c d e f g h i (a', b', c', d', e', f', g', h', i') yield =
    a a' (fun x -> yield (x,b',c',d',e',f',g',h',i'));
    b b' (fun x -> yield (a',x,c',d',e',f',g',h',i'));
    c c' (fun x -> yield (a',b',x,d',e',f',g',h',i'));
    d d' (fun x -> yield (a',b',c',x,e',f',g',h',i'));
    e e' (fun x -> yield (a',b',c',d',x,f',g',h',i'));
    f f' (fun x -> yield (a',b',c',d',e',x,g',h',i'));
    g g' (fun x -> yield (a',b',c',d',e',f',x,h',i'));
    h h' (fun x -> yield (a',b',c',d',e',f',g',x,i'));
    i i' (fun x -> yield (a',b',c',d',e',f',g',h',x))

  let tup9_opt a b c d e f g h i =
    let a = Option.value ~default a in
    let b = Option.value ~default b in
    let c = Option.value ~default c in
    let d = Option.value ~default d in
    let e = Option.value ~default e in
    let f = Option.value ~default f in
    let g = Option.value ~default g in
    let h = Option.value ~default h in
    let i = Option.value ~default i in
    tup9 a b c d e f g h i
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
    let opt f = function
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
    make ~hash:(H.opt p.hash) ~eq:(Eq.option p.eq)
      (Print.option p.print)

  let array p =
    make ~hash:(H.array p.hash) ~eq:(Eq.array p.eq) (Print.array p.print)
  let list p =
    make ~hash:(H.list p.hash) ~eq:(Eq.list p.eq) (Print.list p.print)

  let map f p =
    make ~hash:(fun x -> p.hash (f x)) ~eq:(fun x y -> p.eq (f x)(f y))
      (fun x -> p.print (f x))

  let pair a b =
    make ~hash:(H.pair a.hash b.hash) ~eq:(Eq.pair a.eq b.eq) (Print.pair a.print b.print)
  let triple a b c =
    map (fun (x,y,z) -> x,(y,z)) (pair a (pair b c))
  let quad a b c d =
    map (fun (x,y,z,u) -> x,(y,z,u)) (pair a (triple b c d))
end

type 'a stat = string * ('a -> int)
(** A statistic on a distribution of values of type ['a] *)

type 'a arbitrary = {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a -> 'a Iter.t) option;  (** shrink to smaller examples *)
  collect: ('a -> string) option;  (** map value to tag, and group by tag *)
  stats: 'a stat list; (** statistics to collect and print *)
}

let make ?print ?small ?shrink ?collect ?(stats=[]) gen = {
  gen;
  print;
  small;
  shrink;
  collect;
  stats;
}

let set_small f o = {o with small=Some f}
let set_print f o = {o with print=Some f}
let set_shrink f o = {o with shrink=Some f}
let set_collect f o = {o with collect=Some f}
let set_stats s o = {o with stats=s}
let add_stat s o = {o with stats=s :: o.stats}
let set_gen g o = {o with gen=g}

let add_shrink_invariant f o = match o.shrink with
  | None -> o
  | Some shr -> {o with shrink=Some (Shrink.filter f shr)}

let get_gen o = o.gen
let gen = get_gen
let get_print o = o.print

let small1 _ = 1

let make_scalar ?print ?collect gen =
  make ~shrink:Shrink.nil ~small:small1 ?print ?collect gen
let make_int ?collect gen =
  make ~shrink:Shrink.int ~small:small1 ~print:Print.int ?collect gen

let adapt_ o gen =
  make ?print:o.print ?small:o.small ?shrink:o.shrink ?collect:o.collect gen

let choose l = match l with
  | [] -> raise (Invalid_argument "quickcheck.choose")
  | l ->
      let a = Array.of_list l in
      adapt_ a.(0)
        (fun st ->
          let arb = a.(RS.int st (Array.length a)) in
          arb.gen st)

let unit : unit arbitrary =
  make ~small:small1 ~shrink:Shrink.nil ~print:(fun _ -> "()") Gen.unit

let bool = make_scalar ~print:string_of_bool Gen.bool
let float = make_scalar ~print:string_of_float Gen.float
let pos_float = make_scalar ~print:string_of_float Gen.pfloat
let neg_float = make_scalar ~print:string_of_float Gen.nfloat

let float_bound_inclusive bound =
  make_scalar ~print:string_of_float (Gen.float_bound_inclusive bound)

let float_bound_exclusive bound =
  make_scalar ~print:string_of_float (Gen.float_bound_exclusive bound)

let float_range low high = make_scalar ~print:string_of_float (Gen.float_range low high)

let int = make_int Gen.int
let int_bound n = make_int (Gen.int_bound n)
let int_range a b = make_int (Gen.int_range a b)
let (--) = int_range
let pos_int = make_int Gen.pint
let small_int = make_int Gen.small_int
let small_nat = make_int Gen.small_nat
let small_signed_int = make_int Gen.small_signed_int
let small_int_corners () = make_int (Gen.nng_corners ())
let neg_int = make_int Gen.neg_int

let int32 =
  make ~print:(fun i -> Int32.to_string i ^ "l") ~small:small1
    ~shrink:Shrink.int32 Gen.ui32
let int64 =
  make ~print:(fun i -> Int64.to_string i ^ "L") ~small:small1
    ~shrink:Shrink.int64 Gen.ui64

let small_char target c = abs ((Char.code c) - (Char.code target))

let char =
  make ~print:(sprintf "%C") ~small:(small_char 'a') ~shrink:Shrink.char Gen.char
let printable_char =
  make ~print:(sprintf "%C") ~small:(small_char 'a') ~shrink:Shrink.char_printable Gen.printable
let numeral_char =
  make ~print:(sprintf "%C") ~small:(small_char '0') ~shrink:Shrink.char_numeral Gen.numeral

let bytes_gen_of_size size gen =
  make ~shrink:Shrink.bytes ~small:Bytes.length
    ~print:(Print.bytes) (Gen.bytes_size ~gen size)
let bytes_of gen =
  make ~shrink:Shrink.bytes ~small:Bytes.length
    ~print:(Print.bytes) (Gen.bytes ~gen)

let bytes = bytes_of Gen.char
let bytes_of_size size = bytes_gen_of_size size Gen.char
let bytes_small = bytes_gen_of_size Gen.small_nat Gen.char
let bytes_small_of gen = bytes_gen_of_size Gen.small_nat gen
let bytes_printable =
  make ~shrink:(Shrink.bytes ~shrink:Shrink.char_printable) ~small:Bytes.length
    ~print:(Print.bytes) (Gen.bytes ~gen:Gen.printable)

let string_gen_of_size size gen =
  make ~shrink:Shrink.string ~small:String.length
    ~print:(sprintf "%S") (Gen.string_size ~gen size)
let string_of gen =
  make ~shrink:Shrink.string ~small:String.length
    ~print:(sprintf "%S") (Gen.string ~gen)

let string = string_of Gen.char
let string_of_size size = string_gen_of_size size Gen.char
let string_small = string_gen_of_size Gen.small_nat Gen.char
let string_small_of gen = string_gen_of_size Gen.small_nat gen
let small_string = string_small
let string_gen = string_of

let printable_string =
  make ~shrink:(Shrink.string ~shrink:Shrink.char_printable) ~small:String.length
    ~print:(sprintf "%S") (Gen.string ~gen:Gen.printable)

let printable_string_of_size size =
  make ~shrink:(Shrink.string ~shrink:Shrink.char_printable) ~small:String.length
    ~print:(sprintf "%S") (Gen.string_size ~gen:Gen.printable size)

let small_printable_string =
  make ~shrink:(Shrink.string ~shrink:Shrink.char_printable) ~small:String.length
    ~print:(sprintf "%S") (Gen.string_size ~gen:Gen.printable Gen.small_nat)

let numeral_string =
  make ~shrink:(Shrink.string ~shrink:Shrink.char_numeral) ~small:String.length
    ~print:(sprintf "%S") (Gen.string ~gen:Gen.numeral)

let numeral_string_of_size size =
  make ~shrink:(Shrink.string ~shrink:Shrink.char_numeral) ~small:String.length
    ~print:(sprintf "%S") (Gen.string_size ~gen:Gen.numeral size)

let string_printable = printable_string
let string_printable_of_size = printable_string_of_size
let string_small_printable = small_printable_string
let string_numeral = numeral_string
let string_numeral_of_size = numeral_string_of_size

let list_sum_ f l = List.fold_left (fun acc x-> f x+acc) 0 l

let mk_list a gen =
  (* small sums sub-sizes if present, otherwise just length *)
  let small = _opt_map_or a.small ~f:list_sum_ ~d:List.length in
  let print = _opt_map a.print ~f:Print.list in
  make ~small ~shrink:(Shrink.list ?shrink:a.shrink) ?print gen

let list a = mk_list a (Gen.list a.gen)
let list_of_size size a = mk_list a (Gen.list_size size a.gen)
let small_list a = mk_list a (Gen.small_list a.gen)

let array_sum_ f a = Array.fold_left (fun acc x -> f x+acc) 0 a

let array a =
  let small = _opt_map_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:(Shrink.array ?shrink:a.shrink)
    ?print:(_opt_map ~f:Print.array a.print)
    (Gen.array a.gen)

let array_of_size size a =
  let small = _opt_map_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:(Shrink.array ?shrink:a.shrink)
    ?print:(_opt_map ~f:Print.array a.print)
    (Gen.array_size size a.gen)

let pair a b =
  make
    ?small:(_opt_map_2 ~f:(fun f g (x,y) -> f x+g y) a.small b.small)
    ?print:(_opt_map_2 ~f:Print.pair a.print b.print)
    ~shrink:(Shrink.pair (_opt_or a.shrink Shrink.nil) (_opt_or b.shrink Shrink.nil))
    (Gen.pair a.gen b.gen)

let triple a b c =
  make
    ?small:(_opt_map_3 ~f:(fun f g h (x,y,z) -> f x+g y+h z) a.small b.small c.small)
    ?print:(_opt_map_3 ~f:Print.triple a.print b.print c.print)
    ~shrink:(Shrink.triple (_opt_or a.shrink Shrink.nil)
      (_opt_or b.shrink Shrink.nil) (_opt_or c.shrink Shrink.nil))
    (Gen.triple a.gen b.gen c.gen)

let quad a b c d =
  make
    ?small:(_opt_map_4 ~f:(fun f g h i (x,y,z,w) ->
                             f x+g y+h z+i w) a.small b.small c.small d.small)
    ?print:(_opt_map_4 ~f:Print.quad a.print b.print c.print d.print)
    ~shrink:(Shrink.quad (_opt_or a.shrink Shrink.nil)
                   (_opt_or b.shrink Shrink.nil)
       (_opt_or c.shrink Shrink.nil)
       (_opt_or d.shrink Shrink.nil))
    (Gen.quad a.gen b.gen c.gen d.gen)

let tup2 a b=
  make
    ?small:(_opt_map_2 ~f:(fun a b (a', b') -> a a'+b b') a.small b.small)
    ~print:(Print.tup2_opt a.print b.print)
    ~shrink:(Shrink.pair (_opt_or a.shrink Shrink.nil) (_opt_or b.shrink Shrink.nil))
    (Gen.tup2 a.gen b.gen)

let tup3 a b c =
  make
    ?small:(_opt_map_3 ~f:(fun a b c (a', b', c') ->
        a a'+b b'+c c') a.small b.small c.small)
    ~print:(Print.tup3_opt a.print b.print c.print)
    ~shrink:(Shrink.tup3_opt a.shrink b.shrink c.shrink)
    (Gen.tup3 a.gen b.gen c.gen)

let tup4 a b c d =
  make
    ?small:(_opt_map_4 ~f:(fun a b c d (a', b', c', d') ->
        a a'+b b'+c c'+d d') a.small b.small c.small d.small)
    ~print:(Print.tup4_opt a.print b.print c.print d.print)
    ~shrink:(Shrink.tup4_opt a.shrink b.shrink c.shrink d.shrink)
    (Gen.tup4 a.gen b.gen c.gen d.gen)

let tup5 a b c d e =
  make
    ?small:(_opt_map_5 ~f:(fun a b c d e (a', b', c', d', e') ->
        a a'+b b'+c c'+d d'+e e') a.small b.small c.small d.small e.small)
    ~print:(Print.tup5_opt a.print b.print c.print d.print e.print)
    ~shrink:(Shrink.tup5_opt a.shrink b.shrink c.shrink d.shrink e.shrink)
    (Gen.tup5 a.gen b.gen c.gen d.gen e.gen)

let tup6 a b c d e f =
  make
    ?small:(_opt_map_6 ~f:(fun a b c d e f (a', b', c', d', e', f') ->
        a a'+b b'+c c'+d d'+e e'+f f') a.small b.small c.small d.small e.small f.small)
    ~print:(Print.tup6_opt a.print b.print c.print d.print e.print f.print)
    ~shrink:(Shrink.tup6_opt a.shrink b.shrink c.shrink d.shrink e.shrink f.shrink)
    (Gen.tup6 a.gen b.gen c.gen d.gen e.gen f.gen)

let tup7 a b c d e f g =
  make
    ?small:(_opt_map_7 ~f:(fun a b c d e f g (a', b', c', d', e', f', g') ->
        a a'+b b'+c c'+d d'+e e'+f f'+g g')
        a.small b.small c.small d.small e.small f.small g.small)
    ~print:(Print.tup7_opt
              a.print b.print c.print d.print e.print f.print g.print)
    ~shrink:(Shrink.tup7_opt
               a.shrink b.shrink c.shrink d.shrink e.shrink f.shrink g.shrink)
    (Gen.tup7 a.gen b.gen c.gen d.gen e.gen f.gen g.gen)

let tup8 a b c d e f g h =
  make
    ?small:(_opt_map_8 ~f:(fun a b c d e f g h (a', b', c', d', e', f', g', h') ->
        a a'+b b'+c c'+d d'+e e'+f f'+g g'+h h')
        a.small b.small c.small d.small e.small f.small g.small h.small)
    ~print:(Print.tup8_opt
              a.print b.print c.print d.print e.print f.print g.print h.print)
    ~shrink:(Shrink.tup8_opt
               a.shrink b.shrink c.shrink d.shrink e.shrink f.shrink g.shrink h.shrink)
    (Gen.tup8 a.gen b.gen c.gen d.gen e.gen f.gen g.gen h.gen)

let tup9 a b c d e f g h i =
  make
    ?small:(_opt_map_9 ~f:(fun a b c d e f g h i (a', b', c', d', e', f', g', h', i') ->
        a a'+b b'+c c'+d d'+e e'+f f'+g g'+h h'+i i')
        a.small b.small c.small d.small e.small f.small g.small h.small i.small)
    ~print:(Print.tup9_opt
              a.print b.print c.print d.print e.print f.print g.print h.print i.print)
    ~shrink:(Shrink.tup9_opt
               a.shrink b.shrink c.shrink d.shrink e.shrink f.shrink g.shrink h.shrink i.shrink)
    (Gen.tup9 a.gen b.gen c.gen d.gen e.gen f.gen g.gen h.gen i.gen)

let option ?ratio a =
  let g = Gen.opt ?ratio a.gen
  and shrink = _opt_map a.shrink ~f:Shrink.option
  and small =
    _opt_map_or a.small ~d:(function None -> 0 | Some _ -> 1)
      ~f:(fun f o -> match o with None -> 0 | Some x -> f x)
  in
  make
    ~small
    ?shrink
    ?print:(_opt_map ~f:Print.option a.print)
    g

let map ?rev f a =
  make
    ?print:(_opt_map_2 rev a.print ~f:(fun r p x -> p (r x)))
    ?small:(_opt_map_2 rev a.small ~f:(fun r s x -> s (r x)))
    ?shrink:(_opt_map_2 rev a.shrink ~f:(fun r g x -> Iter.(g (r x) >|= f)))
    ?collect:(_opt_map_2 rev a.collect ~f:(fun r f x -> f (r x)))
    (fun st -> f (a.gen st))


let fun1_unsafe : 'a arbitrary -> 'b arbitrary -> ('a -> 'b) arbitrary =
  fun a1 a2 ->
    let magic_object = Obj.magic (object end) in
    let gen : ('a -> 'b) Gen.t = fun st ->
      let h = Hashtbl.create 10 in
      fun x ->
        if x == magic_object then
          Obj.magic h
        else
          try Hashtbl.find h x
          with Not_found ->
            let b = a2.gen st in
            Hashtbl.add h x b;
            b in
    let pp : (('a -> 'b) -> string) option = _opt_map_2 a1.print a2.print ~f:(fun p1 p2 f ->
      let h : ('a, 'b) Hashtbl.t = Obj.magic (f magic_object) in
      let b = Buffer.create 20 in
      Hashtbl.iter (fun key value -> Printf.bprintf b "%s -> %s; " (p1 key) (p2 value)) h;
      "{" ^ Buffer.contents b ^ "}"
    ) in
    make
      ?print:pp
      gen

let fun2_unsafe gp1 gp2 gp3 = fun1_unsafe gp1 (fun1_unsafe gp2 gp3)

module Poly_tbl : sig
  type ('a, 'b) t

  val create: 'a Observable.t -> 'b arbitrary -> int -> ('a, 'b) t Gen.t
  val get : ('a, 'b) t -> 'a -> 'b option
  val size : ('b -> int) -> (_, 'b) t -> int
  val shrink1 : ('a, 'b) t Shrink.t
  val shrink2 : 'b Shrink.t -> ('a, 'b) t Shrink.t
  val print : (_,_) t Print.t
end = struct
  type ('a, 'b) t = {
    get : 'a -> 'b option;
    p_size: ('b->int) -> int;
    p_shrink1: ('a, 'b) t Iter.t;
    p_shrink2: 'b Shrink.t -> ('a, 'b) t Iter.t;
    p_print: unit -> string;
  }

  let create (type k)(type v) k v size st : (k,v) t =
    let module T = Hashtbl.Make(struct
        type t = k
        let equal = k.Observable.eq
        let hash = k.Observable.hash
      end) in
    let tbl_to_list tbl =
      T.fold (fun k v l -> (k,v)::l) tbl []
    and tbl_of_list l =
      let tbl = T.create (max (List.length l) 8) in
      List.iter (fun (k,v) -> T.add tbl k v) l;
      tbl
    in
    (* split random state to avoid later failed [get]s to side-effect the current [st] *)
    let st' = RS.split st in
    (* make a table
       @param extend if true, extend table on the fly *)
    let rec make ~extend tbl = {
      get=(fun x ->
        try Some (T.find tbl x)
        with Not_found ->
          if extend then (
            let v = v.gen st' in
            T.add tbl x v;
            Some v
          ) else None);
      p_print=(fun () -> match v.print with
        | None -> "<fun>"
        | Some pp_v ->
          let b = Buffer.create 64 in
          T.iter
            (fun key value ->
               Printf.bprintf b "%s -> %s; "
                 (k.Observable.print key) (pp_v value))
            tbl;
        Buffer.contents b);
      p_shrink1=(fun yield ->
        Shrink.list_spine (tbl_to_list tbl)
          (fun l ->
             yield (make ~extend:false (tbl_of_list l)))
      );
      p_shrink2=(fun shrink_val yield ->
        (* shrink bindings one by one *)
        T.iter
          (fun x y ->
             shrink_val y
               (fun y' ->
                  let tbl' = T.copy tbl in
                  T.replace tbl' x y';
                  yield (make ~extend:false tbl')))
          tbl);
      p_size=(fun size_v -> T.fold (fun _ v n -> n + size_v v) tbl 0);
    } in
    make ~extend:true (T.create size)

  let get t x = t.get x
  let shrink1 t = t.p_shrink1
  let shrink2 p t = t.p_shrink2 p
  let print t = t.p_print ()
  let size p t = t.p_size p
end

(** Internal representation of functions *)
type ('a, 'b) fun_repr_tbl = {
  fun_tbl: ('a, 'b) Poly_tbl.t;
  fun_arb: 'b arbitrary;
  fun_default: 'b;
}

type 'f fun_repr =
  | Fun_tbl : ('a, 'ret) fun_repr_tbl -> ('a -> 'ret) fun_repr
  | Fun_map : ('f1 -> 'f2) * 'f1 fun_repr -> 'f2 fun_repr

type _ fun_ =
  | Fun : 'f fun_repr * 'f -> 'f fun_

(** Reifying functions *)
module Fn = struct
  type 'a t = 'a fun_

  let apply (Fun (_,f)) = f

  let make_ (r:_ fun_repr) : _ fun_ =
    let rec call
      : type f. f fun_repr -> f
      = fun r -> match r with
        | Fun_tbl r ->
          begin fun x -> match Poly_tbl.get r.fun_tbl x with
            | None -> r.fun_default
            | Some y -> y
          end
        | Fun_map (g, r') -> g (call r')
    in
    Fun (r, call r)

  let mk_repr tbl arb def =
    Fun_tbl { fun_tbl=tbl; fun_arb=arb; fun_default=def; }

  let map_repr f repr = Fun_map (f,repr)
  let map_fun f (Fun (repr,_)) = make_ (map_repr f repr)

  let shrink_rep (r: _ fun_repr): _ Iter.t =
    let open Iter in
    let rec aux
      : type f. f fun_repr Shrink.t
      = function
        | Fun_tbl {fun_arb=a; fun_tbl=tbl; fun_default=def} ->
          let sh_v = match a.shrink with None -> Shrink.nil | Some s->s in
            (sh_v def >|= fun def' -> mk_repr tbl a def')
          <+>
            (Poly_tbl.shrink1 tbl >|= fun tbl' -> mk_repr tbl' a def)
          <+>
            (Poly_tbl.shrink2 sh_v tbl >|= fun tbl' -> mk_repr tbl' a def)
        | Fun_map (g, r') ->
          aux r' >|= map_repr g
    in
    aux r

  let shrink (Fun (rep,_)) =
    let open Iter in
    shrink_rep rep >|= make_

  let rec size_rep
    : type f. f fun_repr -> int
    = function
      | Fun_map (_, r') -> size_rep r'
      | Fun_tbl r ->
        let size_v x = match r.fun_arb.small with None -> 0 | Some f -> f x in
        Poly_tbl.size size_v r.fun_tbl + size_v r.fun_default

  let size (Fun (rep,_)) = size_rep rep

  let print_rep r =
    let buf = Buffer.create 32 in
    let rec aux
      : type f. Buffer.t -> f fun_repr -> unit
      = fun buf r -> match r with
        | Fun_map (_, r') -> aux buf r'
        | Fun_tbl r ->
          Buffer.add_string buf (Poly_tbl.print r.fun_tbl);
          Printf.bprintf buf "_ -> %s" (match r.fun_arb.print with
            | None -> "<opaque>"
            | Some s -> s r.fun_default
          );
    in
    Printf.bprintf buf "{";
    aux buf r;
    Printf.bprintf buf "}";
    Buffer.contents buf

  let print (Fun (rep,_)) = print_rep rep

  let gen_rep (a:_ Observable.t) (b:_ arbitrary): _ fun_repr Gen.t =
    fun st ->
      mk_repr (Poly_tbl.create a b 8 st) b (b.gen st)

  let gen a b = Gen.map make_ (gen_rep a b)
end

let fun1 o ret =
  make
    ~shrink:Fn.shrink
    ~print:Fn.print
    ~small:Fn.size
    (Fn.gen o ret)

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

  let gen (o:'a obs) (ret:'b arbitrary) : ('a t -> 'b) fun_ Gen.t =
    Fn.gen (observable o) ret

  module Infix = struct
    let (@::) x tail = cons x tail
    let (@->) o tail = o_cons o tail
  end
  include Infix
end

let fun_nary (o:_ Tuple.obs) ret : _ arbitrary =
  make
    ~shrink:Fn.shrink
    ~print:Fn.print
    ~small:Fn.size
    (Tuple.gen o ret)

let fun2 o1 o2 ret =
  let open Tuple in
  map
    ~rev:(Fn.map_fun (fun g (Cons (x, Cons (y,Nil))) -> g x y))
    (Fn.map_fun (fun g x y -> g (x @:: y @:: nil)))
    (fun_nary (o1 @-> o2 @-> o_nil) ret)

let fun3 o1 o2 o3 ret =
  let open Tuple in
  map
    ~rev:(Fn.map_fun (fun g (Cons (x, Cons (y, Cons (z,Nil)))) -> g x y z))
    (Fn.map_fun (fun g x y z -> g (x @:: y @:: z @:: nil)))
    (fun_nary (o1 @-> o2 @-> o3 @-> o_nil) ret)

let fun4 o1 o2 o3 o4 ret =
  let open Tuple in
  map
    ~rev:(Fn.map_fun (fun g (Cons (x, Cons (y, Cons (z,Cons (w,Nil))))) -> g x y z w))
    (Fn.map_fun (fun g x y z w -> g (x @:: y @:: z @:: w @:: nil)))
    (fun_nary (o1 @-> o2 @-> o3 @-> o4 @-> o_nil) ret)

(* Generator combinators *)

(** given a list, returns generator that picks at random from list *)
let oneofl ?print ?collect xs = make ?print ?collect (Gen.oneofl xs)
let oneofa ?print ?collect xs = make ?print ?collect (Gen.oneofa xs)

(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof l =
  let gens = List.map (fun a->a.gen) l in
  let first = List.hd l in
  let print = first.print
  and small = first.small
  and collect = first.collect
  and shrink = first.shrink in
  make ?print ?small ?collect ?shrink (Gen.oneof gens)

(** Generator that always returns given value *)
let always ?print x =
  let gen _st = x in
  make ?print gen

(** like oneof, but with weights *)
let frequency ?print ?small ?shrink ?collect l =
  let first = snd (List.hd l) in
  let small = _opt_sum small first.small in
  let print = _opt_sum print first.print in
  let shrink = _opt_sum shrink first.shrink in
  let collect = _opt_sum collect first.collect in
  let gens = List.map (fun (x,y) -> x, y.gen) l in
  make ?print ?small ?shrink ?collect (Gen.frequency gens)

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequencyl ?print ?small l = make ?print ?small (Gen.frequencyl l)
let frequencya ?print ?small l = make ?print ?small (Gen.frequencya l)

let map_same_type f a =
  adapt_ a (fun st -> f (a.gen st))

let map_keep_input ?print ?small f a =
  make
    ?print:(match print, a.print with
        | Some f1, Some f2 -> Some (Print.pair f2 f1)
        | Some f, None -> Some (Print.comap snd f)
        | None, Some f -> Some (Print.comap fst f)
        | None, None -> None)
    ?small:(match small, a.small with
        | Some f, _ -> Some (fun (_,y) -> f y)
        | None, Some f -> Some (fun (x,_) -> f x)
        | None, None -> None)
    ?shrink:(match a.shrink with
      | None -> None
      | Some s ->
        let s' (x,_) = Iter.map (fun x->x, f x) (s x) in
        Some s')
    Gen.(map_keep_input f a.gen)

module TestResult = struct
  type 'a counter_ex = 'a QCheck2.TestResult.counter_ex = {
    instance: 'a; (** The counter-example(s) *)
    shrink_steps: int; (** How many shrinking steps for this counterex *)
    msg_l: string list; (** messages. @since 0.7 *)
  }

  type 'a failed_state = 'a counter_ex list

  (** Result state.
      changed in 0.10 (move to inline records) *)
  type 'a state = 'a QCheck2.TestResult.state =
    | Success
    | Failed of {
        instances: 'a failed_state; (** Failed instance(s) *)
      }
    | Failed_other of {msg: string}
    | Error of {
        instance: 'a counter_ex;
        exn: exn;
        backtrace: string;
      } (** Error, backtrace, and instance that triggered it *)


  (* result returned by running a test *)
  type 'a t = 'a QCheck2.TestResult.t

  let get_count = QCheck2.TestResult.get_count
  let get_count_gen = QCheck2.TestResult.get_count_gen
  let get_state = QCheck2.TestResult.get_state
  let stats = QCheck2.TestResult.stats
  let collect = QCheck2.TestResult.collect
  let warnings = QCheck2.TestResult.warnings
  let is_success = QCheck2.TestResult.is_success
end

module Test = struct
  type res = QCheck2.Test.res =
    | Success
    | Failure
    | FalseAssumption
    | Error of exn * string
  type 'a event = 'a QCheck2.Test.event =
    | Generating
    | Collecting of 'a
    | Testing of 'a
    | Shrunk of int * 'a
    | Shrinking of int * int * 'a

  type 'a cell = 'a QCheck2.Test.cell
  type 'a handler = 'a QCheck2.Test.handler
  type 'a step = 'a QCheck2.Test.step
  type 'a callback = 'a QCheck2.Test.callback
  type t = QCheck2.Test.t

  include QCheck2.Test_exceptions

  let print_instance = QCheck2.Test.print_instance
  let print_c_ex = QCheck2.Test.print_c_ex
  let print_error = QCheck2.Test.print_error
  let print_fail = QCheck2.Test.print_fail
  let print_fail_other = QCheck2.Test.print_fail_other
  let print_test_fail = QCheck2.Test.print_test_fail
  let print_test_error = QCheck2.Test.print_test_error

  let set_name = QCheck2.Test.set_name
  let get_law = QCheck2.Test.get_law
  let get_name = QCheck2.Test.get_name
  let get_count = QCheck2.Test.get_count
  let get_long_factor = QCheck2.Test.get_long_factor

  let make_cell ?if_assumptions_fail
      ?count ?long_factor ?negative ?max_gen
  ?max_fail ?small:_removed_in_qcheck_2 ?retries ?name arb law
  =
  let {gen; shrink; print; collect; stats; _} = arb in
  QCheck2.Test.make_cell_from_QCheck1 ?if_assumptions_fail ?count ?long_factor ?negative ?max_gen ?max_fail ?retries ?name ~gen ?shrink ?print ?collect ~stats law

  let make' ?if_assumptions_fail ?count ?long_factor ?max_gen ?max_fail ?small ?retries ?name ~negative arb law =
    QCheck2.Test.Test (make_cell ?if_assumptions_fail ?count ?long_factor ?max_gen ?max_fail ?small ?retries ?name ~negative arb law)

  let make = make' ~negative:false
  let make_neg = make' ~negative:true

  let fail_report = QCheck2.Test.fail_report

  let fail_reportf = QCheck2.Test.fail_reportf

  let check_cell_exn = QCheck2.Test.check_cell_exn
  let check_exn = QCheck2.Test.check_exn
  let check_cell = QCheck2.Test.check_cell
end

let find_example ?(name="<example>") ?count ~f g : _ Gen.t =
  (* the random generator of examples satisfying [f]. To do that we
     test the property [fun x -> not (f x)]; any counter-example *)
  let gen st =
    let cell =
      let arb = make g in
      Test.make_cell ~max_fail:1 ?count arb (fun x -> not (f x))
    in
    let res = QCheck2.Test.check_cell ~rand:st cell in
    begin match QCheck2.TestResult.get_state res with
      | QCheck2.TestResult.Success -> raise (No_example_found name)
      | QCheck2.TestResult.Error _ -> raise (No_example_found name)
      | QCheck2.TestResult.Failed {instances=[]} -> assert false
      | QCheck2.TestResult.Failed {instances=failed::_} ->
        (* found counter-example! *)
        failed.QCheck2.TestResult.instance
      | QCheck2.TestResult.Failed_other {msg=_} ->
        raise (No_example_found name)

    end
  in
  gen

let find_example_gen ?rand ?name ?count ~f g =
  let g = find_example ?name ?count ~f g in
  Gen.generate1 ?rand g

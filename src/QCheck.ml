
(*
copyright (c) 2013, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Quickcheck inspired property-based testing} *)

module Arbitrary = struct
  type 'a t = Random.State.t -> 'a

  let return x st = x

  let int n st = Random.State.int st n

  let int_range ~start ~stop st =
    let n = stop - start in
    if n <= 0
      then 0
      else start + Random.State.int st n

  let (--) start stop = int_range ~start ~stop

  let small_int = int 100

  let split_int gen st =
    let n = gen st in
    if n > 0
      then let i = Random.State.int st (n+1) in i, n-i
      else 0, 0

  let bool = Random.State.bool

  let float f st = Random.State.float st f

  let char st = Char.chr (Random.State.int st 128)

  let alpha st =
    Char.chr (Char.code 'a' + Random.State.int st (Char.code 'z' - Char.code 'a'))

  let string_len len st =
    let n = len st in
    assert (n>=0);
    let b = Buffer.create n in
    for _i = 0 to n-1 do
      Buffer.add_char b (alpha st)
    done;
    Buffer.contents b

  let string st = string_len (int 10) st

  let map ar f st = f (ar st)

  let map' f ar st = f (ar st)

  let rec _make_list ar st acc n =
    if n = 0 then acc else
      let x = ar st in
      _make_list ar st (x::acc) (n-1)

  let list ?(len=int 10) ar st =
    let n = len st in
    _make_list ar st [] n

  let opt ar st =
    if Random.State.bool st
      then Some (ar st)
      else None

  let list_repeat len ar st =
    _make_list ar st [] len

  let array ?(len=int 10) ar st =
    let n = len st in
    Array.init n (fun _ -> ar st)

  let array_repeat n ar st =
    Array.init n (fun _ -> ar st)

  let among_array a st =
    if Array.length a < 1
      then failwith "Arbitrary.among: cannot choose in empty array ";
    let i = Random.State.int st (Array.length a) in
    a.(i)

  let shuffle a st =
    for i = Array.length a-1 downto 1 do
      let j = Random.State.int st (i+1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
    done

  let among l =
    if List.length l < 1
      then failwith "Arbitrary.among: cannot choose in empty list";
    among_array (Array.of_list l)

  let choose l = match l with
    | [] -> failwith "cannot choose from empty list"
    | [x] -> x
    | _ ->
      let a = Array.of_list l in
      fun st ->
        let i = Random.State.int st (Array.length a) in
        a.(i) st

  let (|||) a b st =
    if Random.State.bool st
      then a st
      else b st

  let fix ?(max=15) ~base f =
    let rec ar = lazy
      (fun depth st ->
        if depth >= max || Random.State.int st max < depth
          then base st (* base case. The deeper, the more likely. *)
        else  (* recurse *)
          let ar' = Lazy.force ar (depth+1) in
          f ar' st)
    in
    Lazy.force ar 0

  let fix_depth ~depth ~base f st =
    let max = depth st in
    fix ~max ~base f st

  (* split fuel into two parts *)
  let split_fuel fuel st =
    let i = Random.State.int st (fuel+1) in
    i, fuel-i

  type 'a recursive_case =
    [ `Base of 'a t
    | `Base_fuel of (int -> 'a t)
    | `Rec of ((int -> 'a list t) -> 'a t)
    | `Rec_fuel of ((int -> 'a list t) -> int -> 'a t)
    | `Rec1 of ('a t -> 'a t)
    | `Rec2 of ('a t -> 'a t -> 'a t)
    ]

  let split_fuel_n n fuel st =
    if fuel<0 || n < 1 then invalid_arg "split_int_n";
    let rec split_rec n fuel acc = match n with
      | 0 -> assert false
      | 1 -> fuel::acc
      | _ ->
          let left, right = split_fuel fuel st in
          (* divide and conquer *)
          let acc = split_rec (n/2) left acc in
          let acc = split_rec (n - n/2) right acc in
          acc
    in
    let l = split_rec n fuel [] in
    assert (List.length l = n);
    l

  exception RecursiveCallFailed

  let fail_fix() = raise RecursiveCallFailed

  let fix_fuel l =
    assert (l<>[]);
    let a = Array.of_list l in
    (* fixpoint. Each element of [l] can ask for a given number of sub-cases
      but only ONCE. *)
    let rec fix fuel st =
      shuffle a st;
      first fuel 0 st
    (* try each possibility. Each possibility must call exactly once
      [fix] recursively on [n] cases ([n=0] for base cases) *)
    and first fuel i st =
      if i=Array.length a then raise RecursiveCallFailed
      else
        let fix' =
          let is_first=ref true in
          fun num st ->
            if not !is_first
              then failwith "fix_fuel: sub_case can be called only once";
            is_first := false;

            match fuel, num with
            | 0, 0 -> []
            | _, 0 -> raise RecursiveCallFailed (* didn't consume enough *)
            | 0, _ -> raise RecursiveCallFailed (* not enough fuel *)
            | _ ->
                (* split fuel for subcases *)
                assert (fuel>0);
                (* if num>=fuel then raise RecursiveCallFailed; *)
                let fuels = split_fuel_n num (fuel-1) st in
                List.map (fun f -> fix f st) fuels
        in
        try
          match a.(i) with
          | `Base f when fuel=0 -> f st
          | `Base _ -> raise RecursiveCallFailed (* didn't consume enough *)
          | `Base_fuel f -> f fuel st (* yield *)
          | `Rec f -> f fix' st
          | `Rec_fuel f -> f fix' (fuel-1) st
          | `Rec1 _ when fuel=0 -> raise RecursiveCallFailed
          | `Rec1 f -> f (fix (fuel-1)) st
          | `Rec2 _ when fuel<2 -> raise RecursiveCallFailed
          | `Rec2 f ->
              let fuel1, fuel2 = split_fuel (fuel-1) st in
              f (fix fuel1) (fix fuel2) st
        with RecursiveCallFailed ->
          first fuel (i+1) st  (* try next *)
    in
    fun fuel st ->
      try Some (fix fuel st)
      with RecursiveCallFailed -> None

  type ('a, 'state) general_recursive_case =
    [ `Base of ('state -> 'a t)
    | `Base_fuel of (int -> 'state -> 'a t)
    | `Rec of ((int -> ('state -> 'a) list t) -> 'state -> 'a t)
    | `Rec_fuel of ((int -> ('state -> 'a) list t) -> int -> 'state -> 'a t)
    | `Rec1 of (('state -> 'a t) -> 'state -> 'a t)
    | `Rec2 of (('state -> 'a t) -> ('state -> 'a t) -> 'state -> 'a t)
    ]

  let fix_fuel_gen (l:('a,'state) general_recursive_case list) =
    assert (l<>[]);
    let a = Array.of_list l in
    (* fixpoint. Each element of [l] can ask for a given number of sub-cases
      but only ONCE. *)
    let rec fix fuel state st =
      shuffle a st;
      first fuel state 0 st
    (* try each possibility. Each possibility must call exactly once
      [fix] recursively on [n] cases ([n=0] for base cases) *)
    and first fuel state i st =
      if i=Array.length a then raise RecursiveCallFailed
      else
        let fix' =
          let is_first=ref true in
          fun num st ->
            if not !is_first
              then failwith "fix_fuel: sub_case can be called only once";
            is_first := false;

            match fuel, num with
            | 0, 0 -> []
            | _, 0 -> raise RecursiveCallFailed (* didn't consume enough *)
            | 0, _ -> raise RecursiveCallFailed (* not enough fuel *)
            | _ ->
                (* split fuel for subcases *)
                assert (fuel>0);
                if num >= fuel then raise RecursiveCallFailed;
                let fuels = split_fuel_n num (fuel-1) st in
                List.map (fun f state -> fix f state st) fuels
        in
        try
          match a.(i) with
          | `Base f when fuel=0 -> f state st
          | `Base _ -> raise RecursiveCallFailed (* didn't consume enough *)
          | `Base_fuel f -> f fuel state st (* yield *)
          | `Rec f -> f fix' state st
          | `Rec_fuel f -> f fix' fuel state st
          | `Rec1 _ when fuel=0 -> raise RecursiveCallFailed
          | `Rec1 f -> f (fix (fuel-1)) state st
          | `Rec2 _ when fuel<2 -> raise RecursiveCallFailed
          | `Rec2 f ->
              let fuel1, fuel2 = split_fuel (fuel-1) st in
              f (fix fuel1) (fix fuel2) state st
        with RecursiveCallFailed ->
          first fuel state (i+1) st  (* try next *)
    in
    fun fuel state st ->
      try Some (fix fuel state st)
      with RecursiveCallFailed -> None

  let rec retry gen st = match gen st with
    | None -> retry gen st
    | Some x -> x

  let lift f a st = f (a st)

  let lift2 f a b st = f (a st) (b st)

  let lift3 f a b c st = f (a st) (b st) (c st)

  let lift4 f a b c d st = f (a st) (b st) (c st) (d st)

  let pair a b = lift2 (fun x y -> x,y) a b

  let triple a b c = lift3 (fun x y z -> x,y,z) a b c

  let quad a b c d = lift4 (fun x y z w -> x,y,z,w) a b c d

  let (>>=) a f st =
    let x = a st in
    f x st

  let (>|=) a f st = f (a st)

  let (<*>) f x st = f st (x st)

  let pure x _st = x

  let generate ?(n=100) ?(rand=Random.State.make_self_init()) gen =
    let l = ref [] in
    for i = 0 to n-1 do
      l := (gen rand) :: !l
    done;
    !l
end

(** {2 Pretty printing} *)

module PP = struct
  type 'a t = 'a -> string

  let int = string_of_int
  let bool = string_of_bool
  let float = string_of_float
  let string s = s
  let char c = String.make 1 c

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
end

(** {2 Testing} *)

module Prop = struct
  type 'a t = 'a -> bool

  exception PrecondFail

  let assume p =
    if not p then raise PrecondFail

  let assume_lazy (lazy p) =
    if not p then raise PrecondFail

  let raises ~e ~f ~x =
    try
      f x;
      false
    with e' ->
      e = e'

  let (==>) a b =
    fun x ->
      assume (a x);
      b x

  let (&&&) a b x = a x && b x

  let (|||) a b x = a x || b x

  let (!!!) a x = not (a x)
end

type 'a result =
  | Ok of int * int  (* total number / precond failed *)
  | Failed of 'a list
  | Error of 'a option * exn

(* random seed, for repeatability of tests *)
let __seed = [| 89809344; 994326685; 290180182 |]

let check ?(call=fun _ _ -> ()) ?(rand=Random.State.make __seed) ?(n=100) gen prop =
  let precond_failed = ref 0 in
  let failures = ref [] in
  let inst = ref None in
  try
    for i = 0 to n - 1 do
      let x = gen rand in
      inst := Some x;
      try
        let res = prop x in
        call x res;
        if not res
          then failures := x :: !failures
      with Prop.PrecondFail ->
        incr precond_failed
    done;
    match !failures with
    | [] -> Ok (n, !precond_failed)
    | _ -> Failed (!failures)
  with e ->
    Error (!inst, e)

(** {2 Main} *)

type 'a test_cell = {
  n : int;
  pp : 'a PP.t option;
  prop : 'a Prop.t;
  gen : 'a Arbitrary.t;
  name : string option;
  limit : int;
  size : ('a -> int) option;
}
type test =
  | Test : 'a test_cell -> test
  (** GADT needed for the existential type *)

let name (Test {name; _}) = name

let display_name {name; _} =
  match name with
  | None -> "<anon prop>"
  | Some n -> n

let mk_test ?(n=100) ?pp ?name ?size ?(limit=10) gen prop =
  if limit < 0 then failwith "QCheck: limit needs be >= 0";
  if n <= 0 then failwith "QCheck: n needs be >= 0";
  Test { prop; gen; name; n; pp; size; limit; }

(* tail call version of take, that returns (at most) [n] elements of [l] *)
let rec _list_take acc l n = match l, n with
  | _, 0
  | [], _ -> List.rev acc
  | x::l', _ -> _list_take (x::acc) l' (n-1)

let run ?(verbose=false) ?(out=stdout) ?(rand=Random.State.make __seed) (Test test) =
  Printf.fprintf out "testing property %s...\n" (display_name test);
  (* called on each test case *)
  let call x res =
    match test.pp, verbose with
    | Some pp, true ->
      Printf.fprintf out "  test case (%s): %s\n"
        (if res then "ok" else "failed") (pp x)
    | _ -> ()
  in
  match check ~call ~rand ~n:test.n test.gen test.prop with
  | Ok (n, prefail) ->
    Printf.fprintf out "  [✔] passed %d tests (%d preconditions failed)\n" n prefail;
    true
  | Failed l ->
    begin match test.pp with
    | None -> Printf.fprintf out "  [×] %d failures over %d\n" (List.length l) test.n
    | Some pp ->
      Printf.fprintf out "  [×] %d failures over %d (print at most %d):\n"
        (List.length l) test.n test.limit;
      let to_print = match test.size with
      | None -> l
      | Some size ->
        (* sort by increasing size *)
        let l = List.map (fun x -> x, size x) l in
        let l = List.sort (fun (x,sx) (y,sy) -> sx - sy) l in
        List.map fst l
      in
      (* only keep [limit] counter examples *)
      let to_print = _list_take [] to_print test.limit in
      (* print the counter examples *)
      List.iter
        (fun x -> Printf.fprintf out "  %s\n" (pp x))
        to_print
    end;
    false
  | Error (inst, e) ->
    begin match inst, test.pp with
    | _, None
    | None, _ -> Printf.fprintf out "  [×] error: %s\n" (Printexc.to_string e);
    | Some x, Some pp ->
      (* print instance on which the error occurred *)
      Printf.fprintf out "  [×] error on instance %s: %s\n"
        (pp x) (Printexc.to_string e);
    end;
    false

type suite = test list

let flatten = List.flatten

let run_tests ?(verbose=false) ?(out=stdout) ?(rand=Random.State.make __seed) l =
  let start = Unix.gettimeofday () in
  let n = List.length l in
  let failed = ref 0 in
  Printf.fprintf out "check %d properties...\n" (List.length l);
  List.iter
    (fun test ->
      let res = run ~verbose ~out ~rand test in
      flush out;
      if not res then incr failed)
    l;
  Printf.fprintf out "tests run in %.2fs\n" (Unix.gettimeofday() -. start);
  if !failed = 0
    then Printf.fprintf out "[✔] Success! (passed %d tests)\n" n
    else Printf.fprintf out "[×] Failure. (%d tests failed over %d)\n" !failed n;
  !failed = 0

let run_main ?(argv=Sys.argv) l =
  let verbose = ref false in
  let seed = ref (Random.self_init (); Random.int (1 lsl 29)) in
  let opts = Arg.align
      [ "-v", Arg.Set verbose, " verbose"
      ; "-seed", Arg.Set_int seed, " set random seed"
      ]
  in
  try
    Arg.parse_argv argv opts (fun _ -> invalid_arg "no arguments accepted") "usage: ./tests";
    Printf.printf "use random seed %d\n" !seed;
    let rand = Random.State.make [| !seed |] in
    let ok = run_tests ~verbose:!verbose ~rand l in
    if ok then ()
    else exit 1
  with Arg.Help msg ->
    print_endline msg

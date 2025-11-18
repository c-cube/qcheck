(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot,
Jan Midtgaard, Julien Debon, Valentin Chaboche
all rights reserved.
*)

(** {1 Quickcheck inspired property-based testing} *)

(** {1 Introduction}

    The library takes inspiration from Haskell's QuickCheck library. The
    rough idea is that the programmer describes invariants that values of
    a certain type need to satisfy ("properties"), as functions from this type
    to [bool]. The programmer also needs to describe how to generate random values of the type,
    so that the property is tried and checked on a number of random instances.

    This explains the organization of this module:

    - {{!section:arbitrary}The ['a arbitrary] record type} describes how to generate random values,
      shrink them (reduce counter-examples to a minimum), print them, etc.
      It is the generator type expected by {!Test.make}.

    - Auxiliary modules such as {!Gen}, {!Print}, and {!Shrink} can be used along with {!make}
      to build custom generators.

    - {!Test} is used to describe a single test, that is, a property of
      type ['a -> bool] combined with an ['a arbitrary] that is used to generate
      the test cases for this property. Optional parameters
      allow to specify the random generator state, number of instances to generate
      and test, etc.


    {1 Examples}

    - List.rev is involutive:

    {[

      let test =
        QCheck.(Test.make ~count:1000
                  (list int) (fun l -> List.rev (List.rev l) = l));;

      QCheck.Test.check_exn test;;
    ]}

    - Not all lists are sorted (false property that will fail. The 15 smallest
      counter-example lists will be printed):

    {[
      let test = QCheck.(
          Test.make
            ~count:10_000 ~max_fail:3
            (list small_nat)
            (fun l -> l = List.sort compare l));;
      QCheck.Test.check_exn test;;
    ]}


    - generate 20 random trees using {! Gen.fix} :

    {[
      type tree = Leaf of int | Node of tree * tree

      let leaf x = Leaf x
      let node x y = Node (x,y)

      let g = QCheck.Gen.(sized @@ fix
                            (fun self n -> match n with
                               | 0 -> map leaf nat
                               | n ->
                                 frequency
                                   [1, map leaf nat;
                                    2, map2 node (self (n/2)) (self (n/2))]
                            ))

          Gen.generate ~n:20 g;;
    ]}

    More complex and powerful combinators can be found in Gabriel Scherer's
    {{:https://github.com/gasche/random-generator}[Generator]} module. Its documentation can be found
    {{:http://gasche.github.io/random-generator/doc/Generator.html } here}.
*)

(** {1 Assumptions} *)

val (==>) : bool -> bool -> bool
(** [b1 ==> b2] is the logical implication [b1 => b2]
    ie [not b1 || b2] (except that it is strict and will interact
    better with {!Test.check_exn} and the likes, because they will know
    the precondition was not satisfied.).

    {b WARNING}: this function should only be used in a property
    (see {!Test.make}), because it raises a special exception in case of
    failure of the first argument, to distinguish between failed test
    and failed precondition. Because of OCaml's evaluation order,
    both [b1] and [b2] are always evaluated; if [b2] should only be
    evaluated when [b1] holds, see {!assume}.
*)

val assume : bool -> unit
(** [assume cond] checks the precondition [cond], and does nothing
    if [cond=true]. If [cond=false], it interrupts the current test.

    {b WARNING} This function, like {!(==>)}, should only be used in
    a test, not outside.
    Example:
    {[
      Test.make (list int) (fun l ->
          assume (l <> []);
          List.hd l :: List.tl l = l)
    ]}

    @since 0.5.1
*)

val assume_fail : unit -> 'a
(** [assume_fail ()] is like [assume false], but can take any type
    since we know it always fails (like [assert false]).
    This is useful to ignore some branches in [if] or [match].

    Example:
    {[
      Test.make (list int) (function
          | [] -> assume_fail ()
          | _::_ as l -> List.hd l :: List.tl l = l)
    ]}

    @since 0.5.1
*)

(** {1 Generate Random Values} *)

module Gen : sig
  (** The [Gen] module offers combinators to build custom generators.
      Unlike the {{!section:arbitrary}the ['a arbitrary] record type},
      which comes with printers, shrinkers, etc. {!Gen.t} represents
      a type for generation only. *)

  type 'a t = Random.State.t -> 'a
  (** A random generator for values of type 'a. *)

  type 'a sized = int -> Random.State.t -> 'a
  (** Random generator with a size bound. *)

  val return : 'a -> 'a t
  (** Create a constant generator. *)

  val pure : 'a -> 'a t
  (** Synonym for {!return}
      @since 0.8 *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind for writing dependent generators. First generates an ['a] and then
      passes it to the given function, to generate a ['b]. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Infix operator for composing a function generator and an argument generator
      into a result generator. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f g] transforms a generator [g] by applying [f] to each generated element. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f g1 g2] transforms two generators [g1] and [g2] by applying [f] to each
      pair of generated elements. *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** [map3 f g1 g2 g3] transforms three generators [g1], [g2], and [g3] by applying [f]
      to each triple of generated elements. *)

  val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t
  (** [map4 f g1 g2 g3 g4] transforms four generators [g1], [g2], [g3], and [g4]
      by applying [f] to each quadruple of generated elements.

      @since 0.25 *)

  val map5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  (** [map5 f g1 g2 g3 g4 g5] transforms five generators [g1], [g2], [g3], [g4],
      and [g5] by applying [f] to each quintuple of generated elements.

      @since 0.25 *)

  val map_keep_input : ('a -> 'b) -> 'a t -> ('a * 'b) t
  (** [map_keep_input f g] transforms a generator [g] by applying [f] to each generated element.
      Returns both the generated element from [g] and the output from [f]. *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** An infix synonym for {!map}. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** An infix synonym for {!map}
      @since 0.13 *)

  val oneof : 'a t list -> 'a t
  (** Constructs a generator that selects among a given list of generators.
      @raise Invalid_argument or Failure if list is empty *)

  val oneofl : 'a list -> 'a t
  (** Constructs a generator that selects among a given list of values.
      @raise Invalid_argument or Failure if list is empty *)

  val oneofa : 'a array -> 'a t
  (** Constructs a generator that selects among a given array of values.
      @raise Invalid_argument or Failure if list is empty *)

  val frequency : (int * 'a t) list -> 'a t
  (** Constructs a generator that selects among a given list of generators.
      Each of the given generators are chosen based on a positive integer weight. *)

  val frequencyl : (int * 'a) list -> 'a t
  (** Constructs a generator that selects among a given list of values.
      Each of the given values are chosen based on a positive integer weight. *)

  val frequencya : (int * 'a) array -> 'a t
  (** Constructs a generator that selects among a given array of values.
      Each of the array entries are chosen based on a positive integer weight. *)

  val shuffle_a : 'a array -> unit t
  (** Shuffles the array in place. *)

  val shuffle_l : 'a list -> 'a list t
  (** Creates a generator of shuffled lists. *)

  val shuffle_w_l : (int * 'a) list -> 'a list t
  (** Creates a generator of weighted shuffled lists. A given list is shuffled on each
      generation according to the weights of its elements. An element with a larger weight
      is more likely to be at the front of the list than an element with a smaller weight.
      If we want to pick random elements from the (head of) list but need to prioritize
      some elements over others, this generator can be useful.

      Example: given a weighted list [[1, "one"; 5, "five"; 10, "ten"]], the generator is
      more likely to generate [["ten"; "five"; "one"]] or [["five"; "ten"; "one"]] than
      [["one"; "ten"; "five"]] because "ten" and "five" have larger weights than "one".

      @since 0.11
  *)

  val range_subset : size:int -> int -> int -> int array t
  (** [range_subset ~size:k low high] generates an array of length [k]
      of sorted distinct integers in the range [low..high] (included).

      Complexity O(k log k), drawing [k] random integers.

      @raise Invalid_argument outside the valid region [0 <= k <= high-low+1].

      @since 0.18
  *)

  val array_subset : int -> 'a array -> 'a array t
  (** [array_subset k arr] generates a sub-array of [k] elements
      at distinct positions in the input array [arr],
      in the same order.

      Complexity O(k log k), drawing [k] random integers.

      @raise Invalid_argument outside the valid region
      [0 <= size <= Array.length arr].

      @since 0.18
  *)

  (** {3 Primitive generators} *)

  val unit : unit t (** The unit generator. *)

  val bool : bool t (** The boolean generator. *)

  val float : float t   (** Generates floating point numbers. *)

  val float_pos : float t (** Generates positive floating point numbers (0. included). *)

  val float_neg : float t (** Generates negative floating point numbers (-0. included). *)

  val pfloat : float t
  (** Generates positive floating point numbers (0. included).
      @deprecated use {!float_pos} instead. *)

  val nfloat : float t
  (** Generates negative floating point numbers (-0. included).
      @deprecated use {!float_neg} instead. *)

  val float_bound_inclusive : float -> float t
  (** [float_bound_inclusive bound] returns a random floating-point number between 0 and
      [bound] (inclusive).  If [bound] is negative, the result is negative or zero.  If
      [bound] is 0, the result is 0.
      @since 0.11 *)

  val float_bound_exclusive : float -> float t
  (** [float_bound_exclusive bound] returns a random floating-point number between 0 and
      [bound] (exclusive).  If [bound] is negative, the result is negative or zero.
      @raise Invalid_argument if [bound] is zero.
      @since 0.11 *)

  val float_range : float -> float -> float t
  (** [float_range low high] generates floating-point numbers within [low] and [high] (inclusive)
      @raise Invalid_argument if [high < low] or if the range is larger than [max_float].
      @since 0.11 *)

  val (--.) : float -> float -> float t
  (** Synonym for [float_range]
      @since 0.11 *)

  val float_exp : float -> float t
  (** [float_exp m] generates floating-point numbers following an exponential
      distribution with a mean of [m].
      @raise Invalid_argument if [m] is NaN.
      @since NEXT_RELEASE *)

  val exponential : float -> float t
  (** Synonym for {!float_exp}.
      @since 0.23 *)

  val nat : int t (** Generates small natural numbers. *)

  val big_nat : int t
  (** Generates natural numbers, possibly large.
      @since 0.10 *)

  val neg_int : int t
  (** Generates non-strictly negative integers (0 included). *)

  val pint : int t (** Generates non-strictly positive integers uniformly (0 included). *)

  val int : int t (** Generates integers uniformly. *)

  val small_nat : int t
  (** Small integers (< 100)
      @since 0.5.1 *)

  val small_int : int t
  (** Small UNSIGNED integers, for retrocompatibility.
      @deprecated use {!small_nat}. *)

  val small_signed_int : int t
  (** Small SIGNED integers, based on {!small_nat}.
      @since 0.5.2 *)

  val int_bound : int -> int t
  (** Uniform integer generator producing integers between [0] and [bound]
      (inclusive).
      For [bound < 2^{30} - 1] uses [Random.State.int] for integer generation.
      @raise Invalid_argument if the argument is negative. *)

  val int_range : int -> int -> int t
  (** Uniform integer generator producing integers within [low,high] (inclusive).
      @raise Invalid_argument if [low > high]. *)

  val graft_corners : 'a t -> 'a list -> unit -> 'a t
  (** [graft_corners gen l ()] makes a new generator that enumerates
      the corner cases in [l] and then behaves like [g].

      Note that [graft_corners gen l ()] is stateful, meaning that once the
      elements of [l] have been emitted, subsequent calls will not reproduce
      them. It is therefore recommended that separate tests each use a fresh
      generator.

      @since 0.6 *)

  val int_pos_corners : int list
  (** Non-negative corner cases for int.
      @since 0.6 *)

  val int_corners : int list
  (** All corner cases for int.
      @since 0.6 *)

  val (--) : int -> int -> int t (** Synonym for {!int_range}. *)

  val int32 : int32 t
  (** Generates [int32] values uniformly.
      @since 0.24 *)

  val int64 : int64 t
  (** Generates [int64] values uniformly.
      @since 0.24 *)

  val ui32 : int32 t
  (** Generates [int32] values.
      @deprecated use {!val:int32} instead, the name is wrong, values {i are} signed.
  *)

  val ui64 : int64 t
  (** Generates [int64] values.
      @deprecated use {!val:int64} instead, the name is wrong, values {i are} signed.
  *)

  val list : 'a t -> 'a list t
  (** Builds a list generator from an element generator. List size is generated by {!nat}. *)

  val list_size : int t -> 'a t -> 'a list t
  (** Builds a list generator from a (non-negative) size generator and an element generator. *)

  val list_repeat : int -> 'a t -> 'a list t
  (** [list_repeat i g] builds a list generator from exactly [i] elements generated by [g]. *)

  val array : 'a t -> 'a array t
  (** Builds an array generator from an element generator. Array size is generated by {!nat}. *)

  val array_size : int t -> 'a t -> 'a array t
  (** Builds an array generator from a (non-negative) size generator and an element generator. *)

  val array_repeat : int -> 'a t -> 'a array t
  (** [array_repeat i g] builds an array generator from exactly [i] elements generated by [g]. *)

  val option : ?ratio:float -> 'a t -> 'a option t
  (** An option generator, with optional ratio.
      @param ratio a float between [0.] and [1.] indicating the probability of a sample to be [Some _]
      rather than [None].

      @since 0.19 (renamed from [opt])
  *)

  val opt : ?ratio:float -> 'a t -> 'a option t
  (** [opt] is an alias of {!val:option} for backward compatibility.

      @since 0.18 ([?ratio] parameter)
  *)

  val result : ?ratio:float -> 'a t -> 'e t -> ('a, 'e) result t
  (** A result generator, with optional ratio.
      @param ratio a float between [0.] and [1.] indicating the probability of a sample to be [Ok _]
      rather than [Error _].

      @since 0.24
  *)

  val char : char t
  (** Generates characters upto character code 255. *)

  val char_printable : char t
  (** Generates printable ascii characters - either '\n' or in the range 32 to 126, inclusive.
      @since NEXT_RELEASE *)

  val printable : char t
  (** Synonym for {!char_printable}. *)

  val char_numeral : char t
  (** Generates numeral characters uniformly distributed over ['0'..'9'].
      @since NEXT_RELEASE *)

  val numeral : char t
  (** Synonym for {!char_numeral}. *)

  val char_range : char -> char -> char t
  (** Generates chars between the two bounds, inclusive.
      Example: [char_range 'a' 'z'] for all lower case ascii letters.
      @since 0.13 *)

  val bytes_size : ?gen:char t -> int t -> bytes t
  (** Builds a bytes generator from a (non-negative) size generator.
      Accepts an optional character generator (the default is {!char}).
      @since 0.20 *)

  val bytes_size_of : int t -> char t -> bytes t
  (** Builds a bytes generator from a (non-negative) size generator
      and a character generator.
      @since NEXT_RELEASE *)

  val bytes : bytes t
  (** Builds a bytes generator. Bytes size is generated by {!nat}
      and characters are generated by {!char}.

      Note: This had an optional [?gen] parameter removed in NEXT_RELEASE.
      Use {!bytes_of} instead to pass a [char] generator.
      @since 0.20 *)

  val bytes_of : char t -> bytes t
  (** Builds a bytes generator using the given character generator.
      @since 0.20 *)

  val bytes_printable : bytes t
  (** Generator using the {!printable} character generator.
      @since 0.20 *)

  val bytes_small : bytes t
  (** Builds a bytes generator using the {!char} character generator, length is {!small_nat}
      @since 0.20 *)

  val bytes_small_of : char t -> bytes t
  (** Builds a bytes generator using the given character generator, length is {!small_nat}.
      @since 0.20 *)

  val string_size : ?gen:char t -> int t -> string t
  (** Builds a string generator from a (non-negative) size generator.
      Accepts an optional character generator (the default is {!char}). *)

  val string_size_of : int t -> char t -> string t
  (** Builds a string generator from a (non-negative) size generator
      and a character generator.
      @since NEXT_RELEASE *)

  val string : string t
  (** Builds a string generator. String size is generated by {!nat}
      and characters are generated by {!char}.

      Note: This had an optional [?gen] parameter removed in NEXT_RELEASE.
      Use {!string_of} instead to pass a [char] generator. *)

  val string_of : char t -> string t
  (** Builds a string generator using the given character generator.
      @since 0.11 *)

  val string_readable : string t
  (** Builds a string generator using the {!printable} character generator.
      @since 0.11
      @deprecated use {!string_printable} *)
  [@@deprecated "see string_printable"]

  val string_printable : string t
  (** Builds a string generator using the {!printable} character generator.
      @since 0.18 *)

  val small_string : ?gen:char t -> string t
  (** Builds a string generator, length is {!small_nat}
      Accepts an optional character generator (the default is {!char}).
      @deprecated use {!string_small} *)

  val string_small : string t
  (** Builds a string generator using the {!char} character generator, length is {!small_nat}
      @since 0.20 *)

  val string_small_of : char t -> string t
  (** Builds a string generator using the given character generator, length is {!small_nat}.
      @since 0.20 *)

  val small_list : 'a t -> 'a list t
  (** Generates lists of small size (see {!small_nat}).
      @since 0.5.3 *)

  val flatten_l : 'a t list -> 'a list t
  (** Generate a list of elements from individual generators
      @since 0.13 *)

  val flatten_a : 'a t array -> 'a array t
  (** Generate an array of elements from individual generators
      @since 0.13 *)

  val flatten_opt : 'a t option -> 'a option t
  (** Generate an option from an optional generator
      @since 0.13 *)

  val flatten_res : ('a t, 'e) result -> ('a,'e) result t
  (** Generate a result from [Ok g], an error from [Error e]
      @since 0.13 *)

  val array_small : 'a t -> 'a array t
  (** Generates arrays of small size (see {!small_nat}).
      @since NEXT_RELEASE *)

  val small_array : 'a t -> 'a array t
  (** Generates arrays of small size (see {!small_nat}).
      @since 0.10
      @deprecated use {!array_small} instead. *)

  (** {3 Tuple generators}

         Create tuple generators by composing individual element generators. For example,
         [Gen.(tup3 int char bool)] creates a [(int * char * bool)] triple generator
         by composing the [int], [char], and [bool] generators.
  *)

  val pair : 'a t -> 'b t -> ('a * 'b) t (** Generates pairs. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t (** Generates triples. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Generates quadruples.
      @since 0.5.1 *)

  val tup2 : 'a t -> 'b t -> ('a * 'b) t
  (** Combines two generators into a 2-tuple generator. *)

  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Combines three generators into a 3-tuple generator. *)

  val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Combines four generators into a 4-tuple generator. *)

  val tup5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  (** Combines five generators into a 5-tuple generator. *)

  val tup6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t
  (** Combines six generators into a 6-tuple generator. *)

  val tup7 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
  (** Combines seven generators into a 7-tuple generator. *)

  val tup8 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
  (** Combines eight generators into an 8-tuple generator. *)

  val tup9 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t -> 'i t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
  (** Combines nine generators into a 9-tuple generator. *)

  val join : 'a t t -> 'a t
  (** Collapses a generator of generators to simply a generator.
      @since 0.5 *)

  val sized : 'a sized -> 'a t
  (** Creates a generator from a size-bounded generator by first
      generating a size using {!nat} and passing the result to the size-bounded generator. *)

  val sized_size : int t -> 'a sized -> 'a t
  (** Creates a generator from a size-bounded generator by first
      generating a size using the integer generator and passing the result
      to the size-bounded generator.
      @since 0.5 *)

  val fix : (('a -> 'b t) -> ('a -> 'b t)) -> 'a -> 'b t
  (** Parametrized fixpoint combinator for generating recursive values.

      The fixpoint is parametrized over an arbitrary state ('a), and the
      fixpoint computation may change the value of this state in the recursive
      calls.

      In particular, this can be used for size-bounded generators ('a is int).
      The passed size-parameter should decrease to ensure termination. *)

  (** Example:
      {[
        type tree = Leaf of int | Node of tree * tree

        let leaf x = Leaf x
        let node x y = Node (x,y)

        let g = QCheck.Gen.(sized @@ fix
                              (fun self n -> match n with
                                 | 0 -> map leaf nat
                                 | n ->
                                   frequency
                                     [1, map leaf nat;
                                      2, map2 node (self (n/2)) (self (n/2))]
                              ))

      ]}

  *)

  val nat_split2 : int -> (int * int) t
  (** [nat_split2 n] generates pairs [(n1, n2)] of natural numbers
      with [n1 + n2 = n].

      This is useful to split sizes to combine sized generators.

      @since 0.18
  *)

  val pos_split2 : int -> (int * int) t
  (** [pos_split2 n] generates pairs [(n1, n2)] of strictly positive
      (nonzero) natural numbers with [n1 + n2 = n].

      @raise Invalid_argument unless [n >= 2].

      This is useful to split sizes to combine sized generators.

      @since 0.18
  *)

  val nat_split : size:int -> int -> int array t
  (** [nat_split ~size:k n] generates [k]-sized arrays [n1,n2,..nk]
      of natural numbers in [[0;n]] with [n1 + n2 + ... + nk = n].

      This is useful to split sizes to combine sized generators.

      Complexity O(k log k).

      @since 0.18
  *)

  val pos_split : size:int -> int -> int array t
  (** [pos_split ~size:k n] generates [k]-sized arrays [n1,n2,..nk]
      of strictly positive (non-zero) natural numbers with
      [n1 + n2 + ... + nk = n].

      This is useful to split sizes to combine sized generators.

      Complexity O(k log k).

      @raise Invalid_argument unless [0 < k <= n] or [0 = k = n].

      @since 0.18
  *)

  val delay : (unit -> 'a t) -> 'a t
  (** Delay execution of some code until the generator is actually called.
      This can be used to manually implement recursion or control flow
      in a generator.
      @since 0.17 *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!map}. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!(>>=)}. *)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}. *)


  (** {3 Debug generators}

      These functions should not be used in tests: they are provided
      for convenience to debug/investigate what values a generator produces.
  *)

  val generate : ?rand:Random.State.t -> n:int -> 'a t -> 'a list
  (** [generate ~n g] generates [n] instances of [g]. *)

  val generate1 : ?rand:Random.State.t -> 'a t -> 'a
  (** [generate1 g] generates one instance of [g]. *)
end

(** {1 Printing Values} *)

module Print : sig

  (** The [Print] module offers combinators for printing generated values. *)

  type 'a t = 'a -> string
  (** Printer for values of type ['a]. *)


  val unit : unit t
  (** [unit] is a printer of unit.
      @since 0.6 *)

  val int : int t (** Integer printer. *)

  val int32 : int32 t
  (** 32-bit integer printer.
      @since 0.24 *)

  val int64 : int64 t
  (** 64-bit integer printer.
      @since 0.24 *)

  val bool : bool t (** Boolean printer. *)

  val float : float t (** Floating point number printer. *)

  val char : char t (** Character printer. *)

  val bytes : bytes t
  (** Bytes printer.
      @since 0.20 *)

  val string : string t (** String printer. *)

  val option : 'a t -> 'a option t (** Option printer. *)

  val result : 'a t -> 'e t -> ('a, 'e) result t
  (** Result printer.
      @since 0.24 *)

  val pair : 'a t -> 'b t -> ('a*'b) t
  (** Pair printer. Expects printers for each component. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a*'b*'c) t
  (** Triple (3-tuple) printer. Expects printers for each component. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a*'b*'c*'d) t
  (** Quadruple (4-tuple) printer. Expects printers for each component. *)

  val list : 'a t -> 'a list t
  (** List printer. Expects a printer for the list element type. *)

  val array : 'a t -> 'a array t
  (** Array printer. Expects a printer for the array entry type. *)

  val comap : ('a -> 'b) -> 'b t -> 'a t
  (** [comap f p] maps [p], a printer of type ['b], to a printer of type ['a] by
      first converting a printed value using [f : 'a -> 'b]. *)

  val tup2 : 'a t -> 'b t -> ('a * 'b) t
  (** 2-tuple printer. Expects printers for each component. *)

  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** 3-tuple printer. Expects printers for each component. *)

  val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** 4-tuple printer. Expects printers for each component. *)

  val tup5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  (** 5-tuple printer. Expects printers for each component. *)

  val tup6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t
  (** 6-tuple printer. Expects printers for each component. *)

  val tup7 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
  (** 7-tuple printer. Expects printers for each component. *)

  val tup8 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
  (** 8-tuple printer. Expects printers for each component. *)

  val tup9 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t -> 'i t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
  (** 9-tuple printer. Expects printers for each component. *)
end

(** {1 Shrinking Values}

    Shrinking is used to reduce the size of a counter-example. It tries
    to make the counter-example smaller, e.g., by decreasing an integer,
    or removing elements of a list, until the property to test holds again;
    it then returns the smallest value that still made the test fail.

    Shrinking is defined as a type {!Shrink.t} that takes an argument to shrink
    and produces an iterator of type {!Iter.t} of shrinking candidates.

    ⚠️ In order to function well, a shrinker must:
    - never return its argument as a shrinking candidate, and
    - return shrinking candidates which are smaller by some termination measure
      ([list] length, tree depth, number of bits, ...)

    Failure to do so, may result in an infinite shrinker loop.
*)

(** {2 Iterators} *)

module Iter : sig
  (** [Iter] is compatible with the library "sequence". An iterator [i] is simply
      a function that accepts another function [f] (of type ['a -> unit])
      and calls [f] on a sequence of elements [f x1; f x2; ...; f xn]. *)

  type 'a t = ('a -> unit) -> unit
  (** The type of iterators, underlying {!Shrink.t}. *)

  val empty : 'a t
  (** The empty iterator *)

  val return : 'a -> 'a t
  (** The constant iterator *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative operator for iterators, combining a function iterator and
      an argument iterator into a result generator. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind operator for iterators.
      [i >>= f] passes each element of [i] to [f], iterating over each element
      of [f]'s resulting iterators. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f i] returns an iterator of elements from [i], each of which have
      been applied to [f]. *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map f i j] returns an iterator of elements from [i] and [j], which have
      been applied to [f]. *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** An infix synonym for {!map}. *)

  val append : 'a t -> 'a t -> 'a t
  (** [append a b] first iterates over [a]'s elements and then over [b]'s. *)

  val (<+>) : 'a t -> 'a t -> 'a t
  (** Synonym for {!append}. *)

  val of_list : 'a list -> 'a t
  (** [of_list xs] builds an iterator over the list elements of [xs]. *)

  val of_array : 'a array -> 'a t
  (** [of_array xs] builds an iterator over the array elements of [xs]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] iterates over pairs [(x,y)] with [x] coming from [a] and
      [y] coming from [b]. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple a b c] iterates over triples [(x,y,z)] with [x] coming from [a],
      [y] coming from [b], and [z] coming from [c]. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [quad a b c d] iterates over quadruples [(x,y,z,w)] with [x] coming from [a],
      [y] coming from [b], [z] coming from [c], and [w] coming from [d]. *)

  val find : ('a -> bool) -> 'a t -> 'a option
  (** [find p i] maps over the iterator [i], returning [Some _] when the
      predicate [p] is [true] and [None] otherwise. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter p i] returns an iterator of elements from [i] satisfying [p]. *)

  val append_l : 'a t list -> 'a t
  (** Appends a list of iterators into a single iterator.
      @since 0.8 *)

  val flatten : 'a t t -> 'a t
  (** Flattens an iterator of iterators into a single iterator.
      @since 0.8 *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!map}. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!(>>=)}. *)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}. *)
end

(** {2 Shrinkers} *)

module Shrink : sig
  (** The [Shrink] module contains combinators to build up composite shrinkers
      for user-defined types

      ⚠️ Warning: [QCheck]'s shrinking phase may loop infinitely if
      - the shrinker returns its own argument as a shrinking candidate, or
      - fails to return a strictly smaller shrinking candidate by some termination measure
        (e.g., a [list] permutation may lead to an infinite shrinking cycle).
  *)

  type 'a t = 'a -> 'a Iter.t
  (** Given a counter-example, return an iterator on smaller versions
      of the counter-example. *)

  val nil : 'a t
  (** No shrink *)

  val unit : unit t
  (** unit shrinker. Does not produce any shrinking candidates.
      @since 0.6 *)

  val bool : bool t
  (** bool shrinker. Shrinks towards [false].
      @since 0.23 *)

  val char : char t
  (** char shrinker. Shrinks towards ['a'].
      @since 0.6 *)

  val char_numeral : char t
  (** char digit shrinker. Shrinks towards ['0'].
      @since 0.19 *)

  val char_printable : char t
  (** Printable char shrinker. Shrinks towards ['a'] like [!char]. The output is also a printable character.
      @since 0.19 *)

  val int : int t
  (** int shrinker. Shrinks towards [0]. *)

  val int32 : int32 t
  (** int32 shrinker. Shrinks towards [0l].
      @since 0.14 *)

  val int64 : int64 t
  (** int64 shrinker. Shrinks towards [0L].
      @since 0.14 *)

  val float : float t
  (** float shrinker. Shrinks floating point numbers towards [1.0] or [-1.0]
      and prefers a shorter printed rendering.
      @since 0.27 *)

  val float_bound : float -> float t
  (** float shrinker for bounded floating point numbers.
      [float_bound b] shrinks floating point numbers towards [0.] for both
      positive and negative bounds [b], preferring a shorter printed rendering.
      @since 0.27 *)

  val float_range : float -> float -> float t
  (** [float_range low high] shrinks floating point numbers in the range [low; high] (inclusive).
      Shrinks towards [low] if [low >= 0.], towards [high] if [high <= 0.], and
      towards [0.] if [low < 0.] and [0. <. high]. It prefers a shorter printed rendering.
      @raise Invalid_argument if [low > high].
      @since 0.27 *)

  val option : 'a t -> 'a option t
  (** option shrinker. Shrinks towards [None].
      [option shk] reduces [Some v] values using [shk] to reduce [v]. *)

  val result : 'a t -> 'e t -> ('a, 'e) result t
  (** result shrinker.
      [result ashk eshk] reduces [Ok a] values using [ashk] and [Error e] values using [eshk].
      @since 0.24 *)

  val bytes : ?shrink:(char t) -> bytes t
  (** bytes shrinker. Shrinks towards shorter byte strings.
      @param shrink an optional [char] shrinker.
      @since 0.20 *)

  val string : ?shrink:(char t) -> string t
  (** string shrinker. Shrinks towards [""].
      @param shrink an optional [char] shrinker. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter f shrink] shrinks values the same as [shrink], but
      only keep smaller values that satisfy [f].
      This way it's easy to preserve invariants that are enforced by
      generators, when shrinking values
      @since 0.8 *)

  val int_aggressive : int t
  (** Shrink integers by trying all smaller integers (can take a lot of time!)
      @since 0.7 *)

  val list : ?shrink:'a t -> 'a list t
  (** Try to shrink lists by removing one or more elements.
      @param shrink if provided, will be used to also try to reduce
      the elements of the list themselves (e.g. in an [int list]
      one can try to decrease the integers). *)

  val list_spine : 'a list t
  (** Try to shrink lists by removing one or more elements.
      @since 0.10 *)

  val list_elems : 'a t -> 'a list t
  (** Shrinks the elements of a list, without changing the list size.
      @since 0.10 *)

  val array : ?shrink:'a t -> 'a array t
  (** Shrink an array.
      @param shrink see {!list} *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] uses [a] to shrink the first element of tuples,
      then tries to shrink the second element using [b].
      It is often better, when generating tuples, to put the "simplest"
      element first (atomic type rather than list, etc.) because it will be
      shrunk earlier. In particular, putting functions last might help. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Similar to {!pair} *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Similar to {!pair} *)

  val tup2 : 'a t -> 'b t -> ('a * 'b) t
  (** [tup2 a b] uses [a] to shrink the first element of tuples,
      then tries to shrink the second element using [b].
      It is often better, when generating tuples, to put the "simplest"
      element first (atomic type rather than list, etc.) because it will be
      shrunk earlier. In particular, putting functions last might help. *)

  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Similar to {!tup2} *)

  val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Similar to {!tup2} *)

  val tup5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  (** Similar to {!tup2} *)

  val tup6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
    ('a * 'b * 'c * 'd * 'e * 'f) t
  (** Similar to {!tup2} *)

  val tup7 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
  (** Similar to {!tup2} *)

  val tup8 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t
  (** Similar to {!tup2} *)

  val tup9 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t -> 'h t -> 'i t ->
    ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t
  (** Similar to {!tup2} *)
end


(** {1 Arbitrary}

    A value of type ['a arbitrary] glues together a random generator,
    and optional functions for shrinking, printing, computing the size,
    etc. It is the "normal" way of describing how to generate
    values of a given type, to be then used in tests (see {!Test}). *)

type 'a stat = string * ('a -> int)
(** A statistic on a distribution of values of type ['a].
    The function {b MUST} return a positive integer. *)

type 'a arbitrary = private {
  gen: 'a Gen.t;
  print: ('a Print.t) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a Shrink.t) option;  (** shrink to smaller examples *)
  collect: ('a -> string) option;  (** map value to tag, and group by tag *)
  stats: 'a stat list; (** statistics to collect and print *)
}
(** A value of type ['a arbitrary] is an object with a method for generating random
    values of type ['a], and additional methods to compute the size of values,
    print them, and possibly shrink them into smaller counter-examples.

    {b NOTE} the collect field is unstable and might be removed, or
    moved into {!Test}.

    Made private since 0.8
*)

val make :
  ?print:'a Print.t ->
  ?small:('a -> int) ->
  ?shrink:'a Shrink.t ->
  ?collect:('a -> string) ->
  ?stats:'a stat list ->
  'a Gen.t -> 'a arbitrary
(** Builder for arbitrary. Default is to only have a generator, but other
    arguments can be added.
    @param print printer for values (counter-examples)
    @param collect for statistics
    @param shrink to shrink counter-examples
*)

(** {2 Adjusting arbitrary generators }

    There is a range to [get] and [set] fields on an arbitrary record type.
*)

val set_print : 'a Print.t -> 'a arbitrary -> 'a arbitrary
val set_small : ('a -> int) -> 'a arbitrary -> 'a arbitrary
val set_shrink : 'a Shrink.t -> 'a arbitrary -> 'a arbitrary
val set_collect : ('a -> string) -> 'a arbitrary -> 'a arbitrary
val set_stats : 'a stat list -> 'a arbitrary -> 'a arbitrary (** @since 0.6 *)

val add_shrink_invariant : ('a -> bool) -> 'a arbitrary -> 'a arbitrary
(** Update shrinker by only keeping smaller values satisfying the
    given invariant.
    @since 0.8 *)

val set_gen : 'a Gen.t -> 'a arbitrary -> 'a arbitrary
(** Change the generator
    @since 0.7 *)

val add_stat : 'a stat -> 'a arbitrary -> 'a arbitrary
(** Add a statistic to the arbitrary instance.
    @since 0.6 *)

val gen : 'a arbitrary -> 'a Gen.t
(** Access the underlying random generator of this arbitrary object.
    @since 0.6 *)

val get_gen : 'a arbitrary -> 'a Gen.t
(** Access the underlying random generator of this arbitrary object.
    @since 0.6 *)

val get_print : 'a arbitrary -> 'a Print.t option


(** {2 Primitive combinators for arbitrary} *)

val unit : unit arbitrary
(** Always generates [()], obviously. *)

val bool : bool arbitrary
(** Uniform boolean generator. *)

val float : float arbitrary
(** Generates regular floats (no infinities). *)

val float_pos : float arbitrary
(** Positive float generator (no infinities). *)

val float_neg : float arbitrary
(** Negative float generator (no infinities). *)

val pos_float : float arbitrary
(** Positive float generator (no infinities).
    @deprecated use {!float_pos} instead. *)

val neg_float : float arbitrary
(** Negative float generator (no infinities).
    @deprecated use {!float_neg} instead. *)

val float_bound_inclusive : float -> float arbitrary
(** [float_bound_inclusive n] is uniform between [0] and [n] included. If [bound] is
    negative, the result is negative or zero.  If [bound] is 0, the result is 0.
    @since 0.11 *)

val float_bound_exclusive : float -> float arbitrary
(** [float_bound_exclusive n] is uniform between [0] included and [n] excluded.
    If [bound] is negative, the result is negative or zero.
    @raise Invalid_argument if [bound] is zero.
    @since 0.11 *)

val float_range : float -> float -> float arbitrary
(** [float_range low high] is uniform between [low] included and [high] included.
    @raise Invalid_argument if [low > high] or if the range is larger than [max_float].
    @since 0.11 *)

val (--.) : float -> float -> float arbitrary
(** Synonym for [float_range]
    @since NEXT_RELEASE *)

val float_exp : float -> float arbitrary
(** [float_exp m] generates floating-point numbers following an exponential
    distribution with a mean of [m].
    @raise Invalid_argument if [m] is NaN.
    @since NEXT_RELEASE *)

val exponential : float -> float arbitrary
(** Synonym for {!float_exp}.
    @since 0.23 *)

val int : int arbitrary
(** Int generator. Uniformly distributed. *)

val int_bound : int -> int arbitrary
(** [int_bound n] is uniform between [0] and [n] included. *)

val int_range : int -> int -> int arbitrary
(** [int_range a b] is uniform between [a] and [b] included. [b] must be
    larger than [a]. *)

val small_nat : int arbitrary
(** Small unsigned integers.
    @since 0.5.1 *)

val small_int : int arbitrary
(** Small unsigned integers. See {!Gen.small_int}.
    @deprecated use {!small_signed_int}. *)

val small_signed_int : int arbitrary
(** Small signed integers.
    @since 0.5.2 *)

val (--) : int -> int -> int arbitrary
(** Synonym for {!int_range}. *)

val int32 : int32 arbitrary
(** Int32 generator. Uniformly distributed. *)

val int64 : int64 arbitrary
(** Int64 generator. Uniformly distributed. *)

val pos_int : int arbitrary
(** Positive int generator (0 included). Uniformly distributed.
    See {!Gen.pint} *)

val small_int_corners : unit -> int arbitrary
(** As [small_int], but each newly created generator starts with
    a list of corner cases before falling back on random generation.

    Note that [small_int_corners ()] is stateful, meaning that once the list of
    corner cases has been emitted, subsequent calls will not reproduce them.
    As a consequence, in the following example, the first test fails with a
    counter example, whereas the second rerun does not:
    {[
      let gen = QCheck.small_int_corners ()
      let t = QCheck.Test.make ~name:"never max_int" gen (fun i -> i <> max_int)
      let _ = QCheck_base_runner.run_tests ~verbose:true [t;t]
    ]}
 *)

val neg_int : int arbitrary
(** Negative int generator (0 included, see {!Gen.neg_int}).
    The distribution is similar to that of
    [small_int], not of [pos_int].
*)

val char : char arbitrary
(** Uniformly distributed on all the chars (not just ascii or
    valid latin-1). *)

val char_range : char -> char -> char arbitrary
(** Generates chars between the two bounds, inclusive.
    Example: [char_range 'a' 'z'] for all lower case ascii letters.
    @since NEXT_RELEASE *)

val char_printable : char arbitrary
(** Uniformly distributed over a subset of printable ascii chars.
    Ascii character codes 32 to 126, inclusive - or ['\n'] with code 10.
    @since NEXT_RELEASE *)

val printable : char arbitrary
(** Synonym for {!char_printable}.
    @since NEXT_RELEASE *)

val printable_char : char arbitrary
(** Synonym for {!char_printable}.
    @deprecated use {!char_printable} instead. *)

val char_numeral : char arbitrary
(** Uniformly distributed over ['0'..'9'].
    @since NEXT_RELEASE *)

val numeral : char arbitrary
(** Synonym for {!char_numeral}.
    @since NEXT_RELEASE *)

val numeral_char : char arbitrary
(** Synonym for {!char_numeral}.
    @deprecated use {!char_numeral} instead. *)

val bytes_size : ?gen:char Gen.t -> int Gen.t -> bytes arbitrary
(** Builds a bytes generator from a (non-negative) size generator.
    Accepts an optional character generator (the default is {!Gen.char}).
    @since NEXT_RELEASE *)

val bytes_size_of : int Gen.t -> char Gen.t -> bytes arbitrary
(** Builds a bytes generator from a (non-negative) size generator
    and a character generator.
    @since NEXT_RELEASE *)

val bytes_gen_of_size : int Gen.t -> char Gen.t -> bytes arbitrary
(** Builds a bytes generator from a (non-negative) size generator and a character generator.
    @since 0.20
    @deprecated use {!bytes_size} instead. *)

val bytes_of : char Gen.t -> bytes arbitrary
(** Generates bytes with a distribution of length of {!Gen.nat}.
    @since 0.20 *)

val bytes : bytes arbitrary
(** Generates bytes with a distribution of length of {!Gen.nat}
    and distribution of characters of [char].
    @since 0.20 *)

val bytes_small : bytes arbitrary
(** Same as {!bytes} but with a small length (ie {!Gen.small_nat} ).
    @since 0.20 *)

val bytes_small_of : char Gen.t -> bytes arbitrary
(** Same as {!bytes_of} but with a small length (ie {!Gen.small_nat} ).
    @since 0.20 *)

val bytes_of_size : int Gen.t -> bytes arbitrary
(** Generates bytes with distribution of characters of [char].
    @since 0.20
    @deprecated use {!bytes_size} instead. *)

val bytes_printable : bytes arbitrary
(** Generates bytes with a distribution of length of {!Gen.nat}
    and distribution of characters of [printable_char].
    @since 0.20 *)

val string_gen_of_size : int Gen.t -> char Gen.t -> string arbitrary
(** Builds a string generator from a (non-negative) size generator and a character generator.
    @deprecated use {!string_size_of} instead. *)

val string_gen : char Gen.t -> string arbitrary
(** Generates strings with a distribution of length of {!Gen.nat}.
    @deprecated use {!string_of} instead. *)

val string_of : char Gen.t -> string arbitrary
(** Synonym for {!string_gen} added for convenience.
    @since 0.20 *)

val string : string arbitrary
(** Generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of [char]. *)

val string_size : ?gen:char Gen.t -> int Gen.t -> string arbitrary
(** Builds a string generator from a (non-negative) size generator.
    Accepts an optional character generator (the default is {!Gen.char}).
    @since NEXT_VERSION *)

val string_size_of : int Gen.t -> char Gen.t -> string arbitrary
(** Builds a string generator from a (non-negative) size generator
    and a character generator.
    @since NEXT_RELEASE *)

val small_string : string arbitrary
(** Same as {!string} but with a small length (ie {!Gen.small_nat} ).
    @deprecated use {!string_small} *)

val string_small : string arbitrary
(** Same as {!string} but with a small length (ie {!Gen.small_nat} ).
    @since 0.20 *)

val string_small_of : char Gen.t -> string arbitrary
(** Same as {!string_of} but with a small length (ie {!Gen.small_nat} ).
    @since 0.20 *)

val small_list : 'a arbitrary -> 'a list arbitrary
(** Generates lists of small size (see {!Gen.small_nat}).
    @since 0.5.3 *)

val string_of_size : int Gen.t -> string arbitrary
(** Generates strings with distribution of characters of [char].
    @deprecated use {!string_size} instead. *)

val printable_string : string arbitrary
(** Generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of [char_printable].
    @deprecated use {!string_printable} instead. *)

val string_printable : string arbitrary
(** Generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of [char_printable].
    @since 0.20 *)

val printable_string_of_size : int Gen.t -> string arbitrary
(** Generates strings with distribution of characters of [char_printable].
    @deprecated use {!string_size_of} instead. *)

val string_printable_of_size : int Gen.t -> string arbitrary
(** Synonym for [printable_string_of_size] added for convenience.
    @since 0.20
    @deprecated use {!string_size_of} instead. *)

val small_printable_string : string arbitrary
(** Generates strings with a length of [small_nat]
    and distribution of characters of [char_printable].
    @deprecated use {!string_size_of} instead. *)

val string_small_printable : string arbitrary
(** Synonym for [small_printable_string] added for convenience.
    @since 0.20
    @deprecated use {!string_size_of} instead. *)

val numeral_string : string arbitrary
(** Generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of [char_numeral].
    @deprecated use {!string_of} instead. *)

val string_numeral : string arbitrary
(** Synonym for [numeral_string] added for convenience.
    @since 0.20
    @deprecated use {!string_of} instead. *)

val numeral_string_of_size : int Gen.t -> string arbitrary
(** Generates strings with a distribution of characters of [char_numeral].
    @deprecated use {!string_size_of} instead. *)

val string_numeral_of_size : int Gen.t -> string arbitrary
(** Synonym for [numeral_string_of_size] added for convenience.
    @since 0.20
    @deprecated use {!string_size_of} instead. *)

val list : 'a arbitrary -> 'a list arbitrary
(** Generates lists with length generated by {!Gen.nat}. *)

val list_of_size : int Gen.t -> 'a arbitrary -> 'a list arbitrary
(** Generates lists with length from the given distribution. *)

val array : 'a arbitrary -> 'a array arbitrary
(** Generates arrays with length generated by {!Gen.nat}. *)

val array_small : 'a arbitrary -> 'a array arbitrary
(** Generates arrays of small size (see {!Gen.array_small}).
    @since NEXT_RELEASE *)

val array_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** Generates arrays with length from the given distribution.
    @since NEXT_RELEASE *)

val array_of_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** Generates arrays with length from the given distribution.
    @deprecated use {!array_size} instead. *)

val option : ?ratio:float -> 'a arbitrary -> 'a option arbitrary
(** Choose between returning Some random value with optional ratio, or None. *)

val opt : ?ratio:float -> 'a arbitrary -> 'a option arbitrary
(** Synonym for {!option}.
    @since NEXT_RELEASE *)

val result : ?ratio:float -> 'a arbitrary -> 'e arbitrary -> ('a, 'e) result arbitrary
(** [result ~ratio okgen errgen] generates [Ok v] with [v] coming from [okgen]
    or [Error e] with [e] coming from [errgen], depending on [ratio]. The latter
    is a float between [0.] and [1.] indicating the probability of a sample to
    be [Ok _] rather than [Error _].

    @since 0.24 *)


(** {2 Tuples of arbitrary generators}

    These shrink on [gen1], then [gen2], then ... *)

val pair : 'a arbitrary -> 'b arbitrary -> ('a * 'b) arbitrary
(** Combines two generators into a generator of pairs.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.pair}) *)

val triple : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a * 'b * 'c) arbitrary
(** Combines three generators into a generator of 3-tuples.
    Order matters for shrinking, see {!Shrink.pair} and the likes *)

val quad : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> 'd arbitrary -> ('a * 'b * 'c * 'd) arbitrary
(** Combines four generators into a generator of 4-tuples.
    Order matters for shrinking, see {!Shrink.pair} and the likes *)

val tup2 :
  'a arbitrary ->
  'b arbitrary ->
  ('a * 'b) arbitrary
(** Combines two generators into a 2-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup3 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  ('a * 'b * 'c) arbitrary
(** Combines three generators into a 3-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup4 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  ('a * 'b * 'c * 'd) arbitrary
(** Combines four generators into a 4-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup5 : 'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  'e arbitrary ->
  ('a * 'b * 'c * 'd * 'e) arbitrary
(** Combines five generators into a 5-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup6 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  'e arbitrary ->
  'f arbitrary ->
  ('a * 'b * 'c * 'd * 'e * 'f) arbitrary
(** Combines six generators into a 6-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup7 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  'e arbitrary ->
  'f arbitrary ->
  'g arbitrary ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g) arbitrary
(** Combines seven generators into a 7-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup8 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  'e arbitrary ->
  'f arbitrary ->
  'g arbitrary ->
  'h arbitrary ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) arbitrary
(** Combines eight generators into a 8-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)

val tup9 :
  'a arbitrary ->
  'b arbitrary ->
  'c arbitrary ->
  'd arbitrary ->
  'e arbitrary ->
  'f arbitrary ->
  'g arbitrary ->
  'h arbitrary ->
  'i arbitrary ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) arbitrary
(** Combines nine generators into a 9-tuple generator.
    Order of elements can matter (w.r.t shrinking, see {!Shrink.tup2})
    Prints as many elements as available printers *)


(** {2 Combinatoric arbitrary combinators } *)

val choose : 'a arbitrary list -> 'a arbitrary
(** Choose among the given list of generators. The list must not
    be empty; if it is [Invalid_argument] is raised. *)

val oneofl : ?print:'a Print.t -> ?collect:('a -> string) ->
  'a list -> 'a arbitrary
(** Pick an element randomly in the list. *)

val oneofa : ?print:'a Print.t -> ?collect:('a -> string) ->
  'a array -> 'a arbitrary
(** Pick an element randomly in the array. *)

val oneof : 'a arbitrary list -> 'a arbitrary
(** Pick a generator among the list, randomly.
    @deprecated this function is badly specified and will not use shrinkers
    appropriately. Consider using {!Gen.oneof} and then {!make} to build
    a well behaved arbitrary instance.
*)

val always : ?print:'a Print.t -> 'a -> 'a arbitrary
(** Always return the same element. *)

val frequency : ?print:'a Print.t -> ?small:('a -> int) ->
  ?shrink:'a Shrink.t -> ?collect:('a -> string) ->
  (int * 'a arbitrary) list -> 'a arbitrary
(** Similar to {!oneof} but with frequencies. *)

val frequencyl : ?print:'a Print.t -> ?small:('a -> int) ->
  (int * 'a) list -> 'a arbitrary
(** Same as {!oneofl}, but each element is paired with its frequency in
    the probability distribution (the higher, the more likely). *)

val frequencya : ?print:'a Print.t -> ?small:('a -> int) ->
  (int * 'a) array -> 'a arbitrary
(** Same as {!frequencyl}, but with an array. *)

val map : ?rev:('b -> 'a) -> ('a -> 'b) -> 'a arbitrary -> 'b arbitrary
(** [map f a] returns a new arbitrary instance that generates values using
    [a#gen] and then transforms them through [f].
    @param rev if provided, maps values back to type ['a] so that the printer,
      shrinker, etc. of [a] can be used. We assume [f] is monotonic in
      this case (that is, smaller inputs are transformed into smaller outputs).
*)

val map_same_type : ('a -> 'a) -> 'a arbitrary -> 'a arbitrary
(** Specialization of [map] when the transformation preserves the type, which
    makes shrinker, printer, etc. still relevant. *)

val map_keep_input :
  ?print:'b Print.t -> ?small:('b -> int) ->
  ('a -> 'b) -> 'a arbitrary -> ('a * 'b) arbitrary
(** [map_keep_input f a] generates random values from [a], and maps them into
    values of type ['b] using the function [f], but it also keeps the
    original value.
    For shrinking, it is assumed that [f] is monotonic and that smaller input
      values will map into smaller values.
    @param print optional printer for the [f]'s output.
*)


(** {1 Tests}

    A test is a universal property of type [foo -> bool] for some type [foo],
    with an object of type [foo arbitrary] used to generate, print, etc. values
    of type [foo].

    The main features of this module are:
    - {!Test.make} to build a test,
    - {!Test.make_neg} to build a negative test that is expected not to satisfy the tested property,
    - {!Test.check_exn} to run a single test with a simple runner.

    A test fails if the property does not hold for a given input. The {{!Test.fail_report} simple} form or the {{!Test.fail_reportf} rich} form) offer more elaborate forms to fail a test.

    For more serious testing, it is recommended to create a testsuite and use a full-fledged runner:
    - {!QCheck_base_runner} is a QCheck-only runner (useful if you don't have or don't need another test framework)
    - {!QCheck_alcotest} interfaces to the Alcotest framework
    - {!QCheck_ounit} interfaces to the to OUnit framework
*)


(** {2 Test Results } *)

module TestResult : sig
  (** Module to represent the result of running a test *)

  type 'a counter_ex = 'a QCheck2.TestResult.counter_ex = {
    instance: 'a; (** The counter-example(s) *)

    shrink_steps: int; (** How many shrinking steps for this counterex *)

    msg_l: string list;
    (** messages.
        @since 0.7 *)
  }

  type 'a failed_state = 'a counter_ex list

  (** Result state.
      changed in 0.10 (move to inline records, add Fail_other) *)
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

  val get_count : _ t -> int
  (** Get the count of a cell.
     @since 0.5.3 *)

  val get_count_gen : _ t -> int

  val get_state : 'a t -> 'a state

  val collect : _ t -> (string,int) Hashtbl.t option
  (** Obtain statistics
      @since 0.6 *)

  val stats : 'a t -> ('a stat * (int,int) Hashtbl.t) list
  (** Obtain statistics
      @since 0.6 *)

  val warnings : _ t -> string list
  (** Obtain list of warnings
      @since 0.10 *)

  val is_success : _ t -> bool
  (** Returns true iff the state if [Success]
      @since 0.9 *)
end

(** {2 Defining Tests } *)

(** Module related to individual tests.
    Since 0.18 most of it moved to {!QCheck2},
    and the type ['a cell] was made a private implementation detail.
*)
module Test : sig
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

  val fail_report : string -> 'a
  (** Fail the test with some additional message that will
      be reported.
      @since 0.7 *)

  val fail_reportf : ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Format version of {!fail_report}
      @since 0.7 *)

  val make_cell :
    ?if_assumptions_fail:([`Fatal | `Warning] * float) ->
    ?count:int -> ?long_factor:int -> ?negative:bool -> ?max_gen:int -> ?max_fail:int ->
    ?small:('a -> int) -> ?retries:int -> ?name:string ->
    'a arbitrary -> ('a -> bool) -> 'a cell
  (** [make_cell arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      @param name the name of the test.
      @param count number of test cases to run, counting only
        the test cases which satisfy preconditions.
      @param retries number of times to retry the tested property while shrinking.
      @param long_factor the factor by which to multiply count, max_gen and
        max_fail when running a long test (default: 1).
      @param negative whether the test is expected not to satisfy the tested property.
      @param max_gen maximum number of times the generation function
        is called in total to replace inputs that do not satisfy
        preconditions (should be >= count).
      @param max_fail maximum number of failures before we stop generating
        inputs. This is useful if shrinking takes too much time.
      @param small kept for compatibility reasons; if provided, replaces
        the field [arbitrary.small].
        If there is no shrinking function but there is a [small]
        function, only the smallest failures will be printed.
      @param if_assumptions_fail the minimum
        fraction of tests that must satisfy the precondition for a success
        to be considered valid.
        The fraction should be between 0. and 1.
        A warning will be emitted otherwise if
        the flag is [`Warning], the test will be a failure if the flag is [`Fatal].
        (since 0.10)
  *)

  val get_law : 'a cell -> ('a -> bool)
  (** @deprecated use {!QCheck2.Test.get_law} instead *)
  val get_name : _ cell -> string
  (** @deprecated use {!QCheck2.Test.get_name} instead *)
  val set_name : _ cell -> string -> unit
  (** @deprecated use {!QCheck2.Test.set_name} instead *)

  val get_count : _ cell -> int
  (** Get the count of a cell.
      @deprecated use {!QCheck2.Test.get_count} instead
      @since 0.5.3 *)

  val get_long_factor : _ cell -> int
  (** Get the long factor of a cell.
      @deprecated use {!QCheck2.Test.get_long_factor} instead
      @since 0.5.3 *)

  val make :
    ?if_assumptions_fail:([`Fatal | `Warning] * float) ->
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?small:('a -> int) -> ?retries:int -> ?name:string -> 'a arbitrary ->
    ('a -> bool) -> t
  (** [make arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      See {!make_cell} for a description of the parameters.
  *)

  val make_neg :
    ?if_assumptions_fail:([`Fatal | `Warning] * float) ->
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?small:('a -> int) -> ?retries:int -> ?name:string -> 'a arbitrary ->
    ('a -> bool) -> t
  (** [make_neg arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      The test is considered negative, meaning that it is expected not to satisfy the tested property.
      This information is recorded in an underlying test [cell] entry and interpreted suitably by test runners.
      See {!make_cell} for a description of the parameters.
  *)

  include module type of QCheck2.Test_exceptions

  val print_instance : 'a cell -> 'a -> string
  val print_c_ex : 'a cell -> 'a TestResult.counter_ex -> string
  val print_fail : 'a cell -> string -> 'a TestResult.counter_ex list -> string
  val print_fail_other : string -> msg:string -> string
  val print_error : ?st:string -> 'a cell -> string -> 'a TestResult.counter_ex * exn -> string
  val print_test_fail : string -> string list -> string
  val print_test_error : string -> string -> exn -> string -> string

  val check_cell :
    ?long:bool -> ?call:'a callback ->
    ?step:'a step -> ?handler:'a handler ->
    ?rand:Random.State.t -> 'a cell -> 'a TestResult.t
  (** See {!QCheck2.Test.check_cell}. *)

  val check_cell_exn :
    ?long:bool -> ?call:'a callback ->
    ?step:'a step -> ?handler:'a handler ->
    ?rand:Random.State.t -> 'a cell -> unit
  (** See {!QCheck2.Test.check_cell_exn}. *)

  val check_exn : ?long:bool -> ?rand:Random.State.t -> t -> unit
  (** See {!QCheck2.Test.check_exn}. *)
end

(** {2 Sub-tests} *)

(** The infrastructure used to find counter-examples to properties can
    also be used to find data satisfying a predicate,
    {i within a property being tested}.

    See {:https://github.com/c-cube/qcheck/issues/31}
*)

exception No_example_found of string

val find_example :
  ?name:string ->
  ?count:int ->
  f:('a -> bool) ->
  'a Gen.t ->
  'a Gen.t
(** [find_example ~f gen] uses [gen] to generate some values of type ['a],
    and checks them against [f]. If such a value is found, it is returned.
    Otherwise an exception is raised.
    {b NOTE} this should only be used from within a property in {!Test.make}.
    @param count number of attempts.
    @param name description of the example to find (used in the exception).
    @param f the property that the example must satisfy.
    @raise No_example_found if no example is found within [count] tries.
    @since 0.6
*)

val find_example_gen :
  ?rand:Random.State.t ->
  ?name:string ->
  ?count:int ->
  f:('a -> bool) ->
  'a Gen.t ->
  'a
(** Toplevel version of {!find_example}.
    [find_example_gen ~f arb ~n] is roughly the same as
    [Gen.generate1 (find_example ~f arb |> gen)].
    @param rand the random state to use to generate inputs.
    @raise No_example_found if no example was found within [count] tries.
    @since 0.6 *)


(** {1 Generating Functions}

    The [QCheck] module supports generation of pure function values.
    The implementation is inspired from {:https://blogs.janestreet.com/quickcheck-for-core/}
    and {{:https://dl.acm.org/doi/abs/10.1145/2364506.2364516}Koen Claessen's "Shrinking and Showing Functions"}.

    Generated function arguments are of type {!Observable.t} and function results are of type
    {{!section:arbitrary}[arbitrary]}.

    Underneath the hood, generated function values have a table-based representation.
    They therefore need to be applied in a special way, e.g., with {!Fn.apply}.
*)

(** {2 Observing arguments} *)

module Observable : sig
  (** Observables are usable as arguments for random functions.
      The random function will observe its arguments in a way
      that is determined from the observable instance.

      @since 0.6
  *)

  (** An observable for ['a], packing a printer and other things. *)
  type -'a t

  val equal : 'a t -> 'a -> 'a -> bool
  val hash : 'a t -> 'a -> int
  val print : 'a t -> 'a Print.t

  val unit : unit t
  val bool : bool t
  val int : int t
  val int32 : int32 t (** @since 0.24 *)
  val int64 : int64 t (** @since 0.24 *)
  val float : float t
  val string : string t
  val bytes : bytes t (** @since 0.20 *)
  val char : char t

  val make :
    ?eq:('a -> 'a -> bool) ->
    ?hash:('a -> int) ->
    'a Print.t ->
    'a t

  val map : ('a -> 'b) -> 'b t -> 'a t

  val option : 'a t -> 'a option t
  val result : 'a t -> 'e t -> ('a, 'e) result t (** @since 0.24 *)
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
end

(** {2 Internal function generator representation } *)

type _ fun_repr
(** Internal data for functions. A ['f fun_] is a function
    of type ['f], fundamentally. *)

(** A function packed with the data required to print/shrink it. See {!Fn}
    to see how to apply, print, etc. such a function.

    One can also directly pattern match on it to obtain
    the executable function.

    For example:
    {[
      QCheck.Test.make
        QCheck.(pair (fun1 Observable.int bool) (small_list int))
        (fun (Fun (_,f), l) -> l=(List.rev_map f l |> List.rev l))
    ]}
*)
type _ fun_ =
  | Fun : 'f fun_repr * 'f -> 'f fun_

module Fn : sig
  (** A utility module of helpers for printing, shrinking, and applying generated function values.
      @since 0.6 *)

  type 'a t = 'a fun_

  val print : _ t Print.t
  val shrink : _ t Shrink.t

  val apply : 'f t -> 'f
end


(** {2 Defining function generators } *)

val fun1 : 'a Observable.t -> 'b arbitrary -> ('a -> 'b) fun_ arbitrary
(** [fun1 o ret] makes random functions that take an argument observable
    via [o] and map to random values generated from [ret].
    To write functions with multiple arguments, it's better to use {!Tuple}
    or {!Observable.pair} rather than applying {!fun_} several times
    (shrinking will be faster).
    @since 0.6 *)

val fun2 :
  'a Observable.t ->
  'b Observable.t ->
  'c arbitrary ->
  ('a -> 'b -> 'c) fun_ arbitrary
(** @since 0.6 *)

val fun3 :
  'a Observable.t ->
  'b Observable.t ->
  'c Observable.t ->
  'd arbitrary ->
  ('a -> 'b -> 'c -> 'd) fun_ arbitrary
(** @since 0.6 *)

val fun4 :
  'a Observable.t ->
  'b Observable.t ->
  'c Observable.t ->
  'd Observable.t ->
  'e arbitrary ->
  ('a -> 'b -> 'c -> 'd -> 'e) fun_ arbitrary
(** @since 0.6 *)


(** {2 Tuples of observables }

    To circumvent the arity boundaries of {!fun1}, ..., {!fun4}, one can instead
    define uncurried functions, instead accepting a tuple argument. A resulting
    function then needs to be applied with {!fun_nary}.
*)

module Tuple : sig
  (** Heterogeneous tuple, used to pass any number of arguments to
      a function. *)
  type 'a t =
    | Nil : unit t
    | Cons : 'a * 'b t -> ('a * 'b) t

  val nil : unit t
  val cons : 'a -> 'b t -> ('a * 'b) t

  (** How to observe a  {{!t}['a t]} *)
  type 'a obs

  val o_nil : unit obs
  val o_cons : 'a Observable.t -> 'b obs -> ('a * 'b) obs

  module Infix : sig
    val (@::) : 'a -> 'b t -> ('a * 'b) t
    (** Alias to {!cons}. *)

    val (@->) : 'a Observable.t -> 'b obs -> ('a * 'b) obs
    (** Alias to {!o_cons}. *)
  end

  include module type of Infix

  val observable : 'a obs -> 'a t Observable.t
end

val fun_nary : 'a Tuple.obs -> 'b arbitrary -> ('a Tuple.t -> 'b) fun_ arbitrary
(** [fun_nary] makes random n-ary functions.
    Example:
    {[
      let module O = Observable in
      fun_nary Tuple.(O.int @-> O.float @-> O.string @-> o_nil) bool
    ]}
    @since 0.6 *)

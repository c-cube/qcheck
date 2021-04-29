(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

(** {1 QuickCheck-inspired property-based testing}

    The library takes inspiration from Haskell's QuickCheck library. The
    rough idea is that the programmer describes invariants that values of
    a certain type need to satisfy ("properties"), as functions from this type
    to bool. They also need to describe how to generate random values of the type,
    so that the property is tried and checked on a number of random instances.

    This explains the organization of this module:

    - {!arbitrary} is used to describe how to generate random values,
      shrink them (make counter-examples as small as possible), print
      them, etc. Auxiliary modules such as {!Gen} and {!Print}
      can be used along with {!make} to build one's own arbitrary instances.

    - {!Test} is used to describe a single test, that is, a property of
      type ['a -> bool] combined with an ['a arbitrary] that is used to generate
      the test cases for this property. Optional parameters
      allow to specify the random generator state, number of instances to generate
      and test, etc.


    Examples:

    - "{!List.rev} is involutive" (the test passes so [check_exn] returns [()]):

    {[

      let test =
        QCheck2.(Test.make ~count:1000
                   (list int) (fun l -> List.rev (List.rev l) = l));;

      QCheck2.Test.check_exn test;;
    ]}

    - "All lists are sorted" (false property that will fail):
      {ul
        {- QCheck tests this property on random lists and finds a counter-example}
        {- QCheck then looks for the smallest counter-example possible (here [[1; 0]])
           to help you find the problem (called "shrinking")}
      }

    {[
      let test = QCheck2.(
          Test.make
            ~name:"All lists are sorted"
            ~count:10_000
            (list small_nat)
            (fun l -> l = List.sort compare l));;
      QCheck2.Test.check_exn test;;

      Exception:
        test `All lists are sorted` failed on ≥ 1 cases:
        [1; 0] (after 5 shrink steps)
    ]}


    - Generate 20 random trees using {! Gen.fix} :

    {[
      type tree = Leaf of int | Node of tree * tree

      let leaf x = Leaf x
      let node x y = Node (x,y)

      let tree_gen = QCheck2.Gen.(sized @@ fix
                                    (fun self n -> match n with
                                       | 0 -> map leaf nat
                                       | n ->
                                         frequency
                                           [1, map leaf nat;
                                            2, map2 node (self (n/2)) (self (n/2))]
                                    ));;

      QCheck2.Gen.generate ~n:20 tree_gen;;
    ]}

*)

(** A tree represents a generated value and its successive shrunk values. *)
module Tree : sig
  (** Conceptually a pseudo-randomly generated value is packaged with its shrunk values.
      This coupling - called "integrated shrinking" - in a single type has a major benefit:
      most generators get shrinking "for free" by composing from smaller generators, and shrinking
      does not break invariants (e.g. shrinks of a positive number are always positive).
  *)

  type 'a t
  (** A tree of random generated values, where the root contains the value used for the test,
      and the sub-trees contain shrunk values (as trees, to be able to shrink several times a value)
      used if the test fails. *)

  val root : 'a t -> 'a
  (** [root tree] returns the root value of the tree of generated values [t]. *)

  val children : 'a t -> 'a t Seq.t
  (** [children tree] returns the direct sub-trees of the tree of generated values [t]. *)

  val pp : ?depth : int -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** [pp ?depth pp_a ppf tree] pretty-prints the tree of generated values [tree] using the
      pretty-print formatter [ppf]. Values of type ['a] will be printed using the given
      pretty-printer [pp_a].

      As a tree [t] can be potentially huge when fully evaluated, you can control the maximum
      depth the printer goes with [depth].
      - [None] means "everything"
      - [0] means "only the root"
      - [1] means "the root and its direct shrinks"
      - [2] means "the root, its direct shrinks, and the shrinks of its shrinks"
      - etc.
  *)
end

(** A generator is responsible for generating pseudo-random values and provide shrinks (smaller
    values) when a test fails. *)
module Gen : sig
  (** This module provides some of the most important features of QCheck:
      - {{!section:primitive_generators} primitive generators}
      - {{!section:composing_generators} generator compositions}
  *)

  type 'a t
  (** A random generator for values of type ['a]. *)

  type 'a sized = int -> 'a t
  (** Random generator with a size bound. *)

  (** {3:primitive_generators Primitive generators} *)

  val unit : unit t
  (** The unit generator.

      Does not shrink.
  *)

  val bool : bool t
  (** The boolean generator.

      Shrinks towards [false].
  *)

  val int : int t
  (** Generates integers uniformly.

      Shrinks towards [0].
  *)

  val pint : ?origin : int -> int t
  (** Generates non-strictly positive integers uniformly ([0] included).

      Shrinks towards [origin] if specified, otherwise towards [0]. *)

  val small_nat : int t
  (** Small integers (< [100]).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.5.1 *)

  val nat : int t
  (** Generates natural numbers (< [10_000]).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].
  *)

  val big_nat : int t
  (** Generates natural numbers, possibly large (< [1_000_000]).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.10 *)

  val neg_int : int t
  (** Generates non-strictly negative integers ([0] included).

      Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

      Shrinks towards [0].
  *)

  val small_int : int t
  (** Small UNSIGNED integers, for retrocompatibility.

      Shrinks towards [0].

      @deprecated use {!small_nat}. *)

  val small_signed_int : int t
  (** Small SIGNED integers, based on {!small_nat}.

      Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.5.2 *)

  val int32 : int32 t
  (** Generates uniform {!int32} values.

      Shrinks towards [0l].
  *)

  val ui32 : int32 t
  (** Generates (unsigned) {!int32} values.

      Shrinks towards [0l].

      @deprecated use {!val:int32} instead
  *)

  val int64 : int64 t
  (** Generates uniform {!int64} values.

      Shrinks towards [0L].
  *)

  val ui64 : int64 t
  (** Generates (unsigned) {!int64} values.

      Shrinks towards [0L].

      @deprecated use {!val:int64} instead
  *)

  val float : float t
  (** Generates floating point numbers.

      Shrinks towards [0.].
  *)

  val pfloat : float t
  (** Generates positive floating point numbers ([0.] included).

      Shrinks towards [0.].
  *)

  val nfloat : float t
  (** Generates negative floating point numbers. ([-0.] included).

      Shrinks towards [-0.].
  *)

  val char : char t
  (** Generates characters in the [0..255] range.

      Shrinks towards ['a'].
  *)

  val printable : char t
  (** Generates printable characters.

      Shrinks towards ['a'].
  *)

  val numeral : char t
  (** Generates numeral characters ['0'..'9'].

      Shrinks towards ['0'].
  *)

  val char_range : ?origin:char -> char -> char -> char t
  (** [char_range ?origin low high] generates chars between [low] and [high], inclusive.
      Example: [char_range 'a' 'z'] for all lower case ASCII letters.

      Shrinks towards [origin] if specified, otherwise towards [low].

      @raise Invalid_argument if [low > high].

      @since 0.13 *)

  val string_size : ?gen:char t -> int t -> string t
  (** Builds a string generator from a (non-negative) size generator.
      Accepts an optional character generator (the default is {!char}).

      Shrinks on the number of characters first, then on the characters.
  *)

  val string : ?gen:char t -> string t
  (** Builds a string generator. String size is generated by {!nat}.
      Accepts an optional character generator (the default is {!char}).
      See also {!string_of} and {!string_readable} for versions without
      optional parameters.

      Shrinks on the number of characters first, then on the characters.
  *)

  val string_of : char t -> string t
  (** Builds a string generator using the given character generator.

      Shrinks on the number of characters first, then on the characters.

      @since 0.11 *)

  val string_readable : string t
  (** Builds a string generator using the {!char} character generator.

      Shrinks on the number of characters first, then on the characters.

      @since 0.11 *)

  val small_string : ?gen:char t -> string t
  (** Builds a string generator, length is {!small_nat}.
      Accepts an optional character generator (the default is {!char}).

      Shrinks on the number of characters first, then on the characters.
  *)

  val pure : 'a -> 'a t
  (** [pure a] creates a generator that always returns [a].

      Does not shrink.

      @since 0.8
  *)

  val return : 'a -> 'a t
  (** Synonym for {!pure} *)

  val make_primitive : gen : (Random.State.t -> 'a) -> shrink : ('a -> 'a Seq.t) -> 'a t
  (** [make_primitive ~gen ~shrink] creates a generator from a function [gen] that creates
      a random value (this function must only use the given {!Random.State.t} for randomness)
      and a function [shrink] that, given a value [a], returns a lazy list of
      "smaller" values (used when a test fails).

      This lower-level function is meant to build generators for "primitive" types that can neither be
      built with other primitive generators nor through composition, or to have more control on the
      shrinking steps.

      [shrink] must obey the following rules (for your own definition of "small"):
      - [shrink a = Seq.empty] when [a] is the smallest possible value
      - [shrink a] must return values strictly smaller than [a], ideally from smallest to largest (for
        faster shrinking)
      - [let rec loop a = match shrink a () with | Nil -> () | Cons (smaller_a, _) -> loop smaller_a]
        must end for all values [a] of type ['a] (i.e. there must not be an infinite number of shrinking
        steps).
  *)

  (** {3 Ranges} *)

  val int_bound : int -> int t
  (** Uniform integer generator producing integers within [0..bound].

      Shrinks towards [0].

      @raise Invalid_argument if the argument is negative. *)

  val float_bound_inclusive : ?origin : float -> float -> float t
  (** [float_bound_inclusive ?origin bound] returns a random floating-point number between [0.] and
      [bound] (inclusive). If [bound] is negative, the result is negative or zero.  If
      [bound] is [0.], the result is [0.].

      Shrinks towards [origin] if given, otherwise towards [0.].

      @since 0.11 *)

  val float_bound_exclusive : ?origin : float -> float -> float t
  (** [float_bound_exclusive origin bound] returns a random floating-point number between [0.] and
      [bound] (exclusive).  If [bound] is negative, the result is negative or zero.

      Shrinks towards [origin] if given, otherwise towards [0.].

      @raise Invalid_argument if [bound] is [0.].

      @since 0.11 *)

  val int_range : ?origin:int -> int -> int -> int t
  (** [int_range ?origin low high] is an uniform integer generator producing integers within [low..high] (inclusive).

      Shrinks towards [origin] if specified, otherwise towards [low]
      (e.g. [int_range (-5) 15] will shrink towards [-5]).

      @raise Invalid_argument if any of the following holds:
      - [low > high]
      - [origin < low]
      - [origin > high]
  *)

  val (--) : int -> int -> int t
  (** [a -- b] is an alias for [int_range ~origin:a a b]. See {!int_range} for more information.

      Shrinks towards [a].
  *)

  val float_range : ?origin : float -> float -> float -> float t
  (** [float_range ?origin low high] generates floating-point numbers within [low] and [high] (inclusive).

      Shrinks towards [origin] if specified, otherwise towards [low]
      (e.g. [float_range 4.2 7.8] will shrink towards [4.2]).

      @raise Invalid_argument if any of the following holds:
      - [low > high]
      - [high -. low > max_float]
      - [origin < low]
      - [origin > high]

      @since 0.11 *)

  val (--.) : float -> float -> float t
  (** [a --. b] is an alias for [float_range ~origin:a a b]. See {!float_range} for more information.

      Shrinks towards [a].

      @since 0.11 *)

  (** {3 Choosing elements} *)

  val oneof : 'a t list -> 'a t
  (** [oneof l] constructs a generator that selects among the given list of generators [l].

      Shrinks towards the first generator of the list.
  *)

  val oneofl : 'a list -> 'a t
  (** [oneofl l] constructs a generator that selects among the given list of values [l].

      Shrinks towards the first element of the list.
  *)

  val oneofa : 'a array -> 'a t
  (** [oneofa a] constructs a generator that selects among the given array of values [a].

      Shrinks towards the first element of the array.
  *)

  val frequency : (int * 'a t) list -> 'a t
  (** Constructs a generator that selects among a given list of generators.
      Each of the given generators are chosen based on a positive integer weight.

      Shrinks towards the first element of the list.
  *)

  val frequencyl : (int * 'a) list -> 'a t
  (** Constructs a generator that selects among a given list of values.
      Each of the given values are chosen based on a positive integer weight.

      Shrinks towards the first element of the list.
  *)

  val frequencya : (int * 'a) array -> 'a t
  (** Constructs a generator that selects among a given array of values.
      Each of the array entries are chosen based on a positive integer weight.

      Shrinks towards the first element of the array.
  *)

  (** {3 Shuffling elements} *)

  val shuffle_a : 'a array -> 'a array t
  (** Returns a copy of the array with its elements shuffled. *)

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

  (** {3 Shrinkers} *)

  val number_towards : equal : ('a -> 'a -> bool) -> div : ('a -> 'a -> 'a) -> add : ('a -> 'a -> 'a) -> sub : ('a -> 'a -> 'a) -> of_int : (int -> 'a) -> destination : 'a -> 'a -> 'a Seq.t
  (** Shrink a number by edging towards a destination.

      The destination is always the first value for optimal shrinking.

      {[
        let int64_towards_list destination x = List.of_seq @@ Int64.(Gen.number_towards ~equal ~div ~add ~sub ~of_int) ~destination x

        let () =
          assert (int64_towards_list 0L 100L = [0L; 50L; 75L; 88L; 94L; 97L; 99L]);
          assert (int64_towards_list 500L 1000L = [500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L]);
          assert (int64_towards_list (-50L) (-26L) = [-50L; -38L; -32L; -29L; -28L; -27L])
      ]}
  *)

  val int_towards : int -> int -> int Seq.t
  (** {!number_towards} specialized to {!int}. *)

  val int32_towards : int32 -> int32 -> int32 Seq.t
  (** {!number_towards} specialized to {!int32}. *)

  val int64_towards : int64 -> int64 -> int64 Seq.t
  (** {!number_towards} specialized to {!int64}. *)

  val float_towards : float -> float -> float Seq.t
  (** {!number_towards} specialized to {!float}. *)

  (** {3 Corner cases} *)

  val graft_corners : 'a t -> 'a list -> unit -> 'a t
  (** [graft_corners gen l ()] makes a new generator that enumerates
      the corner cases in [l] and then behaves like [g].

      Does not shrink if the test fails on a grafted value.
      Shrinks towards [gen] otherwise.

      @since 0.6 *)

  val int_pos_corners : int list
  (** Non-negative corner cases for int.

      @since 0.6 *)

  val int_corners : int list
  (** All corner cases for int.

      @since 0.6 *)

  (** {3 Lists, arrays and option generators} *)

  val list : 'a t -> 'a list t
  (** Builds a list generator from an element generator. List size is generated by {!nat}.

      Shrinks on the number of elements first, then on elements.
  *)

  val small_list : 'a t -> 'a list t
  (** Generates lists of small size (see {!small_nat}).

      @since 0.5.3 *)

  val list_size : int t -> 'a t -> 'a list t
  (** Builds a list generator from a (non-negative) size generator and an element generator.

      Shrinks on the number of elements first, then on elements.
  *)

  val list_repeat : int -> 'a t -> 'a list t
  (** [list_repeat i g] builds a list generator from exactly [i] elements generated by [g].

      Shrinks on elements only.
  *)

  val array : 'a t -> 'a array t
  (** Builds an array generator from an element generator. Array size is generated by {!nat}.

      Shrinks on the number of elements first, then on elements.
  *)

  val array_size : int t -> 'a t -> 'a array t
  (** Builds an array generator from a (non-negative) size generator and an element generator.

      Shrinks on the number of elements first, then on elements.
  *)

  val small_array : 'a t -> 'a array t
  (** Generates arrays of small size (see {!small_nat}).

      Shrinks on the number of elements first, then on elements.

      @since 0.10 *)

  val array_repeat : int -> 'a t -> 'a array t
  (** [array_repeat i g] builds an array generator from exactly [i] elements generated by [g].

      Shrinks on elements only.
  *)

  val opt : 'a t -> 'a option t
  (** [opt gen] is an option generator.

      Shrinks towards {!None} then towards shrinks of [gen].
  *)

  (** {3 Combining generators} *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair gen1 gen2] generates pairs.

      Shrinks on [gen1] and then [gen2].
  *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple gen1 gen2 gen3] generates triples.

      Shrinks on [gen1], then [gen2] and then [gen3].
  *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [quad gen1 gen2 gen3 gen4] generates quadruples.

      Shrinks on [gen1], then [gen2], then [gen3] and then [gen4].

      @since 0.5.1
  *)

  (** {3 Convert a structure of generator to a generator of structure} *)

  val flatten_l : 'a t list -> 'a list t
  (** Generate a list of elements from individual generators.

      Shrinks on the elements of the list, in the list order.

      @since 0.13 *)

  val flatten_a : 'a t array -> 'a array t
  (** Generate an array of elements from individual generators.

      Shrinks on the elements of the array, in the array order.

      @since 0.13 *)

  val flatten_opt : 'a t option -> 'a option t
  (** Generate an option from an optional generator.

      Shrinks towards {!None} then shrinks on the value.

      @since 0.13 *)

  val flatten_res : ('a t, 'e) result -> ('a,'e) result t
  (** Generate a result from [Ok gen], an error from [Error e].

      Shrinks on [gen] if [Ok gen].
      Does not shrink if [Error e].

      @since 0.13 *)

  val join : 'a t t -> 'a t
  (** Collapses a generator of generators to a generator.

      Shrinks on the generated generators.

      @since 0.5 *)

  (** {3 Influencing the size of generated values} *)

  val sized : 'a sized -> 'a t
  (** Creates a generator from a size-bounded generator by first
      generating a size using {!nat} and passing the result to the size-bounded generator.

      Shrinks on the size first, then on the generator.
  *)

  val sized_size : int t -> 'a sized -> 'a t
  (** Creates a generator from a size-bounded generator by first
      generating a size using the integer generator and passing the result
      to the size-bounded generator.

      Shrinks on the size first, then on the generator.

      @since 0.5 *)

  (** {3 Recursive data structures} *)

  val fix : (('a -> 'b t) -> 'a -> 'b t) -> 'a -> 'b t
  (** Parametrized fixpoint combinator for generating recursive values.

      The fixpoint is parametrized over an arbitrary state ['a], and the
      fixpoint computation may change the value of this state in the recursive
      calls.

      In particular, this can be used for size-bounded generators (with ['a] as [int]).
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

      [fix f] shrinks on the generators returned by [f].
  *)

  val delay : (unit -> 'a t) -> 'a t
  (** Delay execution of some code until the generator is actually called.
      This can be used to manually implement recursion or control flow
      in a generator.
      @since 0.17 *)

  (** {2:composing_generators Composing generators}

      QCheck generators compose well: it means one can easily craft generators for new values
      or types from existing generators.

      Part of the following documentation is greatly inspired by Gabriel Scherer's excellent
      {{:http://gasche.github.io/random-generator/doc/Generator.html } Generator} module documentation.

      {3 Functor}

      [Gen.t] is a functor (in the Haskell sense of "mappable"): it has a [map] function to transform a generator of ['a] into a generator of ['b],
      given a simple function ['a -> 'b].

      {[
        let even_gen : int Gen.t = Gen.map (fun n -> n * 2) Gen.int

        let odd_gen : int Gen.t = Gen.map (fun n -> n * 2 + 1) Gen.int

        let lower_case_string_gen : string Gen.t = Gen.map String.lowercase Gen.string_readable

        type foo = Foo of string * int
        let foo_gen : foo Gen.t =
          Gen.map (fun (s, n) -> Foo (s, n)) Gen.(pair string_readable int)
      ]}

      {3 Applicative}

      [Gen.t] is applicative: it has a [map2] function to apply a function of 2 (or more) arguments to 2 (or more) generators.

      Another equivalent way to look at it is that it has an [ap] function to apply a generator of
      functions to a generator of values. While at first sight this may look almost useless, it actually
      permits a nice syntax (using the operator alias [<*>]) for functions of any number of arguments.

      {[
        (* Notice that this looks suspiciously like the [foo] example above:
           this is no coincidence! [pair] is a special case of [map2] where
           the function wraps arguments in a tuple. *)
        type foo = Foo of string * int
        let foo_gen : foo Gen.t =
          Gen.map2 (fun s n -> Foo (s, n)) Gen.string_readable Gen.int

        let string_prefixed_with_keyword_gen : string Gen.t =
          Gen.map2 (fun prefix s -> prefix ^ s)
            (Gen.oneofl ["foo"; "bar"; "baz"])
            Gen.string_readable
      ]}

      Applicatives are useful when you need several random values to build a new generator,
      {b and the values are unrelated}. A good rule of thumb is: if the values could be generated
      in parallel, then you can use an applicative function to combine those generators.

      Note that while [map2] and [map3] are provided, you can use functions with more than 3
      arguments (and that is where the [<*>] operator alias really shines):

      {[
        val complex_function : bool -> string -> int -> string -> int64 -> some_big_type

        (* Verbose version, using map3 and ap *)
        let big_type_gen : some_big_type Gen.t = Gen.(
            ap (
              ap (
                map3 complex_function
                  bool
                  string_readable
                  int)
                string_readable)
              int64)

        (* Sleeker syntax, using operators aliases for map and ap *)
        let big_type_gen : some_big_type Gen.t = Gen.(
            complex_function
            <$> bool
            <*> string_readable
            <*> int
            <*> string_readable
            <*> int64)
      ]}

      {3 Monad}

      [Gen.t] is a monad: it has a [bind] function to return a {b generator} (not a value)
      based on {b another generated value}.

      As an example, imagine you want to create a generator of [(int, string) result] that is
      an [Ok] 90% of the time and an [Error] 10% of the time. You can generate a number between
      0 and 9 and and return a generator of [int] (warpped in an [Ok] using [map]) if the generated number is
      lower than 9, otherwise return a generator of [string] (wrapped in an [Error] using [map]):
      {[
        let int_string_result : (int, string) result Gen.t =
          Gen.(bind (int_range 0 9) (fun n ->
            if n < 9 
              then map Result.ok int
              else map Result.error string_readable))

        (* Alternative syntax with operators *)
        let int_string_result : (int, string) result Gen.t =
          Gen.(int_range 0 9 >>= fun n ->
            if n < 9
              then int >|= Result.ok
              else string_readable >|= Result.error)
      ]}

      Note that this particular scenario can be simplified by using [frequency]:
      {[
        let int_string_result : (int, string) result Gen.t =
          Gen.(frequency [
            (9, int >|= Result.ok);
            (1, string_readable >|= Result.error)])
      ]}

  *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f gen] transforms a generator [gen] by applying [f] to each generated element.

      Shrinks towards the shrinks of [gen] with [f] applied to them.
  *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** An infix synonym for {!map}. *)

  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** An infix synonym for {!map}

      @since 0.13 *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f gen1 gen2] transforms two generators [gen1] and [gen2] by applying [f] to each
      pair of generated elements.

      Shrinks on [gen1] and then [gen2].
  *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** [map3 f gen1 gen2 gen3] transforms three generators [gen1], [gen2], and [gen3] by applying [f]
      to each triple of generated elements.

      Shrinks on [gen1], then [gen2], and then [gen3].
  *)

  val ap : ('a -> 'b) t -> 'a t -> 'b t
  (** [ap fgen gen] composes a function generator and an argument generator
      into a result generator.

      Shrinks on [fgen] and then [gen].
  *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Synonym for {!ap} *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind for writing dependent generators.

      [bind gen f] first generates a value of type ['a] with [gen] and then
      passes it to [f] to generate a value of type ['b].

      Shrinks on [gen] and then on the resulting generator.
  *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Synonym for {!bind} *)

  (** {3 Observing generated values} *)

  val generate : ?rand:Random.State.t -> n:int -> 'a t -> 'a list
  (** [generate ~n gen] generates [n] instances of [gen]. *)

  val generate1 : ?rand:Random.State.t -> 'a t -> 'a
  (** [generate1 gen] generates one instance of [gen]. *)

  val generate_tree : ?rand:Random.State.t -> 'a t -> 'a Tree.t
  (** [generate_tree ?rand gen] generates a random value and its shrinks using [gen]. *)

  include Qcheck_ops.S with type 'a t_let := 'a t
  (** @since 0.15 *)
end

(** {2 Show Values} *)
module Print : sig
  type 'a t = 'a -> string
  (** Printer for values of type ['a]. *)

  val unit : unit t (** @since 0.6 *)

  val int : int t (** Integer printer. *)

  val bool : bool t (** Boolean printer. *)

  val float : float t (** Floating point number printer. *)

  val char : char t (** Character printer. *)

  val string : string t (** String printer. *)

  val option : 'a t -> 'a option t (** Option printer. *)

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
end

(** {2 Iterators}

    Compatible with the library "sequence". An iterator [i] is simply
    a function that accepts another function [f] (of type ['a -> unit])
    and calls [f] on a sequence of elements [f x1; f x2; ...; f xn]. *)
module Iter : sig
  type 'a t = ('a -> unit) -> unit

  val empty : 'a t
  val return : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val (<+>) : 'a t -> 'a t -> 'a t (** Synonym to {!append}. *)

  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val find : ('a -> bool) -> 'a t -> 'a option

  val filter : ('a -> bool) -> 'a t -> 'a t

  val append_l : 'a t list -> 'a t
  (** @since 0.8 *)

  val flatten : 'a t t -> 'a t
  (** @since 0.8 *)

  include Qcheck_ops.S with type 'a t_let := 'a t
  (** @since 0.15 *)
end

(** {2 Shrink Values}

    Shrinking is used to reduce the size of a counter-example. It tries
    to make the counter-example smaller by decreasing it, or removing
    elements, until the property to test holds again; then it returns the
    smallest value that still made the test fail. *)
module Shrink : sig
  type 'a t = 'a -> 'a Iter.t
  (** Given a counter-example, return an iterator on smaller versions
      of the counter-example. *)

  val nil : 'a t
  (** No shrink *)

  val unit : unit t (** @since 0.6 *)

  val char : char t (** @since 0.6 *)

  val int : int t

  val int32 : int32 t
  (** @since 0.14 *)

  val int64 : int64 t
  (** @since 0.14 *)

  val option : 'a t -> 'a option t

  val string : string t

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
end

(** An observable is a random function {i argument}. *)
module Observable : sig
  (**
     While random functions don't need to generate {i values} of their arguments,
     they need the abilities to:
     - compare, using [equal] and [hash], so that the same argument always returns
       the same generated value
     - [print], in order to print the function implementation (bindings)
       in case of test failure

     Inspired by:
     - Jane Street {{: https://blogs.janestreet.com/quickcheck-for-core/} Quickcheck for Core} blog post
     - Koen Claessen's {{: https://www.youtube.com/watch?v=CH8UQJiv9Q4} Shrinking and Showing functions} paper

     @since 0.6
  *)

  type -'a t
  (** An observable of ['a], packing a printer and other things. *)

  val make :
    ?eq:('a -> 'a -> bool) ->
    ?hash:('a -> int) ->
    'a Print.t ->
    'a t
  (** [make ?eq ?hash print] creates an observable of ['a].

      If [eq] is [None], uses the standard polymorphic [(=)] function.

      If [hash] is [None], uses a default hashing function.
  *)

  val equal : 'a t -> 'a -> 'a -> bool
  (** [equal o] returns the equality function of [o]. *)

  val hash : 'a t -> 'a -> int
  (** [hash o] returns the hashing function of [o]. *)

  val print : 'a t -> 'a Print.t
  (** [print o] returns the printing function of [o]. *)

  val unit : unit t
  (** [unit] is an observable of [unit]. *)

  val bool : bool t
  (** [bool] is an observable of [bool]. *)

  val int : int t
  (** [int] is an observable of [int]. *)

  val float : float t
  (** [float] is an observable of [float]. *)

  val string : string t
  (** [string] is an observable of [string]. *)

  val char : char t
  (** [char] is an observable of [char]. *)

  val map : ('b -> 'a) -> 'a t -> 'b t
  (** [map f o] maps the function [f] on observable [o].

      Note the reverse order of types in [f] which may be
      conter-intuitive: indeed a function that consumes values of type
      ['b] can be obtained by transforming a value of type ['b] to
      ['a] using [f], and then by consuming this value of type ['a] using [o].
  *)

  val option : 'a t -> 'a option t
  (** [option o] wraps the observable [o] of ['a] into an observable of
      ['a option]. *)

  val list : 'a t -> 'a list t
  (** [list o] wraps the observable [o] of ['a] into an observable of
      ['a list]. *)

  val array : 'a t -> 'a array t
  (** [array o] wraps the observable [o] of ['a] into an observable of
      ['a array]. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair o1 o2] is an observable of pairs of [('a * 'b)]. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** [triple o1 o2 o3] is an observable of triples of [('a * 'b * 'c)]. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** [quad o1 o2 o3 o4] is an observable of quadruples of [('a * 'b * 'c * 'd)]. *)
end

(** {2 Arbitrary}

    A value of type ['a arbitrary] glues together a random generator,
    shrinking, and optional functions for printing, computing the size,
    etc. It is the "normal" way of describing how to generate
    values of a given type, to be then used in tests (see {!Test}). *)

type 'a stat = string * ('a -> int)
(** A statistic on a distribution of values of type ['a].
    The function {b MUST} return a positive integer. *)

type 'a arbitrary = private {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
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
  ?collect:('a -> string) ->
  ?stats:'a stat list ->
  'a Gen.t -> 'a arbitrary
(** Builder for [arbitrary]. Default is to only have a generator, but other
    arguments can be added.
    @param print printer for values (counter-examples)
    @param collect for statistics
*)

val set_print : 'a Print.t -> 'a arbitrary -> 'a arbitrary
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

val get_gen : 'a arbitrary -> 'a Gen.t
(** Access the underlying random generator of this arbitrary object.
    @since 0.6 *)

val get_print : 'a arbitrary -> 'a Print.t option
(** Access the underlying printer of this arbitrary object. *)

(** {2 Tests}

    A test is a universal property of type [foo -> bool] for some type [foo],
    with an object of type [foo arbitrary] used to generate, print, etc. values
    of type [foo].

    See {!Test.make} to build a test, and {!Test.check_exn} to
    run one test simply.
    For more serious testing, it is better to create a testsuite
    and use {!QCheck_runner}.
*)

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

(** Result of running a test *)
module TestResult : sig
  type 'a counter_ex = {
    instance: 'a; (** The counter-example(s) *)

    shrink_steps: int; (** How many shrinking steps for this counterex *)

    msg_l: string list;
    (** messages.
        @since 0.7 *)
  }

  type 'a failed_state = 'a counter_ex list

  (** Result state.
      changed in 0.10 (move to inline records, add Fail_other) *)
  type 'a state =
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
  type 'a t = private {
    mutable state : 'a state;
    mutable count: int;  (* Number of tests *)
    mutable count_gen: int; (* Number of generated cases *)
    collect_tbl: (string, int) Hashtbl.t lazy_t;
    stats_tbl: ('a stat * (int, int) Hashtbl.t) list; (** @since 0.6 *)
    mutable warnings: string list;
    mutable instances: 'a list;
    (** List of instances used for this test, in no particular order.
        @since 0.9 *)
  }

  val get_count : _ t -> int

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

module Test : sig
  type 'a cell
  (** A single property test *)

  val fail_report : string -> 'a
  (** Fail the test with some additional message that will
      be reported.
      @since 0.7 *)

  val fail_reportf : ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Format version of {!fail_report}
      @since 0.7 *)

  val make_cell :
    ?if_assumptions_fail:([`Fatal | `Warning] * float) ->
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?name:string -> 'a arbitrary -> ('a -> bool) ->
    'a cell
  (** [make_cell arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      @param name the name of the test.
      @param count number of test cases to run, counting only
        the test cases which satisfy preconditions.
      @param long_factor the factor by which to multiply count, max_gen and
        max_fail when running a long test (default: 1).
      @param max_gen maximum number of times the generation function
        is called in total to replace inputs that do not satisfy
        preconditions (should be >= count).
      @param max_fail maximum number of failures before we stop generating
        inputs. This is useful if shrinking takes too much time.
      @param if_assumptions_fail the minimum
        fraction of tests that must satisfy the precondition for a success
        to be considered valid.
        The fraction should be between 0. and 1.
        A warning will be emitted otherwise if
        the flag is [`Warning], the test will be a failure if the flag is [`Fatal].
        (since 0.10)
  *)

  val get_arbitrary : 'a cell -> 'a arbitrary
  val get_law : 'a cell -> ('a -> bool)
  val get_name : _ cell -> string
  val set_name : _ cell -> string -> unit

  val get_count : _ cell -> int
  (** Get the count of a cell.
      @since 0.5.3 *)

  val get_long_factor : _ cell -> int
  (** Get the long factor of a cell.
      @since 0.5.3 *)

  type t = Test : 'a cell -> t
  (** Same as ['a cell], but masking the type parameter. This allows to
      put tests on different types in the same list of tests. *)

  val make :
    ?if_assumptions_fail:([`Fatal | `Warning] * float) ->
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?name:string -> 'a arbitrary -> ('a -> bool) -> t
  (** [make arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      See {!make_cell} for a description of the parameters.
  *)

  (** {3 Running the test} *)

  exception Test_fail of string * string list
  (** Exception raised when a test failed, with the list of counter-examples.
      [Test_fail (name, l)] means test [name] failed on elements of [l]. *)

  exception Test_error of string * string * exn * string
  (** Exception raised when a test raised an exception [e], with
      the sample that triggered the exception.
      [Test_error (name, i, e, st)]
      means [name] failed on [i] with exception [e], and [st] is the
      stacktrace (if enabled) or an empty string. *)

  val print_instance : 'a arbitrary -> 'a -> string
  val print_c_ex : 'a arbitrary -> 'a TestResult.counter_ex -> string
  val print_fail : 'a arbitrary -> string -> 'a TestResult.counter_ex list -> string
  val print_fail_other : string -> msg:string -> string
  val print_error : ?st:string -> 'a arbitrary -> string -> 'a TestResult.counter_ex * exn -> string
  val print_test_fail : string -> string list -> string
  val print_test_error : string -> string -> exn -> string -> string

  val print_collect : (string,int) Hashtbl.t -> string
  (** Print "collect" results.
      @since 0.6 *)

  val print_stat : ('a stat * (int,int) Hashtbl.t) -> string
  (** Print statistics.
      @since 0.6 *)

  val check_result : 'a cell -> 'a TestResult.t -> unit
  (** [check_result cell res] checks that [res] is [Ok _], and returns unit.
      Otherwise, it raises some exception.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)

  type res =
    | Success
    | Failure
    | FalseAssumption
    | Error of exn * string

  type 'a event =
    | Generating
    | Collecting of 'a
    | Testing of 'a
    | Shrunk of int * 'a
    | Shrinking of int * int * 'a

  type 'a handler = string -> 'a cell -> 'a event -> unit
  (** Handler executed after each event during testing of an instance. *)

  type 'a step = string -> 'a cell -> 'a -> res -> unit
  (** Callback executed after each instance of a test has been run.
      The callback is given the instance tested, and the current results
      of the test. *)

  type 'a callback = string -> 'a cell -> 'a TestResult.t -> unit
  (** Callback executed after each test has been run.
      [f name cell res] means test [cell], named [name], gave [res]. *)

  val check_cell :
    ?long:bool -> ?call:'a callback ->
    ?step:'a step -> ?handler:'a handler ->
    ?rand:Random.State.t -> 'a cell -> 'a TestResult.t
  (** [check_cell ~long ~rand test] generates up to [count] random
      values of type ['a] using [arbitrary] and the random state [st]. The
      predicate [law] is called on them and if it returns [false] or raises an
      exception then we have a counter-example for the [law].

      @param long if [true] then multiply the number of instances to generate
        by the cell's long_factor.
      @param call function called on each test case, with the result.
      @param step function called on each instance of the test case, with the result.
      @return the result of the test.
  *)

  val check_cell_exn :
    ?long:bool -> ?call:'a callback -> ?step:'a step ->
    ?rand:Random.State.t -> 'a cell -> unit
  (** Same as {!check_cell} but calls  {!check_result} on the result.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)

  val check_exn : ?long:bool -> ?rand:Random.State.t -> t -> unit
  (** Checks the property against some test cases, and calls {!check_result},
      which might raise an exception in case of failure.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)
end

(** {2 Sub-tests} *)

(** The infrastructure used to find counter-examples to properties can
    also be used to find data satisfying a predicate,
    {i within a property being tested}.

    See https://github.com/c-cube/qcheck/issues/31
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

(** {2 Combinators for arbitrary} *)

val choose : 'a arbitrary list -> 'a arbitrary
(** Choose among the given list of generators. The list must not
    be empty; if it is Invalid_argument is raised. *)

val unit : unit arbitrary
(** Always generates [()], obviously. *)

val bool : bool arbitrary
(** Uniform boolean generator. *)

val float : float arbitrary
(** Generates regular floats (no nan and no infinities). *)
(* FIXME: does not generate nan nor infinity I think. *)

val pos_float : float arbitrary
(** Positive float generator (no nan and no infinities). *)

val neg_float : float arbitrary
(** Negative float generator (no nan and no infinities). *)

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
(** Synonym to {!int_range}. *)

val int32 : int32 arbitrary
(** Int32 generator. Uniformly distributed. *)

val int64 : int64 arbitrary
(** Int64 generator. Uniformly distributed. *)

val pos_int : int arbitrary
(** Positive int generator (0 included). Uniformly distributed.
    See {!Gen.pint} *)

val small_int_corners : unit -> int arbitrary
(** As [small_int], but each newly created generator starts with
    a list of corner cases before falling back on random generation. *)

val neg_int : int arbitrary
(** Negative int generator (0 included, see {!Gen.neg_int}).
    The distribution is similar to that of
    [small_int], not of [pos_int].
*)

val char : char arbitrary
(** Uniformly distributed on all the chars (not just ascii or
    valid latin-1). *)

val printable_char : char arbitrary
(** Uniformly distributed over a subset of chars. *)
(* FIXME: describe which subset. *)

val numeral_char : char arbitrary
(** Uniformly distributed over ['0'..'9']. *)

val string_gen_of_size : int Gen.t -> char Gen.t -> string arbitrary

val string_gen : char Gen.t -> string arbitrary
(** Generates strings with a distribution of length of [small_nat]. *)

val string : string arbitrary
(** Generates strings with a distribution of length of [small_nat]
    and distribution of characters of [char]. *)

val small_string : string arbitrary
(** Same as {!string} but with a small length (ie {!Gen.small_nat} ). *)

val small_list : 'a arbitrary -> 'a list arbitrary
(** Generates lists of small size (see {!Gen.small_nat}).
    @since 0.5.3 *)

val string_of_size : int Gen.t -> string arbitrary
(** Generates strings with distribution of characters if [char]. *)

val printable_string : string arbitrary
(** Generates strings with a distribution of length of [small_nat]
    and distribution of characters of [printable_char]. *)

val printable_string_of_size : int Gen.t -> string arbitrary
(** Generates strings with distribution of characters of [printable_char]. *)

val small_printable_string : string arbitrary

val numeral_string : string arbitrary
(** Generates strings with a distribution of length of [small_nat]
    and distribution of characters of [numeral_char]. *)

val numeral_string_of_size : int Gen.t -> string arbitrary
(** Generates strings with a distribution of characters of [numeral_char]. *)

val list : 'a arbitrary -> 'a list arbitrary
(** Generates lists with length generated by [small_nat]. *)

val list_of_size : int Gen.t -> 'a arbitrary -> 'a list arbitrary
(** Generates lists with length from the given distribution. *)

val array : 'a arbitrary -> 'a array arbitrary
(** Generates arrays with length generated by [small_nat]. *)

val array_of_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** Generates arrays with length from the given distribution. *)

val pair : 'a arbitrary -> 'b arbitrary -> ('a * 'b) arbitrary
(** Combines two generators into a generator of pairs.
    Order of elements can matter (w.r.t shrinking, see {!Gen.pair}) *)

val triple : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a * 'b * 'c) arbitrary
(** Combines three generators into a generator of 3-tuples.
    Order matters for shrinking, see {!Gen.pair} and the likes *)

val quad : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> 'd arbitrary -> ('a * 'b * 'c * 'd) arbitrary
(** Combines four generators into a generator of 4-tuples.
    Order matters for shrinking, see {!Gen.pair} and the likes *)

val option : 'a arbitrary -> 'a option arbitrary
(** Choose between returning Some random value, or None. *)

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

(** Utils on functions
    @since 0.6 *)
module Fn : sig
  type 'a t = 'a fun_

  val print : _ t Print.t

  val apply : 'f t -> 'f
end

val fun1 : 'a Observable.t -> 'b arbitrary -> ('a -> 'b) fun_ arbitrary
(** [fun1 o ret] makes random functions that take an argument observable
    via [o] and map to random values generated from [ret].
    To write functions with multiple arguments, it's better to use {!Tuple}
    or {!Observable.pair} rather than applying {!fun_} several times
    (shrinking will be faster).
    @since 0.6 *)

module Tuple : sig
  (** Heterogeneous tuple, used to pass any number of arguments to
      a function. *)
  type 'a t =
    | Nil : unit t
    | Cons : 'a * 'b t -> ('a * 'b) t

  val nil : unit t
  val cons : 'a -> 'b t -> ('a * 'b) t

  (** How to observe a  {!'a t} *)
  type 'a obs

  val o_nil : unit obs
  val o_cons : 'a Observable.t -> 'b obs -> ('a * 'b) obs

  module Infix : sig
    val (@::) : 'a -> 'b t -> ('a * 'b) t
    (** Alias to {!cons}. *)

    val (@->) : 'a Observable.t -> 'b obs -> ('a * 'b) obs
    (** Alias to {!B_cons}. *)
  end

  include module type of Infix

  val observable : 'a obs -> 'a t Observable.t
end

val fun_nary : 'a Tuple.obs -> 'b arbitrary -> ('a Tuple.t -> 'b) fun_ arbitrary
(** [fun_nary] makes random n-ary functions.
    Example:
    {[
      let module O = Observable in
      fun_nary Tuple.(O.int @-> O.float @-> O.string @-> o_nil) bool)
    ]}
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

val oneofl : ?print:'a Print.t -> ?collect:('a -> string) ->
  'a list -> 'a arbitrary
(** Pick an element randomly in the list. *)

val oneofa : ?print:'a Print.t -> ?collect:('a -> string) ->
  'a array -> 'a arbitrary
(** Pick an element randomly in the array. *)

val oneof : 'a arbitrary list -> 'a arbitrary
(** Pick a generator among the list, randomly. *)

val always : ?print:'a Print.t -> 'a -> 'a arbitrary
(** Always return the same element. *)

val frequency : ?print:'a Print.t -> ?collect:('a -> string) ->
  (int * 'a arbitrary) list -> 'a arbitrary
(** Similar to {!oneof} but with frequencies. *)

val frequencyl : ?print:'a Print.t -> (int * 'a) list -> 'a arbitrary
(** Same as {!oneofl}, but each element is paired with its frequency in
    the probability distribution (the higher, the more likely). *)

val frequencya : ?print:'a Print.t -> (int * 'a) array -> 'a arbitrary
(** Same as {!frequencyl}, but with an array. *)

val map : ?print:'b Print.t -> ?collect:('b -> string) -> ('a -> 'b) -> 'a arbitrary -> 'b arbitrary
(** [map f a] returns a new arbitrary instance that generates values using
    [a#gen] and then transforms them through [f].
*)

val map_same_type : ('a -> 'a) -> 'a arbitrary -> 'a arbitrary
(** Specialization of [map] when the transformation preserves the type, which
    makes printer, etc. still relevant. *)

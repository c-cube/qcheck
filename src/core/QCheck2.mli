(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

(* Keep the following title alone in its documentation block as it is specially treated by Odoc: it doesn't appear
in the Contents menu on the left. The next documentation block with all the actual
content will appear. *)
(** {1 QuickCheck-inspired property-based testing} *)

(** {1 Introduction}

    This library takes inspiration from Haskell's QuickCheck library. The
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

    💡 If you are migrating from QCheck, check the {{!section:migration_qcheck2} migration guide} below.

    {1 Examples}

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
  (** Small positive integers (< [100], [0] included).

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
  (** Generates {!int32} values.

      Shrinks towards [0l].

      @deprecated use {!val:int32} instead, the name is wrong, values {i are} signed.
  *)

  val int64 : int64 t
  (** Generates uniform {!int64} values.

      Shrinks towards [0L].
  *)

  val ui64 : int64 t
  (** Generates {!int64} values.

      Shrinks towards [0L].

      @deprecated use {!val:int64} instead, the name is wrong, values {i are} signed.
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

    The exhaustive list of character codes is:
    - [32] to [126], inclusive
    - ['\n']

    Shrinks towards ['a'].
  *)

  val numeral : char t
  (** Generates numeral characters ['0'..'9'].

      Shrinks towards ['0'].
  *)

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
      
      ⚠️ This is an unstable API as it partially exposes the implementation. In particular, the type of
      [Random.State.t] may very well change in a future version, e.g. if QCheck switches to another
      randomness library.
  *)

  val add_shrink_invariant : ('a -> bool) -> 'a t -> 'a t
  (** [add_shrink_invariant f gen] returns a generator similar to [gen] except all shrinks satisfy [f].
      This way it's easy to preserve invariants that are enforced by
      generators, when shrinking values

      @since 0.8

      @deprecated is this function still useful? I feel like it is either useless (invariants
      should already be part of the shrinking logic, not be added later) or a special,
      incomplete case of {!Gen.t} being an Alternative (not implemented yet). For now we
      keep it and wait for users feedback (hence deprecation to raise attention).
  *)

  (** {3 Ranges} *)

  val int_bound : int -> int t
  (** Uniform integer generator producing integers within [0..bound].

      Shrinks towards [0].

      @raise Invalid_argument if the argument is negative. *)

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

  val char_range : ?origin:char -> char -> char -> char t
  (** [char_range ?origin low high] generates chars between [low] and [high], inclusive.
      Example: [char_range 'a' 'z'] for all lower case ASCII letters.

      Shrinks towards [origin] if specified, otherwise towards [low].

      @raise Invalid_argument if [low > high].

      @since 0.13 *)

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

  (** {3 Lists, arrays and options} *)

  val list : 'a t -> 'a list t
  (** Builds a list generator from an element generator. List size is generated by {!nat}.

      Shrinks on the number of elements first, then on elements.
  *)

  val small_list : 'a t -> 'a list t
  (** Generates lists of small size (see {!small_nat}).

      Shrinks on the number of elements first, then on elements.

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
      0 and 9 and return a generator of [int] (wrapped in an [Ok] using [map]) if the generated number is
      lower than 9, otherwise return a generator of [string] (wrapped in an [Error] using [map]):
      {[
        let int_string_result : (int, string) result Gen.t = Gen.(
            bind (int_range 0 9) (fun n ->
                if n < 9
                then map Result.ok int
                else map Result.error string_readable))

        (* Alternative syntax with operators *)
        let int_string_result : (int, string) result Gen.t = Gen.(
            int_range 0 9 >>= fun n ->
            if n < 9
            then int >|= Result.ok
            else string_readable >|= Result.error)
      ]}

      Note that this particular use case can be simplified by using [frequency]:
      {[
        let int_string_result : (int, string) result Gen.t = Gen.(
            frequency [
              (9, int >|= Result.ok);
              (1, string_readable >|= Result.error)
            ])
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

  (** {2 Debug generators}

      These functions should not be used in tests: they are provided
      for convenience to debug/investigate what values and shrinks a
      generator produces.
  *)

  val generate : ?rand:Random.State.t -> n:int -> 'a t -> 'a list
  (** [generate ~n gen] generates [n] values using [gen] (shrinks are discarded). *)

  val generate1 : ?rand:Random.State.t -> 'a t -> 'a
  (** [generate1 gen] generates one instance of [gen] (shrinks are discarded). *)

  val generate_tree : ?rand:Random.State.t -> 'a t -> 'a Tree.t
  (** [generate_tree ?rand gen] generates a random value and its shrinks using [gen]. *)

  include Qcheck_ops.S with type 'a t_let := 'a t
  (** @since 0.15 *)
end

(** Printing functions and helpers, used to print generated values on
    test failures. *)
module Print : sig

  type 'a t = 'a -> string
  (** Printer for values of type ['a]. *)

  val unit : unit t
  (** [unit] is a printer of unit.

      @since 0.6
  *)

  val int : int t
  (** [int] is a printer of integer. *)

  val bool : bool t
  (** [bool] is a printer of boolean. *)

  val float : float t
  (** [float] is a printer of float. *)

  val char : char t
  (** [char] is a printer of character. *)

  val string : string t
  (** [string] is a printer of string. *)

  val option : 'a t -> 'a option t
  (** [option p] is a printer of ['a option], using [p] if it is a [Some]. *)

  val pair : 'a t -> 'b t -> ('a*'b) t
  (** [pair p1 p2] is a printer of pair. *)

  val triple : 'a t -> 'b t -> 'c t -> ('a*'b*'c) t
  (** [triple p1 p2 p3] is a printer of triple. *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a*'b*'c*'d) t
  (** [quad p1 p2 p3 p4] is a printer of quadruple. *)

  val list : 'a t -> 'a list t
  (** [list p] is a printer of list, using [p] for each element. *)

  val array : 'a t -> 'a array t
  (** [array p] is a printer of array, using [p] for each element. *)

  val contramap : ('b -> 'a) -> 'a t -> 'b t
  (** [contramap f p] transforms printer [p] into another using [f].

      Note the reverse order of types in [f] which may be
      conter-intuitive: indeed a function that {i prints} values of type
      ['b] can be obtained by transforming a value of type ['b] to
      ['a] using [f], and then by {i printing} this value of type ['a] using [p].
  *)

  val comap : ('b -> 'a) -> 'a t -> 'b t
  (** @deprecated use {!contramap} instead. *)
end

(** Shrinking helper functions. *)
module Shrink : sig
  (** Shrinking is used to reduce the size of a counter-example. It tries
      to make the counter-example smaller by decreasing it, or removing
      elements, until the property to test holds again; then it returns the
      smallest value that still made the test fail.

      This is meant to help developers find a simpler counter-example to
      ease investigation and find more easily the root cause (be it in the
      tested code or in the test).

      This module exposes helper functions that one can reuse in combination
      with {!Gen.make_primitive} to craft custom primitive generators (not
      by composing other generators). The vast majority of use cases will
      probably not need this module.
  *)

  (** Util module representing a number type, used for ad hoc polymorphism of
      some functions like {!number_towards}. *)
  module type Number = sig
    type t
    val equal : t -> t -> bool
    val div : t -> t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val of_int : int -> t
  end

  val number_towards : (module Number with type t = 'a) -> destination : 'a -> 'a -> 'a Seq.t
  (** Shrink a number by edging towards a destination.

      The destination is always the first value for optimal shrinking.

      {[
        let int64_towards_list destination x = List.of_seq @@
          Gen.number_towards (module Int64) ~destination x
        in
        assert (int64_towards_list 0L 100L =
                [0L; 50L; 75L; 88L; 94L; 97L; 99L]);
        assert (int64_towards_list 500L 1000L =
                [500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L]);
        assert (int64_towards_list (-50L) (-26L) =
                [-50L; -38L; -32L; -29L; -28L; -27L])
      ]}

      This generic function is exposed to let users reuse this shrinking
      technique for their custom number types. More specialized, convenient
      functions are provided below, e.g. {!int_towards}.
  *)

  val int_towards : int -> int -> int Seq.t
  (** {!number_towards} specialized to {!int}. *)

  val int32_towards : int32 -> int32 -> int32 Seq.t
  (** {!number_towards} specialized to {!int32}. *)

  val int64_towards : int64 -> int64 -> int64 Seq.t
  (** {!number_towards} specialized to {!int64}. *)

  val float_towards : float -> float -> float Seq.t
  (** {!number_towards} specialized to {!float}.

      There are various ways to shrink a float:
      - try removing floating digits, i.e. towards integer values
      - try to get as close as possible to the destination, no matter the number of digits
      - a mix of both

      This implementation, as it relies on the generic {!number_towards} function,
      tries to get as close as possible to the destination, e.g. the last value of
      [Gen.float_towards 50 100] may be [99.9969482421875] (or a similar value).
  *)

  val int_aggressive_towards : int -> int -> int Seq.t
  (** [int_agressive_towards destination n] gives all integers from [destination] to [n] (excluded).

      {b Be careful about time and memory} as the resulting list can be huge *)

  val int_aggressive : int -> int Seq.t
  (** @deprecated Use [int_aggressive_towards 0] instead.
      @since 0.7 *)

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

  val contramap : ('b -> 'a) -> 'a t -> 'b t
  (** [contramap f o] maps the function [f] on observable [o].

      Note the reverse order of types in [f] which may be
      conter-intuitive: indeed a function that {i consumes} values of type
      ['b] can be obtained by transforming a value of type ['b] to
      ['a] using [f], and then by {i consuming} this value of type ['a] using [o].
  *)

  val map : ('b -> 'a) -> 'a t -> 'b t
  (** @deprecated use {!contramap} instead. *)

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

(** {1 Arbitrary} *)

type 'a arbitrary
(** A value of type ['a arbitrary] is a {!Gen.t} packaged with some optional features:
    - [print] generated values to display counter-examples when a test fails
    - [collect] values by tag, useful to display distribution of generated values
    - [stats] to get some statistics about generated values

    ⚠️ The collect field is unstable and might be removed, or
    moved into {!Test}.

    Abstract since QCheck2.
*)

type 'a stat = string * ('a -> int)
(** A statistic on a distribution of values of type ['a].
    The function {b MUST} return a positive integer. *)

val make :
  ?print:'a Print.t ->
  ?collect:('a -> string) ->
  ?stats:'a stat list ->
  'a Gen.t -> 'a arbitrary
(** [make ?print ?collect ?stats gen] builds an [arbitrary].

    Only the generator is mandatory, all other arguments are optional.
    That being said, we recommend setting at least the [print] parameter to
    get better error messages in case of test failures.
*)

val set_gen : 'a Gen.t -> 'a arbitrary -> 'a arbitrary
(** [set_gen gen arb] returns an arbitrary similar to [arb] where the generating function is [gen].

    @since 0.7
*)

val set_print : 'a Print.t -> 'a arbitrary -> 'a arbitrary
(** [set_print p arb] returns an arbitrary similar to [arb] where the printing function is [p]. *)

val set_collect : ('a -> string) -> 'a arbitrary -> 'a arbitrary
(** [set_collect c arb] returns an arbitrary similar to [arb] where the collecting function is [c]. *)

val set_stats : 'a stat list -> 'a arbitrary -> 'a arbitrary
(** [set_stats s arb] returns an arbitrary similar to [arb] where the statistics function is [s].

    @since 0.6
*)

val add_stat : 'a stat -> 'a arbitrary -> 'a arbitrary
(** [add_stat s arb] adds the statistic [s] to the arbitrary instance [arb].

    @since 0.6 *)

val get_gen : 'a arbitrary -> 'a Gen.t
(** [get_gen arb] returns the underlying random generator of [arb].

    @since 0.6 *)

val get_print : 'a arbitrary -> 'a Print.t option
(** [get_print arb] returns the underlying optional value printer of [arb]. *)

(** {2 Primitive arbitraries} *)

val unit : unit arbitrary
(** Always generates [()].

    Does not shrink. *)

val bool : bool arbitrary
(** Boolean generator. 

    Uniformly distributed.
    
    Shrinks towards [false]. *)

val int : int arbitrary
(** Int generator.

    Uniformly distributed.
    
    Shrinks towards [0]. *)

val pos_int : int arbitrary
(** Positive int generator ([0] included).

    Uniformly distributed.
    
    Shrinks towards [origin] if specified, otherwise towards [0]. *)

val small_nat : int arbitrary
(** Small positive integers (< [100], [0] included).

    Non-uniform: smaller numbers are more likely than bigger numbers.

    Shrinks towards [0].

    @since 0.5.1 *)

val small_int : int arbitrary
(** Small unsigned integers.

    @deprecated use {!small_signed_int}. *)

val small_signed_int : int arbitrary
(** Small SIGNED integers, based on {!small_nat}.

    Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

    Shrinks towards [0].

    @since 0.5.2 *)

val int32 : int32 arbitrary
(** Int32 generator.

    Uniformly distributed.
    
    Shrinks towards [0l]. *)

val int64 : int64 arbitrary
(** Int64 generator.

    Uniformly distributed.
    
    Shrinks towards [0L]. *)

val float : float arbitrary
(** Generates regular floats (no nan and no infinities).

    Shrinks towards [0.]. *)
(* FIXME: does not generate nan nor infinity I think. *)

val pos_float : float arbitrary
(** Positive float generator (no nan and no infinities).

    Shrinks towards [0.]. *)

val neg_float : float arbitrary
(** Negative float generator (no nan and no infinities).

    Shrinks towards [-0.]. *)

val small_int_corners : unit -> int arbitrary
(** As {!small_int}, but each newly created generator starts with
    a list of corner cases before falling back on random generation. *)

val neg_int : int arbitrary
(** Generates non-strictly negative integers ([0] included).

    Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

    Shrinks towards [0].
*)

val char : char arbitrary
(** Generates characters in the [0..255] range.

    Uniformly distributed.

    Shrinks towards ['a']. *)

val printable_char : char arbitrary
(** Generates printable characters.

    The exhaustive list of character codes is:
    - [32] to [126], inclusive
    - ['\n']

    Shrinks towards ['a']. *)

val numeral_char : char arbitrary
(** Generates numeral characters ['0'..'9'].

    Shrinks towards ['0'].
*)

val string_gen_of_size : int Gen.t -> char Gen.t -> string arbitrary
(** [string_gen_of_size size gen] builds a string arbitrary from a (non-negative) size generator [size].
    All characters are generated using [gen].

    Shrinks on [size] first, then on [gen].
*)

val string_gen : char Gen.t -> string arbitrary
(** [string_gen gen] generates strings with a distribution of length of {!Gen.nat}.
    All characters are generated using [gen].

    Shrinks on {!Gen.nat} first, then on [gen]. *)

val string : string arbitrary
(** [string] generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of {!Gen.char}.
    
    Shrinks on {!Gen.nat} first, then on {!Gen.char}. *)

val small_string : string arbitrary
(** Same as {!string} but uses {!Gen.small_nat} for the size. *)

val string_of_size : int Gen.t -> string arbitrary
(** [string_of_size size] generates strings with a distribution of length of [size]
    and distribution of characters of {!Gen.char}.
    
    Shrinks on [size] first, then on {!Gen.char}. *)

val printable_string : string arbitrary
(** [printable_string] generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of {!Gen.printable}.
    
    Shrinks on {!Gen.nat} first, then on {!Gen.printable}. *)

val printable_string_of_size : int Gen.t -> string arbitrary
(** [printable_string_of_size size] generates strings with a distribution of length of [size]
    and distribution of characters of {!Gen.printable}.
    
    Shrinks on [size] first, then on {!Gen.printable}. *)

val small_printable_string : string arbitrary
(** [small_printable_string] generates strings with a distribution of length of {!Gen.small_nat}
    and distribution of characters of {!Gen.printable}.
    
    Shrinks on {!Gen.small_nat} first, then on {!Gen.printable}. *)

val numeral_string : string arbitrary
(** [numeral_string] generates strings with a distribution of length of {!Gen.nat}
    and distribution of characters of {!Gen.numeral}.
    
    Shrinks on {!Gen.nat} first, then on {!Gen.numeral}. *)

val numeral_string_of_size : int Gen.t -> string arbitrary
(** [numeral_string_of_size size] generates strings with a distribution of length of [size]
    and distribution of characters of {!Gen.numeral}.
    
    Shrinks on [size] first, then on {!Gen.numeral}. *)

(** {2 Ranges} *)

val int_bound : int -> int arbitrary
(** Uniform integer generator producing integers within [0..bound].

    Shrinks towards [0].

    @raise Invalid_argument if the argument is negative. *)

val int_range : ?origin:int -> int -> int -> int arbitrary
(** [int_range ?origin low high] is an uniform integer generator producing integers within [low..high] (inclusive).

    Shrinks towards [origin] if specified, otherwise towards [low]
    (e.g. [int_range (-5) 15] will shrink towards [-5]).

    @raise Invalid_argument if any of the following holds:
    - [low > high]
    - [origin < low]
    - [origin > high] *)

val (--) : int -> int -> int arbitrary
(** [a -- b] is an alias for [int_range ~origin:a a b]. See {!int_range} for more information.

    Shrinks towards [a]. *)

val float_bound_inclusive : ?origin : float -> float -> float arbitrary
(** [float_bound_inclusive ?origin bound] returns a random floating-point number between [0.] and
    [bound] (inclusive). If [bound] is negative, the result is negative or zero.  If
    [bound] is [0.], the result is [0.].

    Shrinks towards [origin] if given, otherwise towards [0.].

    @since 0.11 *)

val float_bound_exclusive : ?origin : float -> float -> float arbitrary
(** [float_bound_exclusive origin bound] returns a random floating-point number between [0.] and
    [bound] (exclusive).  If [bound] is negative, the result is negative or zero.

    Shrinks towards [origin] if given, otherwise towards [0.].

    @raise Invalid_argument if [bound] is [0.].

    @since 0.11 *)

val float_range : ?origin : float -> float -> float -> float arbitrary
(** [float_range ?origin low high] generates floating-point numbers within [low] and [high] (inclusive).

    Shrinks towards [origin] if specified, otherwise towards [low]
    (e.g. [float_range 4.2 7.8] will shrink towards [4.2]).

    @raise Invalid_argument if any of the following holds:
    - [low > high]
    - [high -. low > max_float]
    - [origin < low]
    - [origin > high]

    @since 0.11 *)

(** {2 Choosing elements} *)

val choose : 'a arbitrary list -> 'a arbitrary
(** Choose among the given list of generators. The list must not
    be empty; if it is Invalid_argument is raised.
        
    Shrinks towards the first arbitrary of the list. *)

(** {2 Lists, arrays and options} *)

val small_list : 'a arbitrary -> 'a list arbitrary
(** [small_list arb] builds a list arbitrary from an element arbitrary [arb]. List size is generated by {!Gen.small_nat}.

    Shrinks on the number of elements first, then on elements.

    @since 0.5.3 *)

val list : 'a arbitrary -> 'a list arbitrary
(** [list arb] builds a list arbitrary from an element arbitrary [arb]. List size is generated by {!Gen.nat}.

    Shrinks on the number of elements first, then on elements.
*)

val list_of_size : int Gen.t -> 'a arbitrary -> 'a list arbitrary
(** [list_of_size size arb] builds a list arbitrary from an element arbitrary [arb]. List size is generated by [size].

    Shrinks on the number of elements first, then on elements.
*)

val array : 'a arbitrary -> 'a array arbitrary
(** [array arb] builds an array arbitrary from an element arbitrary [arb]. Array size is generated by {!Gen.nat}.

    Shrinks on the number of elements first, then on elements.
*)

val array_of_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** [array_of_size size arb] builds an array arbitrary from an element arbitrary [arb]. Array size is generated by [size].

    Shrinks on the number of elements first, then on elements.
*)

val option : 'a arbitrary -> 'a option arbitrary
(** [option arb] is an option arbitrary.

    Shrinks towards {!None} then towards shrinks of [arb].
*)

(** {2 Combining arbitraries} *)

val pair : 'a arbitrary -> 'b arbitrary -> ('a * 'b) arbitrary
(** [pair arb1 arb2] generates pairs.

    Shrinks on [arb1] and then [arb2].
*)

val triple : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a * 'b * 'c) arbitrary
(** [triple arb1 arb2 arb3] generates triples.

    Shrinks on [arb1], then [arb2], and then [arb3].
*)

val quad : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> 'd arbitrary -> ('a * 'b * 'c * 'd) arbitrary
(** [quad arb1 arb2 arb3 arb4] generates quadruples.

    Shrinks on [arb1], then [arb2], then [arb3], and then [arb4].
*)

(** {2 Shrinking} *)

val add_shrink_invariant : ('a -> bool) -> 'a arbitrary -> 'a arbitrary
(** [add_shrink_invariant f arb] applies {!Gen.add_shrink_invariant} with [f] on the underlying generator of [arb].

    @since 0.8 *)

(** {2 Assumptions} *)

val assume : bool -> unit
(** [assume cond] checks the precondition [cond], and does nothing
    if [cond=true]. If [cond=false], it interrupts the current test (but the test will not be failed).

    ⚠️ This function must only be used in a test, not outside.
    Example:
    {[
      Test.make (list int) (fun l ->
          assume (l <> []);
          List.hd l :: List.tl l = l)
    ]}

    @since 0.5.1
*)

val (==>) : bool -> bool -> bool
(** [b1 ==> b2] is the logical implication [b1 => b2]
    ie [not b1 || b2] (except that it is strict and will interact
    better with {!Test.check_exn} and the likes, because they will know
    the precondition was not satisfied.).

    ⚠️ This function should only be used in a property
    (see {!Test.make}), because it raises a special exception in case of
    failure of the first argument, to distinguish between failed test
    and failed precondition. Because of OCaml's evaluation order,
    both [b1] and [b2] are always evaluated; if [b2] should only be
    evaluated when [b1] holds, see {!assume}.
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

(** {1 Tests}

    A test is a universal property of type [foo -> bool] for some type [foo],
    with an object of type [foo arbitrary] used to generate, print, etc. values
    of type [foo].

    See {!Test.make} to build a test, and {!Test.check_exn} to
    run one test simply.
    For more serious testing, it is better to create a testsuite
    and use {!QCheck_runner}.
*)

(** Result of running a test *)
module TestResult : sig
  type 'a counter_ex = {
    instance: 'a; (** The counter-example *)

    shrink_steps: int; (** How many shrinking steps for this counter-example *)

    msg_l: string list;
    (** Messages of the test. Currently only populated by {!Test.fail_report} and {!Test.fail_reportf}.
        @since 0.7 *)
  }
  (** A counter-example when a test fails. *)

  (** Result state.

      changed in 0.10 (move to inline records, add Fail_other) *)
  type 'a state =
    | Success (** If the test passed. *)
    | Failed of {
        instances: 'a counter_ex list; (** Failed instance(s) *)
      }
    (** If the test failed "normally", i.e. a test returned [false]. *)
    | Failed_other of {msg: string}
    (** If the test failed for an unusual reason:
        - an exception was raised by a generator
        - too many assumptions failed and [Test.if_assumptions_fail] was set to [`Fatal]
    *)
    | Error of {
        instance: 'a counter_ex; (** Instance that triggered the exception in the test *)
        exn: exn; (** The raised exception *)
        backtrace: string; (** A best-effort backtrace of the exception *)
      }
    (** If the test failed "exceptionally" (an exception was raised by the test). *)

  (* Result returned by running a test. *)
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

(** A test is a pair of an arbitrary (to generate, shrink, print values) and a property that
    all generated values must satisfy. *)
module Test : sig
  (** The main features of this module are:
      - {!make} a test
      - fail the test if a property does not hold (using either the {{!fail_report} simple} form or the {{!fail_reportf} rich} form)
      - {!check_exn} a single test

      Note that while {!check_exn} is provided for convenience to discover QCheck or to run a single test in {{: https://opam.ocaml.org/blog/about-utop/} utop}, to run QCheck tests in your project you probably want to opt for a more advanced runner, or convert
      QCheck tests to your favorite test framework:
      - {!QCheck_base_runner} for a QCheck-only runner (useful if you don't have or don't need another test framework)
      - {!QCheck_alcotest} to convert to Alcotest framework
      - {!QCheck_ounit} to convert to OUnit framework
  *)

  type 'a cell
  (** A single property test on a value of type ['a]. A {!Test.t} wraps a [cell]
      and hides its type parameter. *)

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

  val fail_report : string -> 'a
  (** Fail the test with some additional message that will be reported.

      @since 0.7 *)

  val fail_reportf : ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** Format version of {!fail_report}.

      Example:
      {[
        Test.fail_reportf
          "Value N = %i should be greater than M = %i for Foo = %a" n m pp_foo foo
      ]}

      @since 0.7 *)

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
(** Raised by {!find_example} and {!find_example_gen} if no example was found. *)

val find_example :
  ?name:string ->
  ?count:int ->
  f:('a -> bool) ->
  'a Gen.t ->
  'a Gen.t
(** [find_example ~f gen] uses [gen] to generate some values of type ['a],
    and checks them against [f]. If such a value is found, it is returned.
    Otherwise an exception is raised.

    ⚠️ This should only be used from within a property in {!Test.make}.

    @param name Description of the example to find (used in test results/errors).
    @param count Number of attempts.
    @param f The property that the generated values must satisfy.
    @raise No_example_found If no example is found within [count] tries.
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

(** {1:migration_qcheck2 Migration to QCheck2}

    QCheck2 is a major release and as such, there are (as few as possible)
    breaking changes, as well as functional changes you should be aware of.

    {2 Minimal changes}

    Most of your QCheck (v1) code should be able to compile and run the first time you upgrade
    your QCheck version to a QCheck2-compatible version. However you may need to do the
    following minimal changes:
    - {!QCheck.Test.make} return type was changed to {!QCheck2.Test.t} to be able to run
      both QCheck and QCheck2 tests together. This is transparent if you used type inference,
      but if you explicitly used {!QCheck.Test.t} you will need to change it to {!QCheck2.Test.t}.

    
    {2 Recommended changes}
    Now you want to actually start using the QCheck2 features (most importantly: free shrinking!).
    To get started, change all your {!QCheck} references to {!QCheck2} and follow the compiler errors.
    Below are the most common situations you may encounter:
    - as shrinking is now integrated, several function arguments like [~shrink] or [~rev] have been removed: you
      can remove such reverse functions, they will no longer be necessary.
    - {!arbitrary} is no longer private, it is now abstract: if you used field access directly (e.g. [my_arb.print]), you
      must now use getter functions, e.g. {!get_gen} or {!get_print}.
    - {!Gen.t} is no longer public, it is now abstract: it is recommended to use
      {{!section:Gen.composing_generators} generator composition} to make generators. {!Gen.make_primitive}
      was added to create generators with finer control (in particular of shrinking).
*)

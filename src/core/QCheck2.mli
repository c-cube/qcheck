(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot,
Jan Midtgaard, Julien Debon, Valentin Chaboche
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

    - {!Gen} is used to describe how to generate random values.
      Auxiliary module {!Print} can be used along with {!Test.make}
      to build one's own generator instances.

    - {!Test} is used to describe a single test, that is, a property of
      type ['a -> bool] combined with an ['a Gen.t] that is used to generate
      the test cases for this property. Optional parameters
      allow to specify the random generator state, number of instances to generate
      and test, etc.

    💡 If you are migrating from QCheck, check the {{!section:migration_qcheck2} migration guide} below.

    {1 Examples}

    - "{!List.rev} is involutive" (the test passes so [check_exn] returns [()]):

    {[
      let test =
        QCheck2.(Test.make ~count:1000
                  ~pp:Print.(list int)
                  Gen.(list int)
                  (fun l -> List.rev (List.rev l) = l));;

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
            ~pp:Print.(list small_nat)
            Gen.(list small_nat)
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

    @since 0.18
*)

(** A tree represents a generated value and its successive shrunk values. *)
module Tree : sig
  (** Conceptually a pseudo-randomly generated value is packaged with its shrunk values.
      This coupling - called "integrated shrinking" - in a single type has a major benefit:
      most generators get shrinking "for free" by composing from smaller generators, and shrinking
      does not break invariants (e.g. shrinks of a positive number are always positive).
  *)

  (** A tree of random generated values, where the root contains the value used for the test,
      and the sub-trees contain shrunk values (as trees, to be able to shrink several times a value)
      used if the test fails. *)
  type 'a t

  (** [root tree] returns the root value of the tree of generated values [t]. *)
  val root : 'a t -> 'a

  (** [children tree] returns the direct sub-trees of the tree of generated values [t]. *)
  val children : 'a t -> 'a t Seq.t

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
  val pp :
    ?depth:int ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
end

(** A generator is responsible for generating pseudo-random values and provide shrinks (smaller
    values) when a test fails. *)
module Gen : sig
  (** This module provides some of the most important features of QCheck:
      - {{!section:primitive_generators} primitive generators}
      - {{!section:composing_generators} generator compositions}
  *)

  (** A random generator for values of type ['a]. *)
  type 'a t

  (** Random generator with a size bound. *)
  type 'a sized = int -> 'a t

  (** {3:primitive_generators Primitive generators} *)

  (** The unit generator.

      Does not shrink.
  *)
  val unit : unit t

  (** The boolean generator.

      Shrinks towards [false].
  *)
  val bool : bool t

  (** Generates integers uniformly.

      Shrinks towards [0].
  *)
  val int : int t

  (** Generates non-strictly positive integers uniformly ([0] included).

      Shrinks towards [origin] if specified, otherwise towards [0]. *)
  val pint : ?origin:int -> int t

  (** Small positive integers (< [100], [0] included).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.5.1 *)
  val small_nat : int t

  (** Generates natural numbers (< [10_000]).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].
  *)
  val nat : int t

  (** Generates natural numbers, possibly large (< [1_000_000]).

      Non-uniform: smaller numbers are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.10 *)
  val big_nat : int t

  (** Generates non-strictly negative integers ([0] included).

      Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

      Shrinks towards [0].
  *)
  val neg_int : int t

  (** Small UNSIGNED integers, for retrocompatibility.

      Shrinks towards [0].

      @deprecated use {!small_nat}. *)
  val small_int : int t

  (** Small SIGNED integers, based on {!small_nat}.

      Non-uniform: smaller numbers (in absolute value) are more likely than bigger numbers.

      Shrinks towards [0].

      @since 0.5.2 *)
  val small_signed_int : int t

  (** As {!small_int}, but each newly created generator starts with
    a list of corner cases before falling back on random generation. *)
  val small_int_corners : unit -> int t

  (** Generates uniform {!int32} values.

      Shrinks towards [0l].
  *)
  val int32 : int32 t

  (** Generates {!int32} values.

      Shrinks towards [0l].

      @deprecated use {!val:int32} instead, the name is wrong, values {i are} signed.
  *)
  val ui32 : int32 t

  (** Generates uniform {!int64} values.

      Shrinks towards [0L].
  *)
  val int64 : int64 t

  (** Generates {!int64} values.

      Shrinks towards [0L].

      @deprecated use {!val:int64} instead, the name is wrong, values {i are} signed.
  *)
  val ui64 : int64 t

  (** Generates floating point numbers.

      Shrinks towards [0.].
  *)
  val float : float t

  (** Generates positive floating point numbers ([0.] included).

      Shrinks towards [0.].
  *)
  val pfloat : float t

  (** Generates negative floating point numbers. ([-0.] included).

      Shrinks towards [-0.].
  *)
  val nfloat : float t

  (** Generates characters in the [0..255] range.

      Shrinks towards ['a'].
  *)
  val char : char t

  (** Generates printable characters.

    The exhaustive list of character codes is:
    - [32] to [126], inclusive
    - ['\n']

    Shrinks towards ['a'].
  *)
  val printable : char t

  (** Generates numeral characters ['0'..'9'].

      Shrinks towards ['0'].
  *)
  val numeral : char t

  (** Builds a string generator from a (non-negative) size generator.
      Accepts an optional character generator (the default is {!char}).

      Shrinks on the number of characters first, then on the characters.
  *)
  val string_size : ?gen:char t -> int t -> string t

  (** Builds a string generator. String size is generated by {!nat}.
      The default character generator is {!char}.
      See also {!string_of} and {!string_readable} for versions with
      custom char generator.

      Shrinks on the number of characters first, then on the characters.
  *)
  val string : string t

  (** Builds a string generator using the given character generator.

      Shrinks on the number of characters first, then on the characters.

      @since 0.11 *)
  val string_of : char t -> string t

  (** Builds a string generator using the {!char} character generator.

      Shrinks on the number of characters first, then on the characters.

      @since 0.11 *)
  val string_readable : string t

  (** Builds a string generator, length is {!small_nat}.
      Accepts an optional character generator (the default is {!char}).

      Shrinks on the number of characters first, then on the characters.
  *)
  val small_string : ?gen:char t -> string t

  (** [pure a] creates a generator that always returns [a].

      Does not shrink.

      @since 0.8
  *)
  val pure : 'a -> 'a t

  (** Synonym for {!pure} *)
  val return : 'a -> 'a t

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
  val make_primitive :
    gen:(Random.State.t -> 'a) -> shrink:('a -> 'a Seq.t) -> 'a t

  (** [add_shrink_invariant f gen] returns a generator similar to [gen] except all shrinks satisfy [f].
      This way it's easy to preserve invariants that are enforced by
      generators, when shrinking values

      @since 0.8

      @deprecated is this function still useful? I feel like it is either useless (invariants
      should already be part of the shrinking logic, not be added later) or a special,
      incomplete case of {!Gen.t} being an Alternative (not implemented yet). For now we
      keep it and wait for users feedback (hence deprecation to raise attention).
  *)
  val add_shrink_invariant : ('a -> bool) -> 'a t -> 'a t

  (** {3 Ranges} *)

  (** Uniform integer generator producing integers within [0..bound].

      Shrinks towards [0].

      @raise Invalid_argument if the argument is negative. *)
  val int_bound : int -> int t

  (** [int_range ?origin low high] is an uniform integer generator producing integers within [low..high] (inclusive).

      Shrinks towards [origin] if specified, otherwise towards [0] (but always stays within the range).

      Examples:
      - [int_range ~origin:6 (-5) 15] will shrink towards [6]
      - [int_range (-5) 15] will shrink towards [0]
      - [int_range 8 20] will shrink towards [8] (closest to [0] within range)
      - [int_range (-20) (-8)] will shrink towards [-8] (closest to [0] within range)

      @raise Invalid_argument if any of the following holds:
      - [low > high]
      - [origin < low]
      - [origin > high]
  *)
  val int_range : ?origin:int -> int -> int -> int t

  (** [a -- b] is an alias for [int_range a b]. See {!int_range} for more information.
  *)
  val ( -- ) : int -> int -> int t

  (** [float_bound_inclusive ?origin bound] returns a random floating-point number between [0.] and
      [bound] (inclusive). If [bound] is negative, the result is negative or zero.  If
      [bound] is [0.], the result is [0.].

      Shrinks towards [origin] if given, otherwise towards [0.].

      @since 0.11 *)
  val float_bound_inclusive : ?origin:float -> float -> float t

  (** [float_bound_exclusive origin bound] returns a random floating-point number between [0.] and
      [bound] (exclusive).  If [bound] is negative, the result is negative or zero.

      Shrinks towards [origin] if given, otherwise towards [0.].

      @raise Invalid_argument if [bound] is [0.].

      @since 0.11 *)
  val float_bound_exclusive : ?origin:float -> float -> float t

  (** [float_range ?origin low high] generates floating-point numbers within [low] and [high] (inclusive).

      Shrinks towards [origin] if specified, otherwise towards [0.] (but always stays within the range).

      Examples:
      - [float_range ~origin:6.2 (-5.8) 15.1] will shrink towards [6.2]
      - [float_range (-5.8) 15.1] will shrink towards [0.]
      - [float_range 8.5 20.1] will shrink towards [8.5] (closest to [0.] within range)
      - [float_range (-20.1) (-8.5)] will shrink towards [-8.5] (closest to [0.] within range)

      @raise Invalid_argument if any of the following holds:
      - [low > high]
      - [high -. low > max_float]
      - [origin < low]
      - [origin > high]

      @since 0.11 *)
  val float_range : ?origin:float -> float -> float -> float t

  (** [a --. b] is an alias for [float_range ~origin:a a b]. See {!float_range} for more information.

      @since 0.11 *)
  val ( --. ) : float -> float -> float t

  (** [char_range ?origin low high] generates chars between [low] and [high], inclusive.
      Example: [char_range 'a' 'z'] for all lower case ASCII letters.

      Shrinks towards [origin] if specified, otherwise towards [low].

      @raise Invalid_argument if [low > high].

      @since 0.13 *)
  val char_range : ?origin:char -> char -> char -> char t

  (** {3 Choosing elements} *)

  (** [oneof l] constructs a generator that selects among the given list of generators [l].

      Shrinks towards the first generator of the list.
  *)
  val oneof : 'a t list -> 'a t

  (** [oneofl l] constructs a generator that selects among the given list of values [l].

      Shrinks towards the first element of the list.
  *)
  val oneofl : 'a list -> 'a t

  (** [oneofa a] constructs a generator that selects among the given array of values [a].

      Shrinks towards the first element of the array.
  *)
  val oneofa : 'a array -> 'a t

  (** Constructs a generator that selects among a given list of generators.
      Each of the given generators are chosen based on a positive integer weight.

      Shrinks towards the first element of the list.
  *)
  val frequency : (int * 'a t) list -> 'a t

  (** Constructs a generator that selects among a given list of values.
      Each of the given values are chosen based on a positive integer weight.

      Shrinks towards the first element of the list.
  *)
  val frequencyl : (int * 'a) list -> 'a t

  (** Constructs a generator that selects among a given array of values.
      Each of the array entries are chosen based on a positive integer weight.

      Shrinks towards the first element of the array.
  *)
  val frequencya : (int * 'a) array -> 'a t

  (** {3 Shuffling elements} *)

  (** Returns a copy of the array with its elements shuffled. *)
  val shuffle_a : 'a array -> 'a array t

  (** Creates a generator of shuffled lists. *)
  val shuffle_l : 'a list -> 'a list t

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
  val shuffle_w_l : (int * 'a) list -> 'a list t

  (** {3 Corner cases} *)

  (** [graft_corners gen l ()] makes a new generator that enumerates
      the corner cases in [l] and then behaves like [g].

      Does not shrink if the test fails on a grafted value.
      Shrinks towards [gen] otherwise.

      @since 0.6 *)
  val graft_corners : 'a t -> 'a list -> unit -> 'a t

  (** Non-negative corner cases for int.

      @since 0.6 *)
  val int_pos_corners : int list

  (** All corner cases for int.

      @since 0.6 *)
  val int_corners : int list

  (** {3 Lists, arrays and options} *)

  (** Builds a list generator from an element generator. List size is generated by {!nat}.

      Shrinks on the number of elements first, then on elements.
  *)
  val list : 'a t -> 'a list t

  (** Generates lists of small size (see {!small_nat}).

      Shrinks on the number of elements first, then on elements.

      @since 0.5.3 *)
  val small_list : 'a t -> 'a list t

  (** Builds a list generator from a (non-negative) size generator and an element generator.

      Shrinks on the number of elements first, then on elements.
  *)
  val list_size : int t -> 'a t -> 'a list t

  (** [list_repeat i g] builds a list generator from exactly [i] elements generated by [g].

      Shrinks on elements only.
  *)
  val list_repeat : int -> 'a t -> 'a list t

  (** Builds an array generator from an element generator. Array size is generated by {!nat}.

      Shrinks on the number of elements first, then on elements.
  *)
  val array : 'a t -> 'a array t

  (** Builds an array generator from a (non-negative) size generator and an element generator.

      Shrinks on the number of elements first, then on elements.
  *)
  val array_size : int t -> 'a t -> 'a array t

  (** Generates arrays of small size (see {!small_nat}).

      Shrinks on the number of elements first, then on elements.

      @since 0.10 *)
  val small_array : 'a t -> 'a array t

  (** [array_repeat i g] builds an array generator from exactly [i] elements generated by [g].

      Shrinks on elements only.
  *)
  val array_repeat : int -> 'a t -> 'a array t

  (** [opt gen] is an [option] generator that uses [gen] when generating [Some] values.

      Shrinks towards {!None} then towards shrinks of [gen].

      @param ratio a float between [0.] and [1.] indicating the probability of a sample to be [Some _]
      rather than [None] (value is [0.85]).
  *)
  val opt : ?ratio:float -> 'a t -> 'a option t

  (** {3 Combining generators} *)

  (** [pair gen1 gen2] generates pairs.

      Shrinks on [gen1] and then [gen2].
  *)
  val pair : 'a t -> 'b t -> ('a * 'b) t

  (** [triple gen1 gen2 gen3] generates triples.

      Shrinks on [gen1], then [gen2] and then [gen3].
  *)
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [quad gen1 gen2 gen3 gen4] generates quadruples.

      Shrinks on [gen1], then [gen2], then [gen3] and then [gen4].

      @since 0.5.1
  *)
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  (** {3 Convert a structure of generator to a generator of structure} *)

  (** Generate a list of elements from individual generators.

      Shrinks on the elements of the list, in the list order.

      @since 0.13 *)
  val flatten_l : 'a t list -> 'a list t

  (** Generate an array of elements from individual generators.

      Shrinks on the elements of the array, in the array order.

      @since 0.13 *)
  val flatten_a : 'a t array -> 'a array t

  (** Generate an option from an optional generator.

      Shrinks towards {!None} then shrinks on the value.

      @since 0.13 *)
  val flatten_opt : 'a t option -> 'a option t

  (** Generate a result from [Ok gen], an error from [Error e].

      Shrinks on [gen] if [Ok gen].
      Does not shrink if [Error e].

      @since 0.13 *)
  val flatten_res : ('a t, 'e) result -> ('a, 'e) result t

  (** Collapses a generator of generators to a generator.

      Shrinks on the generated generators.

      @since 0.5 *)
  val join : 'a t t -> 'a t

  (** {3 Influencing the size of generated values} *)

  (** Creates a generator from a size-bounded generator by first
      generating a size using {!nat} and passing the result to the size-bounded generator.

      Shrinks on the size first, then on the generator.
  *)
  val sized : 'a sized -> 'a t

  (** Creates a generator from a size-bounded generator by first
      generating a size using the integer generator and passing the result
      to the size-bounded generator.

      Shrinks on the size first, then on the generator.

      @since 0.5 *)
  val sized_size : int t -> 'a sized -> 'a t

  (** {3 Recursive data structures} *)

  (** Parametrized fixpoint combinator for generating recursive values.

      The fixpoint is parametrized over an generator state ['a], and the
      fixpoint computation may change the value of this state in the recursive
      calls.

      In particular, this can be used for size-bounded generators (with ['a] as [int]).
      The passed size-parameter should decrease to ensure termination. *)
  val fix : (('a -> 'b t) -> 'a -> 'b t) -> 'a -> 'b t

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

  (** Delay execution of some code until the generator is actually called.
      This can be used to manually implement recursion or control flow
      in a generator.
      @since 0.17 *)
  val delay : (unit -> 'a t) -> 'a t

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

        (* Another allternative syntax with OCaml 4.08+ binding operators *)
        let int_string_result : (int, string) result Gen.t = Gen.(
            let* n = int_range 0 9 in
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

  (** [map f gen] transforms a generator [gen] by applying [f] to each generated element.

      Shrinks towards the shrinks of [gen] with [f] applied to them.
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** An infix synonym for {!map}. Note the order of arguments is reversed (usually more
      convenient for composing). *)
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

  (** An infix synonym for {!map}

      @since 0.13 *)
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  (** [map2 f gen1 gen2] transforms two generators [gen1] and [gen2] by applying [f] to each
      pair of generated elements.

      Shrinks on [gen1] and then [gen2].
  *)
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  (** [map3 f gen1 gen2 gen3] transforms three generators [gen1], [gen2], and [gen3] by applying [f]
      to each triple of generated elements.

      Shrinks on [gen1], then [gen2], and then [gen3].
  *)
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  (** [ap fgen gen] composes a function generator and an argument generator
      into a result generator.

      Shrinks on [fgen] and then [gen].
  *)
  val ap : ('a -> 'b) t -> 'a t -> 'b t

  (** Synonym for {!ap} *)
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  (** [bind gen f] first generates a value of type ['a] with [gen] and then
      passes it to [f] to generate a value of type ['b]. This is typically
      useful when a generator depends on the value generated by another
      generator.

      Shrinks on [gen] and then on the resulting generator.
  *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** Synonym for {!bind} *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!map}.

      Example:
      {[
      let+ n = int_range 0 10 in
      string_of_int n

      (* is equivalent to *)

      map (fun n -> string_of_int n) (int_range 0 10)
      ]}
   *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}.

      Example:
      {[
      let+ n = int_range 0 10
      and+ b = bool in
      if b then string_of_int n else "Not a number"

      (* is equivalent to *)

      map
        (fun (n, b) -> if b then string_of_int n else "Not a number")
        (pair (int_range 0 10) bool)
      ]}
   *)
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!bind}.

      Example:
      {[
      let* n = int_range 0 9 in
      if n < 9
      then int >|= Result.ok
      else string_readable >|= Result.error

      (* is equivalent to *)

      bind (int_range 0 9) (fun n ->
          if n < 9
          then map Result.ok int
          else map Result.error string_readable)
      ]}
   *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** {{: https://ocaml.org/manual/bindingops.html} Binding operator} alias for {!pair}.

      Example:
      {[
      let* n = int_range 0 9
      and* b = bool in
      if n < 9 then int >|= Result.ok
      else if b then pure (Error "Some specific error")
      else string_readable >|= Result.error

      (* is equivalent to *)

      bind (pair (int_range 0 9) bool) (fun (n, b) ->
          if n < 9 then map Result.ok int
          else if b then pure (Error "Some specific error")
          else map Result.error string_readable)
      ]}
   *)
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

  (** {2 Debug generators}

      These functions should not be used in tests: they are provided
      for convenience to debug/investigate what values and shrinks a
      generator produces.
  *)

  (** [generate ~n gen] generates [n] values using [gen] (shrinks are discarded). *)
  val generate : ?rand:Random.State.t -> n:int -> 'a t -> 'a list

  (** [generate1 gen] generates one instance of [gen] (shrinks are discarded). *)
  val generate1 : ?rand:Random.State.t -> 'a t -> 'a

  (** [generate_tree ?rand gen] generates a random value and its shrinks using [gen]. *)
  val generate_tree : ?rand:Random.State.t -> 'a t -> 'a Tree.t
end

(** Printing functions and helpers, used to print generated values on
    test failures. *)
module Print : sig
  (** Printer for values of type ['a]. *)
  type 'a t = 'a -> string

  (** [unit] is a printer of unit.

      @since 0.6
  *)
  val unit : unit t

  (** [int] is a printer of integer. *)
  val int : int t

  (** [bool] is a printer of boolean. *)
  val bool : bool t

  (** [float] is a printer of float. *)
  val float : float t

  (** [char] is a printer of character. *)
  val char : char t

  (** [string] is a printer of string. *)
  val string : string t

  (** [option p] is a printer of ['a option], using [p] if it is a [Some]. *)
  val option : 'a t -> 'a option t

  (** [pair p1 p2] is a printer of pair. *)
  val pair : 'a t -> 'b t -> ('a * 'b) t

  (** [triple p1 p2 p3] is a printer of triple. *)
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [quad p1 p2 p3 p4] is a printer of quadruple. *)
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  (** [list p] is a printer of list, using [p] for each element. *)
  val list : 'a t -> 'a list t

  (** [array p] is a printer of array, using [p] for each element. *)
  val array : 'a t -> 'a array t

  (** [contramap f p] transforms printer [p] into another using [f].

      Note the reverse order of types in [f] which may be
      conter-intuitive: indeed a function that {i prints} values of type
      ['b] can be obtained by transforming a value of type ['b] to
      ['a] using [f], and then by {i printing} this value of type ['a] using [p].
  *)
  val contramap : ('b -> 'a) -> 'a t -> 'b t

  (** @deprecated use {!contramap} instead. *)
  val comap : ('b -> 'a) -> 'a t -> 'b t
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
  val number_towards :
    (module Number with type t = 'a) -> destination:'a -> 'a -> 'a Seq.t

  (** {!number_towards} specialized to {!int}. *)
  val int_towards : int -> int -> int Seq.t

  (** {!number_towards} specialized to {!int32}. *)
  val int32_towards : int32 -> int32 -> int32 Seq.t

  (** {!number_towards} specialized to {!int64}. *)
  val int64_towards : int64 -> int64 -> int64 Seq.t

  (** {!number_towards} specialized to {!float}.

      There are various ways to shrink a float:
      - try removing floating digits, i.e. towards integer values
      - try to get as close as possible to the destination, no matter the number of digits
      - a mix of both

      This implementation, as it relies on the generic {!number_towards} function,
      tries to get as close as possible to the destination, e.g. the last value of
      [Gen.float_towards 50 100] may be [99.9969482421875] (or a similar value).
  *)
  val float_towards : float -> float -> float Seq.t

  (** [int_agressive_towards destination n] gives all integers from [destination] to [n] (excluded).

      {b Be careful about time and memory} as the resulting list can be huge *)
  val int_aggressive_towards : int -> int -> int Seq.t

  (** @deprecated Use [int_aggressive_towards 0] instead.
      @since 0.7 *)
  val int_aggressive : int -> int Seq.t
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

  (** An observable of ['a], packing a printer and other things. *)
  type -'a t

  (** [make ?eq ?hash print] creates an observable of ['a].

      If [eq] is [None], uses the standard polymorphic [(=)] function.

      If [hash] is [None], uses a default hashing function.
  *)
  val make : ?eq:('a -> 'a -> bool) -> ?hash:('a -> int) -> 'a Print.t -> 'a t

  (** [equal o] returns the equality function of [o]. *)
  val equal : 'a t -> 'a -> 'a -> bool

  (** [hash o] returns the hashing function of [o]. *)
  val hash : 'a t -> 'a -> int

  (** [print o] returns the printing function of [o]. *)
  val print : 'a t -> 'a Print.t

  (** [unit] is an observable of [unit]. *)
  val unit : unit t

  (** [bool] is an observable of [bool]. *)
  val bool : bool t

  (** [int] is an observable of [int]. *)
  val int : int t

  (** [float] is an observable of [float]. *)
  val float : float t

  (** [string] is an observable of [string]. *)
  val string : string t

  (** [char] is an observable of [char]. *)
  val char : char t

  (** [contramap f o] maps the function [f] on observable [o].

      Note the reverse order of types in [f] which may be
      conter-intuitive: indeed a function that {i consumes} values of type
      ['b] can be obtained by transforming a value of type ['b] to
      ['a] using [f], and then by {i consuming} this value of type ['a] using [o].
  *)
  val contramap : ('b -> 'a) -> 'a t -> 'b t

  (** @deprecated use {!contramap} instead. *)
  val map : ('b -> 'a) -> 'a t -> 'b t

  (** [option o] wraps the observable [o] of ['a] into an observable of
      ['a option]. *)
  val option : 'a t -> 'a option t

  (** [list o] wraps the observable [o] of ['a] into an observable of
      ['a list]. *)
  val list : 'a t -> 'a list t

  (** [array o] wraps the observable [o] of ['a] into an observable of
      ['a array]. *)
  val array : 'a t -> 'a array t

  (** [pair o1 o2] is an observable of pairs of [('a * 'b)]. *)
  val pair : 'a t -> 'b t -> ('a * 'b) t

  (** [triple o1 o2 o3] is an observable of triples of [('a * 'b * 'c)]. *)
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [quad o1 o2 o3 o4] is an observable of quadruples of [('a * 'b * 'c * 'd)]. *)
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
end

(** Utils on combining function arguments. *)
module Tuple : sig
  (** Heterogeneous tuple, used to pass any number of arguments to
      a function. *)
  type 'a t = Nil : unit t | Cons : 'a * 'b t -> ('a * 'b) t

  (** [nil] is {!Nil}. *)
  val nil : unit t

  (** [cons] is {!Cons}. *)
  val cons : 'a -> 'b t -> ('a * 'b) t

  (** How to observe a {!t}.

      See {!module:Observable} for more information on what
      "observe" means in the QCheck. *)
  type 'a obs

  (** [o_nil] is the {!obs} equivalent of {!nil}. *)
  val o_nil : unit obs

  (** [o_cons] is the {!obs} equivalent of {!cons}. *)
  val o_cons : 'a Observable.t -> 'b obs -> ('a * 'b) obs

  (** [observable obs] returns the underlying observable of [obs]. *)
  val observable : 'a obs -> 'a t Observable.t

  (** Infix {!module:Tuple} operators for convenience. *)
  module Infix : sig
    (** Alias for {!cons}. *)
    val ( @:: ) : 'a -> 'b t -> ('a * 'b) t

    (** Alias for {!o_cons}. *)
    val ( @-> ) : 'a Observable.t -> 'b obs -> ('a * 'b) obs
  end

  include module type of Infix
end

(** Used by QCheck to shrink and print generated functions of type ['f] in case
    of test failure. You cannot and should not use it yourself. See {!fun_} for more information. *)
type 'f fun_repr

(** A function packed with the data required to print/shrink it.

    The idiomatic way to use any [fun_] Gen.t is to directly pattern match
    on it to obtain the executable function.

    For example (note the [Fun (_, f)] part):
    {[
      QCheck2.(Test.make
        Gen.(pair (fun1 Observable.int bool) (small_list int))
        (fun (Fun (_, f), l) -> l = (List.rev_map f l |> List.rev l))
    ]}

    In this example [f] is a generated function of type [int -> bool].

    The ignored part [_] of [Fun (_, f)] is useless to you, but is used by
    QCheck during shrinking/printing in case of test failure.

    See also {!Fn} for utils to print and apply such a function.
*)
type 'f fun_ = Fun of 'f fun_repr * 'f

(** [fun1 obs gen] generates random functions that take an argument observable
    via [obs] and map to random values generated with [gen].
    To write functions with multiple arguments, it's better to use {!Tuple}
    or {!Observable.pair} rather than applying {!fun_} several times
    (shrinking will be faster).
    @since 0.6 *)
val fun1 :
  'a Observable.t -> ?print:'b Print.t -> 'b Gen.t -> ('a -> 'b) fun_ Gen.t

(** Specialized version of {!fun_nary} for functions of 2 arguments, for convenience.
    @since 0.6 *)
val fun2 :
  'a Observable.t ->
  'b Observable.t ->
  ?print:'c Print.t ->
  'c Gen.t ->
  ('a -> 'b -> 'c) fun_ Gen.t

(** Specialized version of {!fun_nary} for functions of 3 arguments, for convenience.
    @since 0.6 *)
val fun3 :
  'a Observable.t ->
  'b Observable.t ->
  'c Observable.t ->
  ?print:'d Print.t ->
  'd Gen.t ->
  ('a -> 'b -> 'c -> 'd) fun_ Gen.t

(** Specialized version of {!fun_nary} for functions of 4 arguments, for convenience.
    @since 0.6 *)
val fun4 :
  'a Observable.t ->
  'b Observable.t ->
  'c Observable.t ->
  'd Observable.t ->
  ?print:'e Print.t ->
  'e Gen.t ->
  ('a -> 'b -> 'c -> 'd -> 'e) fun_ Gen.t

(** [fun_nary tuple_obs gen] generates random n-ary functions. Arguments are observed
    using [tuple_obs] and return values are generated using [gen].

    Example (the property is wrong as a random function may return [false], this is for
    the sake of demonstrating the syntax):
    {[
      let module O = Observable in
      Test.make
        (fun_nary Tuple.(O.int @-> O.float @-> O.string @-> o_nil) bool)
        (fun (Fun (_, f)) -> f Tuple.(42 @:: 17.98 @:: "foobar" @:: nil))
    ]}

    Note that this particular example can be simplified using {!fun3} directly:
    {[
      let module O = Observable in
      Test.make
        (fun3 O.int O.float O.string bool)
        (fun (Fun (_, f)) -> f 42 17.98 "foobar")
    ]}

    @since 0.6 *)
val fun_nary :
  'a Tuple.obs -> ?print:'b Print.t -> 'b Gen.t -> ('a Tuple.t -> 'b) fun_ Gen.t

(** Utils on generated functions.
    @since 0.6 *)
module Fn : sig
  (** [print f] prints the implementation of generated function [f].

      The implementation always contains a default case, represented as [_].

      Note that printing a function {i before} it was called in the test may not print the full implementation.
   *)
  val print : 'f fun_ Print.t

  (** [apply f] returns the underlying function to be used in tests. This is an alias for
      deconstructing as documented in {!fun_}. *)
  val apply : 'f fun_ -> 'f
end

(** {2 Assumptions} *)

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
val assume : bool -> unit

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
val ( ==> ) : bool -> bool -> bool

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
val assume_fail : unit -> 'a

(** {1 Tests}

    A test is a universal property of type [foo -> bool] for some type [foo],
    with an object of type [foo Gen.t] used to generate values
    of type [foo].

    See {!Test.make} to build a test, and {!Test.check_exn} to
    run one test simply.
    For more serious testing, it is better to create a testsuite
    and use {!QCheck_runner}.
*)

(** A statistic on a distribution of values of type ['a].
  The function {b MUST} return a positive integer. *)
type 'a stat = string * ('a -> int)

(** Result of running a test *)
module TestResult : sig
  (** A counter-example when a test fails. *)
  type 'a counter_ex = {
    instance : 'a;  (** The counter-example *)
    shrink_steps : int;
        (** How many shrinking steps for this counter-example *)
    msg_l : string list;
        (** Messages of the test. Currently only populated by {!Test.fail_report} and {!Test.fail_reportf}.
        @since 0.7 *)
  }

  (** Result state.

      changed in 0.10 (move to inline records, add Fail_other) *)
  type 'a state =
    | Success  (** If the test passed. *)
    | Failed of { instances : 'a counter_ex list  (** Failed instance(s) *) }
        (** If the test failed "normally", i.e. a test returned [false]. *)
    | Failed_other of { msg : string }
        (** If the test failed for an unusual reason:
        - an exception was raised by a generator
        - too many assumptions failed and [Test.if_assumptions_fail] was set to [`Fatal]
    *)
    | Error of {
        instance : 'a counter_ex;
            (** Instance that triggered the exception in the test *)
        exn : exn;  (** The raised exception *)
        backtrace : string;  (** A best-effort backtrace of the exception *)
      }
        (** If the test failed "exceptionally" (an exception was raised by the test). *)

  (* Result returned by running a test. *)
  type 'a t

  (** [get_state t] returns the final state after a test execution. *)
  val get_state : 'a t -> 'a state

  (** [get_count t] returns the number of tests executed. *)
  val get_count : _ t -> int

  (** [get_count_gen t] returns the number of generated cases. *)
  val get_count_gen : _ t -> int

  (** [get_collect t] returns the repartition of generated values.
      @since 0.18 *)
  val get_collect : _ t -> (string, int) Hashtbl.t option

  (** [get_stats t] returns the statistics captured by the test.
      @since 0.18 *)
  val get_stats : 'a t -> ('a stat * (int, int) Hashtbl.t) list

  (** [get_warnings t] returns the list of warnings emitted during the test.
      @since 0.18 *)
  val get_warnings : _ t -> string list

  (** [get_instances t] returns the generated instances, with no guarantee on the order.
      @since 0.18 *)
  val get_instances : 'a t -> 'a list

  (** Returns true iff the state if [Success]
      @since 0.9 *)
  val is_success : _ t -> bool

  (** Obtain statistics
      @since 0.6
      @deprecated use {!get_stats} instead *)
  val stats : 'a t -> ('a stat * (int, int) Hashtbl.t) list

  (** Obtain list of warnings
      @since 0.10
      @deprecated use {!get_warnings} instead *)
  val warnings : _ t -> string list

  (** Obtain statistics
      @since 0.6
      @deprecated use {!get_collect} instead *)
  val collect : _ t -> (string, int) Hashtbl.t option
end

module Test_exceptions : sig
  (** Exception raised when a test failed, with the list of counter-examples.
      [Test_fail (name, l)] means test [name] failed on elements of [l]. *)
  exception Test_fail of string * string list

  (** Exception raised when a test raised an exception [e], with
      the sample that triggered the exception.
      [Test_error (name, i, e, st)]
      means [name] failed on [i] with exception [e], and [st] is the
      stacktrace (if enabled) or an empty string. *)
  exception Test_error of string * string * exn * string
end

(** A test is a pair of an generator and a property thar all generated values must satisfy. *)
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

  (** A single property test on a value of type ['a]. A {!Test.t} wraps a [cell]
      and hides its type parameter. *)
  type 'a cell

  (** [make_cell gen prop] builds a test that checks property [prop] on instances
      of the generator [gen].
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
      @param print used in {!Print} to display generated values failing the [prop]
      @param collect (* collect values by tag, useful to display distribution of generated *)
      @param stats on a distribution of values of type 'a
  *)
  val make_cell :
    ?if_assumptions_fail:[ `Fatal | `Warning ] * float ->
    ?count:int ->
    ?long_factor:int ->
    ?max_gen:int ->
    ?max_fail:int ->
    ?name:string ->
    ?print:'a Print.t ->
    ?collect:('a -> string) ->
    ?stats:'a stat list ->
    'a Gen.t ->
    ('a -> bool) ->
    'a cell

  (** ⚠️ Do not use, this is exposed for internal reasons only. ⚠️ 

      @deprecated Migrate to QCheck2 and use {!make_cell} instead.
   *)
  val make_cell_from_QCheck1 :
    ?if_assumptions_fail:[ `Fatal | `Warning ] * float ->
    ?count:int ->
    ?long_factor:int ->
    ?max_gen:int ->
    ?max_fail:int ->
    ?name:string ->
    gen:(Random.State.t -> 'a) ->
    ?shrink:('a -> ('a -> unit) -> unit) ->
    ?print:('a -> string) ->
    ?collect:('a -> string) ->
    stats:'a stat list ->
    ('a -> bool) ->
    'a cell

  val get_law : 'a cell -> 'a -> bool

  val get_name : _ cell -> string

  val get_gen : 'a cell -> 'a Gen.t

  val get_print_opt : 'a cell -> 'a Print.t option

  val get_collect_opt : 'a cell -> ('a -> string) option

  val get_stats : 'a cell -> 'a stat list

  val set_name : _ cell -> string -> unit

  (** Get the count of a cell.
      @since 0.5.3 *)
  val get_count : _ cell -> int

  (** Get the long factor of a cell.
      @since 0.5.3 *)
  val get_long_factor : _ cell -> int

  type t =
    | Test : 'a cell -> t
        (** Same as ['a cell], but masking the type parameter. This allows to
      put tests on different types in the same list of tests. *)

  (** [make gen prop] builds a test that checks property [prop] on instances
      of the generator [gen].
      See {!make_cell} for a description of the parameters.
  *)
  val make :
    ?if_assumptions_fail:[ `Fatal | `Warning ] * float ->
    ?count:int ->
    ?long_factor:int ->
    ?max_gen:int ->
    ?max_fail:int ->
    ?name:string ->
    ?print:'a Print.t ->
    ?collect:('a -> string) ->
    ?stats:'a stat list ->
    'a Gen.t ->
    ('a -> bool) ->
    t

  val test_get_count : t -> int

  (** Fail the test with some additional message that will be reported.

      @since 0.7 *)
  val fail_report : string -> 'a

  (** Format version of {!fail_report}.

      Example:
      {[
        Test.fail_reportf
          "Value N = %i should be greater than M = %i for Foo = %a" n m pp_foo foo
      ]}

      @since 0.7 *)
  val fail_reportf : ('a, Format.formatter, unit, 'b) format4 -> 'a

  (** {3 Running the test} *)

  include module type of Test_exceptions

  val print_instance : 'a cell -> 'a -> string

  val print_c_ex : 'a cell -> 'a TestResult.counter_ex -> string

  val print_fail : 'a cell -> string -> 'a TestResult.counter_ex list -> string

  val print_fail_other : string -> msg:string -> string

  val print_error :
    ?st:string -> 'a cell -> string -> 'a TestResult.counter_ex * exn -> string

  val print_test_fail : string -> string list -> string

  val print_test_error : string -> string -> exn -> string -> string

  (** Print "collect" results.
      @since 0.6 *)
  val print_collect : (string, int) Hashtbl.t -> string

  (** Print statistics.
      @since 0.6 *)
  val print_stat : 'a stat * (int, int) Hashtbl.t -> string

  (** [check_result cell res] checks that [res] is [Ok _], and returns unit.
      Otherwise, it raises some exception.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)
  val check_result : 'a cell -> 'a TestResult.t -> unit

  type res = Success | Failure | FalseAssumption | Error of exn * string

  type 'a event =
    | Generating
    | Collecting of 'a
    | Testing of 'a
    | Shrunk of int * 'a
    | Shrinking of int * int * 'a

  (** Handler executed after each event during testing of an instance. *)
  type 'a handler = string -> 'a cell -> 'a event -> unit

  (** Callback executed after each instance of a test has been run.
      The callback is given the instance tested, and the current results
      of the test. *)
  type 'a step = string -> 'a cell -> 'a -> res -> unit

  (** Callback executed after each test has been run.
      [f name cell res] means test [cell], named [name], gave [res]. *)
  type 'a callback = string -> 'a cell -> 'a TestResult.t -> unit

  (** [check_cell ~long ~rand test] generates up to [count] random
      values of type ['a] using [Gen.t] and the random state [st]. The
      predicate [law] is called on them and if it returns [false] or raises an
      exception then we have a counter-example for the [law].

      @param long if [true] then multiply the number of instances to generate
        by the cell's long_factor.
      @param call function called on each test case, with the result.
      @param step function called on each instance of the test case, with the result.
      @return the result of the test.
  *)
  val check_cell :
    ?long:bool ->
    ?call:'a callback ->
    ?step:'a step ->
    ?handler:'a handler ->
    ?rand:Random.State.t ->
    'a cell ->
    'a TestResult.t

  (** Same as {!check_cell} but calls  {!check_result} on the result.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)
  val check_cell_exn :
    ?long:bool ->
    ?call:'a callback ->
    ?step:'a step ->
    ?rand:Random.State.t ->
    'a cell ->
    unit

  (** Checks the property against some test cases, and calls {!check_result},
      which might raise an exception in case of failure.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)
  val check_exn : ?long:bool -> ?rand:Random.State.t -> t -> unit
end

(** {2 Sub-tests} *)

(** The infrastructure used to find counter-examples to properties can
    also be used to find data satisfying a predicate,
    {i within a property being tested}.

    See https://github.com/c-cube/qcheck/issues/31
*)

(** Raised by {!find_example} and {!find_example_gen} if no example was found. *)
exception No_example_found of string

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
val find_example :
  ?name:string -> ?count:int -> f:('a -> bool) -> 'a Gen.t -> 'a Gen.t

(** Toplevel version of {!find_example}.
    [find_example_gen ~f gen] is roughly the same as
    [Gen.generate1 @@ find_example ~f gen].
    @param rand the random state to use to generate inputs.
    @raise No_example_found if no example was found within [count] tries.
    @since 0.6 *)
val find_example_gen :
  ?rand:Random.State.t ->
  ?name:string ->
  ?count:int ->
  f:('a -> bool) ->
  'a Gen.t ->
  'a

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
    - accessor functions like {!QCheck.gen} have been renamed to consistent names like {!get_gen}.
    - {!QCheck.map_keep_input} has been removed: you can use {!map} directly.
    - {!Gen.t} is no longer public, it is now abstract: it is recommended to use
      {{!section:Gen.composing_generators} generator composition} to make generators. {!Gen.make_primitive}
      was added to create generators with finer control (in particular of shrinking).
*)

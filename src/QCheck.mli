
(*
QCheck: Random testing for OCaml
Copyright (C) 2016  Vincent Hugot, Simon Cruanes

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(** {1 Quickcheck inspired property-based testing} *)

(** The library takes inspiration from Haskell's QuickCheck library. The
rough idea is that the programer describes invariants that values of
a certain type need to satisfy ("properties"), as functions from this type
to bool. She also needs to desribe how to generate random values of the type,
so that the property is tried and checked on a number of random instances.

This explains the organization of this module:

- {! 'a arbitrary} is used to describe how to generate random values,
  shrink them (make counter-examples as small as possible), print
  them, etc. Auxiliary modules such as {!Gen}, {!Print}, and {!Shrink}
  can be used along with {!make} to build one's own arbitrary instances.

- {!Test} is used to describe a single test, that is, a property of
  type ['a -> bool] combined with an ['a arbitrary] that is used to generate
  the test cases for this property. Optional parameters
  allow to specify the random generator state, number of instances to generate
  and test, etc.


Examples:

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
{!Generator} module. Its documentation can be found
{{:http://gasche.github.io/random-generator/doc/Generator.html } here}.
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
    a test. not outside.
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

(** {2 Generate Random Values} *)
module Gen : sig
  type 'a t = Random.State.t -> 'a
  (** A random generator for values of type 'a *)

  type 'a sized = int -> Random.State.t -> 'a
  (** Random generator with a size bound *)

  val return : 'a -> 'a t
  (** Create a constant generator  *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind for writing dependent generators. First generates an ['a] and then
      passes it to the given function, to generate a ['b]. *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** Infix operator for composing a function generator and an argument generator
      into a result generator *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f g] transforms a generator [g] by applying [f] to each generated element *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map f g1 g2] transforms two generators [g1] and [g2] by applying [f] to each
      pair of generated elements *)

  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  (** [map f g1 g2 g3] transforms two generators [g1], [g2], and [g3] by applying [f]
      to each triple of generated elements *)

  val map_keep_input : ('a -> 'b) -> 'a t -> ('a * 'b) t
  (** [map f g] transforms a generator [g] by applying [f] to each generated element.
      Returns both the generated elememt from [g] and the output from [f]. *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** An infix synonym for {!map} *)

  val oneof : 'a t list -> 'a t
  (** Constructs a generator that selects among a given list of generators *)

  val oneofl : 'a list -> 'a t
  (** Constructs a generator that selects among a given list of values *)

  val oneofa : 'a array -> 'a t
  (** Constructs a generator that selects among a given array of values *)

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
  (** Shuffle the array in place *)

  val shuffle_l : 'a list -> 'a list t
  (** Creates a generator of shuffled lists *)

  val unit: unit t (** The unit generator *)

  val bool: bool t (** The Boolean generator *)

  val float: float t   (** Generates floating point numbers *)

  val pfloat : float t (** Generates positive floating point numbers *)

  val nfloat : float t (** Generates negative floating point numbers *)

  val nat : int t (** Generates small natural numbers *)

  val neg_int : int t (** Generates negative integers *)

  val pint : int t (** Generates positive integers uniformly *)

  val int : int t (** Generates integers uniformly *)

  val small_nat : int t (** Synonym to {!nat} @since 0.5.1 *)

  val small_int : int t
  (** Small UNSIGNED integers, for retrocompatibility
      @deprecated use {!small_nat} *)

  val small_signed_int : int t
  (** small SIGNED integers
      @since 0.5.2 *)

  val int_bound : int -> int t
  (** Uniform integer generator producing integers within [0... bound].
      @raise Invalid_argument if the bound is too high (typically 2^30) *)

  val int_range : int -> int -> int t
  (** Uniform integer generator producing integers within [low,high]
      @raise Invalid_argument if the range is too large (typically 2^30) *)

  val (--) : int -> int -> int t (** Synonym to {!int_range} *)

  val ui32 : int32 t (** Generates (unsigned) [int32] values *)

  val ui64 : int64 t (** Generates (unsigned) [int64] values *)

  val list : 'a t -> 'a list t
  (** Builds a list generator from an element generator. List size is generated by {!nat} *)

  val list_size : int t -> 'a t -> 'a list t
  (** Builds a list generator from a (non-negative) size generator and an element generator *)

  val list_repeat : int -> 'a t -> 'a list t
  (** [list_repeat i g] builds a list generator from exactly [i] elements generated by [g] *)

  val array : 'a t -> 'a array t
  (** Builds an array generator from an element generator. Array size is generated by {!nat} *)

  val array_size : int t -> 'a t -> 'a array t
  (** Builds an array generator from a (non-negative) size generator and an element generator *)

  val array_repeat : int -> 'a t -> 'a array t
  (** [array_repeat i g] builds an array generator from exactly [i] elements generated by [g] *)

  val opt : 'a t -> 'a option t (** An option generator *)

  val pair : 'a t -> 'b t -> ('a * 'b) t (** Generates pairs *)

  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t (** Generates triples *)

  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t (** Generates quadruples @since 0.5.1 *)

  val char : char t (** Generates characters upto character code 255 *)

  val printable : char t (** Generates printable characters *)

  val numeral : char t (** Generates numeral characters *)

  val string_size : ?gen:char t -> int t -> string t
  (** Builds a string generator from a (non-negative) size generator.
      Accepts an optional character generator (the default is {!char}) *)

  val string : ?gen:char t -> string t
  (** Builds a string generator. String size is generated by {!nat}.
      Accepts an optional character generator (the default is {!char}) *)

  val small_string : ?gen:char t -> string t
  (** Builds a string generator. String size is in the range [0--10].
      Accepts an optional character generator (the default is {!char}) *)

  val small_list : 'a t -> 'a list t
  (** Generates lists of small size (range [0 -- 10]).
      @since 0.5.3 *)

  val join : 'a t t -> 'a t
  (** Collapses a generator of generators to simply a generator.
      @since 0.5 *)

  val sized : 'a sized -> 'a t
  (** Create a generator from a size-bounded generator by first
      generating a size using {!nat} and passing the result to the size-bounded generator *)

  val sized_size : int t -> 'a sized -> 'a t
  (** Create a generator from a size-bounded generator by first
      generating a size using the integer generator and passing the result
      to the size-bounded generator
      @since 0.5 *)

  val fix : ('a sized -> 'a sized) -> 'a sized
  (** Fixpoint combinator for generating recursive, size-bounded data types.
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

  val generate : ?rand:Random.State.t -> n:int -> 'a t -> 'a list
  (** [generate ~n g] generates [n] instances of [g] *)

  val generate1 : ?rand:Random.State.t -> 'a t -> 'a
  (** [generate1 g] generates one instance of [g] *)
end

(** {2 Pretty printing} *)

(** {2 Show Values} *)
module Print : sig
  type 'a t = 'a -> string
  (** Printer for values of type ['a] *)

  val int : int t (** Integer printer *)
  val bool : bool t (** Boolean printer *)
  val float : float t (** Floating point number printer *)
  val char : char t (** Character printer *)
  val string : string t (** String printer *)
  val option : 'a t -> 'a option t (** Option printer *)

  val pair : 'a t -> 'b t -> ('a*'b) t
  (** Pair printer. Expects printers for each component *)
  val triple : 'a t -> 'b t -> 'c t -> ('a*'b*'c) t
  (** Triple (3-tuple) printer. Expects printers for each component *)
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a*'b*'c*'d) t
  (** Quadruple (4-tuple) printer. Expects printers for each component *)

  val list : 'a t -> 'a list t
  (** List printer. Expects a printer for the list element type *)
  val array : 'a t -> 'a array t
  (** Array printer. Expects a printer for the array entry type *)

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
  val (<+>) : 'a t -> 'a t -> 'a t (** Synonym to {!append} *)

  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val find : ('a -> bool) -> 'a t -> 'a option
end

(** {2 Shrink Values}

    Shrinking is used to reduce the size of a counter-example. It tries
    to make the counter-example smaller by decreasing it, or removing
    elements, until the property to test holds again; then it returns the
    smallest value that still made the test fail *)
module Shrink : sig
  type 'a t = 'a -> 'a Iter.t
  (** Given a counter-example, return an iterator on smaller versions
      of the counter-example *)

  val nil : 'a t
  (** No shrink *)

  val int : int t
  val option : 'a t -> 'a option t
  val string : string t

  val list : ?shrink:'a t -> 'a list t
  (** Try to shrink lists by removing elements one by one.
      @param shrink if provided, will be used to also try to reduce
      the elements of the list themselves (e.g. in an [int list]
      one can try to decrease the integers) *)

  val array : ?shrink:'a t -> 'a array t
  (** Shrink an array.
      @param shrink see {!list} *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
end

(** {2 Arbitrary}

    A value of type ['a arbitrary] glues together a random generator,
    and optional functions for shrinking, printing, computing the size,
    etc. It is the "normal" way of describing how to generate
    values of a given type, to be then used in tests (see {!Test}) *)

type 'a arbitrary = {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a Shrink.t) option;  (** shrink to smaller examples *)
  collect: ('a -> string) option;  (** map value to tag, and group by tag *)
}
(** a value of type ['a arbitrary] is an object with a method for generating random
    values of type ['a], and additional methods to compute the size of values,
    print them, and possibly shrink them into smaller counterexamples

    {b NOTE} the collect field is unstable and might be removed, or
    moved into {!Test}.
*)

val make :
  ?print:'a Print.t ->
  ?small:('a -> int) ->
  ?shrink:'a Shrink.t ->
  ?collect:('a -> string) ->
  'a Gen.t -> 'a arbitrary
(** Builder for arbitrary. Default is to only have a generator, but other
    arguments can be added *)

val set_print : 'a Print.t -> 'a arbitrary -> 'a arbitrary
val set_small : ('a -> int) -> 'a arbitrary -> 'a arbitrary
val set_shrink : 'a Shrink.t -> 'a arbitrary -> 'a arbitrary
val set_collect : ('a -> string) -> 'a arbitrary -> 'a arbitrary

(** {2 Tests}

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
    instance: 'a; (** The counter-example(s) *)
    shrink_steps: int; (** How many shrinking steps for this counterex *)
  }

  type 'a failed_state = 'a counter_ex list

  type 'a state =
    | Success
    | Failed of 'a failed_state (** Failed instances *)
    | Error of 'a counter_ex * exn * string (** Error, backtrace, and instance
                                                that triggered it *)

  (* result returned by running a test *)
  type 'a t = {
    mutable state : 'a state;
    mutable count: int;  (* number of tests *)
    mutable count_gen: int; (* number of generated cases *)
    collect_tbl: (string, int) Hashtbl.t lazy_t;
  }
end

module Test : sig
  type 'a cell
  (** A single property test *)

  val make_cell :
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?small:('a -> int) -> ?name:string -> 'a arbitrary -> ('a -> bool) ->
    'a cell
  (** [make arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      @param name the name of the test
      @param count number of test cases to run, counting only
        the test cases which satisfy preconditions.
      @param long_factor the factor by which to multiply count, max_gen and
        max_fail when running a long test (default: 1)
      @param max_gen maximum number of times the generation function
        is called in total to replace inputs that do not satisfy
        preconditions (should be >= count)
      @param max_fail maximum number of failures before we stop generating
        inputs. This is useful if shrinking takes too much time.
      @param small kept for compatibility reasons; if provided, replaces
        the field [arbitrary.small].
        If there is no shrinking function but there is a [small]
        function, only the smallest failures will be printed.
  *)

  val get_arbitrary : 'a cell -> 'a arbitrary
  val get_law : 'a cell -> ('a -> bool)
  val get_name : _ cell -> string
  val set_name : _ cell -> string -> unit

  val get_count : _ cell -> int
  (** Get the count of a cell
      @since 0.5.3 *)

  val get_long_factor : _ cell -> int
  (** Get the long factor of a cell
      @since 0.5.3 *)

  type t = Test : 'a cell -> t
  (** Same as ['a cell], but masking the type parameter. This allows to
      put tests on different types in the same list of tests. *)

  val make :
    ?count:int -> ?long_factor:int -> ?max_gen:int -> ?max_fail:int ->
    ?small:('a -> int) -> ?name:string -> 'a arbitrary -> ('a -> bool) -> t
  (** [make arb prop] builds a test that checks property [prop] on instances
      of the generator [arb].
      See {!make_cell} for a description of the parameters.
  *)

  (** {6 Running the test} *)

  exception Test_fail of string * string list
  (** Exception raised when a test failed, with the list of counter-examples.
      [Test_fail (name, l)] means test [name] failed on elements of [l] *)

  exception Test_error of string * string * exn * string
  (** Exception raised when a test raised an exception [e], with
      the sample that triggered the exception.
      [Test_error (name, i, e, st)]
      means [name] failed on [i] with exception [e], and [st] is the
      stacktrace (if enabled) or an empty string *)

  val print_instance : 'a arbitrary -> 'a -> string
  val print_c_ex : 'a arbitrary -> 'a TestResult.counter_ex -> string
  val print_fail : 'a arbitrary -> string -> 'a TestResult.counter_ex list -> string
  val print_error : ?st:string -> 'a arbitrary -> string -> 'a TestResult.counter_ex * exn -> string
  val print_test_fail : string -> string list -> string
  val print_test_error : string -> string -> exn -> string -> string

  val check_result : 'a cell -> 'a TestResult.t -> unit
  (** [check_result cell res] checks that [res] is [Ok _], and returns unit.
      Otherwise, it raises some exception
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)

  type res =
    | Success
    | Failure
    | FalseAssumption
    | Error of exn * string

  type 'a step = string -> 'a cell -> 'a -> res -> unit
  (** Callback executed after each instance of a test has been run.
      The callback is given the instance tested, and the current results
      of the test. *)

  type 'a callback = string -> 'a cell -> 'a TestResult.t -> unit
  (** Callback executed after each test has been run.
      [f name cell res] means test [cell], named [name], gave [res] *)

  val check_cell :
    ?long:bool -> ?call:'a callback -> ?step:'a step ->
    ?rand:Random.State.t -> 'a cell -> 'a TestResult.t
  (** [check ~long ~rand test] generates up to [count] random
      values of type ['a] using [arbitrary] and the random state [st]. The
      predicate [law] is called on them and if it returns [false] or raises an
      exception then we have a counter example for the [law].

      @param long if [true] then multiply the number of instances to generate
        by the cell's long_factor
      @param call function called on each test case, with the result
      @param step function called on each instance of the test case, with the result
      @return the result of the test
  *)

  val check_cell_exn :
    ?long:bool -> ?call:'a callback -> ?step:'a step ->
    ?rand:Random.State.t -> 'a cell -> unit
  (** Same as {!check_cell} but calls  {!check_result} on the result.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)

  val check_exn : ?long:bool -> ?rand:Random.State.t -> t -> unit
  (** Same as {!check_cell} but calls  {!check_result} on the result.
      @raise Test_error if [res = Error _]
      @raise Test_error if [res = Failed _] *)
end

(** {2 Combinators for {!arbitrary}} *)

val choose : 'a arbitrary list -> 'a arbitrary
(** Choose among the given list of generators. The list must not
  be empty; if it is Invalid_argument is raised. *)

val unit : unit arbitrary
(** always generates [()], obviously. *)

val bool : bool arbitrary
(** uniform boolean generator *)

val float : float arbitrary
(** generates regular floats (no nan and no infinities) *)
(* FIXME: does not generate nan nor infinity I think *)

val pos_float : float arbitrary
(** positive float generator (no nan and no infinities) *)

val neg_float : float arbitrary
(** negative float generator (no nan and no infinities) *)

val int : int arbitrary
(** int generator. Uniformly distributed *)

val int_bound : int -> int arbitrary
(** [int_bound n] is uniform between [0] and [n] included *)

val int_range : int -> int -> int arbitrary
(** [int_range a b] is uniform between [a] and [b] included. [b] must be
    larger than [a]. *)

val small_nat : int arbitrary
(** Small unsigned integers
    @since 0.5.1 *)

val small_int : int arbitrary
(** Small unsigned integers. See {!Gen.small_int}.
    @deprecated use {!small_signed_int} *)

val small_signed_int : int arbitrary
(** Small signed integers
    @since 0.5.2 *)

val (--) : int -> int -> int arbitrary
(** Synonym to {!int_range} *)

val int32 : int32 arbitrary
(** int32 generator. Uniformly distributed *)

val int64 : int64 arbitrary
(** int generator. Uniformly distributed *)

val pos_int : int arbitrary
(** positive int generator. Uniformly distributed *)

val small_int_corners : unit -> int arbitrary
(** As [small_int], but each newly created generator starts with
 a list of corner cases before falling back on random generation. *)

val neg_int : int arbitrary
(** negative int generator. The distribution is similar to that of
    [small_int], not of [pos_int].
*)

val char : char arbitrary
(** Uniformly distributed on all the chars (not just ascii or
    valid latin-1) *)

val printable_char : char arbitrary
(** uniformly distributed over a subset of chars *)
(* FIXME: describe which subset *)

val numeral_char : char arbitrary
(** uniformy distributed over ['0'..'9'] *)

val string_gen_of_size : int Gen.t -> char Gen.t -> string arbitrary

val string_gen : char Gen.t -> string arbitrary
(** generates strings with a distribution of length of [small_nat] *)

val string : string arbitrary
(** generates strings with a distribution of length of [small_nat]
    and distribution of characters of [char] *)

val small_string : string arbitrary
(** Same as {!string} but with a small length (that is, [0--10]) *)

val small_list : 'a arbitrary -> 'a list arbitrary
(** Generates lists of small size (range [0 -- 10]).
    @since 0.5.3 *)

val string_of_size : int Gen.t -> string arbitrary
(** generates strings with distribution of characters if [char] *)

val printable_string : string arbitrary
(** generates strings with a distribution of length of [small_nat]
    and distribution of characters of [printable_char] *)

val printable_string_of_size : int Gen.t -> string arbitrary
(** generates strings with distribution of characters of [printable_char] *)

val small_printable_string : string arbitrary

val numeral_string : string arbitrary
(** generates strings with a distribution of length of [small_nat]
    and distribution of characters of [numeral_char] *)

val numeral_string_of_size : int Gen.t -> string arbitrary
(** generates strings with a distribution of characters of [numeral_char] *)

val list : 'a arbitrary -> 'a list arbitrary
(** generates lists with length generated by [small_nat] *)

val list_of_size : int Gen.t -> 'a arbitrary -> 'a list arbitrary
(** generates lists with length from the given distribution *)

val array : 'a arbitrary -> 'a array arbitrary
(** generates arrays with length generated by [small_nat] *)

val array_of_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** generates arrays with length from the given distribution *)

val pair : 'a arbitrary -> 'b arbitrary -> ('a * 'b) arbitrary
(** combines two generators into a generator of pairs *)

val triple : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a * 'b * 'c) arbitrary
(** combines three generators into a generator of 3-tuples *)

val quad : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> 'd arbitrary -> ('a * 'b * 'c * 'd) arbitrary
(** combines four generators into a generator of 4-tuples *)

val option : 'a arbitrary -> 'a option arbitrary
(** choose between returning Some random value, or None *)

val fun1 : 'a arbitrary -> 'b arbitrary -> ('a -> 'b) arbitrary
(** generator of functions of arity 1.
    The functions are always pure and total functions:
    - when given the same argument (as decided by Pervasives.(=)), it returns the same value
    - it never does side effects, like printing or never raise exceptions etc.
    The functions generated are really printable.
*)

val fun2 : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a -> 'b -> 'c) arbitrary
(** generator of functions of arity 2. The remark about [fun1] also apply
    here.
*)

val oneofl : ?print:'a Print.t -> ?collect:('a -> string) ->
             'a list -> 'a arbitrary
(** Pick an element randomly in the list *)

val oneofa : ?print:'a Print.t -> ?collect:('a -> string) ->
             'a array -> 'a arbitrary
(** Pick an element randomly in the array *)

val oneof : 'a arbitrary list -> 'a arbitrary
(** Pick a generator among the list, randomly *)

val always : ?print:'a Print.t -> 'a -> 'a arbitrary
(** Always return the same element *)

val frequency : ?print:'a Print.t -> ?small:('a -> int) ->
                ?shrink:'a Shrink.t -> ?collect:('a -> string) ->
                (int * 'a arbitrary) list -> 'a arbitrary
(** Similar to {!oneof} but with frequencies *)

val frequencyl : ?print:'a Print.t -> ?small:('a -> int) ->
                (int * 'a) list -> 'a arbitrary
(** Same as {!oneofl}, but each element is paired with its frequency in
    the probability distribution (the higher, the more likely) *)

val frequencya : ?print:'a Print.t -> ?small:('a -> int) ->
                (int * 'a) array -> 'a arbitrary
(** Same as {!frequencyl}, but with an array *)

val map : ?rev:('b -> 'a) -> ('a -> 'b) -> 'a arbitrary -> 'b arbitrary
(** [map f a] returns a new arbitrary instance that generates values using
    [a#gen] and then transforms them through [f].
    @param rev if provided, maps values back to type ['a] so that the printer,
      shrinker, etc. of [a] can be used. We assume [f] is monotonic in
      this case (that is, smaller inputs are transformed into smaller outputs).
*)

val map_same_type : ('a -> 'a) -> 'a arbitrary -> 'a arbitrary
(** Specialization of [map] when the transformation preserves the type, which
   makes shrinker, printer, etc. still relevant *)

val map_keep_input :
  ?print:'b Print.t -> ?small:('b -> int) ->
  ('a -> 'b) -> 'a arbitrary -> ('a * 'b) arbitrary
(** [map_keep_input f a] generates random values from [a], and maps them into
    values of type ['b] using the function [f], but it also keeps  the
    original value.
    For shrinking, it is assumed that [f] is monotonic and that smaller input
      values will map into smaller values
    @param print optional printer for the [f]'s output
*)

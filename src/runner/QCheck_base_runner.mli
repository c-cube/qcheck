(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

(** {1 Runners for Tests}

    Once you built some tests using {!QCheck.Test.make}, you need to
    run the tests. This module contains several {b runners},
    which are designed to run every test and report the result.

    By default, you can use {!run_tests} in a test program as follows:
    {[
      let testsuite = [
        Test.make ...;
        Test.make ...;
      ]

      let () =
        let errcode = QCheck_runners.run_tests ~verbose:true testsuite in
        exit errcode
    ]}
    which will run the tests, and exit the program. The error code
    will be 0 if all tests pass, 1 otherwise.

    {!run_tests_main} can be used as a shortcut for that, also
    featuring command-line parsing (using {!Arg}) to activate
    verbose mode and others.
*)

(** {2 State} *)

val random_state : unit -> Random.State.t
(** Access the current random state *)

val verbose : unit -> bool
(** Is the default mode verbose or quiet? *)

val long_tests : unit -> bool
(** Is the default mode to run long tests or nor? *)

val set_seed : int -> unit
(** Change the {!random_state} by creating a new one, initialized with
    the given seed. *)

val set_verbose : bool -> unit
(** Change the value of [verbose ()] *)

val set_long_tests : bool -> unit
(** Change the value of [long_tests ()] *)

val get_time_between_msg : unit -> float
(** Get the minimum time to wait between printing messages.
    @since 0.9 *)

val set_time_between_msg : float -> unit
(** Set the minimum tiem between messages.
    @since 0.9 *)

(** {2 Run a Suite of Tests and Get Results} *)

val run_tests :
  ?colors:bool -> ?verbose:bool -> ?long:bool ->
  ?out:out_channel -> ?rand:Random.State.t ->
  QCheck.Test.t list -> int
(** Run a suite of tests, and print its results. This is an heritage from
    the "qcheck" library.
    @return an error code, [0] if all tests passed, [1] otherwise.
    @param colors if true, colorful output
    @param verbose if true, prints more information about test cases *)

val run_tests_main : ?argv:string array -> QCheck.Test.t list -> 'a
(** Can be used as the main function of a test file. Exits with a non-0 code
    if the tests fail. It refers to {!run_tests} for actually running tests
    after CLI options have been parsed.

    The available options are:

    - "--verbose" (or "-v") for activating verbose tests
    - "--seed <n>" (or "-s <n>") for repeating a previous run by setting the random seed
    - "--long" for running the long versions of the tests

    Below is an example of the output of the [run_tests] and [run_tests_main]
    function: {v
random seed: 438308050
generated  error;  fail; pass / total -     time -- test name
[✓] (1000)    0 ;    0 ; 1000 / 1000 --     0.5s -- list_rev_is_involutive
[✗] (   1)    0 ;    1 ;    0 /   10 --     0.0s -- should_fail_sort_id
[✗] (   1)    1 ;    0 ;    0 /   10 --     0.0s -- should_error_raise_exn
[✓] (1000)    0 ;    0 ; 1000 / 1000 --     0.0s -- collect_results

--- Failure --------------------------------------------------------------------

Test should_fail_sort_id failed (11 shrink steps):

[1; 0]

=== Error ======================================================================

Test should_error_raise_exn errored on (62 shrink steps):

0

exception QCheck_runner_test.Error
Raised at file "example/QCheck_runner_test.ml", line 20, characters 20-25
Called from file "src/QCheck.ml", line 839, characters 13-33


+++ Collect ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Collect results for test collect_results:

4: 207 cases
3: 190 cases
2: 219 cases
1: 196 cases
0: 188 cases

================================================================================
failure (1 tests failed, 1 tests errored, ran 4 tests)
v}
*)

(** {2 Utils for colored output} *)
module Color : sig
  type color =
    [ `Red
    | `Yellow
    | `Green
    | `Blue
    | `Normal
    | `Cyan
    ]

  val reset_line : string
  val pp_str_c : ?bold:bool -> colors:bool -> color -> out_channel -> string -> unit
end

(** {2 Internal Utils}

    We provide {b NO} stability guarantee for this module. Use at your
    own risks. *)
module Raw : sig
  type ('b,'c) printer = {
    info: 'a. ('a,'b,'c,unit) format4 -> 'a;
    fail: 'a. ('a,'b,'c,unit) format4 -> 'a;
    err: 'a. ('a,'b,'c,unit) format4 -> 'a;
  }

  val print_std : (out_channel, unit) printer

  (* main callback for display *)
  val callback :
    verbose:bool ->
    print_res:bool ->
    print:('a, 'b) printer ->
    string -> 'c QCheck.Test.cell -> 'c QCheck.TestResult.t -> unit

  type cli_args = {
    cli_verbose : bool;
    cli_long_tests : bool;
    cli_print_list : bool;
    cli_rand : Random.State.t;
    cli_slow_test : int; (* how many slow tests to display? *)
    cli_colors: bool;
  }

  val parse_cli : full_options:bool -> string array -> cli_args
end

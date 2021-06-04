
(** {1 Conversion of tests to OUnit Tests}

    @since 0.9
*)

val to_ounit_test :
  ?verbose:bool -> ?long:bool -> ?rand:Random.State.t ->
  QCheck2.Test.t -> OUnit.test
(** [to_ounit_test ~rand t] wraps [t] into a OUnit test
    @param verbose used to print information on stdout (default: [verbose()])
    @param rand the random generator to use (default: [random_state ()]) *)

val to_ounit_test_cell :
  ?verbose:bool -> ?long:bool -> ?rand:Random.State.t ->
  _ QCheck2.Test.cell -> OUnit.test
(** Same as {!to_ounit_test} but with a polymorphic test cell *)

val (>:::) : string -> QCheck2.Test.t list -> OUnit.test
(** Same as [OUnit.(>:::)] but with a list of QCheck2 tests *)

val to_ounit2_test : ?rand:Random.State.t -> QCheck2.Test.t -> OUnit2.test
(** [to_ounit2_test ?rand t] wraps [t] into a OUnit2 test
    @param rand the random generator to use (default: a static seed for reproducibility),
    can be overridden with "-seed" on the command-line
*)

val to_ounit2_test_list : ?rand:Random.State.t -> QCheck2.Test.t list -> OUnit2.test list
(** [to_ounit2_test_list ?rand t] like [to_ounit2_test] but for a list of tests *)

(** {2 OUnit runners}

    QCheck provides some custom runners for OUnit tests.

    - {!run} is used by {{: https://github.com/vincent-hugot/qtest} qtest}.
    - {!run_tap} should be compatible with {{: https://en.wikipedia.org/wiki/Test_Anything_Protocol} TAP}.

    Note that {!OUnit.run_test_tt} or {!OUnit.run_test_tt_main} can be used as well,
    in particular when QCheck tests are mixed with normal unit tests.

    For OUnit2 you can use {!OUnit2.run_test_tt_main}.
*)


val run : ?argv:string array -> OUnit.test -> int
(** [run test] runs the test, and returns an error code  that is [0]
    if all tests passed, [1] otherwise.
    This is the default runner used by the comment-to-test generator.

    @param argv the command line arguments to parse parameters from (default [Sys.argv])
    @raise Arg.Bad in case [argv] contains unknown arguments
    @raise Arg.Help in case [argv] contains "--help"

    This test runner displays execution in a compact way, making it good
    for suites that have lots of tests.

    Output example:
    {v
random seed: 101121210
random seed: 101121210
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
Error: tests>error_raise_exn

test `error_raise_exn` raised exception `QCheck_ounit_test.Error`
on `0 (after 62 shrink steps)`
Raised at file "example/QCheck_ounit_test.ml", line 19, characters 20-25
Called from file "src/QCheck.ml", line 846, characters 13-33

///////////////////////////////////////////////////////////////////////////////
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
Failure: tests>fail_sort_id

fail_sort_id
///////////////////////////////////////////////////////////////////////////////
Ran: 4 tests in: 0.74 seconds.
WARNING! SOME TESTS ARE NEITHER SUCCESSES NOR FAILURES!
v}
*)

val run_tap : OUnit.test -> OUnit.test_results
(** TAP-compatible test runner, in case we want to use a test harness.
    It prints one line per test. *)


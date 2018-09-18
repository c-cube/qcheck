
(** {1 Alcotest backend for QCheck}

    We use environment variables for controlling QCheck here, since alcotest
    doesn't seem to provide a lot of flexibility.

    [QCHECK_VERBOSE] if "1" or "true", will make tests verbose
    [QCHECK_SEED] if an integer, will fix the seed
    [QCHECK_LONG] is present, will trigger long tests

    @since 0.9
*)

val to_alcotest :
  ?verbose:bool -> ?long:bool -> ?rand:Random.State.t ->
  QCheck.Test.t -> unit Alcotest.test_case
(** Convert a qcheck test into an alcotest test
    @param verbose used to print information on stdout (default: [verbose()])
    @param rand the random generator to use (default: [random_state ()])
    @since 0.9
*)

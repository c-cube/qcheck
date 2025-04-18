
(** {1 Alcotest backend for QCheck}

    We use environment variables for controlling QCheck here, since alcotest
    doesn't seem to provide a lot of flexibility.

    - [QCHECK_VERBOSE] if "1" or "true", will make tests verbose
    - [QCHECK_SEED] if an integer, will fix the seed
    - [QCHECK_LONG] is present, will trigger long tests

    @since 0.9
*)

val to_alcotest :
  ?colors:bool -> ?verbose:bool -> ?long:bool ->
  ?debug_shrink:(out_channel option) ->
  ?debug_shrink_list:(string list) ->
  ?speed_level:Alcotest.speed_level -> ?rand:Random.State.t ->
  QCheck2.Test.t -> unit Alcotest.test_case
(** Convert a qcheck test into an alcotest test.

    The optional [speed_level] is [`Slow] by default, meaning Alcotest can skip
    such a test when the [-q] flag is passed. Passing [`Quick] instead means the
    test is always run.

    In addition to [speed_level] and the environment variables mentioned above,
    you can control the behavior of QCheck tests using optional parameters that
    behave in the same way as the parameters of {!QCheck_base_runner.run_tests}.

    @since 0.9
    @since 0.9 parameters [verbose], [long], [rand]
    @since 0.19 parameters [colors], [debug_shrink], [debug_shrink_list]
    @since 0.24 parameter [speed_level]
*)

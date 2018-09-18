
(** {1 Alcotest backend for QCheck} *)

val to_alcotest :
  ?verbose:bool -> ?long:bool -> ?rand:Random.State.t ->
  QCheck.Test.t -> unit Alcotest.test_case
(** Convert a qcheck test into an alcotest test
    @param verbose used to print information on stdout (default: [verbose()])
    @param rand the random generator to use (default: [random_state ()]) *)

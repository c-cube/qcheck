open QCheck_tests

(* Calling runners *)

let () = QCheck_base_runner.set_seed 1234
let _ =
  QCheck_base_runner.run_tests ~colors:false (
    Overall.tests @
    Generator.tests @
    Shrink.tests @
    Function.tests @
    FindExample.tests @
    Stats.tests)

let () = QCheck_base_runner.set_seed 153870556
let _  = QCheck_base_runner.run_tests ~colors:false [Stats.int_dist_empty_bucket]

(* incomplete tests, checking the exit code *)
let () = QCheck_base_runner.set_seed 1234
let () = assert (QCheck_base_runner.run_tests ~colors:false Incomplete.tolerated = 0)

let () = QCheck_base_runner.set_seed 1234
let () = assert (QCheck_base_runner.run_tests ~colors:false Incomplete.failing = 1)

let () = QCheck_base_runner.set_seed 1234
let () = assert (QCheck_base_runner.run_tests ~colors:false ~fail_on_incomplete:true Incomplete.tolerated = 1)

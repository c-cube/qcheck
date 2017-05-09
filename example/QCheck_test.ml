let (|>) x f = f x

let passing =
  QCheck.Test.make ~count:1000 ~long_factor:2
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.Test.make ~count:10
    ~name:"should_fail_sort_id"
    QCheck.(small_list small_int)
    (fun l -> l = List.sort compare l);;

exception Error

let error =
  QCheck.Test.make ~count:10
    ~name:"should_error_raise_exn"
    QCheck.int
    (fun _ -> raise Error)

open OUnit

let regression_23 =
  "issue_23" >::
    (fun () ->
       let l = QCheck.Gen.(generate ~n:100_000 char) in
       OUnit.assert_bool "must contain '\255'"
         (List.exists (fun c->c = '\255') l)
    )

let regressions = [ regression_23 ]
let others =
  [passing; failing; error; ]
  |> List.map (fun t -> QCheck_runner.to_ounit_test t)

let suite =
  "tests" >::: (regressions @ others)

let () =
  exit (QCheck_runner.run suite)


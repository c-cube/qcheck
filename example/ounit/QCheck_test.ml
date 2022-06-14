(* Tests to check integration with the 'OUnit.test' interface *)

let (|>) x f = f x

module Q = QCheck

let passing =
  Q.Test.make ~count:1000 ~long_factor:2
    ~name:"list_rev_is_involutive"
    Q.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  Q.Test.make ~count:10
    ~name:"should_fail_sort_id"
    Q.(small_list small_int)
    (fun l -> l = List.sort compare l);;

exception Error

let error =
  Q.Test.make ~count:10
    ~name:"should_error_raise_exn"
    Q.int
    (fun _ -> raise Error)

let neg_test_failing_as_expected =
  Q.Test.make_neg ~name:"neg test pass (failing as expected)" QCheck.small_int (fun i -> i mod 2 = 0)

let neg_test_unexpected_success =
  Q.Test.make_neg ~name:"neg test unexpected success" QCheck.small_int (fun i -> i + i = i * 2)

let neg_test_error =
  Q.Test.make_neg ~name:"neg fail with error" QCheck.small_int (fun _i -> raise Error)

open OUnit

let regression_23 =
  "issue_23" >::
    (fun () ->
       let l = Q.Gen.(generate ~n:100_000 char) in
       OUnit.assert_bool "must contain '\255'"
         (List.exists (fun c->c = '\255') l)
    )

let regressions = [ regression_23 ]
let others =
  [ passing;
    failing;
    error;
    neg_test_failing_as_expected;
    neg_test_unexpected_success;
    neg_test_error;
  ] |> List.map (fun t -> QCheck_ounit.to_ounit_test t)

let suite =
  "tests" >::: (regressions @ others)

let () =
  try exit (QCheck_ounit.run suite)
  with Arg.Bad msg -> print_endline msg; exit 1
     | Arg.Help msg -> print_endline msg; exit 0


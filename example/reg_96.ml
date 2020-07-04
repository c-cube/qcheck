
let test =
  QCheck_ounit.to_ounit_test
    ~verbose:true
    (QCheck.Test.make ~count:1000 ~name:"foo"
       QCheck.(list small_nat)
       (fun l -> l = List.rev l))

let test2 =
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:1000 ~name:"foo2"
       QCheck.(list small_nat)
       (fun l -> l = List.rev l))

let _ =
  OUnit.perform_test
    (function
      | OUnit.EStart _ |OUnit.EEnd _ -> ()
      | OUnit.EResult (OUnit.RFailure (_, s)) ->
        Printf.printf "failed with msg %S\n%!" s
      | OUnit.EResult (OUnit.RError (_, s)) ->
        Printf.printf "err with msg %S\n%!" s
      | OUnit.EResult _ -> ())
  test

let _ =
  OUnit2.run_test_tt_main ~exit:ignore
(*
  OUnit.perform_test
    (function
      | OUnit.EStart _ |OUnit.EEnd _ -> ()
      | OUnit.EResult (OUnit.RFailure (_, s)) ->
        Printf.printf "failed with msg %S\n%!" s
      | OUnit.EResult (OUnit.RError (_, s)) ->
        Printf.printf "err with msg %S\n%!" s
      | OUnit.EResult _ -> ())
*)
  test2

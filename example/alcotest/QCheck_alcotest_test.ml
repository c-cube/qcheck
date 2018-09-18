let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list small_int)
    (fun l -> l = List.sort compare l);;

exception Error

let error =
  QCheck.Test.make ~count:10
    ~name:"error_raise_exn"
    QCheck.int
    (fun _ -> raise Error)

let simple_qcheck =
  QCheck.Test.make ~name:"fail_check_err_message"
    ~count: 100
    QCheck.small_int
    (fun _ -> QCheck.Test.fail_reportf "@[<v>this@ will@ always@ fail@]")

let () =
  Printexc.record_backtrace true;
  let module A = Alcotest in
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing; failing; error; simple_qcheck ]
  in
  A.run "my test" [
    "suite", suite
  ]

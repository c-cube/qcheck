
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

let collect =
  QCheck.Test.make ~count:1000
    ~name:"collect_results"
    QCheck.(make ~collect:string_of_int (Gen.int_bound 4))
    (fun _ -> true)

let () =
  QCheck_runner.run_tests_main [
    passing;
    failing;
    error;
    collect;
  ]


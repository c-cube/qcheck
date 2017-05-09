
let passing =
  QCheck.Test.make ~count:100 ~long_factor:100
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
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"collect_results"
    QCheck.(make ~collect:string_of_int (Gen.int_bound 4))
    (fun _ -> true)

let stats =
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"with_stats"
    QCheck.(make (Gen.int_bound 120)
        ~stats:[
          "mod4", (fun i->i mod 4);
          "num", (fun i->i);
        ]
    )
    (fun _ -> true)

let fun1 =
  QCheck.Test.make ~count:100
    ~name:"FAIL_pred_map_commute"
    QCheck.(triple
        (small_list small_int)
        (fun1_unsafe int int)
        (fun1_unsafe int bool))
    (fun (l,f,p) ->
       List.filter p (List.map f l) = List.map f (List.filter p l))

let fun2 =
  QCheck.Test.make ~count:100
    ~name:"FAIL_fun2_pred_strings"
    QCheck.(fun1_unsafe string bool)
    (fun p ->
       not (p "some random string") || p "some other string")

let () =
  QCheck_runner.run_tests_main [
    passing;
    failing;
    error;
    collect;
    stats;
    fun1;
    fun2;
  ]


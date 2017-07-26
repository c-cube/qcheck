
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
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"FAIL_pred_map_commute"
    QCheck.(triple
        (small_list small_int)
        (fun1 Observable.int int)
        (fun1 Observable.int bool))
    (fun (l,QCheck.Fun (_,f), QCheck.Fun (_,p)) ->
       List.filter p (List.map f l) = List.map f (List.filter p l))

let fun2 =
  QCheck.Test.make ~count:100
    ~name:"FAIL_fun2_pred_strings"
    QCheck.(fun1 Observable.string bool)
    (fun (QCheck.Fun (_,p)) ->
       not (p "some random string") || p "some other string")

let int_gen = QCheck.small_nat (* int *)

(* Another example (false) property *)
let prop_foldleft_foldright =
  let open QCheck in
  Test.make ~name:"fold_left fold_right" ~count:1000 ~long_factor:20
    (triple
       int_gen
       (list int_gen)
       (fun2 Observable.int Observable.int int_gen))
    (fun (z,xs,f) ->
       let l1 = List.fold_right (Fn.apply f) xs z in
       let l2 = List.fold_left (Fn.apply f) z xs in
       if l1=l2 then true
       else QCheck.Test.fail_reportf "l=%s, fold_left=%s, fold_right=%s@."
           (QCheck.Print.(list int) xs)
           (QCheck.Print.int l1)
           (QCheck.Print.int l2)
    )

(* Another example (false) property *)
let prop_foldleft_foldright_uncurry =
  let open QCheck in
  Test.make ~name:"fold_left fold_right uncurried" ~count:1000 ~long_factor:20
    (triple
       (fun1 Observable.(pair int int) int_gen)
       int_gen
       (list int_gen))
    (fun (f,z,xs) ->
       List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
       List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

let long_shrink =
  let open QCheck in
  let listgen = list_of_size (Gen.int_range 1000 10000) int in
  Test.make ~name:"long_shrink" (pair listgen listgen)
    (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

let find_ex =
  let open QCheck in
  Test.make ~name:"find_example" (2--50)
  (fun n ->
    let st = Random.State.make [| 0 |] in
    let f m = n < m && m < 2 * n in
    try
      let m = find_example_gen ~rand:st ~count:100_000 ~f Gen.(0 -- 1000) in
      f m
     with No_example_found _ -> false)

(* test shrinking on integers *)
let shrink_int =
  QCheck.Test.make ~count:1000 ~name:"mod3_should_fail"
   QCheck.int (fun i -> i mod 3 <> 0);;

let stats_negs =
  QCheck.(Test.make ~count:5_000 ~name:"stats_neg"
      (add_stat ("dist",fun x -> x) small_signed_int))
    (fun _ -> true)

let stats_tests =
  let open QCheck in
  [
    Test.make ~name:"stat_display_test_1" ~count:1000 (add_stat ("dist",fun x -> x) small_signed_int) (fun _ -> true);
    Test.make ~name:"stat_display_test_2" ~count:1000 (add_stat ("dist",fun x -> x) small_nat) (fun _ -> true);
    Test.make ~name:"stat_display_test_3" ~count:1000 (add_stat ("dist",fun x -> x) (int_range (-43643) 435434)) (fun _ -> true);
    Test.make ~name:"stat_display_test_4" ~count:1000 (add_stat ("dist",fun x -> x) (int_range (-40000) 40000)) (fun _ -> true);
    Test.make ~name:"stat_display_test_5" ~count:1000 (add_stat ("dist",fun x -> x) (int_range (-4) 4)) (fun _ -> true);
    Test.make ~name:"stat_display_test_6" ~count:1000 (add_stat ("dist",fun x -> x) (int_range (-4) 17)) (fun _ -> true);
    Test.make ~name:"stat_display_test_7" ~count:100000 (add_stat ("dist",fun x -> x) int) (fun _ -> true);
  ]

let () =
  QCheck_runner.run_tests_main ([
    passing;
    failing;
    error;
    collect;
    stats;
    fun1;
    fun2;
    prop_foldleft_foldright;
    prop_foldleft_foldright_uncurry;
    long_shrink;
    find_ex;
    shrink_int;
    stats_negs;
  ] @ stats_tests)


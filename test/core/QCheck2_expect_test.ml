(** QCheck2 tests **)

(* tests of overall functionality *)
module Overall = struct
  let passing =
    QCheck2.Test.make ~count:100 ~long_factor:100 ~print:QCheck2.Print.(list int)
      ~name:"list_rev_is_involutive"
      QCheck2.Gen.(list small_int)
      (fun l -> List.rev (List.rev l) = l);;

  let failing =
    QCheck2.Test.make ~count:10
      ~name:"should_fail_sort_id" ~print:QCheck2.Print.(list int)
      QCheck2.Gen.(small_list small_int)
      (fun l -> l = List.sort compare l);;

  exception Error

  let error =
    QCheck2.Test.make ~count:10
      ~name:"should_error_raise_exn" ~print:QCheck2.Print.int
      QCheck2.Gen.int
      (fun _ -> raise Error)

  let collect =
    QCheck2.Test.make ~count:100 ~long_factor:100
      ~name:"collect_results" ~print:QCheck2.Print.int
      ~collect:string_of_int (QCheck2.Gen.int_bound 4)
      (fun _ -> true)

  let stats =
    QCheck2.Test.make ~count:100 ~long_factor:100
      ~name:"with_stats" ~print:QCheck2.Print.int
      ~stats:[
        "mod4", (fun i->i mod 4);
        "num", (fun i->i);
      ]
      (QCheck2.Gen.int_bound 120)
      (fun _ -> true)

  let bad_assume_warn =
    let open QCheck2 in
    Test.make ~count:2_000
      ~name:"WARN_unlikely_precond" ~print:Print.int
      Gen.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)

  let bad_assume_fail =
    let open QCheck2 in
    Test.make ~count:2_000 ~if_assumptions_fail:(`Fatal, 0.1)
      ~name:"FAIL_unlikely_precond" ~print:Print.int
      Gen.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)
end

(* test various generators *)
module Generator = struct
  (* example from issue #23 *)
  let char_dist_issue_23 =
    let open QCheck2 in
    Test.make ~name:"char never produces '\\255'" ~count:1_000_000 ~print:Print.char Gen.char (fun c -> c <> '\255')
end

(* tests function generator and shrinker *)
module Function = struct
  let fun1 =
    let open QCheck2 in
    Test.make ~count:100 ~long_factor:100
      ~name:"FAIL_pred_map_commute" ~print:Print.(triple (list int) Fn.print Fn.print)
      Gen.(triple
             (small_list small_int)
             (fun1 ~print:Print.int Observable.int int)
             (fun1 ~print:Print.bool Observable.int bool))
      (fun (l,QCheck2.Fun (_,f), QCheck2.Fun (_,p)) ->
         List.filter p (List.map f l) = List.map f (List.filter p l))

  let fun2 =
    let open QCheck2 in
    Test.make ~count:100
      ~name:"FAIL_fun2_pred_strings" ~print:Fn.print
      (fun1 Observable.string ~print:Print.bool Gen.bool)
      (fun (Fun (_,p)) ->
         not (p "some random string") || p "some other string")

  let int_gen = QCheck2.Gen.small_nat (* int *)

  (* Another example (false) property *)
  let prop_foldleft_foldright =
    let open QCheck2 in
    Test.make ~name:"fold_left fold_right" ~count:1000 ~long_factor:20
      ~print:Print.(triple int (list int) Fn.print)
      Gen.(triple
             int_gen
             (list int_gen)
             (fun2 ~print:Print.int Observable.int Observable.int int_gen))
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
    let open QCheck2 in
    Test.make ~name:"fold_left fold_right uncurried" ~count:1000 ~long_factor:20
      ~print:Print.(triple Fn.print int (list int))
      Gen.(triple
             (fun1 ~print:Print.int Observable.(pair int int) int_gen)
             int_gen
             (list int_gen))
      (fun (f,z,xs) ->
         List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
         List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

  (* Same as the above (false) property, but generating+shrinking functions last *)
  let prop_foldleft_foldright_uncurry_funlast =
    let open QCheck2 in
    Test.make ~name:"fold_left fold_right uncurried fun last" ~count:1000 ~long_factor:20
      ~print:Print.(triple int (list int) Fn.print)
      Gen.(triple
             int_gen
             (list int_gen)
             (fun1 ~print:Print.int Observable.(pair int int) int_gen))
      (fun (z,xs,f) ->
         List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
         List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)
end

(* tests of shrinking behaviour *)
module Shrink = struct
  let rec fac n = match n with
    | 0 -> 1
    | n -> n * fac (n - 1)

  (* example from issue #59 *)
  let test_fac_issue59 =
    let open QCheck2 in
    Test.make ~name:"test fac issue59"
      (Gen.make_primitive ~gen:(fun st -> Gen.generate1 ~rand:st (Gen.small_int_corners ())) ~shrink:(fun _ -> Seq.empty))
      (fun n -> try (fac n) mod n = 0
                with
                (*| Stack_overflow   -> false*)
                | Division_by_zero -> (n=0))

  let big_bound_issue59 =
    QCheck2.Test.make ~name:"big bound issue59" ~print:QCheck2.Print.int
      (QCheck2.Gen.small_int_corners()) (fun i -> i < 209609)

  let long_shrink =
    let open QCheck2 in
    let listgen = Gen.(list_size (int_range 1000 10000) int) in
    Test.make ~name:"long_shrink" ~print:Print.(pair (list int) (list int))
      (Gen.pair listgen listgen)
      (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

  (* test shrinking on integers *)
  let shrink_int =
    let open QCheck2 in
    Test.make ~count:1000 ~name:"mod3_should_fail" ~print:Print.int
      Gen.int (fun i -> i mod 3 <> 0)
end

(* tests of (inner) find_example(_gen) behaviour *)
module FindExample = struct
  let find_ex =
    let open QCheck2 in
    Test.make ~name:"find_example" ~print:Print.int
      Gen.(2--50)
      (fun n ->
         let st = Random.State.make [| 0 |] in
         let f m = n < m && m < 2 * n in
         try
           let m = find_example_gen ~rand:st ~count:100_000 ~f Gen.(0 -- 1000) in
           f m
         with No_example_found _ -> false)

  let find_ex_uncaught_issue_99 : _ list =
    let open QCheck2 in
    let t1 =
      let rs = (find_example ~count:10 ~f:(fun _ -> false) Gen.int) in
      Test.make ~name:"FAIL_#99_1" rs (fun _ -> true) in
    let t2 =
      Test.make ~name:"should_succeed_#99_2" ~count:10 Gen.int
        (fun i -> i <= max_int) in
    [t1;t2]
end

(* tests of statistics and histogram display *)
module Stats = struct
  let bool_dist =
    QCheck2.(Test.make ~count:500_000 ~name:"bool dist"
               ~collect:Bool.to_string Gen.bool) (fun _ -> true)
  
  let char_dist =
    QCheck2.(Test.make ~count:500_000 ~name:"char code dist"
              ~stats:[("char code", Char.code)] Gen.char) (fun _ -> true)

  (* test from issue #40 *)
  let int_stats_neg =
    QCheck2.(Test.make ~count:5_000 ~name:"int_stats_neg"
               ~stats:[("dist",fun x -> x)] Gen.small_signed_int)
      (fun _ -> true)

  (* distribution tests from PR #45 *)
  let int_stats_tests =
    let open QCheck2 in
    let dist = ("dist",fun x -> x) in
    [
      Test.make ~name:"int_stat_display_test_1" ~count:1000   ~stats:[dist] Gen.small_signed_int                 (fun _ -> true);
      Test.make ~name:"int_stat_display_test_2" ~count:1000   ~stats:[dist] Gen.small_nat                        (fun _ -> true);
      Test.make ~name:"int_stat_display_test_3" ~count:1000   ~stats:[dist] (Gen.int_range (-43643) 435434)      (fun _ -> true);
      Test.make ~name:"int_stat_display_test_4" ~count:1000   ~stats:[dist] (Gen.int_range (-40000) 40000)       (fun _ -> true);
      Test.make ~name:"int_stat_display_test_5" ~count:1000   ~stats:[dist] (Gen.int_range (-4) 4)               (fun _ -> true);
      Test.make ~name:"int_stat_display_test_6" ~count:1000   ~stats:[dist] (Gen.int_range (-4) 17)              (fun _ -> true);
      Test.make ~name:"int_stat_display_test_7" ~count:100000 ~stats:[dist] Gen.int                              (fun _ -> true);
      Test.make ~name:"int_stat_display_test_8" ~count:1000   ~stats:[dist] (Gen.oneofl[min_int;-1;0;1;max_int]) (fun _ -> true);
    ]

  let int_stat_display_test9  =
    let open QCheck2 in
    Test.make ~name:"int_stat_display_test9" ~count:1_000
      ~stats:[("dist",fun x -> x)] Gen.(oneof [small_int_corners ();int]) (fun _ -> true)
end

(* Calling runners *)

let () = QCheck_base_runner.set_seed 1234
let _ =
  QCheck_base_runner.run_tests ~colors:false ([
    Overall.passing;
    Overall.failing;
    Overall.error;
    Overall.collect;
    Overall.stats;
    Overall.bad_assume_warn;
    Overall.bad_assume_fail;
    Generator.char_dist_issue_23;
    Function.fun1;
    Function.fun2;
    Function.prop_foldleft_foldright;
    Function.prop_foldleft_foldright_uncurry;
    Function.prop_foldleft_foldright_uncurry_funlast;
    (*Shrink.test_fac_issue59;*)
    Shrink.big_bound_issue59;
    Shrink.long_shrink;
    Shrink.shrink_int;
  ] @ FindExample.find_ex :: FindExample.find_ex_uncaught_issue_99
    @ [Stats.bool_dist; Stats.char_dist; Stats.int_stats_neg] @ Stats.int_stats_tests)

let () = QCheck_base_runner.set_seed 153870556
let _  = QCheck_base_runner.run_tests ~colors:false [Stats.int_stat_display_test9]


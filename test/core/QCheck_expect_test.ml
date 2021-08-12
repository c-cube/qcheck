(** QCheck(1) tests **)

(* tests of overall functionality *)
module Overall = struct
  let passing =
    QCheck.Test.make ~count:100 ~long_factor:100
      ~name:"list_rev_is_involutive"
      QCheck.(list small_int)
      (fun l -> List.rev (List.rev l) = l)

  let failing =
    QCheck.Test.make ~count:10
      ~name:"should_fail_sort_id"
      QCheck.(small_list small_int)
      (fun l -> l = List.sort compare l)

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

  let bad_assume_warn =
    QCheck.Test.make ~count:2_000
      ~name:"WARN_unlikely_precond"
      QCheck.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)

  let bad_assume_fail =
    QCheck.Test.make ~count:2_000 ~if_assumptions_fail:(`Fatal, 0.1)
      ~name:"FAIL_unlikely_precond"
      QCheck.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)
end

(* test various generators *)
module Generator = struct
  (* example from issue #23 *)
  let char_dist_issue_23 =
    QCheck.Test.make ~name:"char never produces '\\255'" ~count:1_000_000 QCheck.char (fun c -> c <> '\255')
end

(* tests function generator and shrinker *)
module Function = struct
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

  (* Same as the above (false) property, but generating+shrinking functions last *)
  let prop_foldleft_foldright_uncurry_funlast =
    let open QCheck in
    Test.make ~name:"fold_left fold_right uncurried fun last" ~count:1000 ~long_factor:20
      (triple
         int_gen
         (list int_gen)
         (fun1 Observable.(pair int int) int_gen))
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
    let open QCheck in
    Test.make ~name:"test fac issue59"
      (set_shrink Shrink.nil (small_int_corners ()))
      (fun n -> try (fac n) mod n = 0
                with
                (*| Stack_overflow   -> false*)
                | Division_by_zero -> (n=0))

  let big_bound_issue59 =
    QCheck.Test.make ~name:"big bound issue59"
      (QCheck.small_int_corners()) (fun i -> i < 209609)

  let long_shrink =
    let open QCheck in
    let listgen = list_of_size (Gen.int_range 1000 10000) int in
    Test.make ~name:"long_shrink" (pair listgen listgen)
      (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

  (* test shrinking on integers *)
  let shrink_int =
    QCheck.Test.make ~count:1000 ~name:"mod3_should_fail"
      QCheck.int (fun i -> i mod 3 <> 0)
end

(* tests of (inner) find_example(_gen) behaviour *)
module FindExample = struct
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

  let find_ex_uncaught_issue_99 : _ list =
    let open QCheck in
    let t1 =
      let rs = make (find_example ~count:10 ~f:(fun _ -> false) Gen.int) in
      Test.make ~name:"FAIL_#99_1" rs (fun _ -> true) in
    let t2 =
      Test.make ~name:"should_succeed_#99_2" ~count:10 int
        (fun i -> i <= max_int) in
    [t1;t2]
end

(* tests of statistics and histogram display *)
module Stats = struct
  let bool_dist =
    QCheck.(Test.make ~count:500_000 ~name:"bool dist"
              (set_collect Bool.to_string bool)) (fun _ -> true)

  let char_dist =
    QCheck.(Test.make ~count:500_000 ~name:"char code dist"
              (add_stat ("char code", Char.code) char)) (fun _ -> true)

  let list_len_tests =
    let open QCheck in
    let len = ("len",List.length) in
    [ (* test from issue #30 *)
      Test.make ~count:5_000 ~name:"list len dist"         (add_stat len (list int))                              (fun _ -> true);
      Test.make ~count:5_000 ~name:"small_list len dist"   (add_stat len (small_list int))                        (fun _ -> true);
      Test.make ~count:5_000 ~name:"list_of_size len dist" (add_stat len (list_of_size (Gen.int_range 5 10) int)) (fun _ -> true);
    ]

  (* test from issue #40 *)
  let int_stats_neg =
    QCheck.(Test.make ~count:5_000 ~name:"int_stats_neg"
              (add_stat ("dist",fun x -> x) small_signed_int)) (fun _ -> true)

  (* distribution tests from PR #45 *)
  let int_stats_tests =
    let open QCheck in
    let dist = ("dist",fun x -> x) in
    [
      Test.make ~name:"int_stat_display_test_1" ~count:1000   (add_stat dist small_signed_int)                 (fun _ -> true);
      Test.make ~name:"int_stat_display_test_2" ~count:1000   (add_stat dist small_nat)                        (fun _ -> true);
      Test.make ~name:"int_stat_display_test_3" ~count:1000   (add_stat dist (int_range (-43643) 435434))      (fun _ -> true);
      Test.make ~name:"int_stat_display_test_4" ~count:1000   (add_stat dist (int_range (-40000) 40000))       (fun _ -> true);
      Test.make ~name:"int_stat_display_test_5" ~count:1000   (add_stat dist (int_range (-4) 4))               (fun _ -> true);
      Test.make ~name:"int_stat_display_test_6" ~count:1000   (add_stat dist (int_range (-4) 17))              (fun _ -> true);
      Test.make ~name:"int_stat_display_test_7" ~count:100000 (add_stat dist int)                              (fun _ -> true);
      Test.make ~name:"int_stat_display_test_8" ~count:1000   (add_stat dist (oneofl[min_int;-1;0;1;max_int])) (fun _ -> true);
    ]

  let int_stat_display_test9  =
    let open QCheck in
    Test.make ~name:"int_stat_display_test9" ~count:1_000
      (add_stat ("dist",fun x -> x) (oneof [small_int_corners ();int])) (fun _ -> true)
end

(* Calling runners *)

let () = QCheck_base_runner.set_seed 1234
let i =
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
    @ [Stats.bool_dist;
       Stats.char_dist]
    @ Stats.list_len_tests
    @ [Stats.int_stats_neg]
    @ Stats.int_stats_tests)

let () = QCheck_base_runner.set_seed 153870556
let _  = QCheck_base_runner.run_tests ~colors:false [Stats.int_stat_display_test9]

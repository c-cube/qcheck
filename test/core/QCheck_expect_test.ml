(** QCheck(1) tests **)

(* tests of overall functionality *)
module Overall = struct
  open QCheck

  let passing =
    Test.make ~name:"list_rev_is_involutive" ~count:100 ~long_factor:100
      (list small_int) (fun l -> List.rev (List.rev l) = l)

  let failing =
    Test.make ~name:"should_fail_sort_id" ~count:10
      (small_list small_int) (fun l -> l = List.sort compare l)

  exception Error

  let error =
    Test.make ~name:"should_error_raise_exn" ~count:10
      int (fun _ -> raise Error)

  let collect =
    Test.make ~name:"collect_results" ~count:100 ~long_factor:100
      (make ~collect:string_of_int (Gen.int_bound 4))
      (fun _ -> true)

  let stats =
    Test.make ~name:"with_stats" ~count:100 ~long_factor:100
      (make (Gen.int_bound 120)
         ~stats:[
           "mod4", (fun i->i mod 4);
           "num", (fun i->i);
         ])
      (fun _ -> true)

  let bad_assume_warn =
    Test.make ~name:"WARN_unlikely_precond" ~count:2_000
      int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)

  let bad_assume_fail =
    Test.make ~name:"FAIL_unlikely_precond" ~count:2_000
      ~if_assumptions_fail:(`Fatal, 0.1)
      int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)
end

(* positive tests of the various generators *)
module Generator = struct
  open QCheck

  (* example from issue #23 *)
  let char_dist_issue_23 =
    Test.make ~name:"char never produces '\\255'" ~count:1_000_000 char (fun c -> c <> '\255')

  let char_test =
    Test.make ~name:"char has right range'" ~count:1000
      char (fun c -> '\000' <= c && c <= '\255')

  let string_test =
    Test.make ~name:"string has right length and content" ~count:1000
      string
      (fun s ->
         let len = String.length s in
         0 <= len && len < 10000
         && String.to_seq s |>
            Seq.fold_left (fun acc c -> acc && '\000' <= c && c <= '\255') true)

  let list_test =
    Test.make ~name:"list has right length" ~count:1000
      (list unit) (fun l -> let len = List.length l in 0 <= len && len < 10_000)

  let list_repeat_test =
    let gen = Gen.(small_nat >>= fun i -> list_repeat i unit >>= fun l -> return (i,l)) in
    Test.make ~name:"list_repeat has constant length" ~count:1000
      (make ~print:Print.(pair int (list unit)) gen) (fun (i,l) -> List.length l = i)

  let array_repeat_test =
    let gen = Gen.(small_nat >>= fun i -> array_repeat i unit >>= fun l -> return (i,l)) in
    Test.make ~name:"array_repeat has constant length" ~count:1000
      (make ~print:Print.(pair int (array unit)) gen) (fun (i,l) -> Array.length l = i)
end

(* negative tests that exercise shrinking behaviour *)
module Shrink = struct
  open QCheck

  let rec fac n = match n with
    | 0 -> 1
    | n -> n * fac (n - 1)

  (* example from issue #59 *)
  let test_fac_issue59 =
    Test.make ~name:"test fac issue59"
      (set_shrink Shrink.nil (small_int_corners ()))
      (fun n -> try (fac n) mod n = 0
                with
                (*| Stack_overflow   -> false*)
                | Division_by_zero -> (n=0))

  let big_bound_issue59 =
    Test.make ~name:"big bound issue59"
      (small_int_corners()) (fun i -> i < 209609)

  let long_shrink =
    let listgen = list_of_size (Gen.int_range 1000 10000) int in
    Test.make ~name:"long_shrink" (pair listgen listgen)
      (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

  let ints_arent_0_mod_3 =
    Test.make ~name:"ints arent 0 mod 3" ~count:1000
      int (fun i -> i mod 3 <> 0)

  let ints_are_0 =
    Test.make ~name:"ints are 0" ~count:1000
      int (fun i -> Printf.printf "%i\n" i; i = 0)

  (* test from issue #59 *)
  let ints_smaller_209609 =
    Test.make ~name:"ints < 209609"
      (small_int_corners()) (fun i -> i < 209609)

  let char_is_never_abcdef =
    Test.make ~name:"char is never produces 'abcdef'" ~count:1000
      char (fun c -> not (List.mem c ['a';'b';'c';'d';'e';'f']))

  let string_never_has_000_char =
    Test.make ~name:"string never has a \\000 char" ~count:1000
      string
      (fun s -> String.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\000') true)

  let string_never_has_255_char =
    Test.make ~name:"string never has a \\255 char" ~count:1000
      string
      (fun s -> String.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\255') true)

  let print_list xs = print_endline Print.(list int xs)
  (* test from issue #64 *)
  let lists_are_empty_issue_64 =
    Test.make ~name:"lists are empty"
      (list small_int) (fun xs -> print_list xs; xs = [])

  let list_shorter_10 =
    Test.make ~name:"lists shorter than 10"
      (list small_int) (fun xs -> (*print_list xs;*) List.length xs < 10)

  let length_printer xs =
    Printf.sprintf "[...] list length: %i" (List.length xs)

  let size_gen = Gen.(oneof [small_nat; int_bound 750_000])

  let list_shorter_432 =
    Test.make ~name:"lists shorter than 432"
      (set_print length_printer (list_of_size size_gen small_int)) (*(list small_int)*)
      (fun xs -> (*print_list xs;*) List.length xs < 432)

  let list_shorter_4332 =
    Test.make ~name:"lists shorter than 4332"
      (set_shrink Shrink.list_spine (set_print length_printer (list_of_size size_gen small_int)))
      (fun xs -> (*print_list xs;*) List.length xs < 4332)

  let list_equal_dupl =
    Test.make ~name:"lists equal to duplication"
      (set_print length_printer (list_of_size size_gen small_int))
      (*(set_print length_printer (list small_int))*)
      (fun xs -> try xs = xs @ xs
                 with Stack_overflow -> false)
end

(* tests function generator and shrinker *)
module Function = struct
  open QCheck

  let fail_pred_map_commute =
    Test.make ~name:"fail_pred_map_commute" ~count:100 ~long_factor:100
      (triple
         (small_list small_int)
         (fun1 Observable.int int)
         (fun1 Observable.int bool))
      (fun (l,Fun (_,f),Fun (_,p)) ->
         List.filter p (List.map f l) = List.map f (List.filter p l))

  let fail_pred_strings =
    Test.make ~name:"fail_pred_strings" ~count:100
      (fun1 Observable.string bool)
      (fun (Fun (_,p)) -> not (p "some random string") || p "some other string")

  let int_gen = small_nat (* int *)

  (* Another example (false) property *)
  let prop_foldleft_foldright =
    Test.make ~name:"fold_left fold_right" ~count:1000 ~long_factor:20
      (triple
         int_gen
         (list int_gen)
         (fun2 Observable.int Observable.int int_gen))
      (fun (z,xs,f) ->
         let l1 = List.fold_right (Fn.apply f) xs z in
         let l2 = List.fold_left (Fn.apply f) z xs in
         if l1=l2 then true
         else Test.fail_reportf "l=%s, fold_left=%s, fold_right=%s@."
             (Print.(list int) xs)
             (Print.int l1)
             (Print.int l2)
      )

  (* Another example (false) property *)
  let prop_foldleft_foldright_uncurry =
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
    Test.make ~name:"fold_left fold_right uncurried fun last" ~count:1000 ~long_factor:20
      (triple
         int_gen
         (list int_gen)
         (fun1 Observable.(pair int int) int_gen))
      (fun (z,xs,f) ->
         List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
         List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

  (* test from issue #64 *)
  let fold_left_test =
    Test.make ~name:"false fold, fun first"
      (quad  (* string -> int -> string *)
         (fun2 Observable.string Observable.int small_string)
         small_string
         (list small_int)
         (list small_int))
      (fun (f,acc,is,js) ->
         let f = Fn.apply f in
         List.fold_left f acc (is @ js)
         = List.fold_left f (List.fold_left f acc is) is) (*Typo*)
end

(* tests of (inner) find_example(_gen) behaviour *)
module FindExample = struct
  open QCheck

  let find_ex =
    Test.make ~name:"find_example" (2--50)
      (fun n ->
         let st = Random.State.make [| 0 |] in
         let f m = n < m && m < 2 * n in
         try
           let m = find_example_gen ~rand:st ~count:100_000 ~f Gen.(0 -- 1000) in
           f m
         with No_example_found _ -> false)

  let find_ex_uncaught_issue_99_1_fail =
    let rs = make (find_example ~count:10 ~f:(fun _ -> false) Gen.int) in
    Test.make ~name:"FAIL_#99_1" rs (fun _ -> true)

  let find_ex_uncaught_issue_99_2_succeed =
    Test.make ~name:"should_succeed_#99_2" ~count:10
      int (fun i -> i <= max_int)
end

(* tests of statistics and histogram display *)
module Stats = struct
  open QCheck

  let bool_dist =
    Test.make ~name:"bool dist" ~count:500_000 (set_collect Bool.to_string bool) (fun _ -> true)

  let char_dist =
    Test.make ~name:"char code dist" ~count:500_000 (add_stat ("char code", Char.code) char) (fun _ -> true)

  let string_len_tests =
    let len = ("len",String.length) in
    [
      Test.make ~name:"string_size len dist"      ~count:5_000 (add_stat len (string_of_size (Gen.int_range 5 10))) (fun _ -> true);
      Test.make ~name:"string len dist"           ~count:5_000 (add_stat len string)                                (fun _ -> true);
      Test.make ~name:"string_of len dist"        ~count:5_000 (add_stat len (string_gen (Gen.return 'a')))         (fun _ -> true);
      Test.make ~name:"printable_string len dist" ~count:5_000 (add_stat len printable_string)                      (fun _ -> true);
      Test.make ~name:"small_string len dist"     ~count:5_000 (add_stat len small_string)                          (fun _ -> true);
    ]

  let list_len_tests =
    let len = ("len",List.length) in
    [ (* test from issue #30 *)
      Test.make ~name:"list len dist"         ~count:5_000 (add_stat len (list int))                              (fun _ -> true);
      Test.make ~name:"small_list len dist"   ~count:5_000 (add_stat len (small_list int))                        (fun _ -> true);
      Test.make ~name:"list_of_size len dist" ~count:5_000 (add_stat len (list_of_size (Gen.int_range 5 10) int)) (fun _ -> true);
      Test.make ~name:"list_repeat len dist"  ~count:5_000 (add_stat len (make Gen.(list_repeat 42 int)))         (fun _ -> true);
    ]

  let array_len_tests =
    let len = ("len",Array.length) in
    [
      Test.make ~name:"array len dist"         ~count:5_000 (add_stat len (array int))                              (fun _ -> true);
      Test.make ~name:"small_array len dist"   ~count:5_000 (add_stat len (make Gen.(small_array int)))             (fun _ -> true);
      Test.make ~name:"array_of_size len dist" ~count:5_000 (add_stat len (array_of_size (Gen.int_range 5 10) int)) (fun _ -> true);
      Test.make ~name:"array_repeat len dist"  ~count:5_000 (add_stat len (make Gen.(array_repeat 42 int)))         (fun _ -> true);
    ]

  let int_dist_tests =
    let dist = ("dist",fun x -> x) in
    [ (* test from issue #40 *)
      Test.make ~name:"int_stats_neg"                  ~count:5000   (add_stat dist small_signed_int)                 (fun _ -> true);
      (* distribution tests from PR #45 *)
      Test.make ~name:"small_signed_int dist"          ~count:1000   (add_stat dist small_signed_int)                 (fun _ -> true);
      Test.make ~name:"small_nat dist"                 ~count:1000   (add_stat dist small_nat)                        (fun _ -> true);
      Test.make ~name:"int_range (-43643) 435434 dist" ~count:1000   (add_stat dist (int_range (-43643) 435434))      (fun _ -> true);
      Test.make ~name:"int_range (-40000) 40000 dist"  ~count:1000   (add_stat dist (int_range (-40000) 40000))       (fun _ -> true);
      Test.make ~name:"int_range (-4) 4 dist"          ~count:1000   (add_stat dist (int_range (-4) 4))               (fun _ -> true);
      Test.make ~name:"int_range (-4) 17 dist"         ~count:1000   (add_stat dist (int_range (-4) 17))              (fun _ -> true);
      Test.make ~name:"int dist"                       ~count:100000 (add_stat dist int)                              (fun _ -> true);
      Test.make ~name:"oneof int dist"                 ~count:1000   (add_stat dist (oneofl[min_int;-1;0;1;max_int])) (fun _ -> true);
    ]

  let int_dist_empty_bucket =
    Test.make ~name:"int_dist_empty_bucket" ~count:1_000
      (add_stat ("dist",fun x -> x) (oneof [small_int_corners ();int])) (fun _ -> true)
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
    Generator.char_test;
    Generator.string_test;
    Generator.list_test;
    Generator.list_repeat_test;
    Generator.array_repeat_test;
    (*Shrink.test_fac_issue59;*)
    Shrink.big_bound_issue59;
    Shrink.long_shrink;
    Shrink.ints_arent_0_mod_3;
    Shrink.ints_are_0;
    Shrink.ints_smaller_209609;
    Shrink.char_is_never_abcdef;
    Shrink.string_never_has_000_char;
    Shrink.string_never_has_255_char;
    Shrink.lists_are_empty_issue_64;
    Shrink.list_shorter_10;
    Shrink.list_shorter_432;
    Shrink.list_shorter_4332;
    Shrink.list_equal_dupl;
    Function.fail_pred_map_commute;
    Function.fail_pred_strings;
    Function.prop_foldleft_foldright;
    Function.prop_foldleft_foldright_uncurry;
    Function.prop_foldleft_foldright_uncurry_funlast;
    Function.fold_left_test;
    FindExample.find_ex;
    FindExample.find_ex_uncaught_issue_99_1_fail;
    FindExample.find_ex_uncaught_issue_99_2_succeed;
    Stats.bool_dist;
    Stats.char_dist]
    @ Stats.string_len_tests
    @ Stats.list_len_tests
    @ Stats.array_len_tests
    @ Stats.int_dist_tests)

let () = QCheck_base_runner.set_seed 153870556
let _  = QCheck_base_runner.run_tests ~colors:false [Stats.int_dist_empty_bucket]

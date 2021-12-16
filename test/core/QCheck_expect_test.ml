(** QCheck(1) tests **)

(** Module representing a tree data structure, used in tests *)
module IntTree = struct
  type tree = Leaf of int | Node of tree * tree

  let leaf x = Leaf x
  let node x y = Node (x,y)

  let rec depth = function
    | Leaf _ -> 1
    | Node (x, y) -> 1 + max (depth x) (depth y)

  let rec print_tree = function
    | Leaf x -> Printf.sprintf "Leaf %d" x
    | Node (x, y) -> Printf.sprintf "Node (%s, %s)" (print_tree x) (print_tree y)

  let gen_tree = QCheck.Gen.(sized @@ fix
                        (fun self n -> match n with
                           | 0 -> map leaf nat
                           | n ->
                             frequency
                               [1, map leaf nat;
                                2, map2 node (self (n/2)) (self (n/2))]
                        ))

  let rec rev_tree = function
    | Node (x, y) -> Node (rev_tree y, rev_tree x)
    | Leaf x -> Leaf x

  let passing_tree_rev =
    QCheck.Test.make ~count:1000
      ~name:"tree_rev_is_involutive"
      QCheck.(make gen_tree)
      (fun tree -> rev_tree (rev_tree tree) = tree)
end

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

  let retries =
    Test.make ~name:"with shrinking retries" ~retries:10
      small_nat (fun i -> Printf.printf "%i %!" i; i mod 3 <> 1)

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

  let tests = [
    passing;
    failing;
    error;
    collect;
    stats;
    retries;
    bad_assume_warn;
    bad_assume_fail;
  ]
end

(* positive tests of the various generators

   Note: it is important to disable shrinking for these tests, as the
   shrinkers will suggest inputs that are coming from the generator
   themselves -- which we want to test -- so their reduced
   counter-example are confusing rather than helpful.

   This is achieved by using (Test.make ~print ...), without a ~shrink
   argument.
*)
module Generator = struct
  open QCheck

  (* example from issue #23 *)
  let char_dist_issue_23 =
    Test.make ~name:"char never produces '\\255'" ~count:1_000_000 char (fun c -> c <> '\255')

  let char_test =
    Test.make ~name:"char has right range'" ~count:1000
      char (fun c -> '\000' <= c && c <= '\255')

  let nat_test =
    Test.make ~name:"nat has right range" ~count:1000
      (make ~print:Print.int Gen.nat) (fun n -> 0 <= n && n < 10000)

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

  let passing_tree_rev =
    QCheck.Test.make ~count:1000
      ~name:"tree_rev_is_involutive"
      QCheck.(make IntTree.gen_tree)
      (fun tree -> IntTree.(rev_tree (rev_tree tree)) = tree)

  let nat_split2_spec =
    Test.make ~name:"nat_split2 spec"
      (make
        ~print:Print.(pair int (pair int int))
         Gen.(small_nat >>= fun n ->
              pair (return n) (nat_split2 n)))
      (fun (n, (a, b)) ->
         0 <= a && 0 <= b && a + b = n)

  let pos_split2_spec =
    Test.make ~name:"pos_split2 spec"
      (make
        ~print:Print.(pair int (pair int int))
         Gen.(small_nat >>= fun n ->
              (* we need n > 2 *)
              let n = n + 2 in
              pair (return n) (pos_split2 n)))
      (fun (n, (a, b)) ->
         (0 < a && 0 < b && a + b = n))

  let range_subset_spec =
    Test.make ~name:"range_subset_spec"
      (make
         ~print:Print.(quad int int int (array int))
         Gen.(pair small_nat small_nat >>= fun (m, n) ->
              (* we must guarantee [low <= high]
                 and [size <= high - low + 1] *)
              let low = m and high = m + n in
              int_range 0 (high - low + 1) >>= fun size ->
              quad (return size) (return low) (return high)
                (range_subset ~size low high)))
      (fun (size, low, high, arr) ->
         if size = 0 then arr = [||]
         else
           Array.length arr = size
           && low <= arr.(0)
           && Array.for_all (fun (a, b) -> a < b)
               (Array.init (size - 1) (fun k -> arr.(k), arr.(k+1)))
           && arr.(size - 1) <= high)

  let nat_split_n_way =
    Test.make ~name:"nat_split n-way"
      (make
         ~print:Print.(pair int (array int))
         Gen.(small_nat >>= fun n ->
              pair (return n) (nat_split ~size:n n)))
      (fun (n, arr) ->
         Array.length arr = n
         && Array.for_all (fun k -> 0 <= k) arr
         && Array.fold_left (+) 0 arr = n)

  let nat_split_smaller =
    Test.make ~name:"nat_split smaller"
      (make
         ~print:Print.(triple int int (array int))
         Gen.(small_nat >>= fun size ->
              int_bound size >>= fun n ->
              triple (return size) (return n) (nat_split ~size n)))
      (fun (m, n, arr) ->
         Array.length arr = m
         && Array.for_all (fun k -> 0 <= k) arr
         && Array.fold_left (+) 0 arr = n)

  let pos_split =
    Test.make ~name:"pos_split"
      (make
         ~print:Print.(triple int int (array int))
         Gen.(pair small_nat small_nat >>= fun (m, n) ->
              (* we need both size>0 and n>0 and size <= n *)
              let size = 1 + min m n and n = 1 + max m n in
              triple (return size) (return n) (pos_split ~size n)))
      (fun (m, n, arr) ->
         Array.length arr = m
         && Array.for_all (fun k -> 0 < k) arr
         && Array.fold_left (+) 0 arr = n)

  let test_tup2 =
    Test.make ~count:10
      ~name:"forall x in (0, 1): x = (0, 1)"
      (tup2 (always 0) (always 1))
      (fun x -> x = (0, 1))

  let test_tup3 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2): x = (0, 1, 2)"
      (tup3 (always 0) (always 1) (always 2))
      (fun x -> x = (0, 1, 2))

  let test_tup4 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3): x = (0, 1, 2, 3)"
      (tup4 (always 0) (always 1) (always 2) (always 3))
      (fun x -> x = (0, 1, 2, 3))

  let test_tup5 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4): x = (0, 1, 2, 3, 4)"
      (tup5 (always 0) (always 1) (always 2) (always 3) (always 4))
      (fun x -> x = (0, 1, 2, 3, 4))

  let test_tup6 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5): x = (0, 1, 2, 3, 4, 5)"
      (tup6 (always 0) (always 1) (always 2) (always 3) (always 4) (always 5))
      (fun x -> x = (0, 1, 2, 3, 4, 5))

  let test_tup7 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6): x = (0, 1, 2, 3, 4, 5, 6)"
      (tup7
         (always 0) (always 1) (always 2) (always 3) (always 4)
         (always 5) (always 6))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6))

  let test_tup8 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6, 7): x = (0, 1, 2, 3, 4, 5, 6, 7)"
      (tup8
         (always 0) (always 1) (always 2) (always 3) (always 4)
         (always 5) (always 6) (always 7))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6, 7))

  let test_tup9 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6, 7, 8): x = (0, 1, 2, 3, 4, 5, 6, 7, 8)"
      (tup9
         (always 0) (always 1) (always 2) (always 3) (always 4)
         (always 5) (always 6) (always 7) (always 8))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6, 7, 8))

  let tests = [
    char_dist_issue_23;
    char_test;
    nat_test;
    string_test;
    list_test;
    list_repeat_test;
    array_repeat_test;
    passing_tree_rev;
    nat_split2_spec;
    pos_split2_spec;
    range_subset_spec;
    nat_split_n_way;
    nat_split_smaller;
    pos_split;
    test_tup2;
    test_tup3;
    test_tup4;
    test_tup5;
    test_tup6;
    test_tup7;
    test_tup8;
    test_tup9;
  ]
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

  let nats_smaller_5001 =
    Test.make ~name:"nat < 5001" ~count:1000
      (make ~print:Print.int ~shrink:Shrink.int Gen.nat) (fun n -> n < 5001)

  let char_is_never_abcdef =
    Test.make ~name:"char is never produces 'abcdef'" ~count:1000
      char (fun c -> not (List.mem c ['a';'b';'c';'d';'e';'f']))

  let strings_are_empty =
    Test.make ~name:"strings are empty" ~count:1000
      string (fun s -> s = "")

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
      (list small_int) (fun xs -> List.length xs < 10)

  let length_printer xs =
    Printf.sprintf "[...] list length: %i" (List.length xs)

  let size_gen = Gen.(oneof [small_nat; int_bound 750_000])

  let list_shorter_432 =
    Test.make ~name:"lists shorter than 432"
      (set_print length_printer (list_of_size size_gen small_int))
      (fun xs -> List.length xs < 432)

  let list_shorter_4332 =
    Test.make ~name:"lists shorter than 4332"
      (set_shrink Shrink.list_spine (set_print length_printer (list_of_size size_gen small_int)))
      (fun xs -> List.length xs < 4332)

  let list_equal_dupl =
    Test.make ~name:"lists equal to duplication"
      (list_of_size size_gen small_int)
      (fun xs -> try xs = xs @ xs
                 with Stack_overflow -> false)

  let list_unique_elems =
    Test.make ~name:"lists have unique elems"
      (list small_int)
      (fun xs -> let ys = List.sort_uniq Int.compare xs in
                 print_list xs; List.length xs = List.length ys)

  let test_tup2 =
    Test.make
      ~name:"forall (a, b) in nat: a < b"
      (tup2 small_int small_int)
      (fun (a, b) -> a < b)

  let test_tup3 =
    Test.make
      ~name:"forall (a, b, c) in nat: a < b < c"
      (tup3 small_int small_int small_int)
      (fun (a, b, c) -> a < b && b < c)

  let test_tup4 =
    Test.make
      ~name:"forall (a, b, c, d) in nat: a < b < c < d"
      (tup4 small_int small_int small_int small_int)
      (fun (a, b, c, d) -> a < b && b < c && c < d)

  let test_tup5 =
    Test.make
      ~name:"forall (a, b, c, d, e) in nat: a < b < c < d < e"
      (tup5 small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e) -> a < b && b < c && c < d && d < e)

  let test_tup6 =
    Test.make
      ~name:"forall (a, b, c, d, e, f) in nat: a < b < c < d < e < f"
      (tup6 small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f) -> a < b && b < c && c < d && d < e && e < f)

  let test_tup7 =
    Test.make
      ~name:"forall (a, b, c, d, e, f, g) in nat: a < b < c < d < e < f < g"
      (tup7 small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g) -> a < b && b < c && c < d && d < e && e < f && f < g)

  let test_tup8 =
    Test.make
      ~name:"forall (a, b, c, d, e, f, g, h) in nat: a < b < c < d < e < f < g < h"
      (tup8 small_int small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g, h) -> a < b && b < c && c < d && d < e && e < f && f < g && g < h)

  let test_tup9 =
    Test.make
      ~name:"forall (a, b, c, d, e, f, g, h, i) in nat: a < b < c < d < e < f < g < h < i"
      (tup9 small_int small_int small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g, h, i) -> a < b && b < c && c < d && d < e && e < f && f < g && g < h && h < i)

  let tests = [
    (*test_fac_issue59;*)
    big_bound_issue59;
    long_shrink;
    ints_arent_0_mod_3;
    ints_are_0;
    ints_smaller_209609;
    nats_smaller_5001;
    char_is_never_abcdef;
    strings_are_empty;
    string_never_has_000_char;
    string_never_has_255_char;
    lists_are_empty_issue_64;
    list_shorter_10;
    list_shorter_432;
    list_shorter_4332;
    list_equal_dupl;
    list_unique_elems;
    test_tup2;
    test_tup3;
    test_tup4;
    test_tup5;
    test_tup6;
    test_tup7;
    test_tup8;
    test_tup9;
  ]
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

  let tests = [
    fail_pred_map_commute;
    fail_pred_strings;
    prop_foldleft_foldright;
    prop_foldleft_foldright_uncurry;
    prop_foldleft_foldright_uncurry_funlast;
    fold_left_test;
  ]
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

  let tests = [
    find_ex;
    find_ex_uncaught_issue_99_1_fail;
    find_ex_uncaught_issue_99_2_succeed;
  ]
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
      Test.make ~name:"nat dist"                       ~count:1000   (add_stat dist (make Gen.nat))                   (fun _ -> true);
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

  let tree_depth_test =
    let depth = ("depth", IntTree.depth) in
    Test.make ~name:"tree's depth" ~count:1000 (add_stat depth (make IntTree.gen_tree)) (fun _ -> true)

  let range_subset_test =
    Test.make ~name:"range_subset_spec" ~count:5_000
      (add_stat ("dist", fun a -> a.(0)) (make (Gen.range_subset ~size:1 0 20)))
      (fun a -> Array.length a = 1)

  let tests =
    [
      bool_dist;
      char_dist;
      tree_depth_test;
      range_subset_test
    ]
    @ string_len_tests
    @ list_len_tests
    @ array_len_tests
    @ int_dist_tests
end

(* Calling runners *)

let () = QCheck_base_runner.set_seed 1234
let _ =
  QCheck_base_runner.run_tests ~colors:false (
    Overall.tests @
    Generator.tests @
    Shrink.tests @
    Function.tests @
    FindExample.tests @
    Stats.tests)

let () = QCheck_base_runner.set_seed 153870556
let _  = QCheck_base_runner.run_tests ~colors:false [Stats.int_dist_empty_bucket]

(** QCheck2 tests **)

(* Please add any additional tests to both [QCheck_tests.ml] and [QCheck2_tests.ml].
   This ensures that both generator approaches continue to work as expected
   and furthermore allows us to compare their behaviour with
   [diff -y test/core/QCheck_expect_test.expected test/core/QCheck2_expect_test.expected] *)

(** Module representing a integer tree data structure, used in tests *)
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

  let gen_tree = QCheck2.Gen.(sized @@ fix
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

  let rec contains_only_n tree n = match tree with
    | Leaf n' -> n = n'
    | Node (x, y) -> contains_only_n x n && contains_only_n y n
end

(* tests of overall functionality *)
module Overall = struct
  open QCheck2

  let passing =
    Test.make ~name:"list_rev_is_involutive" ~count:100 ~long_factor:100
      ~print:Print.(list int)
      Gen.(list small_int) (fun l -> List.rev (List.rev l) = l)

  let failing =
    Test.make ~name:"should_fail_sort_id" ~count:10 ~print:Print.(list int)
      Gen.(small_list small_int) (fun l -> l = List.sort compare l)

  exception Error

  let error =
    Test.make ~name:"should_error_raise_exn" ~count:10 ~print:Print.int
      Gen.int (fun _ -> raise Error)

  let collect =
    Test.make ~name:"collect_results" ~count:100 ~long_factor:100
      ~print:Print.int ~collect:string_of_int
      (Gen.int_bound 4) (fun _ -> true)

  let stats =
    Test.make ~name:"with_stats" ~count:100 ~long_factor:100 ~print:Print.int
      ~stats:[
        "mod4", (fun i->i mod 4);
        "num", (fun i->i);
      ]
      (Gen.int_bound 120) (fun _ -> true)

  let retries =
    Test.make ~name:"with shrinking retries" ~retries:10 ~print:Print.int
      Gen.small_nat (fun i -> Printf.printf "%i %!" i; i mod 3 <> 1)

  let bad_assume_warn =
    Test.make ~name:"WARN_unlikely_precond" ~count:2_000 ~print:Print.int
      Gen.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)

  let bad_assume_fail =
    Test.make ~name:"FAIL_unlikely_precond" ~count:2_000
      ~if_assumptions_fail:(`Fatal, 0.1) ~print:Print.int
      Gen.int
      (fun x ->
         QCheck.assume (x mod 100 = 1);
         true)

  let bad_gen_fail =
    Test.make ~name:"FAIL_bad_gen"
      Gen.(int >>= fun j -> int_bound j >>= fun i -> return (i,j))
      (fun (_i,_j) -> true) (* i may be negative, causing int_bound to fail *)

  let bad_shrinker_fail =
    Test.make ~name:"FAIL_bad_shrinker"
      (Gen.make_primitive
        ~shrink:(fun _i -> raise Error)
        ~gen:(fun rs -> Random.State.int rs))
      (fun _i -> false)

  let neg_test_fail_as_expected =
    Test.make_neg ~name:"all ints are even" ~print:Print.int Gen.small_int (fun i -> i mod 2 = 0)

  let neg_test_unexpected_success =
    Test.make_neg ~name:"int double" ~print:Print.int Gen.small_int (fun i -> i + i = i * 2)

  let neg_test_fail_with_shrinking =
    Test.make_neg ~name:"list rev concat" ~print:Print.(pair (list int) (list int))
      Gen.(pair (list small_int) (list small_int)) (fun (is,js) -> (List.rev is)@(List.rev js) = List.rev (is@js))

  let pos_test_fails_with_error =
    Test.make ~name:"pos fail with error" ~print:Print.int Gen.small_int (fun _i -> raise Error)

  let neg_test_fail_with_error =
    Test.make_neg ~name:"neg fail with error" ~print:Print.int Gen.small_int (fun _i -> raise Error)

  (* [apply_n f x n] computes f(f(...f(x))) with n applications of f *)
  let rec apply_n f x n =
    if n=0
    then x
    else apply_n f (f x) (pred n)

  (* test from #236 *)
  let bad_fun_repro =
    let sleep_time = 0.175 in
    let count = ref 0 in
    Test.make ~count:10 ~name:"bad function reproducability"
      Gen.(triple small_int (fun1 Observable.int small_int) small_int)
      (fun (i,f,j) ->
         incr count;
         Printf.printf "(%i,fun,%i)%s%!" i j (if !count mod 10 = 0 then "\n" else " ");
         Unix.sleepf sleep_time;
         if 1 = Float.to_int (Unix.time ()) mod 2
         then
           (ignore(apply_n (Fn.apply f) i j > 0); true)
         else
           (ignore(apply_n (Fn.apply f) i i > 0); true))

  let tests = [
    passing;
    failing;
    error;
    collect;
    stats;
    retries;
    bad_assume_warn;
    bad_assume_fail;
    bad_gen_fail;
    (*bad_shrinker_fail;*)
    neg_test_fail_as_expected;
    neg_test_unexpected_success;
    neg_test_fail_with_shrinking;
    pos_test_fails_with_error;
    neg_test_fail_with_error;
    (* we repeat the following multiple times to check the expected output for duplicate lines *)
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
    bad_fun_repro;
  ]
end

(* positive tests of the various generators *)
module Generator = struct
  open QCheck2

  (* example from issue #23 *)
  let char_dist_issue_23 =
    Test.make ~name:"char never produces '\\255'" ~count:1_000_000
      ~print:Print.char
      Gen.char (fun c -> c <> '\255')

  let char_test =
    Test.make ~name:"char has right range'" ~count:1000 ~print:Print.char
      Gen.char (fun c -> '\000' <= c && c <= '\255')

  let printable_test =
    Test.make ~name:"printable has right range" ~count:1000 ~print:Print.char
      Gen.printable (fun c -> c = '\n' || 32 <= Char.code c && Char.code c <= 126)

  let numeral_test =
    Test.make ~name:"numeral has right range" ~count:1000 ~print:Print.char
      Gen.numeral (fun c -> '0' <= c && c <= '9')

  let nat_test =
    Test.make ~name:"nat has right range" ~count:1000 ~print:Print.int
      Gen.nat (fun n -> 0 <= n && n < 10000)

  let bytes_test =
    Test.make ~name:"bytes has right length and content" ~count:1000 ~print:Print.bytes
      Gen.bytes
      (fun s ->
         let len = Bytes.length s in
         0 <= len && len < 10000
         && Bytes.to_seq s |>
            Seq.fold_left (fun acc c -> acc && '\000' <= c && c <= '\255') true)

  let string_test =
    Test.make ~name:"string has right length and content" ~count:1000 ~print:Print.string
      Gen.string
      (fun s ->
         let len = String.length s in
         0 <= len && len < 10000
         && String.to_seq s |>
            Seq.fold_left (fun acc c -> acc && '\000' <= c && c <= '\255') true)

  let pair_test =
    Test.make ~name:"int pairs - commute over +" ~count:1000 ~print:Print.(pair int int)
      Gen.(pair small_nat small_nat) (fun (i,j) -> i+j = j+i)

  let triple_test =
    Test.make ~name:"int triples - associative over +" ~count:1000
      ~print:Print.(triple int int int)
      Gen.(triple small_nat small_nat small_nat) (fun (i,j,k) -> i+(j+k) = (i+j)+k)

  let quad_test =
    Test.make ~name:"int quadruples - product of sums" ~count:1000
      ~print:Print.(quad int int int int)
      Gen.(quad small_nat small_nat small_nat small_nat)
      (fun (h,i,j,k) -> (h+i)*(j+k) = h*j + h*k + i*j + i*k)

  let test_tup2 =
    Test.make ~count:10
      ~name:"forall x in (0, 1): x = (0, 1)"
      Gen.(tup2 (pure 0) (pure 1))
      (fun x -> x = (0, 1))

  let test_tup3 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2): x = (0, 1, 2)"
      Gen.(tup3 (pure 0) (pure 1) (pure 2))
      (fun x -> x = (0, 1, 2))

  let test_tup4 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3): x = (0, 1, 2, 3)"
      Gen.(tup4 (pure 0) (pure 1) (pure 2) (pure 3))
      (fun x -> x = (0, 1, 2, 3))

  let test_tup5 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4): x = (0, 1, 2, 3, 4)"
      Gen.(tup5 (pure 0) (pure 1) (pure 2) (pure 3) (pure 4))
      (fun x -> x = (0, 1, 2, 3, 4))

  let test_tup6 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5): x = (0, 1, 2, 3, 4, 5)"
      Gen.(tup6 (pure 0) (pure 1) (pure 2) (pure 3) (pure 4) (pure 5))
      (fun x -> x = (0, 1, 2, 3, 4, 5))

  let test_tup7 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6): x = (0, 1, 2, 3, 4, 5, 6)"
      Gen.(tup7
         (pure 0) (pure 1) (pure 2) (pure 3) (pure 4)
         (pure 5) (pure 6))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6))

  let test_tup8 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6, 7): x = (0, 1, 2, 3, 4, 5, 6, 7)"
      Gen.(tup8
         (pure 0) (pure 1) (pure 2) (pure 3) (pure 4)
         (pure 5) (pure 6) (pure 7))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6, 7))

  let test_tup9 =
    Test.make ~count:10
      ~name:"forall x in (0, 1, 2, 3, 4, 5, 6, 7, 8): x = (0, 1, 2, 3, 4, 5, 6, 7, 8)"
      Gen.(tup9
         (pure 0) (pure 1) (pure 2) (pure 3) (pure 4)
         (pure 5) (pure 6) (pure 7) (pure 8))
      (fun x -> x = (0, 1, 2, 3, 4, 5, 6, 7, 8))

  let bind_test =
    Test.make ~name:"bind test for ordered pairs" ~count:1000 ~print:Print.(pair int int)
      Gen.(small_nat >>= fun j -> int_bound j >>= fun i -> return (i,j))
      (fun (i,j) -> i<=j)

  let bind_pair_list_length =
    Test.make ~name:"bind list length" ~count:1000 ~print:Print.(pair int (list int))
      Gen.(int_bound 1000 >>= fun len ->
           list_size (return len) (int_bound 10) >>= fun xs -> return (len,xs))
      (fun (len,xs) -> len = List.length xs)

  let list_test =
    Test.make ~name:"list has right length" ~count:1000
      ~print:Print.(list unit)
      Gen.(list unit) (fun l -> let len = List.length l in 0 <= len && len < 10_000)

  let list_repeat_test =
    Test.make ~name:"list_repeat has constant length" ~count:1000
      ~print:Print.(pair int (list unit))
      Gen.(small_nat >>= fun i -> list_repeat i unit >>= fun l -> return (i,l))
      (fun (i,l) -> List.length l = i)

  let array_repeat_test =
    Test.make ~name:"array_repeat has constant length" ~count:1000
      ~print:Print.(pair int (array unit))
      Gen.(small_nat >>= fun i -> array_repeat i unit >>= fun l -> return (i,l))
      (fun (i,l) -> Array.length l = i)

  let passing_tree_rev =
    Test.make ~name:"tree_rev_is_involutive" ~count:1000
      IntTree.gen_tree
      (fun tree -> IntTree.(rev_tree (rev_tree tree)) = tree)

  let tests = [
    char_dist_issue_23;
    char_test;
    nat_test;
    bytes_test;
    string_test;
    pair_test;
    triple_test;
    quad_test;
    test_tup2;
    test_tup3;
    test_tup4;
    test_tup5;
    test_tup6;
    test_tup7;
    test_tup8;
    test_tup9;
    bind_test;
    bind_pair_list_length;
    list_test;
    list_repeat_test;
    array_repeat_test;
    passing_tree_rev;
  ]
end

(* negative tests that exercise shrinking behaviour *)
module Shrink = struct
  open QCheck2

  let rec fac n = match n with
    | 0 -> 1
    | n -> n * fac (n - 1)

  (* example from issue #59 *)
  let test_fac_issue59 =
    Test.make ~name:"test fac issue59"
      (Gen.make_primitive ~gen:(fun st -> Gen.generate1 ~rand:st (Gen.small_int_corners ())) ~shrink:(fun _ -> Seq.empty))
      (fun n -> try (fac n) mod n = 0
                with
                (*| Stack_overflow   -> false*)
                | Division_by_zero -> (n=0))

  let big_bound_issue59 =
    Test.make ~name:"big bound issue59" ~print:Print.int
      (Gen.small_int_corners()) (fun i -> i < 209609)

  let long_shrink =
    let listgen = Gen.(list_size (int_range 1000 10000) int) in
    Test.make ~name:"long_shrink" ~print:Print.(pair (list int) (list int))
      (Gen.pair listgen listgen)
      (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

  (* test from issue #36 *)
  let ints_arent_0_mod_3 =
    Test.make ~name:"ints arent 0 mod 3" ~count:1000 ~print:Print.int
      Gen.int (fun i -> i mod 3 <> 0)

  let ints_are_0 =
    Test.make ~name:"ints are 0" ~count:1000 ~print:Print.int
      Gen.int (fun i -> Printf.printf "%i\n" i; i = 0)

  (* test from issue #59 *)
  let ints_smaller_209609 =
    Test.make ~name:"ints < 209609" ~print:Print.int
      (Gen.small_int_corners()) (fun i -> i < 209609)

  let nats_smaller_5001 =
    Test.make ~name:"nat < 5001" ~count:1000 ~print:Print.int
      Gen.nat (fun n -> n < 5001)

  let char_is_never_abcdef =
    Test.make ~name:"char never produces 'abcdef'" ~count:1000 ~print:Print.char
      Gen.char (fun c -> not (List.mem c ['a';'b';'c';'d';'e';'f']))

  let printable_is_never_sign = (* should shrink towards '!' with lowest ascii code 33 *)
    Test.make ~name:"printable never produces '!\"#$%&'" ~count:1000 ~print:Print.char
      Gen.printable (fun c -> not (List.mem c ['!';'"';'#';'$';'%';'&']))

  let numeral_is_never_less_5 =
    Test.make ~name:"printable never produces less than '5" ~count:1000 ~print:Print.char
      Gen.numeral (fun c -> c >= '5')

  let bytes_are_empty =
    Test.make ~name:"bytes are empty" ~count:1000 ~print:Print.bytes
      Gen.bytes (fun s -> s = Bytes.empty)

  let bytes_never_has_000_char =
    Test.make ~name:"bytes never has a \\000 char" ~count:1000 ~print:Print.bytes
      Gen.bytes
      (fun s -> Bytes.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\000') true)

  let bytes_never_has_255_char =
    Test.make ~name:"bytes never has a \\255 char" ~count:1000 ~print:Print.bytes
      Gen.bytes
      (fun s -> Bytes.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\255') true)

  let bytes_unique_chars =
    Test.make ~name:"bytes have unique chars" ~count:1000 ~print:Print.bytes
      Gen.bytes
      (fun s ->
         let ch_list = Bytes.to_seq s |> List.of_seq in
         List.length ch_list = List.length (List.sort_uniq Char.compare ch_list))

  let strings_are_empty =
    Test.make ~name:"strings are empty" ~count:1000 ~print:Print.string
      Gen.string (fun s -> s = "")

  let string_never_has_000_char =
    Test.make ~name:"string never has a \\000 char" ~count:1000 ~print:Print.string
      Gen.string
      (fun s -> String.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\000') true)

  let string_never_has_255_char =
    Test.make ~name:"string never has a \\255 char" ~count:1000 ~print:Print.string
      Gen.string
      (fun s -> String.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\255') true)

  let string_unique_chars =
    Test.make ~name:"strings have unique chars" ~count:1000 ~print:Print.string
      Gen.string
      (fun s ->
         let ch_list = String.to_seq s |> List.of_seq in
         List.length ch_list = List.length (List.sort_uniq Char.compare ch_list))

  (* test from issue #167 *)
  let pair_diff_issue_64 =
    Test.make ~name:"pairs have different components" ~print:Print.(pair int int)
      Gen.(pair small_int small_int) (fun (i,j) -> i<>j)

  let pair_same =
    Test.make ~name:"pairs have same components" ~print:Print.(pair int int)
      Gen.(pair int int) (fun (i,j) -> i=j)

  let pair_one_zero =
    Test.make ~name:"pairs have a zero component" ~print:Print.(pair int int)
      Gen.(pair int int) (fun (i,j) -> i=0 || j=0)

  let pair_all_zero =
    Test.make ~name:"pairs are (0,0)" ~print:Print.(pair int int)
      Gen.(pair int int) (fun (i,j) -> i=0 && j=0)

  let pair_ordered =
    Test.make ~name:"pairs are ordered" ~print:Print.(pair int int)
      Gen.(pair (pint ~origin:0) (pint ~origin:0)) (fun (i,j) -> i<=j)

  let pair_ordered_rev =
    Test.make ~name:"pairs are ordered reversely" ~print:Print.(pair int int)
      Gen.(pair (pint ~origin:0) (pint ~origin:0)) (fun (i,j) -> i>=j)

  let pair_sum_lt_128 =
    Test.make ~name:"pairs sum to less than 128" ~print:Print.(pair int int)
      Gen.(pair (pint ~origin:0) (pint ~origin:0)) (fun (i,j) -> i+j<128)

  let pair_lists_rev_concat =
    Test.make ~name:"pairs lists rev concat" ~print:Print.(pair (list int) (list int))
      Gen.(pair (list (pint ~origin:0)) (list (pint ~origin:0)))
      (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

  let pair_lists_no_overlap =
    Test.make ~name:"pairs lists no overlap" ~print:Print.(pair (list int) (list int))
      Gen.(pair (list small_nat) (list small_nat))
      (fun (xs,ys) -> List.for_all (fun x -> not (List.mem x ys)) xs)

  let triple_diff =
    Test.make ~name:"triples have pair-wise different components" ~print:Print.(triple int int int)
      Gen.(triple small_int small_int small_int) (fun (i,j,k) -> i<>j && j<>k)

  let triple_same =
    Test.make ~name:"triples have same components" ~print:Print.(triple int int int)
      Gen.(triple int int int) (fun (i,j,k) -> i=j || j=k)

  let triple_ordered =
    Test.make ~name:"triples are ordered" ~print:Print.(triple int int int)
      Gen.(triple int int int) (fun (i,j,k) -> i<=j && j<=k)

  let triple_ordered_rev =
    Test.make ~name:"triples are ordered reversely" ~print:Print.(triple int int int)
      Gen.(triple int int int) (fun (i,j,k) -> i>=j && j>=k)

  let quad_diff =
    Test.make ~name:"quadruples have pair-wise different components" ~print:Print.(quad int int int int)
      Gen.(quad small_int small_int small_int small_int) (fun (h,i,j,k) -> h<>i && i<>j && j<>k)

  let quad_same =
    Test.make ~name:"quadruples have same components" ~print:Print.(quad int int int int)
      Gen.(quad int int int int) (fun (h,i,j,k) -> h=i || i=j || j=k)

  let quad_ordered =
    Test.make ~name:"quadruples are ordered" ~print:Print.(quad int int int int)
      Gen.(quad int int int int) (fun (h,i,j,k) -> h <= i && i <= j && j <= k)

  let quad_ordered_rev =
    Test.make ~name:"quadruples are ordered reversely" ~print:Print.(quad int int int int)
      Gen.(quad int int int int) (fun (h,i,j,k) -> h >= i && i >= j && j >= k)

  let test_tup2 =
    Test.make
      ~print:Print.(tup2 int int)
      ~name:"forall (a, b) in nat: a < b"
      Gen.(tup2 small_int small_int)
      (fun (a, b) -> a < b)

  let test_tup3 =
    Test.make
      ~print:Print.(tup3 int int int)
      ~name:"forall (a, b, c) in nat: a < b < c"
      Gen.(tup3 small_int small_int small_int)
      (fun (a, b, c) -> a < b && b < c)

  let test_tup4 =
    Test.make
      ~print:Print.(tup4 int int int int)
      ~name:"forall (a, b, c, d) in nat: a < b < c < d"
      Gen.(tup4 small_int small_int small_int small_int)
      (fun (a, b, c, d) -> a < b && b < c && c < d)

  let test_tup5 =
    Test.make
      ~print:Print.(tup5 int int int int int)
      ~name:"forall (a, b, c, d, e) in nat: a < b < c < d < e"
      Gen.(tup5 small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e) -> a < b && b < c && c < d && d < e)

  let test_tup6 =
    Test.make
      ~print:Print.(tup6 int int int int int int)
      ~name:"forall (a, b, c, d, e, f) in nat: a < b < c < d < e < f"
      Gen.(tup6 small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f) -> a < b && b < c && c < d && d < e && e < f)

  let test_tup7 =
    Test.make
      ~print:Print.(tup7 int int int int int int int)
      ~name:"forall (a, b, c, d, e, f, g) in nat: a < b < c < d < e < f < g"
      Gen.(tup7 small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g) -> a < b && b < c && c < d && d < e && e < f && f < g)

  let test_tup8 =
    Test.make
      ~print:Print.(tup8 int int int int int int int int)
      ~name:"forall (a, b, c, d, e, f, g, h) in nat: a < b < c < d < e < f < g < h"
      Gen.(tup8 small_int small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g, h) -> a < b && b < c && c < d && d < e && e < f && f < g && g < h)

  let test_tup9 =
    Test.make
      ~print:Print.(tup9 int int int int int int int int int)
      ~name:"forall (a, b, c, d, e, f, g, h, i) in nat: a < b < c < d < e < f < g < h < i"
      Gen.(tup9 small_int small_int small_int small_int small_int small_int small_int small_int small_int)
      (fun (a, b, c, d, e, f, g, h, i) -> a < b && b < c && c < d && d < e && e < f && f < g && g < h && h < i)

  let bind_pair_ordered =
    Test.make ~name:"bind ordered pairs" ~print:Print.(pair int int)
      Gen.(pint ~origin:0 >>= fun j -> int_bound j >>= fun i -> return (i,j))
      (fun (_i,_j) -> false)

  let bind_pair_list_size =
    Test.make ~name:"bind list_size constant" ~print:Print.(pair int (list int))
      Gen.(int_bound 1000 >>= fun len ->
           list_size (return len) (int_bound 1000) >>= fun xs -> return (len,xs))
      (fun (len,xs) -> let len' = List.length xs in len=len' && len' < 4)

  (* tests from issue #64 *)
  let print_list xs = print_endline Print.(list int xs)

  let lists_are_empty_issue_64 =
    Test.make ~name:"lists are empty" ~print:Print.(list int)
      Gen.(list small_int) (fun xs -> print_list xs; xs = [])

  let list_shorter_10 =
    Test.make ~name:"lists shorter than 10" ~print:Print.(list int)
      Gen.(list small_int) (fun xs -> List.length xs < 10)

  let length_printer xs =
    Printf.sprintf "[...] list length: %i" (List.length xs)

  let size_gen = Gen.(oneof [small_nat; int_bound 750_000])

  let list_shorter_432 =
    Test.make ~name:"lists shorter than 432" ~print:length_printer
      Gen.(list_size size_gen small_int)
      (fun xs -> List.length xs < 432)

  let list_shorter_4332 =
    Test.make ~name:"lists shorter than 4332" ~print:length_printer
      Gen.(list_size size_gen small_int)
      (fun xs -> List.length xs < 4332)

  let list_equal_dupl =
    Test.make ~name:"lists equal to duplication" ~print:Print.(list int)
      Gen.(list_size size_gen small_int)
      (fun xs -> try xs = xs @ xs
                 with Stack_overflow -> false)

  let list_unique_elems =
    Test.make ~name:"lists have unique elems" ~print:Print.(list int)
      Gen.(list small_int)
      (fun xs -> let ys = List.sort_uniq Int.compare xs in
                 print_list xs; List.length xs = List.length ys)

  let tree_contains_only_42 =
    Test.make ~name:"tree contains only 42" ~print:IntTree.print_tree
      IntTree.gen_tree
      (fun tree -> IntTree.contains_only_n tree 42)

  let test_gen_no_shrink =
    Test.make ~name:"sum list = 0" ~print:Print.(list int)
      Gen.(no_shrink @@ list small_int)
      (fun xs -> List.fold_left (+) 0 xs = 0)

  let tests = [
    (*test_fac_issue59;*)
    big_bound_issue59;
    long_shrink;
    ints_arent_0_mod_3;
    ints_are_0;
    ints_smaller_209609;
    nats_smaller_5001;
    char_is_never_abcdef;
    printable_is_never_sign;
    numeral_is_never_less_5;
    bytes_are_empty;
    bytes_never_has_000_char;
    bytes_never_has_255_char;
    bytes_unique_chars;
    strings_are_empty;
    string_never_has_000_char;
    string_never_has_255_char;
    string_unique_chars;
    pair_diff_issue_64;
    pair_same;
    pair_one_zero;
    pair_all_zero;
    pair_ordered;
    pair_ordered_rev;
    pair_sum_lt_128;
    pair_lists_rev_concat;
    pair_lists_no_overlap;
    triple_diff;
    triple_same;
    triple_ordered;
    triple_ordered_rev;
    quad_diff;
    quad_same;
    quad_ordered;
    quad_ordered_rev;
    test_tup2;
    test_tup3;
    test_tup4;
    test_tup5;
    test_tup6;
    test_tup7;
    test_tup8;
    test_tup9;
    bind_pair_ordered;
    bind_pair_list_size;
    lists_are_empty_issue_64;
    list_shorter_10;
    list_shorter_432;
    list_shorter_4332;
    (*list_equal_dupl;*)
    list_unique_elems;
    tree_contains_only_42;
    test_gen_no_shrink;
  ]
end

(* tests function generator and shrinker *)
module Function = struct
  open QCheck2

  let fail_pred_map_commute =
    Test.make ~name:"fail_pred_map_commute" ~count:100 ~long_factor:100
      ~print:Print.(triple (list int) Fn.print Fn.print)
      Gen.(triple
             (small_list small_int)
             (fun1 ~print:Print.int Observable.int int)
             (fun1 ~print:Print.bool Observable.int bool))
      (fun (l,Fun (_,f),Fun (_,p)) ->
         List.filter p (List.map f l) = List.map f (List.filter p l))

  let fail_pred_strings =
    Test.make ~name:"fail_pred_strings" ~count:100 ~print:Fn.print
      (fun1 Observable.string ~print:Print.bool Gen.bool)
      (fun (Fun (_,p)) -> not (p "some random string") || p "some other string")

  let int_gen = Gen.small_nat (* int *)

  (* Another example (false) property *)
  let prop_foldleft_foldright =
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
         else Test.fail_reportf "l=%s, fold_left=%s, fold_right=%s@."
             (Print.(list int) xs)
             (Print.int l1)
             (Print.int l2)
      )

  (* Another example (false) property *)
  let prop_foldleft_foldright_uncurry =
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
    Test.make ~name:"fold_left fold_right uncurried fun last" ~count:1000 ~long_factor:20
      ~print:Print.(triple int (list int) Fn.print)
      Gen.(triple
             int_gen
             (list int_gen)
             (fun1 ~print:Print.int Observable.(pair int int) int_gen))
      (fun (z,xs,f) ->
         List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
         List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

  (* test from issue #64 *)
  let fold_left_test =
    Test.make ~name:"fold_left test, fun first" ~print:Print.(quad Fn.print string (list int) (list int))
      Gen.(quad  (* string -> int -> string *)
             (fun2 ~print:Print.string Observable.string Observable.int (small_string ~gen:char))
             (small_string ~gen:char)
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
  open QCheck2

  let find_ex =
    Test.make ~name:"find_example" ~print:Print.int
      Gen.(2--50)
      (fun n ->
         let st = Random.State.make [| 0 |] in
         let f m = n < m && m < 2 * n in
         try
           let m = find_example_gen ~rand:st ~count:100_000 ~f Gen.(0 -- 1000) in
           f m
         with No_example_found _ -> false)

  let find_ex_uncaught_issue_99_1_fail =
    let rs = (find_example ~count:10 ~f:(fun _ -> false) Gen.int) in
    Test.make ~name:"FAIL_#99_1" rs (fun _ -> true)

  let find_ex_uncaught_issue_99_2_succeed =
    Test.make ~name:"should_succeed_#99_2" ~count:10
      Gen.int (fun i -> i <= max_int)

  let tests = [
    find_ex;
    find_ex_uncaught_issue_99_1_fail;
    find_ex_uncaught_issue_99_2_succeed;
  ]
end

(* tests of statistics and histogram display *)
module Stats = struct
  open QCheck2

  let bool_dist =
    Test.make ~name:"bool dist" ~count:500_000 ~collect:Bool.to_string Gen.bool (fun _ -> true)

  let char_dist_tests =
    [
    Test.make ~name:"char code dist"           ~count:500_000 ~stats:[("char code", Char.code)] Gen.char      (fun _ -> true);
    Test.make ~name:"printable char code dist" ~count:500_000 ~stats:[("char code", Char.code)] Gen.printable (fun _ -> true);
    Test.make ~name:"numeral char code dist"   ~count:500_000 ~stats:[("char code", Char.code)] Gen.numeral   (fun _ -> true);
  ]

  let bytes_len_tests =
    let len = ("len",Bytes.length) in
    [
      Test.make ~name:"bytes_size len dist"      ~count:5_000 ~stats:[len] Gen.(bytes_size (int_range 5 10)) (fun _ -> true);
      Test.make ~name:"bytes len dist"           ~count:5_000 ~stats:[len] Gen.bytes                         (fun _ -> true);
      Test.make ~name:"bytes_of len dist"        ~count:5_000 ~stats:[len] Gen.(bytes_of (return 'a'))       (fun _ -> true);
      Test.make ~name:"bytes_printable len dist" ~count:5_000 ~stats:[len] Gen.bytes_printable               (fun _ -> true);
      Test.make ~name:"bytes_small len dist"     ~count:5_000 ~stats:[len] Gen.(bytes_small_of char)         (fun _ -> true);
    ]

  let string_len_tests =
    let len = ("len",String.length) in
    [
      Test.make ~name:"string_size len dist"      ~count:5_000 ~stats:[len] Gen.(string_size (int_range 5 10)) (fun _ -> true);
      Test.make ~name:"string len dist"           ~count:5_000 ~stats:[len] Gen.string                         (fun _ -> true);
      Test.make ~name:"string_of len dist"        ~count:5_000 ~stats:[len] Gen.(string_of (return 'a'))       (fun _ -> true);
      Test.make ~name:"string_printable len dist" ~count:5_000 ~stats:[len] Gen.string_printable               (fun _ -> true);
      Test.make ~name:"small_string len dist"     ~count:5_000 ~stats:[len] Gen.(small_string ~gen:char)(*ugh*)(fun _ -> true);
    ]

  let pair_dist =
    Test.make ~name:"pair dist" ~count:500_000 ~stats:[("pair sum", (fun (i,j) -> i+j))]
      Gen.(pair (int_bound 100) (int_bound 100)) (fun _ -> true)

  let triple_dist =
    Test.make ~name:"triple dist" ~count:500_000 ~stats:[("triple sum", (fun (i,j,k) -> i+j+k))]
      Gen.(triple (int_bound 100) (int_bound 100) (int_bound 100)) (fun _ -> true)

  let quad_dist =
    Test.make ~name:"quad dist" ~count:500_000 ~stats:[("quad sum", (fun (h,i,j,k) -> h+i+j+k))]
      Gen.(quad (int_bound 100) (int_bound 100) (int_bound 100) (int_bound 100)) (fun _ -> true)

  let bind_dist =
    Test.make ~name:"bind dist" ~count:1_000_000
      ~stats:[("ordered pair difference", (fun (i,j) -> j-i));("ordered pair sum", (fun (i,j) -> i+j))]
      Gen.(int_bound 100 >>= fun j -> int_bound j >>= fun i -> return (i,j)) (fun _ -> true)

  let list_len_tests =
    let len = ("len",List.length) in
    [ (* test from issue #30 *)
      Test.make ~name:"list len dist"        ~count:5_000 ~stats:[len] Gen.(list int)                       (fun _ -> true);
      Test.make ~name:"small_list len dist"  ~count:5_000 ~stats:[len] Gen.(small_list int)                 (fun _ -> true);
      Test.make ~name:"list_size len dist"   ~count:5_000 ~stats:[len] Gen.(list_size (int_range 5 10) int) (fun _ -> true);
      Test.make ~name:"list_repeat len dist" ~count:5_000 ~stats:[len] Gen.(list_repeat 42 int)             (fun _ -> true);
    ]

  let array_len_tests =
    let len = ("len",Array.length) in
    [
      Test.make ~name:"array len dist"        ~count:5_000 ~stats:[len] Gen.(array int)                       (fun _ -> true);
      Test.make ~name:"small_array len dist"  ~count:5_000 ~stats:[len] Gen.(small_array int)                 (fun _ -> true);
      Test.make ~name:"array_size len dist"   ~count:5_000 ~stats:[len] Gen.(array_size (int_range 5 10) int) (fun _ -> true);
      Test.make ~name:"array_repeat len dist" ~count:5_000 ~stats:[len] Gen.(array_repeat 42 int)             (fun _ -> true);
    ]

  let int_dist_tests =
    let dist = ("dist",fun x -> x) in
    [
      (* test from issue #40 *)
      Test.make ~name:"int_stats_neg"                  ~count:5000   ~stats:[dist] Gen.small_signed_int                 (fun _ -> true);
      (* distribution tests from PR #45 *)
      Test.make ~name:"small_signed_int dist"          ~count:1000   ~stats:[dist] Gen.small_signed_int                 (fun _ -> true);
      Test.make ~name:"small_nat dist"                 ~count:1000   ~stats:[dist] Gen.small_nat                        (fun _ -> true);
      Test.make ~name:"nat dist"                       ~count:1000   ~stats:[dist] Gen.nat                              (fun _ -> true);
      Test.make ~name:"int_range (-43643) 435434 dist" ~count:1000   ~stats:[dist] (Gen.int_range (-43643) 435434)      (fun _ -> true);
      Test.make ~name:"int_range (-40000) 40000 dist"  ~count:1000   ~stats:[dist] (Gen.int_range (-40000) 40000)       (fun _ -> true);
      Test.make ~name:"int_range (-4) 4 dist"          ~count:1000   ~stats:[dist] (Gen.int_range (-4) 4)               (fun _ -> true);
      Test.make ~name:"int_range (-4) 17 dist"         ~count:1000   ~stats:[dist] (Gen.int_range (-4) 17)              (fun _ -> true);
      Test.make ~name:"int dist"                       ~count:100000 ~stats:[dist] Gen.int                              (fun _ -> true);
      Test.make ~name:"oneof int dist"                 ~count:1000   ~stats:[dist] (Gen.oneofl[min_int;-1;0;1;max_int]) (fun _ -> true);
    ]

  let tree_depth_test =
    let depth = ("depth", IntTree.depth) in
    Test.make ~name:"tree's depth" ~count:1000 ~stats:[depth] IntTree.gen_tree (fun _ -> true)

  let int_dist_empty_bucket =
    Test.make ~name:"int_dist_empty_bucket" ~count:1_000 ~stats:[("dist",fun x -> x)]
      Gen.(oneof [small_int_corners ();int]) (fun _ -> true)

  let tests =
    [ bool_dist; ]
    @ char_dist_tests
    @ [ tree_depth_test;]
    @ bytes_len_tests
    @ string_len_tests
    @ [pair_dist;
       triple_dist;
       quad_dist;
       bind_dist;]
    @ list_len_tests
    @ array_len_tests
    @ int_dist_tests
end

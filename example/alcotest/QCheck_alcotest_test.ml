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

let neg_test_failing_as_expected =
  QCheck.Test.make_neg ~name:"neg test pass (failing as expected)" QCheck.small_int (fun i -> i mod 2 = 0)

let neg_test_unexpected_success =
  QCheck.Test.make_neg ~name:"neg test unexpected success" QCheck.small_int (fun i -> i + i = i * 2)

let neg_test_error =
  QCheck.Test.make_neg ~name:"neg fail with error" QCheck.small_int (fun _i -> raise Error)

let simple_qcheck =
  QCheck.Test.make ~name:"fail_check_err_message"
    ~count: 100
    QCheck.small_int
    (fun _ -> QCheck.Test.fail_reportf "@[<v>this@ will@ always@ fail@]")

type tree = Leaf of int | Node of tree * tree

let leaf x = Leaf x
let node x y = Node (x,y)

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

let debug_shrink =
  QCheck.Test.make ~count:10
    ~name:"debug_shrink"
    (* we use a very constrained test to have a smaller shrinking tree *)
    QCheck.(pair (1 -- 3) (1 -- 3))
    (fun (a, b) -> a = b);;

let () =
  Printexc.record_backtrace true;
  let module A = Alcotest in
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing; failing; error;
        neg_test_failing_as_expected; neg_test_unexpected_success; neg_test_error;
        simple_qcheck; passing_tree_rev ]
  in
  A.run ~show_errors:true "my test" [
    "suite", suite;
    "shrinking", [
      QCheck_alcotest.to_alcotest ~verbose:true ~debug_shrink:(Some stdout) debug_shrink
    ];
  ];

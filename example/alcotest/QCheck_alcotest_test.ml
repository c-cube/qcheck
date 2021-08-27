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

let () =
  Printexc.record_backtrace true;
  let module A = Alcotest in
  let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing; failing; error; simple_qcheck; passing_tree_rev ]
  in
  A.run "my test" [
    "suite", suite
  ]

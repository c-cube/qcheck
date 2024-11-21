open QCheck2
open Helpers

type tree = Leaf | Node of  tree * tree
and name  = { a: tree }
[@@deriving qcheck2]

let rec pp_tree  fmt x =
  let open Format in
  match x with
  | Leaf ->
     fprintf fmt "Leaf"
  | Node (l, r) ->
     fprintf fmt "Node (%a, %a)"
       (pp_tree ) l
       (pp_tree ) r

let eq_tree = Alcotest.of_pp (pp_tree )

let gen_tree_ref =
  let open Gen in
  sized @@ fix (fun self ->
             function
             | 0 -> pure Leaf
             | n ->
                oneof [
                    pure Leaf;
                    map2 (fun  l r -> Node (l,r)) (self (n/2)) (self (n/2));
             ])

let test_tree_ref () =

  test_compare ~msg:"gen tree <=> derivation tree"
    ~eq:(eq_tree )
    (gen_tree_ref) (gen_tree )

let test_leaf =
  Test.make
    ~name:"gen_tree_sized 0 = Node (_, Leaf, Leaf)"
    (gen_tree_sized 0)
    (function
     | Leaf -> true
     | Node (Leaf, Leaf) -> true
     | _ -> false)
  |>
    QCheck_alcotest.to_alcotest


let () = Alcotest.run "Test_Recursive"
           [("Recursive",
             Alcotest.[
                 test_case "test_tree_ref" `Quick test_tree_ref;

             ])]

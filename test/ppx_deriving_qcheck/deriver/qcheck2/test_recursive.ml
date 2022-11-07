open QCheck2
open Helpers

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
[@@deriving qcheck2]

let rec pp_tree pp fmt x =
  let open Format in
  match x with
  | Leaf ->
     fprintf fmt "Leaf"
  | Node (x, l, r) ->
     fprintf fmt "Node (%a, %a, %a)"
       pp x
       (pp_tree pp) l
       (pp_tree pp) r

let eq_tree pp = Alcotest.of_pp (pp_tree pp)

let gen_tree_ref gen =
  let open Gen in
  sized @@ fix (fun self ->
             function
             | 0 -> pure Leaf
             | n ->
                oneof [
                    pure Leaf;
                    map3 (fun x l r -> Node (x,l,r)) gen (self (n/2)) (self (n/2));
             ])

let test_tree_ref () =
  let gen = Gen.int in
  test_compare ~msg:"gen tree <=> derivation tree"
    ~eq:(eq_tree Format.pp_print_int)
    (gen_tree_ref gen) (gen_tree gen)

let test_leaf =
  Test.make
    ~name:"gen_tree_sized 0 = Node (_, Leaf, Leaf)"
    (gen_tree_sized Gen.int 0)
    (function
     | Leaf -> true
     | Node (_, Leaf, Leaf) -> true
     | _ -> false)
  |>
    QCheck_alcotest.to_alcotest

(* A slight error has been found here:
   If the type is named `list` then `'a list` will be derived with the
   QCheck generator `list` instead of the `gen_list_sized`.

   This could lead to a design choice:
   - do we allow overriding primitive types?
   - do we prioritize `Env.curr_types` over primitive types?
*)
type 'a my_list = Cons of 'a * 'a my_list | Nil
[@@deriving qcheck2]

let rec length = function
  | Nil -> 0
  | Cons (_, xs) -> 1 + length xs

let test_length =
  Test.make
    ~name:"gen_list_sized n >>= fun l -> length l <= n"
    Gen.small_int
    (fun n ->
      let l = Gen.(generate1 (gen_my_list_sized Gen.int n)) in
      length l <= n)
  |>
    QCheck_alcotest.to_alcotest

let () = Alcotest.run "Test_Recursive"
           [("Recursive",
             Alcotest.[
                 test_case "test_tree_ref" `Quick test_tree_ref;
                 test_leaf
             ])]

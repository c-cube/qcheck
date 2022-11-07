open QCheck2

(** {1. Helpers} *)

let seed = [| 42 |]

let generate gen = Gen.generate ~n:20 ~rand:(Random.State.make seed) gen

(** [test_compare msg eq gen_ref gen_cand] will generate with the same seed
    [gen_ref] and [gen_cand], and test with Alcotest that both generators
    generates the same values. *)
let test_compare ~msg ~eq gen_ref gen_candidate =
  let expected = generate gen_ref in
  let actual = generate gen_candidate in
  Alcotest.(check (list eq)) msg expected actual

open QCheck

(** {1. Helpers} *)

let seed = [| 42 |]

let generate arb =
  let gen = QCheck.gen arb in
  Gen.generate ~n:20 ~rand:(Random.State.make seed) gen

(** [test_compare msg eq arb_ref arb_cand] will arberate with the same seed
    [arb_ref] and [arb_cand], and test with Alcotest that both arberators
    arberates the same values. *)
let test_compare ~msg ~eq arb_ref arb_candidate =
  let expected = generate arb_ref in
  let actual = generate arb_candidate in
  Alcotest.(check (list eq)) msg expected actual

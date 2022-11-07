open QCheck
open Helpers

(** {1. Test variants and polymorphic variants derivation} *)

(** {2. Variants} *)

type colors = Red | Green | Blue [@@deriving qcheck]

let pp_colors fmt x =
  let open Format in
  match x with
  | Red -> fprintf fmt "Red"
  | Green -> fprintf fmt "Green"
  | Blue -> fprintf fmt "Blue"

let eq_colors = Alcotest.of_pp pp_colors

let arb = oneofl [Red; Green; Blue]

let test_variants () =
  test_compare ~msg:"Gen.oneofl <=> deriving variants" ~eq:eq_colors arb arb_colors

type poly_colors = [`Red | `Green | `Blue] [@@deriving qcheck]

let pp_poly_colors fmt x =
  let open Format in
  match x with
  | `Red -> fprintf fmt "`Red"
  | `Green -> fprintf fmt "`Green"
  | `Blue -> fprintf fmt "`Blue"

let eq_poly_colors = Alcotest.of_pp pp_poly_colors

let arb_poly = oneofl [`Red; `Green; `Blue]

let test_poly_variants () =
  test_compare ~msg:"Gen.oneofl <=> deriving variants"
    ~eq:eq_poly_colors arb_poly arb_poly_colors

(** {2. Tests weight} *)

type letters =
  | A [@weight 0]
  | B
[@@deriving qcheck]

let test_weight =
  Test.make ~name:"arb_letters always produces B"
    arb_letters
    (function
     | A -> false
     | B -> true)
  |>
    QCheck_alcotest.to_alcotest

type poly_letters = [
    | `A [@weight 0]
    | `B
  ]
[@@deriving qcheck]

let test_weight_poly =
  Test.make ~name:"arb_poly_letters always produces B"
    arb_poly_letters
    (function
     | `A -> false
     | `B -> true)
  |>
    QCheck_alcotest.to_alcotest

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Variant"
           [("Variants",
             Alcotest.[
                 test_case "test_variants" `Quick test_variants;
                 test_case "test_poly_variants" `Quick test_poly_variants;
                 test_weight;
                 test_weight_poly
           ])]

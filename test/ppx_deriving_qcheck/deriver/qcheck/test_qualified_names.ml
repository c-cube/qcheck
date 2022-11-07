open QCheck
open Helpers

module type S = sig
  type t = int

  val gen : int QCheck.Gen.t
  val arb : int QCheck.arbitrary
end

module Q : S = struct
  type t = int [@@deriving qcheck]
end

module F (X : S) = struct
  type t = X.t [@@deriving qcheck]
end

module G = F (Q)

type t = Q.t [@@deriving qcheck]

type u = G.t [@@deriving qcheck]

let test_module () =
  test_compare ~msg:"Gen.int <=> deriving Q.t" ~eq:Alcotest.int int arb

let test_functor () =
  test_compare ~msg:"Gen.int <=> deriving F.t" ~eq:Alcotest.int int arb_u

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Qualified_names"
           [("Qualified names",
             Alcotest.[
                 test_case "test_module" `Quick test_module;
                 test_case "test_functor" `Quick test_functor
           ])]

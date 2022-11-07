open QCheck2
open Helpers

module type S = sig
  type t = int

  val gen : int QCheck2.Gen.t
end

module Q : S = struct
  type t = int [@@deriving qcheck2]
end

module F (X : S) = struct
  type t = X.t [@@deriving qcheck2]
end

module G = F (Q)

type t = Q.t [@@deriving qcheck2]

type u = G.t [@@deriving qcheck2]

let test_module () =
  test_compare ~msg:"Gen.int <=> deriving Q.t" ~eq:Alcotest.int Gen.int gen

let test_functor () =
  test_compare ~msg:"Gen.int <=> deriving F.t" ~eq:Alcotest.int Gen.int gen_u

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Qualified_names"
           [("Qualified names",
             Alcotest.[
                 test_case "test_module" `Quick test_module;
                 test_case "test_functor" `Quick test_functor
           ])]

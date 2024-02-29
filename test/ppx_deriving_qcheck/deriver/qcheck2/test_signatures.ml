open QCheck2
open Helpers

(* Here we check that deriving qcheck2 works in signature, and cover the cases
   where the type is named "t" and when it is not. *)

module T : sig
  type t = int [@@deriving_inline qcheck2]
    include sig
      [@@@ocaml.warning "-32"]
      val gen : t QCheck2.Gen.t 
    end
    [@@ocaml.doc "@inline"]
  [@@@deriving.end]

  type string' = string [@@deriving_inline qcheck2]
    include sig
      [@@@ocaml.warning "-32"]
      val gen_string' : string' QCheck2.Gen.t 
    end
    [@@ocaml.doc "@inline"]
  [@@@deriving.end]
end = struct
  type t = int [@@deriving qcheck2]

  type string' = string [@@deriving qcheck2]
end

let test_int () =
  test_compare ~msg:"Gen.int <=> deriving int exported by signature" ~eq:Alcotest.int Gen.int T.gen

let test_string () =
  test_compare ~msg:"Gen.string <=> deriving string exported by signature" ~eq:Alcotest.string Gen.string T.gen_string'

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Signatures"
           [("Signatures",
             Alcotest.[
                 test_case "test_int (sig)" `Quick test_int;
                 test_case "test_string (sig)" `Quick test_string;
           ])]

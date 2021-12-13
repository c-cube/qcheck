open QCheck2
open Helpers

(** {1. Test primitives derivation} *)

(** {2. Tests} *)

type int' = int [@@deriving qcheck2]

let test_int () =
  test_compare ~msg:"Gen.int <=> deriving int" ~eq:Alcotest.int Gen.int gen_int'

type unit' = unit [@@deriving qcheck2]

(* Pretty useless though, but, meh *)
let test_unit () =
  test_compare ~msg:"Gen.unit <=> deriving unit" ~eq:Alcotest.unit Gen.unit gen_unit'

type string' = string [@@deriving qcheck2]

let test_string () =
  test_compare ~msg:"Gen.string <=> deriving string" ~eq:Alcotest.string Gen.string gen_string'

type char' = char [@@deriving qcheck2]

let test_char () =
  test_compare ~msg:"Gen.char <=> deriving char" ~eq:Alcotest.char Gen.char gen_char'

type bool' = bool [@@deriving qcheck2]

let test_bool () =
  test_compare ~msg:"Gen.bool <=> deriving bool" ~eq:Alcotest.bool Gen.bool gen_bool'

type float' = float [@@deriving qcheck2]

let test_float () =
  test_compare ~msg:"Gen.float <=> deriving float" ~eq:(Alcotest.float 0.) Gen.float gen_float'

type int32' = int32 [@@deriving qcheck2]

let test_int32 () =
  test_compare ~msg:"Gen.int32 <=> deriving int32" ~eq:Alcotest.int32 Gen.ui32 gen_int32'

type int64' = int64 [@@deriving qcheck2]

let test_int64 () =
  test_compare ~msg:"Gen.int64 <=> deriving int64" ~eq:Alcotest.int64 Gen.ui64 gen_int64'

type 'a option' = 'a option [@@deriving qcheck2]

let test_option () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"Gen.opt <=> deriving opt"
    ~eq:Alcotest.(option int)
    (Gen.opt zero) (gen_option' zero)

type 'a array' = 'a array [@@deriving qcheck2]

let test_array () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"Gen.array <=> deriving array"
    ~eq:Alcotest.(array int)
    (Gen.array zero) (gen_array' zero)

type 'a list' = 'a list [@@deriving qcheck2]

let test_list () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"Gen.list <=> deriving list"
    ~eq:Alcotest.(list int)
    (Gen.list zero) (gen_list' zero)

(** {2. Execute tests} *)

let () = Alcotest.run "Test_Primitives"
           [("Primitives",
             Alcotest.[
                 test_case "test_int" `Quick test_int;
                 test_case "test_unit" `Quick test_unit;
                 test_case "test_string" `Quick test_string;
                 test_case "test_char" `Quick test_char;
                 test_case "test_bool" `Quick test_bool;
                 test_case "test_float" `Quick test_float;
                 test_case "test_int32" `Quick test_int32;
                 test_case "test_int64" `Quick test_int64;
                 test_case "test_option" `Quick test_option;
                 test_case "test_array" `Quick test_array;
                 test_case "test_list" `Quick test_list;
           ])]

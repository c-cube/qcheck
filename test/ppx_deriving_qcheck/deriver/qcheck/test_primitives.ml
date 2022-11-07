open QCheck
open Helpers

(** {1. Test primitives derivation} *)

(** {2. Tests} *)

type int' = int [@@deriving qcheck]

let test_int () =
  test_compare ~msg:"int <=> deriving int" ~eq:Alcotest.int int arb_int'

type unit' = unit [@@deriving qcheck]

(* Pretty useless though, but, meh *)
let test_unit () =
  test_compare ~msg:"unit <=> deriving unit" ~eq:Alcotest.unit unit arb_unit'

type string' = string [@@deriving qcheck]

let test_string () =
  test_compare ~msg:"string <=> deriving string" ~eq:Alcotest.string string arb_string'

type char' = char [@@deriving qcheck]

let test_char () =
  test_compare ~msg:"char <=> deriving char" ~eq:Alcotest.char char arb_char'

type bool' = bool [@@deriving qcheck]

let test_bool () =
  test_compare ~msg:"bool <=> deriving bool" ~eq:Alcotest.bool bool arb_bool'

type float' = float [@@deriving qcheck]

let test_float () =
  test_compare ~msg:"float <=> deriving float" ~eq:(Alcotest.float 0.) float arb_float'

type int32' = int32 [@@deriving qcheck]

let test_int32 () =
  test_compare ~msg:"int32 <=> deriving int32" ~eq:Alcotest.int32 int32 arb_int32'

type int64' = int64 [@@deriving qcheck]

let test_int64 () =
  test_compare ~msg:"int64 <=> deriving int64" ~eq:Alcotest.int64 int64 arb_int64'

type 'a option' = 'a option [@@deriving qcheck]

let test_option () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"option <=> deriving opt"
    ~eq:Alcotest.(option int)
    (option (make zero)) (arb_option' zero)

type 'a array' = 'a array [@@deriving qcheck]

let test_array () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"array <=> deriving array"
    ~eq:Alcotest.(array int)
    (array (make zero)) (arb_array' zero)

type 'a list' = 'a list [@@deriving qcheck]

let test_list () =
  let zero = Gen.pure 0 in
  test_compare ~msg:"list <=> deriving list"
    ~eq:Alcotest.(list int)
    (list (make zero)) (arb_list' zero)

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

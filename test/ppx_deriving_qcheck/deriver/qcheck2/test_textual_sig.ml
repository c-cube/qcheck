(** Module test for ppx_deriving_qcheck2 signature derivation *)
open Ppxlib

let loc = Location.none

let f = Ppx_deriving_qcheck.derive_gen_sigs ~version:`QCheck2 ~loc

let extract stri =
  match stri.pstr_desc with Pstr_type (x, y) -> (x, y) | _ -> assert false

let check_eq ~expected ~actual name =
  let f x = Format.asprintf "%a" Ppxlib.Pprintast.signature x in
  Alcotest.(check string) name (f expected) (f actual)

let test_simple () =
  let expected = [ [%sigi: val gen : t QCheck2.Gen.t] ] in
  let actual = f @@ extract [%stri type t = int] in
  check_eq ~expected ~actual "sig for simple type"

let test_named () =
  let expected = [ [%sigi: val gen_color : color QCheck2.Gen.t] ] in
  let actual = f @@ extract [%stri type color = Red | Green | Blue] in
  check_eq ~expected ~actual "sig for named type"

let test_parametrized () =
  let expected =
    [ [%sigi: val gen : 'a QCheck2.Gen.t -> 'a t QCheck2.Gen.t] ]
  in
  let actual = f @@ extract [%stri type 'a t = 'a list] in
  check_eq ~expected ~actual "sig for parametrized type"

let test_two_params () =
  let expected =
    [
      [%sigi: val gen : 'a QCheck2.Gen.t -> 'b QCheck2.Gen.t -> ('a, 'b) t QCheck2.Gen.t];
    ]
  in
  let actual = f @@ extract [%stri type ('a, 'b) t = 'a * 'b] in
  check_eq ~expected ~actual "sig for two-parameter type"

let test_mutual () =
  let expected =
    [
      [%sigi: val gen_tree : 'a QCheck2.Gen.t -> 'a tree QCheck2.Gen.t];
      [%sigi: val gen_forest : 'a QCheck2.Gen.t -> 'a forest QCheck2.Gen.t];
    ]
  in
  let actual =
    f
    @@ extract
         [%stri
           type 'a tree = Node of ('a * 'a forest)

           and 'a forest = Nil | Cons of ('a tree * 'a forest)]
  in
  check_eq ~expected ~actual "sig for mutual recursive types"

let test_abstract () =
  let expected = [ [%sigi: val gen : t QCheck2.Gen.t] ] in
  let actual = f (Nonrecursive, [Ast_builder.Default.type_declaration
    ~loc ~name:{txt = "t"; loc}
    ~params:[] ~cstrs:[] ~kind:Ptype_abstract
    ~private_:Public ~manifest:None])
  in
  check_eq ~expected ~actual "sig for abstract type"

let () =
  Alcotest.(
    run
      "ppx_deriving_qcheck2 sig tests"
      [
        ( "deriving generator sig",
          [
            test_case "sig for simple type" `Quick test_simple;
            test_case "sig for named type" `Quick test_named;
            test_case "sig for parametrized type" `Quick test_parametrized;
            test_case "sig for two-parameter type" `Quick test_two_params;
            test_case "sig for mutual recursive types" `Quick test_mutual;
            test_case "sig for abstract type" `Quick test_abstract;
          ] );
      ])

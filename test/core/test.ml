open QCheck2
open Alcotest

let test_gen_opt ~ratio =
  let opt_int = Gen.opt ?ratio Gen.int in
  let nb = ref 0 in
  for _i = 0 to 1000 do
    Gen.generate1 opt_int |> function None -> () | Some _ -> nb := !nb + 1
  done;
  !nb

let test_gen_opt_default () =
  let nb = test_gen_opt ~ratio:None in
  let b = nb > 800 && nb < 900 in
  (check bool) "Gen.opt produces around 85% of Some" b true

let test_gen_opt_custom () =
  let nb = test_gen_opt ~ratio:(Some 0.5) in
  let b = nb > 450 && nb < 550 in
  (check bool) "Gen.opt produces around 50% of Some" b true

let () =
  run "QCheck"
    [
      ( "gen ",
        [
          test_case "Gen.opt with default ratio" `Quick test_gen_opt_default;
          test_case "Gen.opt with custom ratio" `Quick test_gen_opt_custom;
        ] );
    ]

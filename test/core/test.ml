open QCheck2

module Shrink = struct
  let test_int_towards () =
    Alcotest.(check' (list int))
      ~msg:"int_towards 0 100"
      ~actual:(Shrink.int_towards 0 100 |> List.of_seq)
      ~expected:[0; 50; 75; 88; 94; 97; 99];
    Alcotest.(check' (list int))
      ~msg:"int_towards 500 1000"
      ~actual:(Shrink.int_towards 500 1000 |> List.of_seq)
      ~expected:[500; 750; 875; 938; 969; 985; 993; 997; 999];
    Alcotest.(check' (list int))
      ~msg:"int_towards (-50) (-26)"
      ~actual:(Shrink.int_towards (-50) (-26) |> List.of_seq)
      ~expected:[-50; -38; -32; -29; -28; -27]

  let test_int32_towards () =
    Alcotest.(check' (list int32))
      ~msg:"int32_towards 0l 100l"
      ~actual:(Shrink.int32_towards 0l 100l |> List.of_seq)
      ~expected:[0l; 50l; 75l; 88l; 94l; 97l; 99l];
    Alcotest.(check' (list int32))
      ~msg:"int32_towards 500l 1000l"
      ~actual:(Shrink.int32_towards 500l 1000l |> List.of_seq)
      ~expected:[500l; 750l; 875l; 938l; 969l; 985l; 993l; 997l; 999l];
    Alcotest.(check' (list int32))
      ~msg:"int32_towards (-50l) (-26l)"
      ~actual:(Shrink.int32_towards (-50l) (-26l) |> List.of_seq)
      ~expected:[-50l; -38l; -32l; -29l; -28l; -27l]

  let test_int64_towards () =
    Alcotest.(check' (list int64))
      ~msg:"int64_towards 0L 100L"
      ~actual:(Shrink.int64_towards 0L 100L |> List.of_seq)
      ~expected:[0L; 50L; 75L; 88L; 94L; 97L; 99L];
    Alcotest.(check' (list int64))
      ~msg:"int64_towards 500L 1000L"
      ~actual:(Shrink.int64_towards 500L 1000L |> List.of_seq)
      ~expected:[500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L];
    Alcotest.(check' (list int64))
      ~msg:"int64_towards (-50L) (-26L)"
      ~actual:(Shrink.int64_towards (-50L) (-26L) |> List.of_seq)
      ~expected:[-50L; -38L; -32L; -29L; -28L; -27L]

  let test_float_towards () =
    Alcotest.(check' (list (float 0.0001)))
      ~msg:"float_towards 0. 100."
      ~actual:(Shrink.float_towards 0. 100. |> List.of_seq)
      ~expected:[0.; 50.; 75.; 87.5; 93.75; 96.875; 98.4375; 99.2188; 99.6094; 99.8047; 99.9023; 99.9512; 99.9756; 99.9878; 99.9939];
    Alcotest.(check' (list (float 0.001)))
      ~msg:"float_towards 500. 1000."
      ~actual:(Shrink.float_towards 500. 1000. |> List.of_seq)
      ~expected:[500.; 750.; 875.; 937.5; 968.75; 984.375; 992.188; 996.094; 998.047; 999.023; 999.512; 999.756; 999.878; 999.939; 999.969];
    Alcotest.(check' (list (float 0.0001)))
      ~msg:"float_towards (-50.) (-26.)"
      ~actual:(Shrink.float_towards (-50.) (-26.) |> List.of_seq)
      ~expected:[-50.; -38.; -32.; -29.; -27.5; -26.75; -26.375; -26.1875; -26.0938; -26.0469; -26.0234; -26.0117; -26.0059; -26.0029; -26.0015]

  let tests = ("Shrink", Alcotest.[
      test_case "int_towards" `Quick test_int_towards;
      test_case "int32_towards" `Quick test_int32_towards;
      test_case "int64_towards" `Quick test_int64_towards;
      test_case "float_towards" `Quick test_float_towards
    ])
end

module Gen = struct
  let test_gen_opt ?ratio () =
    let opt_int =
      match ratio with
      | None -> Gen.option Gen.int
      | Some ratio -> Gen.option_ratio ~ratio Gen.int
    in
    let nb = ref 0 in
    for _i = 0 to 1000 do
      Gen.generate1 opt_int |> function None -> () | Some _ -> nb := !nb + 1
    done;
    !nb

  let test_gen_opt_default () =
    let nb = test_gen_opt () in
    let b = nb > 800 && nb < 900 in
    Alcotest.(check bool) "[Gen.option] produces around 85% of Some" b true

  let test_gen_opt_custom () =
    let nb = test_gen_opt ~ratio:0.5 () in
    let b = nb > 450 && nb < 550 in
    Alcotest.(check bool) "[Gen.option_ratio ~ratio:0.5] produces around 50% of Some" b true

  let tests =
    ("Gen", Alcotest.[
         test_case "opt with default ratio" `Quick test_gen_opt_default;
         test_case "opt with custom ratio" `Quick test_gen_opt_custom;
       ])
end

module Test = struct
  let test_count_n ?count expected =
    let t = QCheck2.(Test.make ?count Gen.int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck2.Test.make ~count:%s |> get_count = %d"
                (Option.fold ~none:"None" ~some:string_of_int count) expected
    in
    Alcotest.(check int) msg expected (QCheck2.Test.test_get_count t)

  let test_count_10 () = test_count_n ~count:10 10

  let test_count_0 () = test_count_n ~count:0 0

  let test_count_default () = test_count_n 100

  let test_count_env () =
    let () = Unix.putenv "QCHECK_COUNT" "5" in
    let t = QCheck2.(Test.make Gen.int (fun _ -> true)) in
    let actual = QCheck2.Test.test_get_count t in
    Alcotest.(check int) "default count is from QCHECK_COUNT" 5 actual

  let tests =
    ("Test", Alcotest.[
         test_case "make with custom count" `Quick test_count_10;
         test_case "make with custom count" `Quick test_count_0;
         test_case "make with default count" `Quick test_count_default;
         test_case "make with env count" `Quick test_count_env;
       ])
end

module String = struct

  let test_string_shrinking () =
    let shrink_result = QCheck2.(find_example_gen ~f:(fun s -> s <> s ^ s) Gen.string) in
    Alcotest.(check string) "Shrinking a non-empty string shrinks to \"a\"" "a" shrink_result

  let tests = ("String", Alcotest.[test_case "shrinking" `Quick test_string_shrinking])
end

let () =
  Alcotest.run "QCheck"
    [
      Shrink.tests;
      Gen.tests;
      Test.tests;
      String.tests
    ]

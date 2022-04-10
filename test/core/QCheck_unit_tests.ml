open QCheck

module Shrink = struct

  let trace_false shrinker x =
    let res = ref [] in
    shrinker x (fun x -> res := x::!res);
    List.rev !res

  let trace_true shrinker x =
    let rec loop x =
      match Iter.find (fun _ -> true) (shrinker x) with
      | None -> []
      | Some y -> y::loop y in
    loop x

  let alco_check typ func msg_suffix (msg,input,expected) =
    Alcotest.(check (list typ)) (msg ^ " - " ^ msg_suffix) (func input) expected

  let test_int () =
    List.iter (alco_check Alcotest.int (trace_false Shrink.int) "on repeated failure")
      [ ("int 100",   100,  [50; 75; 88; 94; 97; 99; 99]); (*WTF?*)
        ("int 1000",  1000, [500; 750; 875; 938; 969; 985; 993; 997; 999; 999]); (*WTF?*)
        ("int (-26)", -26,  [-13; -20; -23; -25; -25]) ]; (*WTF?*)
    List.iter (alco_check Alcotest.int (trace_true Shrink.int) "on repeated success")
      [ ("int 100",   100,  [50; 25; 13; 7; 4; 2; 1; 0]);
        ("int 1000",  1000, [500; 250; 125; 63; 32; 16; 8; 4; 2; 1; 0]);
        ("int (-26)", -26,  [-13; -7; -4; -2; -1; 0]) ]

  let test_int32 () =
    List.iter (alco_check Alcotest.int32 (trace_false Shrink.int32) "on repeated failure")
      [ ("int 100",   100l,  [50l; 75l; 88l; 94l; 97l; 99l; 99l]);
        ("int 1000",  1000l, [500l; 750l; 875l; 938l; 969l; 985l; 993l; 997l; 999l; 999l]);
        ("int (-26)", -26l,  [-13l; -20l; -23l; -25l; -25l]) ];
    List.iter (alco_check Alcotest.int32 (trace_true Shrink.int32) "on repeated success")
      [ ("int 100",   100l,  [50l; 25l; 13l; 7l; 4l; 2l; 1l; 0l]);
        ("int 1000",  1000l, [500l; 250l; 125l; 63l; 32l; 16l; 8l; 4l; 2l; 1l; 0l]);
        ("int (-26)", -26l,  [-13l; -7l; -4l; -2l; -1l; 0l]) ]

  let test_int64 () =
    List.iter (alco_check Alcotest.int64 (trace_false Shrink.int64) "on repeated failure")
      [ ("int 100",   100L,  [50L; 75L; 88L; 94L; 97L; 99L; 99L]);
        ("int 1000",  1000L, [500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L; 999L]);
        ("int (-26)", -26L,  [-13L; -20L; -23L; -25L; -25L]) ];
    List.iter (alco_check Alcotest.int64 (trace_true Shrink.int64) "on repeated success")
      [ ("int 100",   100L,  [50L; 25L; 13L; 7L; 4L; 2L; 1L; 0L]);
        ("int 1000",  1000L, [500L; 250L; 125L; 63L; 32L; 16L; 8L; 4L; 2L; 1L; 0L]);
        ("int (-26)", -26L,  [-13L; -7L; -4L; -2L; -1L; 0L]) ]

  let tests = ("Shrink", Alcotest.[
      test_case "int"   `Quick test_int;
      test_case "int32" `Quick test_int32;
      test_case "int64" `Quick test_int64;
    ])
end

module Check_exn = struct

  let check_exn = Test.check_exn

  let test_pass_trivial () =
    let run_test () = check_exn QCheck.(Test.make int (fun _ -> true)) in
    Alcotest.(check unit) "Success-trivial" () @@ run_test ()

  let test_pass_random () =
    let run_test () =
      check_exn QCheck.(Test.make (list int) (fun l -> List.rev (List.rev l) = l)) in
    Alcotest.(check unit) "Success-random" () @@ run_test ()

  let test_fail_always () =
    let name = "will-always-fail" in
    let counterex_str = "0 (after 63 shrink steps)" in
    let run_test () =
      check_exn QCheck.(Test.make ~name int (fun _ -> false)) in
    Alcotest.check_raises "Fail" (Test.Test_fail (name,[counterex_str])) run_test

  let test_fail_random () =
    let name = "list is own reverse" in
    let counterex_str = "[0; -1] (after 126 shrink steps)" in
    let run_test () =
      check_exn
        QCheck.(Test.make ~name (list int) (fun l -> List.rev l = l)) in
    Alcotest.check_raises "Fail" (Test.Test_fail (name,[counterex_str])) run_test

  exception MyError

  let test_error () =
    let name = "will-always-error" in
    let counterex_str = "0 (after 63 shrink steps)" in
    let run_test () =
      let () = Printexc.record_backtrace false in (* for easier pattern-matching below *)
      check_exn QCheck.(Test.make ~name int (fun _ -> raise MyError)) in
    Alcotest.check_raises "MyError" (Test.Test_error (name,counterex_str,MyError,"")) run_test

  let tests =
    ("Test.check_exn", Alcotest.[
         test_case "check_exn pass trivial" `Quick test_pass_trivial;
         test_case "check_exn pass random" `Quick test_pass_random;
         test_case "check_exn fail always" `Quick test_fail_always;
         test_case "check_exn fail random" `Quick test_fail_random;
         test_case "check_exn Error" `Quick test_error;
       ])
end

let () =
  Alcotest.run "QCheck"
    [
      Shrink.tests;
      Check_exn.tests;
    ]

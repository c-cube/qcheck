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
    Alcotest.(check (list typ)) (msg ^ " - " ^ msg_suffix) expected (func input)

  let test_int () =
    List.iter (alco_check Alcotest.int (trace_false Shrink.int) "on repeated failure")
      [ ("int 100",   100,  [50; 75; 88; 94; 97; 99]);
        ("int 1000",  1000, [500; 750; 875; 938; 969; 985; 993; 997; 999]);
        ("int (-26)", -26,  [-13; -20; -23; -25]) ];
    List.iter (alco_check Alcotest.int (trace_true Shrink.int) "on repeated success")
      [ ("int 100",   100,  [50; 25; 13; 7; 4; 2; 1; 0]);
        ("int 1000",  1000, [500; 250; 125; 63; 32; 16; 8; 4; 2; 1; 0]);
        ("int (-26)", -26,  [-13; -7; -4; -2; -1; 0]) ]

  let test_int32 () =
    List.iter (alco_check Alcotest.int32 (trace_false Shrink.int32) "on repeated failure")
      [ ("int 100",   100l,  [50l; 75l; 88l; 94l; 97l; 99l]);
        ("int 1000",  1000l, [500l; 750l; 875l; 938l; 969l; 985l; 993l; 997l; 999l]);
        ("int (-26)", -26l,  [-13l; -20l; -23l; -25l]) ];
    List.iter (alco_check Alcotest.int32 (trace_true Shrink.int32) "on repeated success")
      [ ("int 100",   100l,  [50l; 25l; 13l; 7l; 4l; 2l; 1l; 0l]);
        ("int 1000",  1000l, [500l; 250l; 125l; 63l; 32l; 16l; 8l; 4l; 2l; 1l; 0l]);
        ("int (-26)", -26l,  [-13l; -7l; -4l; -2l; -1l; 0l]) ]

  let test_int64 () =
    List.iter (alco_check Alcotest.int64 (trace_false Shrink.int64) "on repeated failure")
      [ ("int 100",   100L,  [50L; 75L; 88L; 94L; 97L; 99L]);
        ("int 1000",  1000L, [500L; 750L; 875L; 938L; 969L; 985L; 993L; 997L; 999L]);
        ("int (-26)", -26L,  [-13L; -20L; -23L; -25L]) ];
    List.iter (alco_check Alcotest.int64 (trace_true Shrink.int64) "on repeated success")
      [ ("int 100",   100L,  [50L; 25L; 13L; 7L; 4L; 2L; 1L; 0L]);
        ("int 1000",  1000L, [500L; 250L; 125L; 63L; 32L; 16L; 8L; 4L; 2L; 1L; 0L]);
        ("int (-26)", -26L,  [-13L; -7L; -4L; -2L; -1L; 0L]) ]

  let test_char () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char) "on repeated failure")
      [ ("char 'a'",   'a',  []);
        ("char 'z'",   'z',  ['n'; 't'; 'w'; 'y']);
        ("char 'A'",   'A',  ['Q'; 'I'; 'E'; 'C'; 'B']);
        ("char '~'",   '~',  ['p'; 'w'; '{'; '}']) ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char) "on repeated success")
      [ ("char 'a'",   'a',  []);
        ("char 'z'",   'z',  ['n'; 'h'; 'e'; 'c'; 'b'; 'a']);
        ("char 'A'",   'A',  ['Q'; 'Y'; ']'; '_'; '`'; 'a']);
        ("char '~'",   '~',  ['p'; 'i'; 'e'; 'c'; 'b'; 'a']); ]

  let test_char_numeral () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char_numeral) "on repeated failure")
      [ ("char '0'",   '0',  []);
        ("char '9'",   '9',  ['5'; '7'; '8']) ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char_numeral) "on repeated success")
      [ ("char '0'",   '0',  []);
        ("char '9'",   '9',  ['5'; '3'; '2'; '1'; '0']); ]

  let test_char_printable () =
    List.iter (alco_check Alcotest.char (trace_false Shrink.char_printable) "on repeated failure")
      [ ("char 'A'",   'A',  ['Q'; 'I'; 'E'; 'C'; 'B']);
        ("char 'a'",   'a',  []);
        ("char ' '",   ' ',  ['@'; '0'; '('; '$'; '"'; '!']);
        ("char '~'",   '~',  ['p'; 'w'; '{'; '}']);
        ("char '\\n'", '\n', ['p'; 'w'; '{'; '}']); ];
    List.iter (alco_check Alcotest.char (trace_true Shrink.char_printable) "on repeated success")
      [ ("char 'A'",   'A',  ['Q'; 'Y'; ']'; '_'; '`'; 'a']);
        ("char 'a'",   'a',  []);
        ("char ' '",   ' ',  ['@'; 'P'; 'X'; '\\'; '^'; '_'; '`'; 'a']);
        ("char '~'",   '~',  ['p'; 'i'; 'e'; 'c'; 'b'; 'a']);
        ("char '\\n'", '\n', ['p'; 'i'; 'e'; 'c'; 'b'; 'a']); ]

  let test_string () =
    List.iter (alco_check Alcotest.string (trace_false Shrink.string) "on repeated failure")
      [ ("string \"\"",     "",     []);
        ("string \"a\"",    "a",    [""]);
        ("string \"aa\"",   "aa",   [""; "a"]);
        ("string \"aaaa\"", "aaaa", ["aa"; "aa"; "aaa"]);
        ("string \"abcd\"", "abcd", ["ab"; "cd"; "acd"; "bcd"; "aacd"; "abbd"; "abcc"]);
        ("string \"E'*\"",  "E'*",  ["E'"; "*"; "E*"; "'*"; "S'*"; "L'*"; "H'*"; "F'*"; "ED*";
                                     "E5*"; "E.*"; "E**"; "E(*"; "E'E"; "E'7"; "E'0"; "E'-"; "E'+"]);
        ("string \"vi5x92xgG\"", "vi5x92xgG", (* A less exhaustive string shrinker would be preferable *)
         ["vi5x9"; "vi52xgG"; "vix92xgG"; "5x92xgG";
          "v5x92xgG"; "i5x92xgG"; "li5x92xgG"; "qi5x92xgG"; "ti5x92xgG"; "ui5x92xgG";
          "ve5x92xgG"; "vg5x92xgG"; "vh5x92xgG";
          "viKx92xgG"; "vi@x92xgG"; "vi:x92xgG"; "vi7x92xgG"; "vi6x92xgG";
          "vi5m92xgG"; "vi5s92xgG"; "vi5v92xgG"; "vi5w92xgG";
          "vi5xM2xgG"; "vi5xC2xgG"; "vi5x>2xgG"; "vi5x;2xgG"; "vi5x:2xgG";
          "vi5x9IxgG"; "vi5x9=xgG"; "vi5x97xgG"; "vi5x94xgG"; "vi5x93xgG";
          "vi5x92mgG"; "vi5x92sgG"; "vi5x92vgG"; "vi5x92wgG";
          "vi5x92xdG"; "vi5x92xfG";
          "vi5x92xgT"; "vi5x92xgM"; "vi5x92xgJ"; "vi5x92xgH"]);
        ("string \"~~~~\"", "~~~~", ["~~"; "~~"; "~~~"; "p~~~"; "w~~~"; "{~~~"; "}~~~"; "~p~~";
                                     "~w~~"; "~{~~"; "~}~~"; "~~p~"; "~~w~"; "~~{~"; "~~}~";
                                     "~~~p"; "~~~w"; "~~~{"; "~~~}"]); ];
    List.iter (alco_check Alcotest.string (trace_true Shrink.string) "on repeated success")
      [ ("string \"\"",     "",     []);
        ("string \"a\"",    "a",    [""]);
        ("string \"aa\"",   "aa",   [""]);
        ("string \"aaaa\"", "aaaa", ["aa"; ""]);
        ("string \"abcd\"", "abcd", ["ab"; ""]);
        ("string \"E'*\"",  "E'*",  ["E'"; ""]);
        ("string \"vi5x92xgG\"", "vi5x92xgG", ["vi5x9"; "vi5"; "vi"; ""]); ]

  let test_int_list () =
    List.iter (alco_check Alcotest.(list int) (trace_false (Shrink.list_spine)) "on repeated failure")
      [ ("list int [0]",       [0],       [[]]);
        ("list int [0;1]",     [0;1],     [[]; [0]; [1]]);
        ("list int [0;1;2]",   [0;1;2],   [[0; 1]; [2]; [0; 2]; [1; 2]]);
        ("list int [0;1;2;3]", [0;1;2;3], [[0; 1]; [2; 3]; [0; 2; 3]; [1; 2; 3]]);
        ("list int [0;0]",     [0;0],     [[]; [0]]);
        ("list int [0;0;0]",   [0;0;0],   [[0; 0]; [0]; [0; 0]]);
        ("list int [0;0;0;0]", [0;0;0;0], [[0; 0]; [0; 0]; [0; 0; 0]]); ];
    List.iter (alco_check Alcotest.(list int) (trace_true (Shrink.list_spine)) "on repeated success")
      [ ("list int [0]",       [0],       [[]]);
        ("list int [0;1]",     [0;1],     [[]]);
        ("list int [0;1;2]",   [0;1;2],   [[0; 1]; []]);
        ("list int [0;1;2;3]", [0;1;2;3], [[0; 1]; []]);
        ("list int [0;0]",     [0;0],     [[]]);
        ("list int [0;0;0]",   [0;0;0],   [[0; 0]; []]);
        ("list int [0;0;0;0]", [0;0;0;0], [[0; 0]; []]); ]

  let test_int32_list () = (* use int32 as a boxed type and List.map to force run-time allocations *)
    List.iter (alco_check Alcotest.(list int32) (trace_false (Shrink.list_spine)) "on repeated failure")
      [ ("list int32 [0l]",          List.map Int32.of_int [0],       [[]]);
        ("list int32 [0l;1l]",       List.map Int32.of_int [0;1],     [[]; [0l]; [1l]]);
        ("list int32 [0l;1l;2l]",    List.map Int32.of_int [0;1;2],   [[0l; 1l]; [2l]; [0l; 2l]; [1l; 2l]]);
        ("list int32 [0l;1l;2l;3l]", List.map Int32.of_int [0;1;2;3], [[0l; 1l]; [2l; 3l]; [0l; 2l; 3l]; [1l; 2l; 3l]]);
        ("list int32 [0l;0l]",       List.map Int32.of_int [0;0],     [[]; [0l]]);
        ("list int32 [0l;0l;0l]",    List.map Int32.of_int [0;0;0],   [[0l; 0l]; [0l]; [0l; 0l]]);
        ("list int32 [0l;0l;0l;0l]", List.map Int32.of_int [0;0;0;0], [[0l; 0l]; [0l; 0l]; [0l; 0l; 0l]]); ];
    List.iter (alco_check Alcotest.(list int32) (trace_true (Shrink.list_spine)) "on repeated success")
      [ ("list int [0l]",          List.map Int32.of_int [0],       [[]]);
        ("list int [0l;1l]",       List.map Int32.of_int [0;1],     [[]]);
        ("list int [0l;1l;2l]",    List.map Int32.of_int [0;1;2],   [[0l; 1l]; []]);
        ("list int [0l;1l;2l;3l]", List.map Int32.of_int [0;1;2;3], [[0l; 1l]; []]);
        ("list int [0l;0l]",       List.map Int32.of_int [0;0],     [[]]);
        ("list int [0l;0l;0l]",    List.map Int32.of_int [0;0;0],   [[0l; 0l]; []]);
        ("list int [0l;0l;0l;0l]", List.map Int32.of_int [0;0;0;0], [[0l; 0l]; []]); ]

  let test_list_spine_compare () =
    let run_test () = QCheck.Shrink.list_spine [pred;succ] ignore in
    Alcotest.(check unit) "doesn't compare elements" () @@ run_test ()

  let tests = ("Shrink", Alcotest.[
      test_case "int"   `Quick test_int;
      test_case "int32" `Quick test_int32;
      test_case "int64" `Quick test_int64;
      test_case "char"  `Quick test_char;
      test_case "char_numeral"   `Quick test_char_numeral;
      test_case "char_printable" `Quick test_char_printable;
      test_case "string" `Quick test_string;
      test_case "int list" `Quick test_int_list;
      test_case "int32 list" `Quick test_int32_list;
      test_case "list_spine" `Quick test_list_spine_compare;
    ])
end

module Check_exn = struct

  (* String.starts_with was introduced in 4.13.
     Include the below to support pre-4.13 OCaml. *)
  let string_starts_with ~prefix s =
    let prefix_len = String.length prefix in
    prefix_len <= String.length s
    && prefix = String.sub s 0 prefix_len

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
    try
      check_exn QCheck.(Test.make ~name int (fun _ -> false));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
        Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  let test_fail_random () =
    let name = "list is own reverse" in
    try
      check_exn QCheck.(Test.make ~name (list int) (fun l -> List.rev l = l));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_fail (n,[c_ex_str])) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"[0; 1]" c_ex_str
                || string_starts_with ~prefix:"[0; -1]" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  exception MyError

  let test_error () =
    let name = "will-always-error" in
    try
      Printexc.record_backtrace false; (* for easier pattern-matching below *)
      check_exn QCheck.(Test.make ~name int (fun _ -> raise MyError));
      Alcotest.failf "%s: Unexpected success" name
    with
      (Test.Test_error (n,c_ex_str,MyError,"")) ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name;
        if not (string_starts_with ~prefix:"0" c_ex_str)
        then
          Alcotest.failf "%s: counter-example prefix. Received \"%s\"" name c_ex_str

  let test_negative_trivial () =
    let run_test () = check_exn QCheck2.(Test.make_neg Gen.int (fun _ -> false)) in
    Alcotest.(check unit) "Success-negative-trivial" () @@ run_test ()

  let test_negative_test_unexpected_success () =
    let name = "negative-trivial-test" in
    let run_test () = check_exn QCheck2.(Test.make_neg ~name Gen.int (fun _ -> true)) in
    try
      run_test ();
      Alcotest.failf "Negative test didn't raise expected exception."
    with
      Test.Test_unexpected_success n ->
        Alcotest.(check string) (Printf.sprintf "%s: name" name) n name

  let tests =
    ("Test.check_exn", Alcotest.[
         test_case "check_exn pass trivial" `Quick test_pass_trivial;
         test_case "check_exn pass random" `Quick test_pass_random;
         test_case "check_exn fail always" `Quick test_fail_always;
         test_case "check_exn fail random" `Quick test_fail_random;
         test_case "check_exn Error" `Quick test_error;
         test_case "check_exn negative pass trivial" `Quick test_negative_trivial;
         test_case "check_exn Unexpected success" `Quick test_negative_test_unexpected_success;
       ])
end

module TestCount = struct
  let test_count_n ?count expected =
    let (Test cell) = QCheck.(Test.make ?count int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck.Test.make ~count:%s |> get_count = %d"
                (Option.fold ~none:"None" ~some:string_of_int count) expected
    in
    Alcotest.(check int) msg expected (QCheck.Test.get_count cell)

  let test_count_10 () = test_count_n ~count:10 10

  let test_count_default () = test_count_n 100

  let test_count_env () =
    let () = Unix.putenv "QCHECK_COUNT" "5" in
    let (Test cell) = QCheck.(Test.make int (fun _ -> true)) in
    let actual = QCheck.Test.get_count cell in
    Alcotest.(check int) "default count is from QCHECK_COUNT" 5 actual

  let test_count_0 () = test_count_n ~count:0 0

  let test_count_negative_fail () =
    try
      let _ = test_count_n ~count:(-1) (-1) in
      Alcotest.fail "A negative count in a test should fail"
    with
    | _ -> ()

  let tests =
    ("Test.make ~count", Alcotest.[
         test_case "make with custom count" `Quick test_count_10;
         test_case "make with default count" `Quick test_count_default;
         test_case "make with env count" `Quick test_count_env;
         test_case "make with 0 count" `Quick test_count_0;
         test_case "make with negative count should fail"
           `Quick test_count_negative_fail;
       ])
end

module TestLongFactor = struct
  let test_long_factor_n ?long_factor expected =
    let (Test cell) = QCheck.(Test.make ?long_factor int (fun _ -> true)) in
    let msg = Printf.sprintf "QCheck.Test.make ~long_factor:%s |> long_factor = %d"
                (Option.fold ~none:"None" ~some:string_of_int long_factor) expected
    in
    Alcotest.(check int) msg expected (QCheck.Test.get_long_factor cell)

  let test_long_factor_10 () = test_long_factor_n ~long_factor:10 10

  let test_long_factor_default () = test_long_factor_n 1

  let test_long_factor_env () =
    let () = Unix.putenv "QCHECK_LONG_FACTOR" "5" in
    let (Test cell) = QCheck.(Test.make int (fun _ -> true)) in
    let actual = QCheck.Test.get_long_factor cell in
    Alcotest.(check int) "default long factor is from QCHECK_LONG_FACTOR" 5 actual

   let test_long_factor_0 () = test_long_factor_n ~long_factor:0 0
  
   let test_long_factor_negative_fail () =
     try
       let _ = test_long_factor_n ~long_factor:(-1) (-1) in
       Alcotest.fail "A negative long factor in a test should fail"
     with
     | _ -> ()
  
   let tests =
     ("Test.make ~long_factor", Alcotest.[
          test_case "make with custom long_factor" `Quick test_long_factor_10;
          test_case "make with default long_factor" `Quick test_long_factor_default;
          test_case "make with env long_factor" `Quick test_long_factor_env;
          test_case "make with 0 long_factor" `Quick test_long_factor_0;
          test_case "make with negative long_factor should fail"
            `Quick test_long_factor_negative_fail;
     ])
end

let () =
  Alcotest.run "QCheck"
    [
      Shrink.tests;
      Check_exn.tests;
      TestCount.tests;
      TestLongFactor.tests;
    ]

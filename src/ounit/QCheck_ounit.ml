
open OUnit
open QCheck_base_runner

let ps = Printf.printf "%s"
let va = Printf.sprintf
let pf = Printf.printf

let not_success = function RSuccess _ -> false | _ -> true

let result_path = function
    | RSuccess path
    | RError (path, _)
    | RFailure (path, _)
    | RSkip (path, _)
    | RTodo (path, _) -> path

let result_msg = function
    | RSuccess _ -> "Success"
    | RError (_, msg)
    | RFailure (_, msg)
    | RSkip (_, msg)
    | RTodo (_, msg) -> msg

let result_flavour = function
    | RError _ -> `Red, "Error"
    | RFailure _ -> `Red, "Failure"
    | RSuccess _ -> `Green, "Success"
    | RSkip _ -> `Blue, "Skip"
    | RTodo _ -> `Yellow, "Todo"

let string_of_path path =
  let path = List.filter (function Label _ -> true | _ -> false) path in
  String.concat ">" (List.rev_map string_of_node path)

let separator1 = "\027[K" ^ (String.make 79 '\\')
let separator2 = String.make 79 '/'

let print_result_list ~colors =
  List.iter (fun result ->
    let c, res = result_flavour result in
    pf "%s\n%a: %s\n\n%s\n%s\n"
    separator1 (Color.pp_str_c ~colors c) res
    (string_of_path (result_path result))
    (result_msg result) separator2)

let conf_seed = OUnit2.Conf.make_int "seed" ~-1 "set random seed"
let conf_verbose = OUnit2.Conf.make_bool "qcheck_verbose" true "enable verbose QCheck tests"
let conf_long = OUnit2.Conf.make_bool "qcheck_long" false "enable long QCheck tests"

let default_rand () =
  (* random seed, for repeatability of tests *)
  Random.State.make [| 89809344; 994326685; 290180182 |]

let to_ounit2_test ?(rand =default_rand()) (QCheck2.Test.Test cell) =
  let module T = QCheck2.Test in
  let name = T.get_name cell in
  let open OUnit2 in
  name >: test_case ~length:OUnitTest.Long (fun ctxt ->
      let rand = match conf_seed ctxt with
        | -1 ->
          Random.State.copy rand
        | s ->
          (* user provided random seed *)
          Random.State.make [| s |]
      in
      let verbose = conf_verbose ctxt in
      let long = conf_long ctxt in
      let print = {
        Raw.
        info = (fun fmt -> logf ctxt `Info fmt);
        fail = (fun fmt -> Printf.ksprintf assert_failure fmt);
        err = (fun fmt -> logf ctxt `Error fmt);
      } in
      T.check_cell_exn cell
        ~long ~rand ~call:(Raw.callback ~colors:false ~verbose ~print_res:true ~print))

let to_ounit2_test_list ?rand lst =
  List.rev (List.rev_map (to_ounit2_test ?rand) lst)

(* to convert a test to a [OUnit.test], we register a callback that will
   possibly print errors and counter-examples *)
let to_ounit_test_cell ?(verbose=verbose()) ?(long=long_tests())
    ?(rand=random_state()) cell =
  let module T = QCheck2.Test in
  let name = T.get_name cell in
  let run () =
    try
      T.check_cell_exn cell ~long ~rand
        ~call:(Raw.callback ~colors:false ~verbose ~print_res:verbose ~print:Raw.print_std);
      true
    with T.Test_fail _ ->
      false
  in
  name >:: (fun () -> assert_bool name (run ()))

let to_ounit_test ?verbose ?long ?rand (QCheck2.Test.Test c) =
  to_ounit_test_cell ?verbose ?long ?rand c

let (>:::) name l =
  name >::: (List.map (fun t -> to_ounit_test t) l)

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = Unix.gettimeofday () in
  let res = f x y in (* evaluate this first *)
  Unix.gettimeofday () -. begin_time, res

let run ?(argv=Sys.argv) test =
  let cli_args = Raw.parse_cli ~full_options:true argv in
  let colors = cli_args.Raw.cli_colors in
  (* print in colors *)
  let pp_color = Color.pp_str_c ~bold:true ~colors in
  let _counter = ref (0,0,0) in (* Success, Failure, Other *)
  let total_tests = test_case_count test in
  (* list of (test, execution time) *)
  let exec_times = ref [] in
  let update = function
    | RSuccess _ -> let (s,f,o) = !_counter in _counter := (succ s,f,o)
    | RFailure _ -> let (s,f,o) = !_counter in _counter := (s,succ f,o)
    | _ -> let (s,f,o) = !_counter in _counter := (s,f, succ o)
  in
  (* time each test *)
  let start = ref 0. and stop = ref 0. in
  (* display test as it starts and ends *)
  let display_test ?(ended=false) p  =
    let (s,f,o) = !_counter in
    let cartouche = va " [%d%s%s / %d] " s
      (if f=0 then "" else va "+%d" f)
      (if o=0 then "" else va " %d!" o) total_tests
    and path = string_of_path p in
    let end_marker =
      if cli_args.Raw.cli_print_list then (
        (* print a single line *)
        if ended then va " (after %.2fs)\n" (!stop -. !start) else "\n"
      ) else (
        ps Color.reset_line;
        if ended then " *" else ""
      )
    in
    let line = cartouche ^ path ^ end_marker in
    let remaining = 79 - String.length line in
    let cover = if remaining > 0 && not cli_args.Raw.cli_print_list
      then String.make remaining ' ' else "" in
    pf "%s%s%!" line cover;
  in
  let hdl_event = function
    | EStart p ->
      start := Unix.gettimeofday();
      display_test p
    | EEnd p  ->
      stop := Unix.gettimeofday();
      display_test p ~ended:true;
      let exec_time = !stop -. !start in
      exec_times := (p, exec_time) :: !exec_times
    | EResult result -> update result
  in
  ps "Running tests...";
  let running_time, results = time_fun perform_test hdl_event test in
  let (_s, f, o) = !_counter in
  let failures = List.filter not_success results in
  (*  assert (List.length failures = f);*)
  ps Color.reset_line;
  print_result_list ~colors failures;
  assert (List.length results = total_tests);
  pf "Ran: %d tests in: %.2f seconds.%s\n"
    total_tests running_time (String.make 40 ' ');
  (* XXX: suboptimal, but should work fine *)
  if cli_args.Raw.cli_slow_test > 0 then (
    pf "Display the %d slowest tests:\n" cli_args.Raw.cli_slow_test;
    let l = !exec_times in
    let l = List.sort (fun (_,t1)(_,t2) -> compare t2 t1) l in
    List.iteri
      (fun i (p,t) ->
         if i<cli_args.Raw.cli_slow_test
         then pf "  %s in %.2fs\n" (OUnit.string_of_path p) t)
      l
  );
  if failures = [] then (
    pf "%a\n" (pp_color `Green) "SUCCESS";
  );
  if o <> 0 then (
    pf "%a SOME TESTS ARE NEITHER SUCCESSES NOR FAILURES!\n"
      (pp_color `Yellow) "WARNING!";
  );
  if failures <> [] then (
    pf "%a\n" (pp_color `Red) "FAILURE";
  );
  (* create a meaningful return code for the process running the tests *)
  match f, o with
    | 0, 0 -> 0
    | _ -> 1

(* TAP-compatible test runner, in case we want to use a test harness *)

let run_tap test =
  let test_number = ref 0 in
  let handle_event = function
    | EStart _ | EEnd _ -> incr test_number
    | EResult (RSuccess p) ->
      pf "ok %d - %s\n%!" !test_number (string_of_path p)
    | EResult (RFailure (p,m)) ->
      pf "not ok %d - %s # %s\n%!" !test_number (string_of_path p) m
    | EResult (RError (p,m)) ->
      pf "not ok %d - %s # ERROR:%s\n%!" !test_number (string_of_path p) m
    | EResult (RSkip (p,m)) ->
      pf "not ok %d - %s # skip %s\n%!" !test_number (string_of_path p) m
    | EResult (RTodo (p,m)) ->
      pf "not ok %d - %s # todo %s\n%!" !test_number (string_of_path p) m
  in
  let total_tests = test_case_count test in
  pf "TAP version 13\n1..%d\n" total_tests;
  perform_test handle_event test

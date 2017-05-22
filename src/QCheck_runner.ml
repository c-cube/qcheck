(*
QCheck: Random testing for OCaml
Copyright (C) 2016  Vincent Hugot, Simon Cruanes

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open OUnit

let ps,pl = print_string,print_endline
let va = Printf.sprintf
let pf = Printf.printf

module Color = struct
  let fpf = Printf.fprintf
  type color =
    [ `Red
    | `Yellow
    | `Green
    | `Blue
    | `Normal
    | `Cyan
    ]

  let int_of_color_ = function
    | `Normal -> 0
    | `Red -> 1
    | `Green -> 2
    | `Yellow -> 3
    | `Blue -> 4
    | `Cyan -> 6

  (* same as [pp], but in color [c] *)
  let in_color c pp out x =
    let n = int_of_color_ c in
    fpf out "\x1b[3%dm" n;
    pp out x;
    fpf out "\x1b[0m"

  (* same as [pp], but in bold color [c] *)
  let in_bold_color c pp out x =
    let n = int_of_color_ c in
    fpf out "\x1b[3%d;1m" n;
    pp out x;
    fpf out "\x1b[0m"

  let pp_str_c ?(bold=true) ~colors c out s =
    if colors then
      if bold then in_bold_color c output_string out s
      else in_color c output_string out s
    else output_string out s
end

let separator1 = "\027[K" ^ (String.make 79 '\\')
let separator2 = String.make 79 '/'

let string_of_path path =
  let path = List.filter (function Label _ -> true | _ -> false) path in
  String.concat ">" (List.rev_map string_of_node path)

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

let not_success = function RSuccess _ -> false | _ -> true

let print_result_list ~colors =
  List.iter (fun result ->
    let c, res = result_flavour result in
    pf "%s\n%a: %s\n\n%s\n%s\n"
    separator1 (Color.pp_str_c ~colors c) res
    (string_of_path (result_path result))
    (result_msg result) separator2)

let seed = ref ~-1
let st = ref None

let set_seed_ s =
  seed := s;
  Printf.printf "\rrandom seed: %d\n%!" s;
  let state = Random.State.make [| s |] in
  st := Some state;
  state

let set_seed s = ignore (set_seed_ s)

let setup_random_state_ () =
  let s = if !seed = ~-1 then (
      Random.self_init ();  (* make new, truly random seed *)
      Random.int (1 lsl 29);
  ) else !seed in
  set_seed_ s

(* initialize random generator from seed (if any) *)
let random_state () = match !st with
  | Some st -> st
  | None -> setup_random_state_ ()

let verbose, set_verbose =
  let r = ref false in
  (fun () -> !r), (fun b -> r := b)

let long_tests, set_long_tests =
  let r = ref false in
  (fun () -> !r), (fun b -> r := b)

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = Unix.gettimeofday () in
  let res = f x y in (* evaluate this first *)
  Unix.gettimeofday () -. begin_time, res

type cli_args = {
  cli_verbose : bool;
  cli_long_tests : bool;
  cli_print_list : bool;
  cli_rand : Random.State.t;
  cli_slow_test : int; (* how many slow tests to display? *)
  cli_colors: bool;
}

let parse_cli ~full_options argv =
  let print_list = ref false in
  let set_verbose () = set_verbose true in
  let set_long_tests () = set_long_tests true in
  let set_backtraces () = Printexc.record_backtrace true in
  let set_list () = print_list := true in
  let colors = ref true in
  let slow = ref 0 in
  let options = Arg.align (
    [ "-v", Arg.Unit set_verbose, " "
    ; "--verbose", Arg.Unit set_verbose, " enable verbose tests"
    ; "--colors", Arg.Set colors, " colored output"
    ; "--no-colors", Arg.Clear colors, " disable colored output"
    ] @
    (if full_options then
      [ "-l", Arg.Unit set_list, " "
      ; "--list", Arg.Unit set_list, " print list of tests (2 lines each)"
      ; "--slow", Arg.Set_int slow, " print the <n> slowest tests"
      ] else []
    ) @
    [ "-s", Arg.Set_int seed, " "
    ; "--seed", Arg.Set_int seed, " set random seed (to repeat tests)"
    ; "--long", Arg.Unit set_long_tests, " run long tests"
    ; "-bt", Arg.Unit set_backtraces, " enable backtraces"
    ]
  ) in
  Arg.parse_argv argv options (fun _ ->()) "run qtest suite";
  let cli_rand = setup_random_state_ () in
  { cli_verbose=verbose(); cli_long_tests=long_tests(); cli_rand;
    cli_print_list= !print_list; cli_slow_test= !slow;
    cli_colors= !colors; }

let run ?(argv=Sys.argv) test =
  let cli_args = parse_cli ~full_options:true argv in
  let colors = cli_args.cli_colors in
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
      if cli_args.cli_print_list then (
        (* print a single line *)
        if ended then va " (after %.2fs)\n" (!stop -. !start) else "\n"
      ) else (
        ps "\r";
        if ended then " *" else ""
      )
    in
    let line = cartouche ^ path ^ end_marker in
    let remaining = 79 - String.length line in
    let cover = if remaining > 0 && not cli_args.cli_print_list
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
  ps "\r";
  print_result_list ~colors failures;
  assert (List.length results = total_tests);
  pf "Ran: %d tests in: %.2f seconds.%s\n"
    total_tests running_time (String.make 40 ' ');
  (* XXX: suboptimal, but should work fine *)
  if cli_args.cli_slow_test > 0 then (
    pf "Display the %d slowest tests:\n" cli_args.cli_slow_test;
    let l = !exec_times in
    let l = List.sort (fun (_,t1)(_,t2) -> compare t2 t1) l in
    List.iteri
      (fun i (p,t) ->
         if i<cli_args.cli_slow_test
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

type ('b,'c) printer = {
  info: 'a. ('a,'b,'c,unit) format4 -> 'a;
  fail: 'a. ('a,'b,'c,unit) format4 -> 'a;
  err: 'a. ('a,'b,'c,unit) format4 -> 'a;
}

(* main callback for individual tests
   @param verbose if true, print statistics and details
   @param print_res if true, print the result on [out] *)
let callback ~verbose ~print_res ~print name cell result =
  let module R = QCheck.TestResult in
  let module T = QCheck.Test in
  let arb = T.get_arbitrary cell in
  if verbose then (
    print.info "\rlaw %s: %d relevant cases (%d total)\n"
      name result.R.count result.R.count_gen;
    begin match QCheck.TestResult.collect result with
      | None -> ()
      | Some tbl ->
        print_string (QCheck.Test.print_collect tbl)
    end;
  );
  if print_res then (
    (* even if [not verbose], print errors *)
    match result.R.state with
      | R.Success -> ()
      | R.Failed l ->
        print.fail "\r  %s\n" (T.print_fail arb name l);
      | R.Error (i,e,st) ->
        print.err "\r  %s\n" (T.print_error ~st arb name (i,e));
  )

let name_of_cell cell =
  QCheck.Test.get_name cell

let print_std = { info = Printf.printf; fail = Printf.printf; err = Printf.printf }

(* to convert a test to a [OUnit.test], we register a callback that will
   possibly print errors and counter-examples *)
let to_ounit_test_cell ?(verbose=verbose()) ?(long=long_tests())
    ?(rand=random_state()) cell =
  let module T = QCheck.Test in
  let name = name_of_cell cell in
  let run () =
    try
      T.check_cell_exn cell ~long ~rand
        ~call:(callback ~verbose ~print_res:verbose ~print:print_std);
      true
    with T.Test_fail _ ->
      false
  in
  name >:: (fun () -> assert_bool name (run ()))

let to_ounit_test ?verbose ?long ?rand (QCheck.Test.Test c) =
  to_ounit_test_cell ?verbose ?long ?rand c

let (>:::) name l =
  name >::: (List.map (fun t -> to_ounit_test t) l)

let conf_seed = OUnit2.Conf.make_int "seed" ~-1 "set random seed"
let conf_verbose = OUnit2.Conf.make_bool "qcheck_verbose" false "enable verbose QCheck tests"
let conf_long = OUnit2.Conf.make_bool "qcheck_long" false "enable long QCheck tests"

let default_rand () =
  (* random seed, for repeatability of tests *)
  Random.State.make [| 89809344; 994326685; 290180182 |]

let to_ounit2_test ?(rand = default_rand()) (QCheck.Test.Test cell) =
  let module T = QCheck.Test in
  let name = name_of_cell cell in
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
        info = (fun fmt -> logf ctxt `Info fmt);
        fail = (fun fmt -> Printf.ksprintf assert_failure fmt);
        err = (fun fmt -> logf ctxt `Error fmt);
      } in
      T.check_cell_exn cell
        ~long ~rand ~call:(callback ~verbose ~print_res:true ~print))

let to_ounit2_test_list ?rand lst =
  List.rev (List.rev_map (to_ounit2_test ?rand) lst)

(* Counter for a test's instances *)
type counter = {
  start : float;
  expected : int;
  mutable gen : int;
  mutable passed : int;
  mutable failed : int;
  mutable errored : int;
}

type res =
  | Res : 'a QCheck.Test.cell * 'a QCheck.TestResult.t -> res

let pp_counter ~size out c =
  let t = Unix.gettimeofday () -. c.start in
  Printf.fprintf out "(%*d) %*d ; %*d ; %*d / %*d -- %7.1fs"
    size c.gen size c.errored size c.failed
    size c.passed size c.expected t

let step ~size ~out ~verbose c name _ _ r =
  let aux = function
    | QCheck.Test.Success -> c.passed <- c.passed + 1
    | QCheck.Test.Failure -> c.failed <- c.failed + 1
    | QCheck.Test.FalseAssumption -> ()
    | QCheck.Test.Error _ -> c.errored <- c.errored + 1
  in
  c.gen <- c.gen + 1;
  aux r;
  if verbose then
    Printf.fprintf out "\r[ ] %a -- %s%!" (pp_counter ~size) c name

let callback ~size ~out ~verbose c name _ _ =
  let pass = c.failed = 0 && c.errored = 0 in
  if verbose then
    Printf.fprintf out "\r[%s] %a -- %s\n%!"
      (if pass then "✓" else "✗") (pp_counter ~size) c name

let print_inst arb x =
  match arb.QCheck.print with
  | Some f -> f x
  | None -> "<no printer>"

let expect long cell =
  let count = QCheck.Test.get_count cell in
  if long then QCheck.Test.get_long_factor cell * count else count

let expect_size long cell =
  let rec aux n = if n < 10 then 1 else 1 + (aux (n / 10)) in
  aux (expect long cell)

let print_success out cell r =
  begin match QCheck.TestResult.collect r with
    | None -> ()
    | Some tbl ->
      Printf.fprintf out
        "\n+++ Collect %s\n\nCollect results for test %s:\n\n%s%!"
        (String.make 68 '+') (QCheck.Test.get_name cell) (QCheck.Test.print_collect tbl)
  end;
  List.iter
    (fun st -> print_string (QCheck.Test.print_stat st))
    (QCheck.TestResult.stats r);
  ()

let print_fail ~colors out cell c_ex =
  Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Red) "Failure" (String.make 68 '-');
  Printf.fprintf out "Test %s failed (%d shrink steps):\n\n%s\n%!"
    (QCheck.Test.get_name cell) c_ex.QCheck.TestResult.shrink_steps
    (print_inst (QCheck.Test.get_arbitrary cell) c_ex.QCheck.TestResult.instance)

let print_error ~colors out cell c_ex exn bt =
  Printf.fprintf out "\n=== %a %s\n\n" (Color.pp_str_c ~colors `Red) "Error" (String.make 70 '=');
  Printf.fprintf out "Test %s errored on (%d shrink steps):\n\n%s\n\nexception %s\n%s\n%!"
    (QCheck.Test.get_name cell)
    c_ex.QCheck.TestResult.shrink_steps
    (print_inst (QCheck.Test.get_arbitrary cell) c_ex.QCheck.TestResult.instance)
    (Printexc.to_string exn)
    bt

let run_tests
    ?(colors=true) ?(verbose=verbose()) ?(long=long_tests()) ?(out=stdout) ?(rand=random_state()) l =
  let module T = QCheck.Test in
  let module R = QCheck.TestResult in
  let pp_color = Color.pp_str_c ~bold:true ~colors in
  let size = List.fold_left (fun acc (T.Test cell) ->
      max acc (expect_size long cell)) 4 l in
  if verbose then
    Printf.fprintf out
      "%*s; %*s; %*s; %*s / %*s -     time -- test name\n%!"
      (size + 5) "generated" (size + 1) "error"
      (size + 1) "fail" size "pass" (size + 1) "total";
  let aux_map (T.Test cell) =
    let expected = expect long cell in
    let start = Unix.gettimeofday () in
    let c = {
      start; expected; gen = 0;
      passed = 0; failed = 0; errored = 0;
    } in
    let r = QCheck.Test.check_cell ~long ~rand
        ~step:(step ~size ~out ~verbose c)
        ~call:(callback ~size ~out ~verbose c)
        cell
    in
    Res (cell, r)
  in
  let res = List.map aux_map l in
  let aux_fold (total, fail, error) (Res (cell, r)) =
    match r.R.state with
    | R.Success ->
      print_success out cell r;
      (total + 1, fail, error)
    | R.Failed l ->
      List.iter (print_fail ~colors out cell) l;
      (total + 1, fail + 1, error)
    | R.Error (c_ex, exn, bt) ->
      print_error ~colors out cell c_ex exn bt;
      (total + 1, fail, error + 1)
  in
  let total, fail, error = List.fold_left aux_fold (0, 0, 0) res in
  Printf.fprintf out "%s\n" (String.make 80 '=');
  if fail = 0 && error = 0 then (
    Printf.fprintf out "%a (ran %d tests)\n%!"
      (pp_color `Green) "success" total;
    0
  ) else (
    Printf.fprintf out
      "%a (%d tests failed, %d tests errored, ran %d tests)\n%!"
      (pp_color `Red) "failure" fail error total;
    1
  )

let run_tests_main ?(argv=Sys.argv) l =
  try
    let cli_args = parse_cli ~full_options:false argv in
    exit
      (run_tests l
         ~colors:cli_args.cli_colors
         ~verbose:cli_args.cli_verbose
         ~long:cli_args.cli_long_tests ~out:stdout ~rand:cli_args.cli_rand)
  with
    | Arg.Bad msg -> print_endline msg; exit 1
    | Arg.Help msg -> print_endline msg; exit 0

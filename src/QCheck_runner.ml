(*
QCheck: Random testing for OCaml
copyright (c) 2013-2017, Guillaume Bury, Simon Cruanes, Vincent Hugot, Jan Midtgaard
all rights reserved.
*)

let ps = print_string
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

  let int_of_color_ : color -> int = function
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

  let reset_line = "\x1b[2K\r"

  let pp_str_c ?(bold=true) ~colors c out s =
    if colors then
      if bold then in_bold_color c output_string out s
      else in_color c output_string out s
    else output_string out s
end

let separator1 = "\027[K" ^ (String.make 79 '\\')
let separator2 = String.make 79 '/'

let seed = ref ~-1
let st = ref None

let set_seed_ s =
  seed := s;
  Printf.printf "%srandom seed: %d\n%!" Color.reset_line s;
  let state = Random.State.make [| s |] in
  st := Some state;
  state

(* time of last printed message. Useful for rate limiting in verbose mode *)
let last_msg = ref 0.

let time_between_msg = ref 0.1

let get_time_between_msg () = !time_between_msg

let set_time_between_msg f = time_between_msg := f

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

module Raw = struct
  type ('b,'c) printer = {
    info: 'a. ('a,'b,'c,unit) format4 -> 'a;
    fail: 'a. ('a,'b,'c,unit) format4 -> 'a;
    err: 'a. ('a,'b,'c,unit) format4 -> 'a;
  }

  type cli_args = {
    cli_verbose : bool;
    cli_long_tests : bool;
    cli_print_list : bool;
    cli_rand : Random.State.t;
    cli_slow_test : int; (* how many slow tests to display? *)
    cli_colors: bool;
  }

  (* main callback for individual tests
     @param verbose if true, print statistics and details
     @param print_res if true, print the result on [out] *)
  let callback ~verbose ~print_res ~print name cell result =
    let module R = QCheck.TestResult in
    let module T = QCheck.Test in
    let arb = T.get_arbitrary cell in
    if verbose then (
      print.info "%slaw %s: %d relevant cases (%d total)\n"
        Color.reset_line name result.R.count result.R.count_gen;
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
          print.fail "%s%s\n" Color.reset_line (T.print_fail arb name l);
        | R.Error (i,e,st) ->
          print.err "%s%s\n" Color.reset_line (T.print_error ~st arb name (i,e));
    )

  let print_std = { info = Printf.printf; fail = Printf.printf; err = Printf.printf }

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
end

open Raw

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
  Printf.fprintf out "%*d %*d %*d %*d / %*d %7.1fs"
    size c.gen size c.errored size c.failed
    size c.passed size c.expected t

let handler ~size ~out ~verbose c name _ r =
  let st = function
    | QCheck.Test.Generating    -> "generating"
    | QCheck.Test.Collecting _  -> "collecting"
    | QCheck.Test.Testing _     -> "   testing"
    | QCheck.Test.Shrunk (i, _) ->
      Printf.sprintf "shrinking: %4d" i
    | QCheck.Test.Shrinking (i, j, _) ->
      Printf.sprintf "shrinking: %4d.%04d" i j
  in
  (* use timestamps for rate-limiting *)
  let now=Unix.gettimeofday() in
  if verbose && now -. !last_msg > get_time_between_msg () then (
    last_msg := now;
    Printf.fprintf out "%s[ ] %a %s (%s)%!"
      Color.reset_line (pp_counter ~size) c name (st r)
  )


let step ~size ~out ~verbose c name _ _ r =
  let aux = function
    | QCheck.Test.Success -> c.passed <- c.passed + 1
    | QCheck.Test.Failure -> c.failed <- c.failed + 1
    | QCheck.Test.FalseAssumption -> ()
    | QCheck.Test.Error _ -> c.errored <- c.errored + 1
  in
  c.gen <- c.gen + 1;
  aux r;
  let now=Unix.gettimeofday() in
  if verbose && now -. !last_msg > get_time_between_msg () then (
    last_msg := now;
    Printf.fprintf out "%s[ ] %a %s%!"
      Color.reset_line (pp_counter ~size) c name
  )

let callback ~size ~out ~verbose ~colors c name _ _ =
  let pass = c.failed = 0 && c.errored = 0 in
  let color = if pass then `Green else `Red in
  if verbose then (
    Printf.fprintf out "%s[%a] %a %s\n%!"
      Color.reset_line
      (Color.pp_str_c ~bold:true ~colors color) (if pass then "✓" else "✗")
      (pp_counter ~size) c name
  )

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

(* print user messages for a test *)
let print_messages ~colors out cell l =
  if l<>[] then (
    Printf.fprintf out
      "\n+++ %a %s\n\nMessages for test %s:\n\n%!"
      (Color.pp_str_c ~colors `Blue) "Messages"
      (String.make 68 '+') (QCheck.Test.get_name cell);
    List.iter (Printf.fprintf out "%s\n%!") l
  )

let print_success ~colors out cell r =
  begin match QCheck.TestResult.collect r with
    | None -> ()
    | Some tbl ->
      Printf.fprintf out
        "\n+++ %a %s\n\nCollect results for test %s:\n\n%s%!"
        (Color.pp_str_c ~colors `Blue) "Collect"
        (String.make 68 '+') (QCheck.Test.get_name cell) (QCheck.Test.print_collect tbl)
  end;
  List.iter
    (fun st ->
       Printf.fprintf out
         "\n+++ %a %s\n\nStat for test %s:\n\n%s%!"
        (Color.pp_str_c ~colors `Blue) "Stat" (String.make 68 '+')
        (QCheck.Test.get_name cell) (QCheck.Test.print_stat st))
    (QCheck.TestResult.stats r);
  ()

let print_fail ~colors out cell c_ex =
  Printf.fprintf out "\n--- %a %s\n\n" (Color.pp_str_c ~colors `Red) "Failure" (String.make 68 '-');
  Printf.fprintf out "Test %s failed (%d shrink steps):\n\n%s\n%!"
    (QCheck.Test.get_name cell) c_ex.QCheck.TestResult.shrink_steps
    (print_inst (QCheck.Test.get_arbitrary cell) c_ex.QCheck.TestResult.instance);
  print_messages ~colors out cell c_ex.QCheck.TestResult.msg_l

let print_error ~colors out cell c_ex exn bt =
  Printf.fprintf out "\n=== %a %s\n\n" (Color.pp_str_c ~colors `Red) "Error" (String.make 70 '=');
  Printf.fprintf out "Test %s errored on (%d shrink steps):\n\n%s\n\nexception %s\n%s\n%!"
    (QCheck.Test.get_name cell)
    c_ex.QCheck.TestResult.shrink_steps
    (print_inst (QCheck.Test.get_arbitrary cell) c_ex.QCheck.TestResult.instance)
    (Printexc.to_string exn)
    bt;
  print_messages ~colors out cell c_ex.QCheck.TestResult.msg_l

let run_tests
    ?(colors=true) ?(verbose=verbose()) ?(long=long_tests()) ?(out=stdout) ?(rand=random_state()) l =
  let module T = QCheck.Test in
  let module R = QCheck.TestResult in
  let pp_color = Color.pp_str_c ~bold:true ~colors in
  let size = List.fold_left (fun acc (T.Test cell) ->
      max acc (expect_size long cell)) 4 l in
  if verbose then
    Printf.fprintf out
      "%*s %*s %*s %*s / %*s     time test name\n%!"
      (size + 4) "generated" size "error"
      size "fail" size "pass" size "total";
  let aux_map (T.Test cell) =
    let expected = expect long cell in
    let start = Unix.gettimeofday () in
    let c = {
      start; expected; gen = 0;
      passed = 0; failed = 0; errored = 0;
    } in
    if verbose then
      Printf.fprintf out "%s[ ] %a %s%!"
        Color.reset_line (pp_counter ~size) c (T.get_name cell);
    let r = QCheck.Test.check_cell ~long ~rand
        ~handler:(handler ~size ~out ~verbose c)
        ~step:(step ~size ~out ~verbose c)
        ~call:(callback ~size ~out ~verbose ~colors c)
        cell
    in
    Res (cell, r)
  in
  let res = List.map aux_map l in
  let aux_fold (total, fail, error) (Res (cell, r)) =
    let acc = match r.R.state with
      | R.Success ->
        print_success ~colors out cell r;
        (total + 1, fail, error)
      | R.Failed l ->
        List.iter (print_fail ~colors out cell) l;
        (total + 1, fail + 1, error)
      | R.Error (c_ex, exn, bt) ->
        print_error ~colors out cell c_ex exn bt;
        (total + 1, fail, error + 1)
    in
    acc
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

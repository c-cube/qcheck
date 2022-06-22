open QCheck2

(** For timing and summing run times *)
let time f () =
  let start_time = Sys.time () in
  let res = f () in
  let end_time = Sys.time () in
  (end_time -. start_time,res)

let sum_timing_pairs times =
  let sum_timings = List.fold_left (+.) 0.0 in
  let t1,t2 = List.split times in
  sum_timings t1,sum_timings t2

let get_name (Test.Test cell) = Test.get_name cell


(** Runners for single tests, test pairs, and test pair lists *)

(* run a single test with the given seed *)
let run_timed_test seed cell =
  let open TestResult in
  let rand = Random.State.make [| seed |] in
  (* For total attempts, count occ. of 'Shrinking' in "event protocol":
     Shrunk 0 - Shrinking 0.1 - Shrinking 0.2 - Shrunk 1 - Shrinking 1.1 - Shrinking 1.2  *)
  let shr_attempts = ref 0 in
  let handler _ _ e = match e with
    | Test.Shrinking (_,_,_) -> incr shr_attempts | _ -> () in
  let dur,res = time (fun () -> QCheck.Test.check_cell ~rand ~handler cell) () in
  let name = Test.get_name cell in
  let res_str,shr_c,_msg = match get_state res with
    | Success            -> failwith (Printf.sprintf "Test %s returned unexpected Success" name)
    | Error {exn;_}      -> failwith (Printf.sprintf "Test %s returned unexpected Error %s" name (Printexc.to_string exn))
    | Failed_other {msg} -> failwith (Printf.sprintf "Test %s returned unexpected Failed_other %s" name msg)
    | Failed {instances} -> "fail",(List.hd instances).shrink_steps, "Failed" (* expected *) in
  (dur,res_str,shr_c,!shr_attempts)

(* run a pair of corresponding tests with the given seed *)
let run_timed_test_pair seed (Test.Test c1, Test.Test c2) =
  let (dur1,res_str1,shr_c1,shr_att1) = run_timed_test seed c1 in
  let (dur2,res_str2,shr_c2,shr_att2) = run_timed_test seed c2 in
  if res_str1 <> res_str2
  then failwith (Printf.sprintf "benchmark %s gave different errors: %s and %s" (Test.get_name c1) res_str1 res_str2)
  else (res_str1,(dur1,shr_c1,shr_att1),(dur2,shr_c2,shr_att2))

let non_repeatable_tests = ["big bound issue59";"ints < 209609"]

(* run a list of corresponding test pairs over the given seed list *)
(* and print the benchmark result to channel [ch]                  *)
let run_timing ch seeds testpairs =
  let fprintf = Printf.fprintf in
  let multiple_runs = List.length seeds > 1 in
  (* print iteration header - name (48 chars) *)
  Printf.fprintf ch "%-48s" "";
  List.iter (fun seed -> fprintf ch "         iteration seed %-7i       %!" seed) seeds;
  if multiple_runs then fprintf ch "     total\n%!" else print_newline ();
  (* print column header - name + 38 chars per iteration *)
  fprintf ch "%-48s" "Shrink test name";
  List.iter (fun _ ->
               fprintf ch "  %-6s%-10s %!" "Q1/s" "#succ/#att";
               fprintf ch "  %-6s%-10s %!" "Q2/s" "#succ/#att") seeds;
  if multiple_runs then fprintf ch " %6s %6s" "Q1/s" "Q2/s";
  fprintf ch "\n%!";
  (* print separator *)
  fprintf ch "%s%!" (String.make 48 '-');
  List.iter (fun _ -> fprintf ch "%s%!" (String.make 38 '-')) seeds;
  if multiple_runs then fprintf ch "%s%!" (String.make 16 '-');
  fprintf ch "\n%!";
  (* print timings for each test_pair and seed *)
  let times =
    List.map
      (fun ((test1,_test2) as test_pair) ->
         let name = get_name test1 in
         let max_len = 48 in
         fprintf ch "%-48s%!" (if String.length name<max_len then name else String.sub name 0 max_len);
         if multiple_runs && List.mem name non_repeatable_tests
         then
           begin
             fprintf ch " - skipped as generator is stateful, making it non-repeatable\n%!";
             (0.0,0.0)
           end
         else
           let times =
             List.map (fun seed ->
                 let _res_str,(dur1,shr_cnt1,shr_att1),(dur2,shr_cnt2,shr_att2) = run_timed_test_pair seed test_pair in
                 fprintf ch " %6.3f %4i/%-6i%!" dur1 shr_cnt1 shr_att1;
                 fprintf ch " %6.3f %4i/%-6i%!" dur2 shr_cnt2 shr_att2;
                 (dur1,dur2)
               ) seeds in
           let t1_sum,t2_sum = sum_timing_pairs times in
           if multiple_runs then fprintf ch " %6.3f %6.3f%!" t1_sum t2_sum;
           fprintf ch "\n%!";
           (t1_sum,t2_sum))
      testpairs in
  let t1_sum,t2_sum = sum_timing_pairs times in
  fprintf ch "%s%!" (String.make (48 + 38*List.length seeds) ' ');
  fprintf ch " %6.3f %6.3f\n%!" t1_sum t2_sum

(* merge two corresponding lists of tests *)
let rec merge_and_validate xs ys = match xs,ys with
  | [],[] -> []
  | [],_  -> failwith "QCheck2_tests.Shrink has more tests than QCheck_tests.Shrink"
  | _,[]  -> failwith "QCheck_tests.Shrink has more tests than QCheck2_tests.Shrink"
  | t1::xs,t2::ys ->
    if get_name t1 = get_name t2
    then (t1,t2) :: merge_and_validate xs ys
    else
      let msg = Printf.sprintf "Found \"%s\" and \"%s\". Are QCheck_tests.Shrink and QCheck2_tests.Shrink not in the same order?" (get_name t1) (get_name t2) in
      failwith msg

let seeds = [1234;(*4321;*)8743;(*9876;*)6789;
             (*2143*) (* ouch: seed 2143 causes test "lists equal to duplication" to segfault *)
            ]
let () =
  let ch = open_out "shrink_bench.log" in
  try
    merge_and_validate
      QCheck_tests.(Shrink.tests@Function.tests)
      QCheck2_tests.(Shrink.tests@Function.tests)
    |> run_timing ch seeds;
    close_out ch
  with e ->
    close_out ch;
    raise e

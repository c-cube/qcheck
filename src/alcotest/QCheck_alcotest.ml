
module Q = QCheck2
module T = QCheck2.Test
module Raw = QCheck_base_runner.Raw

let seed_ = lazy (
  let s =
    try int_of_string @@ Sys.getenv "QCHECK_SEED"
    with _ ->
      Random.self_init();
      Random.int 1_000_000_000
  in
  Printf.printf "qcheck random seed: %d\n%!" s;
  s
)

let default_rand () =
  (* random seed, for repeatability of tests *)
  Random.State.make [| Lazy.force seed_  |]

let verbose_ = lazy (
  match Sys.getenv "QCHECK_VERBOSE" with
  | "1" | "true" -> true
  | _ -> false
  | exception Not_found -> false
)

let long_ = lazy (
  match Sys.getenv "QCHECK_LONG" with
  | "1" | "true" -> true
  | _ -> false
  | exception Not_found -> false
)

let to_alcotest
    ?(colors=false) ?(verbose=Lazy.force verbose_) ?(long=Lazy.force long_)
    ?(debug_shrink = None) ?debug_shrink_list ?(rand=default_rand())
    (t:T.t) =
  let T.Test cell = t in
  let handler name cell r =
    match r, debug_shrink with
    | QCheck2.Test.Shrunk (step, x), Some out ->
      let go = match debug_shrink_list with
        | None -> true
        | Some test_list -> List.mem name test_list in
      if not go then ()
      else
        QCheck_base_runner.debug_shrinking_choices
          ~colors ~out ~name cell ~step x
    | _ ->
      ()
  in
  let print = Raw.print_std in
  let name = T.get_name cell in
  let run () =
    let call = Raw.callback ~colors ~verbose ~print_res:true ~print in
    T.check_cell_exn ~long ~call ~handler ~rand cell
  in
  ((name, `Slow, run) : unit Alcotest.test_case)

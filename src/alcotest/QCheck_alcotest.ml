
module Q = QCheck
module T = QCheck.Test
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
    ?(verbose=Lazy.force verbose_) ?(long=Lazy.force long_) ?(rand=default_rand())
    (t:T.t) =
  let T.Test cell = t in
  let print = Raw.print_std in
  let run() =
    T.check_cell_exn cell
      ~long ~rand ~call:(Raw.callback ~verbose ~print_res:true ~print)
  in
  let name = T.get_name cell in
  name, `Slow, run

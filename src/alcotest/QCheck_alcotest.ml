
module Q = QCheck
module T = QCheck.Test
module Raw = QCheck_base_runner.Raw

let default_rand () =
  (* random seed, for repeatability of tests *)
  Random.State.make [| 89809344; 994326685; 290180182 |]

let to_alcotest
    ?(verbose=true) ?(long=false) ?(rand=default_rand())
    (t:T.t) =
  let T.Test cell = t in
  let print = Raw.print_std in
  let run() =
    T.check_cell_exn cell
      ~long ~rand ~call:(Raw.callback ~verbose ~print_res:true ~print)
  in
  let name = T.get_name cell in
  name, `Slow, run

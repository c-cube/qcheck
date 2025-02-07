open QCheck2

(*
From https://github.com/haskell/random/issues/25
Assess splitting behaviour by generating stat tests of the form:

let test_to_1 =
  Test.make ~count:2000 ~name:"2000 pairs in [0;1]" ~collect
    Gen.(pair (int_bound 1) (int_bound 1)) (fun _ -> true)
*)

let collect (x,y) = if x=y then "equal    " else "not-equal"

let gen_test i =
  let count = 1000 + i * 1000 in
  let name = Printf.sprintf "%i pairs in [0;%i] - should be around 1000" count i in
  Test.make ~count ~name ~collect
    Gen.(pair (int_bound i) (int_bound i)) (fun _ -> true)

let _ =
  QCheck_base_runner.run_tests ~verbose:true (List.init 14 (fun i -> gen_test (i+1)))

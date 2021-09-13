open QCheck2

let pair_lists_no_overlap =
  Test.make ~name:"pairs lists no overlap"
    Gen.(pair (list small_nat) (list small_nat))
    (Log.shrinks Print.(pair (list int) (list int)) @@
       fun (xs,ys) -> List.for_all (fun x -> not (List.mem x ys)) xs)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand pair_lists_no_overlap with Test.Test_fail _ -> ()

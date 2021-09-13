open QCheck

let pair_sum_lt_128 =
  Test.make ~name:"pairs sum to less than 128"
    (pair pos_int pos_int) (Log.shrinks Print.(pair int int) @@ fun (i,j) -> i+j<128)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand pair_sum_lt_128 with Test.Test_fail _ -> ()

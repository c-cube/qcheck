open QCheck

let size_gen = Gen.(oneof [small_nat; int_bound 750_000])
let list_shorter_100 =
  Test.make ~name:"lists shorter than 100"
    (list_of_size size_gen small_int) (Log.shrinks_list_length @@ fun xs -> List.length xs < 100)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand list_shorter_100 with Test.Test_fail _ -> ()

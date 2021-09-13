open QCheck

let ints_are_0 =
  Test.make ~name:"ints are 0"
    int (Log.shrinks_int @@ fun i -> i = 0)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand ints_are_0 with Test.Test_fail _ -> ()

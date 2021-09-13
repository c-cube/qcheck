open QCheck

(* test from issue #59 *)
let ints_smaller_209609 =
  Test.make ~name:"ints < 209609"
    (small_int_corners()) (Log.shrinks_int @@ fun i -> i < 209609)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand ints_smaller_209609 with Test.Test_fail _ -> ()

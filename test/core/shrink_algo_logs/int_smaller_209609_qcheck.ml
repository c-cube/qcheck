open QCheck

(* test from issue #59 *)
let ints_smaller_209609 =
  Test.make ~name:"ints < 209609"
    (small_int_corners()) (Log.shrinks_int @@ fun i -> i < 209609)
;;
try Test.check_exn ints_smaller_209609 with Test.Test_fail _ -> ()

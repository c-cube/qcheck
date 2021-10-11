open QCheck2

let ints_are_0 =
  Test.make ~name:"ints are 0"
    Gen.int (Log.shrinks_int @@ fun i -> i = 0)
;;
try Test.check_exn ints_are_0 with Test.Test_fail _ -> ()

open QCheck

let list_shorter_10 =
  Test.make ~name:"lists shorter than 10"
    (list small_int) (Log.shrinks Print.(list int) @@ fun xs -> List.length xs < 10)
;;
try Test.check_exn list_shorter_10 with Test.Test_fail _ -> ()

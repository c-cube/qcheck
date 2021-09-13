open QCheck

let list_shorter_100 =
  Test.make ~name:"lists shorter than 100"
    (list small_int) (Log.shrinks_list_length @@ fun xs -> List.length xs < 100)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand list_shorter_100 with Test.Test_fail _ -> ()

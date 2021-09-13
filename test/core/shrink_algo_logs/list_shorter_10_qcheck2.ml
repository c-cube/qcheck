open QCheck2

let list_shorter_10 =
  Test.make ~name:"lists shorter than 10"
    Gen.(list small_int) (Log.shrinks Print.(list int) @@ fun xs -> List.length xs < 10)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand list_shorter_10 with Test.Test_fail _ -> ()

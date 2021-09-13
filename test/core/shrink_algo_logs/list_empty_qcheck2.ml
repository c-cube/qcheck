open QCheck2

let lists_are_empty_issue_64 =
  Test.make ~name:"lists are empty"
    Gen.(list small_int) (Log.shrinks Print.(list int) @@ fun xs -> xs = [])

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand lists_are_empty_issue_64 with Test.Test_fail _ -> ()

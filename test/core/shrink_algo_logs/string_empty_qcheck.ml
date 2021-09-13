open QCheck

let strings_are_empty =
  Test.make ~name:"strings are empty"
    string (Log.shrinks_string @@ fun s -> s = "")

let rand = Random.State.make [|1234|];;
try QCheck2.Test.check_exn ~rand strings_are_empty with Test.Test_fail _ -> ()

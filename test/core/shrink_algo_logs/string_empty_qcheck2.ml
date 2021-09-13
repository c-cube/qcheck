open QCheck2

let strings_are_empty =
  Test.make ~name:"strings are empty"
    Gen.string (Log.shrinks_string @@ fun s -> s = "")

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand strings_are_empty with Test.Test_fail _ -> ()

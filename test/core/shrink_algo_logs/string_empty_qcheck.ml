open QCheck

let strings_are_empty =
  Test.make ~name:"strings are empty"
    string (Log.shrinks_string @@ fun s -> s = "")
;;
try Test.check_exn strings_are_empty with Test.Test_fail _ -> ()

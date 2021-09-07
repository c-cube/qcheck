open QCheck

let lists_are_empty_issue_64 =
  Test.make ~name:"lists are empty"
    (list small_int) (Log.shrinks Print.(list int) @@ fun xs -> xs = [])
;;
try Test.check_exn lists_are_empty_issue_64 with Test.Test_fail _ -> ()

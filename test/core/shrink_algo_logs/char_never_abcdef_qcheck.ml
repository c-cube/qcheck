open QCheck

let char_is_never_abcdef =
  Test.make ~name:"char is never 'abcdef'"
    char (Log.shrinks_char @@ fun c -> not (List.mem c ['a';'b';'c';'d';'e';'f']))
;;
try Test.check_exn char_is_never_abcdef with Test.Test_fail _ -> ()

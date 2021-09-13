open QCheck

let char_is_never_abcdef =
  Test.make ~name:"char is never produces 'abcdef'"
    char (Log.shrinks_char @@ fun c -> not (List.mem c ['a';'b';'c';'d';'e';'f']))

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand char_is_never_abcdef with Test.Test_fail _ -> ()

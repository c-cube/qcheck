open QCheck

let string_never_has_000_char =
  Test.make ~name:"string never has a \\000 char"
    string
    (Log.shrinks_string @@
     fun s -> String.to_seq s |> Seq.fold_left (fun acc c -> acc && c <> '\000') true)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand string_never_has_000_char with Test.Test_fail _ -> ()

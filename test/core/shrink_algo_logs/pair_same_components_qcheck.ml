open QCheck

let pair_same =
  Test.make ~name:"pairs have same components"
    (pair int int) (Log.shrinks Print.(pair int int) @@ fun (i,j) -> i=j)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand pair_same with Test.Test_fail _ -> ()

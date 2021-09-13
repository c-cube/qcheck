open QCheck2

let pair_one_zero =
  Test.make ~name:"pairs have a zero component"
    Gen.(pair int int)
    (Log.shrinks Print.(pair int int) @@
     fun (i,j) -> i=0 || j=0)

let rand = Random.State.make [|1234|];;
try Test.check_exn ~rand pair_one_zero with Test.Test_fail _ -> ()

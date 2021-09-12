open QCheck

let pair_one_zero =
  Test.make ~name:"pairs have a zero component" (pair int int)
    (Log.shrinks Print.(pair int int) @@ fun (i,j) -> i=0 || j=0)
;;
try Test.check_exn pair_one_zero with Test.Test_fail _ -> ()

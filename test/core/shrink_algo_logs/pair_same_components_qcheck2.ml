open QCheck2

let pair_same =
  Test.make ~name:"pairs have same components"
    Gen.(pair int int)
    (Log.shrinks Print.(pair int int) @@
      fun (i,j) -> i=j)
;;
try Test.check_exn pair_same with Test.Test_fail _ -> ()

open QCheck


let pair_lists_rev_concat =
  Test.make ~name:"pairs lists rev concat"
    (pair (list pos_int) (list pos_int))
    (Log.shrinks Print.(pair (list int) (list int)) @@
     fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))
;;
try Test.check_exn pair_lists_rev_concat with Test.Test_fail _ -> ()

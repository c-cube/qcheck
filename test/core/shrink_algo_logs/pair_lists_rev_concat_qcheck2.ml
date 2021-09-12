open QCheck2

let pair_lists_rev_concat =
  Test.make ~name:"pairs lists rev concat"
    Gen.(pair (list (pint ~origin:0)) (list (pint ~origin:0)))
    (Log.shrinks Print.(pair (list int) (list int)) @@
       fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))
;;
try Test.check_exn pair_lists_rev_concat with Test.Test_fail _ -> ()

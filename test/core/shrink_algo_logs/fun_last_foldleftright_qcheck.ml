open QCheck

let prop_foldleft_foldright_uncurry_funlast =
  Test.make ~name:"fold_left fold_right uncurried fun last"
    (triple
       small_nat
       (list small_nat)
       (fun1 Observable.(pair int int) small_nat))
    (Log.shrinks Print.(triple int (list int) Fn.print) @@
     fun (z,xs,f) ->
       List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
       List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)
;;
try Test.check_exn prop_foldleft_foldright_uncurry_funlast with Test.Test_fail _ -> ()

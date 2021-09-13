open QCheck

let prop_foldleft_foldright_uncurry_funfirst =
  Test.make ~name:"fold_left fold_right uncurried fun first"
    (triple
       (fun1 Observable.(pair int int) small_nat)
       small_nat
       (list small_nat))
    (Log.shrinks Print.(triple Fn.print int (list int)) @@
     fun (f,z,xs) ->
       List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
       List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

let rand = Random.State.make [|1234|];;
try Test.check_exn (*~rand*) prop_foldleft_foldright_uncurry_funfirst with Test.Test_fail _ -> ()

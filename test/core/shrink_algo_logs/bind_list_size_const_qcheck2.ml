open QCheck2

let bind_pair_list_size =
  Test.make ~name:"bind list_size constant"
    Gen.(int_bound 1000 >>= fun len ->
         list_size (return len) (int_bound 1000) >>= fun xs -> return (len,xs))
    (Log.shrinks Print.(pair int (list int)) @@
     fun (len,xs) -> let len' = List.length xs in len=len' && len' < 4)
;;
try Test.check_exn bind_pair_list_size with Test.Test_fail _ -> ()

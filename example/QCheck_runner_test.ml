
let passing =
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.Test.make ~count:10
    ~name:"should_fail_sort_id"
    QCheck.(small_list small_int)
    (fun l -> l = List.sort compare l);;

exception Error

let error =
  QCheck.Test.make ~count:10
    ~name:"should_error_raise_exn"
    QCheck.int
    (fun _ -> raise Error)

let collect =
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"collect_results"
    QCheck.(make ~collect:string_of_int (Gen.int_bound 4))
    (fun _ -> true)

let stats =
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"with_stats"
    QCheck.(make (Gen.int_bound 120)
        ~stats:[
          "mod4", (fun i->i mod 4);
          "num", (fun i->i);
        ]
    )
    (fun _ -> true)

let fun1 =
  QCheck.Test.make ~count:100 ~long_factor:100
    ~name:"FAIL_pred_map_commute"
    QCheck.(triple
        (small_list small_int)
        (fun1 Observable.int int)
        (fun1 Observable.int bool))
    (fun (l,QCheck.Fun (_,f), QCheck.Fun (_,p)) ->
       List.filter p (List.map f l) = List.map f (List.filter p l))

let fun2 =
  QCheck.Test.make ~count:100
    ~name:"FAIL_fun2_pred_strings"
    QCheck.(fun1 Observable.string bool)
    (fun (QCheck.Fun (_,p)) ->
       not (p "some random string") || p "some other string")

let int_gen = QCheck.small_nat (* int *)

(* Another example (false) property *)
let prop_foldleft_foldright =
  let open QCheck in
  Test.make ~name:"fold_left fold_right" ~count:1000 ~long_factor:20
    (triple 
       (fun2 Observable.int Observable.int int_gen)
       int_gen
       (list int_gen))
    (fun (f,z,xs) -> List.fold_right (Fn.apply f) xs z = List.fold_left (Fn.apply f) z xs)

(* Another example (false) property *)
let prop_foldleft_foldright_uncurry =
  let open QCheck in
  Test.make ~name:"fold_left fold_right uncurried" ~count:1000 ~long_factor:20
    (triple 
       (fun1 Observable.(pair int int) int_gen)
       int_gen
       (list int_gen))
    (fun (f,z,xs) ->
       List.fold_right (fun x y -> Fn.apply f (x,y)) xs z =
       List.fold_left (fun x y -> Fn.apply f (x,y)) z xs)

let long_shrink =
  let open QCheck in
  let listgen = list_of_size (Gen.int_range 1000 10000) int in
  Test.make ~name:"long_shrink" (pair listgen listgen)
    (fun (xs,ys) -> List.rev (xs@ys) = (List.rev xs)@(List.rev ys))

let () =
  QCheck_runner.run_tests_main [
    passing;
    failing;
    error;
    collect;
    stats;
    fun1;
    fun2;
    prop_foldleft_foldright;
    prop_foldleft_foldright_uncurry;
    long_shrink;
  ]

